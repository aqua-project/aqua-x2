import Chisel._

object Implicits {
  implicit class RichUInt(self: UInt) {
    def sext(width: Int): UInt = {
      val w = self.getWidth
      Fill(width - w, self(w - 1)) ## self
    }
  }
}

class IMemIO extends Bundle {
  val valid = Bool(INPUT)
  val ready = Bool(OUTPUT)
  val addr = UInt(INPUT, width = 32)
  val data = UInt(OUTPUT, width = 32)
}

class DMemIO extends Bundle {
  val ready = Bool(OUTPUT)
  val req = new Bundle {
    val addr = UInt(INPUT, width = 32)
    val data = UInt(INPUT, width = 32)
    // memory access is done when reg_ma is true
    val reg_ma = Bool(INPUT)
    // reg_write is true when writing to register (So memory access type is read)
    val reg_write = Bool(INPUT)
    val reg_dest = UInt(INPUT, width = 5)
  }
  val resp = new Bundle {
    val reg_write = Bool(OUTPUT)
    val reg_dest = UInt(OUTPUT, width = 32)
    val data = UInt(OUTPUT, width = 32)
  }
}

class DRamIO extends Bundle {
  val req = Bool (OUTPUT);
  val grant = Bool (INPUT);
  val we = Bool (OUTPUT);
  val addr = UInt (OUTPUT,width = 22);
  val wdataToDRAM = Valid(UInt(width = 16));
  val rdataToDRAM = Valid(UInt(width = 16)).flip;
}

class ICache extends Module {

  val io = new Bundle {
    val core = new IMemIO();
    val ram  = new DRamIO();
  };
  val waitCnt = 1000;
  val wayBits = 1;
  val lineBits = 1;

  val cache = Module (new Cache (wayBits,lineBits,waitCnt));
  cache.io.cmdin.valid := io.core.valid;
  cache.io.cmdin.bits.we := Bool(false);
  cache.io.cmdin.bits.addr := io.core.addr;
  
  io.ram.req := cache.io.req;
  cache.io.grant := io.ram.grant;
  io.ram.we := Bool(false);
  io.ram.addr := cache.io.cmdout.bits.addr;
//  io.ram.wdataToDRAM := cache.io.wdataToDRAM;
  cache.io.rdataToDRAM := io.ram.rdataToDRAM;
  
  io.core.ready := cache.io.cmdin.ready;
  io.core.data := cache.io.rdataFromCore.bits;
}


class DCache extends Module {

  val io = new Bundle {
    val core = new DMemIO;
    val ram  = new DRamIO;
  };
  val waitCnt = 1000;
  val wayBits = 1;
  val lineBits = 1;
  val reg_write = Reg(next = io.core.req.reg_write)
  val reg_dest = Reg(next = io.core.req.reg_dest)

  val mem_read = io.core.req.reg_ma && io.core.req.reg_write

  val cache = Module (new Cache (wayBits,lineBits,waitCnt));

  cache.io.cmdin.valid := io.core.req.reg_ma;
  cache.io.cmdin.bits.we := ! io.core.req.reg_write;
  cache.io.cmdin.bits.addr := io.core.req.addr;
  cache.io.wdataFromCore.bits := io.core.req.data;
  cache.io.wdataFromCore.valid := Bool(true);
  
  io.ram.req := cache.io.req;
  cache.io.grant := io.ram.grant;

  io.ram.we := Bool(false);
  io.ram.addr := cache.io.cmdout.bits.addr;
  io.ram.wdataToDRAM := cache.io.wdataToDRAM;
  cache.io.rdataToDRAM := io.ram.rdataToDRAM;
  

  io.core.ready := cache.io.cmdin.ready;
  io.core.resp.data := cache.io.rdataFromCore.bits;

  // TODO: usankusai
  io.core.resp.reg_write := reg_write
  io.core.resp.reg_dest := reg_dest
}


class Core extends Module {

  import Implicits._

  val io = new Bundle {
    val imem = new IMemIO().flip
    val dmem = new DMemIO().flip
  }


  // Forward Declarations

  val pc_src = Reg(Bool())
  val pc_addr = Reg(UInt(width = 32))

  val ex_reg_write = Reg(Bool())
  val ex_reg_dest = Reg(UInt(width = 5))

  val stop_fetch = Wire(Bool())
  val stop_decode = Wire(Bool())

  def stop(regs: Data*) {
    for (reg <- regs) { reg := reg }
  }



  /** Stage 1. Fetch */

  val nextpc = Reg(UInt(width = 32))

  val pc = Mux(pc_src, pc_addr, nextpc)
  val pc4 = pc + UInt(4)

  io.imem.valid := !stop_fetch
  io.imem.addr := pc

  unless (stop_fetch) {
    nextpc := pc4
  }


  /** Stage 2. Decode & Read & Forward & Branch & Hazard */

  val idata = io.imem.data
  val opcode = idata(31, 26)
  val use_rx = opcode(5).toBool
  val use_ra = opcode(4).toBool
  val use_rb = opcode(3).toBool
  val rs1 = idata(25, 21)
  val rs2 = idata(20, 16)
  val lit = idata(20, 9).sext(32)
  val func = idata(15, 9)
  val tag = idata(8, 5)
  val rd = idata(4, 0)
  val disp_n = idata(25, 5).sext(32)
  val disp_l = idata(20, 5).sext(32)
  val disp_c = idata(20, 0).sext(32)
  val disp_s = idata(15, 0).sext(32)

  // Read
  val rf = Mem(UInt(width = 32), 32)
  val raw_rv1 = rf(rs1)
  val raw_rv2 = rf(rs2)

  // Forward
  val rs1_wb_fw = io.dmem.resp.reg_write && io.dmem.resp.reg_dest === rs1
  val rv1 = Mux(rs1_wb_fw, io.dmem.resp.data, raw_rv1)
  val rs2_wb_fw = io.dmem.resp.reg_write && io.dmem.resp.reg_dest === rs2
  val rv2 = Mux(rs2_wb_fw, io.dmem.resp.data, raw_rv2)

  // Decode
  val disp16 = Mux(use_rx, disp_l, disp_s)
  val immv = Mux(opcode(0), disp_n << 11, disp_n)
  val rv2_or_lit = Mux(use_rb, rv2, lit)
  val rd_is_r31 = rd === UInt(31)
  val reg_write = use_rx && !rd_is_r31
  val ucjmp = opcode(2, 0) === UInt("b000")
  val cjmp = opcode(2, 0) === UInt("b010")
  val reg_ma = opcode(2) && opcode(1) && (opcode(5) || opcode(3))
  val reg_ex = opcode(5, 2) === UInt("b1110") || opcode(5, 1) === UInt("b11001")
  val reg_id = ucjmp || opcode(5, 4) === UInt("b10") || opcode === UInt("b110001")
  val data = Mux(ucjmp, nextpc, immv)

  val id_va = Reg(next = rv1)
  val id_vb = Reg(next = rv2_or_lit)
  val id_tag = Reg(next = tag)
  val id_disp16 = Reg(next = disp16)
  val id_data = Reg(next = data)
  val id_reg_id = Reg(next = reg_id)
  val id_reg_ex = Reg(next = reg_ex)
  val id_reg_ma = Reg(next = reg_ma)
  val id_reg_write = Reg(next = reg_write)
  val id_reg_dest = Reg(next = rd)

  // Branch
  val eq0 = rv1 === UInt(0)
  val lt0 = rv1 < UInt(0)
  val le0 = rv1 <= UInt(0)
  val cjcases = (0 until 8) map (UInt(_)) zip Seq(Bool(false), eq0, lt0, le0, Bool(false), !eq0, !lt0, !le0)
  val cjtaken = MuxLookup(opcode(2, 0), Bool(false), cjcases)
  val jtaken = ucjmp || (cjmp && cjtaken)
  val disp21 = Mux(use_rx, disp_n, disp_c)
  val npc_disp = nextpc + (disp21 << 2)
  val jaddr = Mux(opcode(5, 4) === UInt("b11"), rv1, npc_disp)
  pc_src := jtaken
  pc_addr := jaddr

  // Hazard
  val rawh1 = id_reg_write && (id_reg_dest === rs1 || id_reg_dest === rs2) && use_ra
  val rawh2 = ex_reg_write && (ex_reg_dest === rs1 || ex_reg_dest === rs2) && use_rb
  val data_hazard = rawh1 || rawh2             // We could make this more precisely, but leave it for simplicity.
  stop_fetch := !io.imem.ready || !io.dmem.ready || data_hazard
  stop_decode := !io.dmem.ready
  val kill_decode = data_hazard || pc_src

  when (kill_decode) {
    pc_src := Bool(false)
    id_reg_ma := Bool(false)
    id_reg_write := Bool(false)
  }

  when (stop_decode) {
    stop(id_va, id_vb, id_tag, id_disp16, id_data)
    stop(id_reg_id, id_reg_ex, id_reg_ma, id_reg_write, id_reg_dest)
    stop(pc_src, pc_addr)
  }


  // --- point of no return --- //



  val stop_execute = !io.dmem.ready


  /** Stage 3. Execute */

  val alu = Module(new ALU)
  alu.io.tag := id_tag
  alu.io.a := id_va
  alu.io.b := id_vb

  val raddr = id_va + id_disp16
  val rdata = Mux(id_reg_ex, alu.io.c, id_data)

  val ex_addr = Reg(next = raddr)
  val ex_data = Reg(next = rdata)
  val ex_reg_ma = Reg(next = id_reg_ma)
  ex_reg_write := id_reg_write
  ex_reg_dest := id_reg_dest

  when (stop_execute) {
    stop(ex_addr, ex_data, ex_reg_ma, ex_reg_write, ex_reg_dest)
  }

  /** Stage 4. Memory */

  io.dmem.req.addr := ex_addr
  io.dmem.req.data := ex_data
  io.dmem.req.reg_ma := ex_reg_ma
  io.dmem.req.reg_write := ex_reg_write
  io.dmem.req.reg_dest := ex_reg_dest


  /** Stage 5. Write */

  when (io.dmem.resp.reg_write) { rf(io.dmem.resp.reg_dest) := io.dmem.resp.data }

}

class CPU extends Module {

  val io = new Bundle {
  }

  val core0 = Module(new Core)
  val icache0 = Module(new ICache)
  val dcache0 = Module(new DCache)

  core0.io.imem <> icache0.io
  core0.io.dmem <> dcache0.io

}

class CPUTests(c: CPU) extends Tester(c) {
  step(1)
}

object CPU {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new CPU)) { c =>
      new CPUTests(c)
    }
  }
}

