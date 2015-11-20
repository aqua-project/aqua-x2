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
    val mem_valid = Bool(INPUT)
    val reg_write = Bool(INPUT)
    val reg_dest = UInt(INPUT, width = 5)
  }
  val resp = new Bundle {
    val reg_write = Bool(INPUT)
    val reg_dest = UInt(OUTPUT, width = 32)
    val data = UInt(OUTPUT, width = 32)
  }
}


class ALU extends Module {

  val io = new Bundle {
    val tag = UInt(INPUT, width = 4)
    val a = UInt(INPUT, width = 32)
    val b = UInt(INPUT, width = 32)
    val c = UInt(OUTPUT, width = 32)
  }

  val (tag, a, b) = (io.tag, io.a, io.b)

  val vadd = a + b
  val vsub = a - b
  val vsll = a << b
  val vsrl = a >> b
  val vsra = (a.toSInt >> b).toUInt
  val vand = a & b
  val vior = a | b
  val vxor = a ^ b

  val cases = (0 until 8) map (UInt(_)) zip Seq(vadd, vsub, vsll, vsrl, vsra, vand, vior, vxor)

  io.c := MuxLookup(tag, UInt(0), cases)

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


  /** Stage 2. Decode & Read & Branch & Hazard */

  val idata = io.imem.data
  val opcode = idata(31, 26)
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
  val rv1 = rf(rs1)
  val rv2 = rf(rs2)

  // Decode
  val disp16 = Mux(idata(26), disp_s, disp_l)
  val raw_disp21 = Mux(idata(31, 29) === UInt("b101"), disp_c, disp_l)
  val disp21 = Mux(idata(26), raw_disp21 << 11, raw_disp21)
  val rv2_or_lit = Mux(idata(26), rv2, lit)
  val mem_valid = idata(31, 29) === UInt("b011")
  val rd_is_r31 = rd === UInt(31)
  val reg_write = (idata(29) || (mem_valid && idata(28, 26) =/= UInt("b001"))) && !rd_is_r31
  val ucjmp = idata(31, 29) === UInt("b100")
  val cjmp = idata(31, 29) === UInt("b101")
  val ild = idata(31, 29) === UInt("b010")
  val reg_alu = idata(31, 29) === UInt("b000")
  val mdata = Mux(ucjmp, nextpc, disp21)

  val id_va = Reg(next = rv1)
  val id_vb = Reg(next = rv2_or_lit)
  val id_tag = Reg(next = tag)
  val id_disp16 = Reg(next = disp16)
  val id_mdata = Reg(next = mdata)
  val id_reg_alu = Reg(next = reg_alu)
  val id_mem_valid = Reg(next = mem_valid)
  val id_reg_write = Reg(next = reg_write)
  val id_reg_dest = Reg(next = rd)

  // Branch
  val eq0 = rv1 === UInt(0)
  val lt0 = rv1 < UInt(0)
  val le0 = rv1 <= UInt(0)
  val cjcases = (0 until 6) map (UInt(_)) zip Seq(eq0, !eq0, lt0, le0, !le0, !lt0)
  val cjtaken = MuxLookup(idata(28, 26), Bool(false), cjcases)
  val jtaken = ucjmp || (cjmp && cjtaken)
  val disp21x4 = raw_disp21 << 2
  val npc_disp = nextpc + disp21x4
  val jaddr = Mux(idata === UInt("b100001"), rv1, npc_disp)
  pc_src := jtaken
  pc_addr := jaddr

  // Hazard
  val rawh1 = id_reg_write && (id_reg_dest === rs1 || id_reg_dest === rs2)
  val rawh2 = ex_reg_write && (ex_reg_dest === rs1 || ex_reg_dest === rs2)
  val rawh3 = io.dmem.resp.reg_write && (io.dmem.resp.reg_dest === rs1 || io.dmem.resp.reg_dest === rs2)
  val data_hazard = rawh1 || rawh2 || rawh3    // We could make this more precise, but leave it for simplicity.
  stop_fetch := !io.imem.ready || !io.dmem.ready || data_hazard
  stop_decode := !io.dmem.ready
  val kill_decode = data_hazard || pc_src

  when (kill_decode) {
    pc_src := Bool(false)
    id_mem_valid := Bool(false)
    id_reg_write := Bool(false)
  }

  when (stop_decode) {
    stop(id_va, id_vb, id_tag, id_disp16, id_mdata)
    stop(id_reg_alu, id_mem_valid, id_reg_write, id_reg_dest)
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
  val rdata = Mux(id_reg_alu, alu.io.c, id_mdata)

  val ex_addr = Reg(next = raddr)
  val ex_data = Reg(next = rdata)
  val ex_mem_valid = Reg(next = id_mem_valid)
  ex_reg_write := id_reg_write
  ex_reg_dest := id_reg_dest

  when (stop_execute) {
    stop(ex_addr, ex_data, ex_mem_valid, ex_reg_write, ex_reg_dest)
  }

  /** Stage 4. Memory */

  io.dmem.req.addr := ex_addr
  io.dmem.req.data := ex_data
  io.dmem.req.mem_valid := ex_mem_valid
  io.dmem.req.reg_write := ex_reg_write
  io.dmem.req.reg_dest := ex_reg_dest


  /** Stage 5. Write */

  when (io.dmem.resp.reg_write) { rf(io.dmem.resp.reg_dest) := io.dmem.resp.data }

}

class CPU extends Module {

  val io = new Bundle {
  }

  val core0 = Module(new Core)

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

