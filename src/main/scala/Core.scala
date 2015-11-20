import Chisel._

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
    val reg_dest = (OUTPUT, width = 32)
    val data = UInt(OUTPUT, width = 32)
  }
}


class ALUIO extends Bundle {
  val func = UInt(INPUT, width = 4)
  val a = UInt(INPUT, width = 32)
  val b = UInt(INPUT, width = 32)
  val c = UInt(OUTPUT, width = 32)
}

class Core extends Module {

  val io = new Bundle {
    val imem = new IMemIO().flip
    val dmem = new DMemIO().flip
  }


  /** Hazard */

  val stop_fetch = !io.imem.ready || !io.dmem.ready || data_hazard
  val stop_decode = !io.dmem.ready


  /** Fetch */

  val nextpc = Reg(UInt(width = 32))

  val pc = Mux(pc_src, pc_addr, nextpc)
  val pc4 = pc + 4

  io.imem.addr.valid := !stop_fetch
  io.imem.addr.bits := pc

  unless (stop_fetch) {
    nextpc := pc4
  }


  /** Decode & Read */

  val idata = io.imem.data
  val opcode = idata(31, 26)
  val rs1 = idata(25, 21)
  val rs2 = idata(20, 16)
  val lit = idata(20, 9).toSInt
  val func = idata(15, 9)
  val tag = idata(8, 5)
  val rd = idata(4, 0)
  val disp_n = idata(25, 5).toSInt
  val disp_l = idata(20, 5).toSInt
  val disp_c = idata(20, 0).toSInt
  val disp_s = idata(15, 0).toSInt

  val rf = Reg(Vec(32, UInt(width = 32)))
  val rv1 = rf(rs1)
  val rv2 = rf(rs2)

  val disp16 = Mux(idata(26), disp_s, disp_l)
  val disp21 = Mux(idata(31, 29) === UInt("b101"), disp_c, disp_l)
  val rv2_or_lit = Mux(idata(26), rv2, lit)
  val mem_valid = idata(31, 29) === UInt("b011")
  val reg_write = idata(29) || (mem_valid && idata(28, 26) =/= UInt("b001"))

  val id_va = Reg(next = rv1)
  val id_vb = Reg(next = rv2)
  val id_tag = Reg(next = tag)
  val id_func = Reg(next = func)
  val id_disp16 = Reg(next = disp16)
  val id_mem_valid = Reg(next = mem_valid)
  val id_reg_write = Reg(next = reg_write)
  val id_reg_dest = Reg(next = rd)


  /** Execute */

  val alu = Module(new ALU)
  alu.io.tag := id_tag
  alu.io.a := id_va
  alu.io.b := id_vb

  val raddr = id_va + id_disp16
  val rdata = alu.io.c

  val ex_addr = Reg(next = raddr)
  val ex_data = Reg(next = rdata)
  val ex_mem_valid = Reg(next = id_mem_valid)
  val ex_reg_write = Reg(next = id_reg_write)
  val ex_reg_dest = Reg(next = id_reg_dest)


  /** Memory */

  io.dmem.req.addr := ex_addr
  io.dmem.req.data := ex_data
  io.dmem.req.mem_valid := ex_mem_valid
  io.dmem.req.reg_write := ex_reg_write
  io.dmem.req.reg_dest := ex_reg_dest


  /** Write */

  when (io.dmem.resp.reg_write) { rf(io.dmem.resp.reg_dest) := io.dmem.resp.data }

}

class CPU extends Module {

  val io = new Bundle {
  }

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

