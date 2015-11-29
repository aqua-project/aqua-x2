import Chisel._

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


