import Chisel._

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

