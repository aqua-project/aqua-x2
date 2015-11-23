import Chisel._

/*
 Specification
=================

 * line size :128 bit
 * Size of DRAM data :16 bit
 * Size of word: 32 bit
 * length of DRAM Addr : 22 bit
 * length of CPU Addr : 21 bit
 * NOW, DO NOT CARE ready bit of DRAM !! wtime must be correct.
 * write addr is not correct

 Arguments
=================
 * wayBits : bit size of way
 * lineBits : bit size of lines

 */
class Cache (wayBits : Int,lineBits : Int,wtime : Int) extends Module {
  val io = new Bundle {
    val cmdin = Decoupled (new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 21);
    }).flip;
    val cmdout = Valid(new Bundle {
      val we = Bool (INPUT);
      val addr = UInt (width = 22);
    });
    val req  = Bool(OUTPUT);
    val grant = Bool(INPUT);
    val wdataFromCore = Valid(UInt(width = 32)).flip;
    val rdataFromCore = Valid(UInt(width = 32));
    val wdataToDRAM = Valid(UInt(width = 16));
    val rdataToDRAM = Valid(UInt(width = 16)).flip;
  }
  val time = UInt(wtime-1, log2Down(wtime-1) + 1)
  val wordsPerLine = 4;

  // addr width
  val tagWidth   = 21 - lineBits - 2;
  val indexWidth = lineBits;
  val wordsWidth = 2;

  // splited addr 
  val tag        = Reg(UInt (width = tagWidth))
  val index      = Reg(UInt (width = indexWidth))
  val posInLine = Reg(UInt (width = wordsWidth))

  val numOfLine = 1 << lineBits
  val numOfWay  = 1 << wayBits

  val cacheSize = numOfLine * numOfWay;

  val cache = Mem(UInt(width = 32), cacheSize * wordsPerLine);
  val tagArray = Mem(UInt (width = tagWidth), cacheSize);
  val nextWay = Mem(UInt(width = log2Up (numOfWay) + 1), numOfLine);

  val busy :: ready :: cacheStates = Enum (UInt(),2)
  val readMode :: writeMode :: readWriteMode = Enum (UInt(),2)
  val state   = Reg (init = ready)
  val count   = Reg (init = time)
  val wordCnt = Reg (init = UInt(0,3))
  val addr    = Reg (init = UInt(0,22));
  val we      = Reg (init = Bool(false));
  val mode    = Reg (init = Bool (false));
  val req     = Reg (init = Bool (false));

  val wdata_from_core = Reg (init = UInt (0,32));

  val dram_buff = Reg (init = Decoupled(UInt(width = 16)));

  val core_buff = Reg (init = Decoupled(UInt(width = 32)));

  // vaild is up while running and access right is granted
  io.cmdout.valid := (state === busy) && (io.grant === Bool(true))//(state === busy);
  io.cmdout.bits.addr := addr;
  io.cmdout.bits.we := mode;
  io.req := req;

  io.rdataFromCore := core_buff;
  io.wdataToDRAM := dram_buff;

  switch (state) {
    is (ready) {
      when (io.cmdin.valid) {
        val varIndex = io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth);
        val varTag = io.cmdin.bits.addr.apply(20,indexWidth + wordsWidth);
        val nowWay = MuxCase (UInt (numOfWay),Array (
          (varTag === tagArray(Cat(varIndex,UInt(0)))) -> UInt (0),
          (varTag === tagArray(Cat(varIndex,UInt(1)))) -> UInt (1)));

        when (nowWay != UInt(numOfWay)) {
          // cache hit
          val tmp_posInLine = io.cmdin.bits.addr.apply (wordsWidth - 1,0)
          dram_buff.valid := Bool (false)
          nextWay (index) := UInt (1) - nowWay;
          when (io.cmdin.bits.we) {
            // write
            when (io.wdataFromCore.valid) {
              cache (Cat(varIndex,nowWay,tmp_posInLine)) := io.wdataFromCore.bits
            }
          } .otherwise {
            // read
            core_buff.bits := cache (Cat(varIndex,nowWay,tmp_posInLine))
            core_buff.valid := Bool(true)
          }
        } .otherwise {
          // cache miss
          // values are set before access right is granted
          // but, valid flag is not true before access right is granted
          core_buff.valid := Bool (false)
          state := busy
          count := time
          wordCnt := UInt(0)

          // burst access
          when (io.cmdin.bits.we) {
            // tag index words
            val tmp_index = io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth)
            // addr for write back
            addr := tagArray (Cat(tmp_index,nextWay (tmp_index),io.cmdin.bits.addr(indexWidth + wordsWidth - 1,0),UInt (0)))
          }.otherwise {
            addr := io.cmdin.bits.addr ## UInt (0)
          }
          we   := io.cmdin.bits.we
          req  := Bool(true);
          tag        := io.cmdin.bits.addr.apply(20,indexWidth + wordsWidth);
          index      := io.cmdin.bits.addr.apply(indexWidth + wordsWidth - 1,wordsWidth);
          posInLine := io.cmdin.bits.addr.apply (wordsWidth - 1,0)

          when (io.cmdin.bits.we) {
            dram_buff.bits := cache (Cat(varIndex,nextWay (index),UInt(0,lineBits + 1)))
            dram_buff.valid := Bool(true)
            wdata_from_core := io.wdataFromCore.bits;
            // write back
            mode      := writeMode;
          } .otherwise {
            mode      := readMode;
          }
        }
      }
    }
    // wait count decreases only when state is busy and access right is given
    // (grant is looked below)
    is (busy) {
      when (count === UInt (0)) {
        count := time
        when (mode === readMode && io.rdataToDRAM.valid) {
          when (wordCnt (0) === UInt (0)) {
            cache (Cat (index,nextWay (index),posInLine) ^ wordCnt (wordsWidth,1)).apply(15,0) := io.rdataToDRAM.bits
          }.otherwise {
            cache (Cat (index,nextWay (index),posInLine) ^ wordCnt (wordsWidth,1)).apply(31,16) := io.rdataToDRAM.bits
          }
        }

        // finish
        when (wordCnt === UInt(7)) {
          when (we) {
            when (mode === readMode) {
              cache (Cat (index,nextWay (index),posInLine)) := wdata_from_core
              nextWay (index) := UInt (1) - nextWay (index);
              state := ready
            }.otherwise {
              // end of write back
              mode := readMode
              addr := Cat (tag,index,posInLine);
            }
          } .otherwise {
            core_buff.bits := cache (Cat(index,nextWay (index),posInLine));
            core_buff.valid := Bool(true)
            state := ready
            io.req := Bool(false)
            nextWay (index) := UInt (1) - nextWay (index);
          }

          wordCnt := UInt(0)
          tagArray (index ## nextWay (index)) := tag
        } .otherwise {
          wordCnt := wordCnt + UInt (1);
        }

        when (mode) {
          when (wordCnt (0) === UInt (0)) {
            dram_buff.bits := cache (Cat(index,nextWay (index),posInLine) ^ wordCnt (wordsWidth,1)).apply(15,0)
          }.otherwise {
            dram_buff.bits := cache (Cat(index,nextWay (index),posInLine) ^ wordCnt (wordsWidth,1)).apply(31,16)
          }
          dram_buff.valid := Bool(true)
        }
      } .otherwise {
        when (io.grant === Bool(true)) {
          count := count - UInt (1);
        }
      }
    }
  }

  io.cmdin.ready := (state === ready);
}

object Cache {
  val wayBits = 1;
  val lineBits = 5;
  val wtime = 2;

  def main (args : Array[String]) : Unit = {
    chiselMainTest (args, () => Module (new Cache (wayBits,lineBits,wtime))) { c =>
      new CacheTests (c)
    }
  }

  class CacheTests (c: Cache) extends Tester (c) {

    def readTest (addr: Int,value: Int,expectMiss: Boolean) {
      print ("start of read test\n");
      while (peek (c.io.cmdin.ready) == 0) {
        step (1);
      }
      poke (c.io.cmdin.bits.we,false);
      poke (c.io.cmdin.bits.addr,addr);
      poke (c.io.cmdin.valid,true);
      step (1);
      if (!expectMiss) {
        expect (c.io.cmdin.ready,1)
        expect (c.io.rdataFromCore.valid,1)
        expect (c.io.rdataFromCore.bits,value)
        print ("end of read test\n");
        return
      }
      readRoutine (addr,value)

      expect (c.io.rdataFromCore.valid,1)
      expect (c.io.rdataFromCore.bits,value)

      print ("end of read test\n");
    }

    def readRoutine (addr: Int,value: Int) {
      expect (c.io.cmdout.valid,1);
      expect (c.io.cmdout.bits.we,0);
      expect (c.io.cmdout.bits.addr,addr);
      for (i <- 0 to 3) {
        // TODO: In this test,timing is critical
        // low bit
        step (wtime)
        poke (c.io.rdataToDRAM.bits,value & ((1 << 16) - 1));
        poke (c.io.rdataToDRAM.valid,true);

        // high bit
        step (wtime);
        poke (c.io.rdataToDRAM.valid,true);
        poke (c.io.rdataToDRAM.bits,value >> 16);
      }
    }

    def writeTest (addr: Int,value: Int,expectMiss: Boolean) {
      print ("start of write test\n");
      while (peek (c.io.cmdin.ready) == 0) {
        step (1);
      }
      poke (c.io.cmdin.bits.we,true);
      poke (c.io.cmdin.bits.addr,addr);
      poke (c.io.cmdin.valid,true);
      poke (c.io.wdataFromCore.valid,true)
      poke (c.io.wdataFromCore.bits,value)

      step (1);
      if (!expectMiss) {
        expect (c.io.cmdin.ready,1)
        print ("end of write test\n");
        return
      }
      print ("start of write back\n");
      expect (c.io.cmdout.valid,1);
      expect (c.io.cmdout.bits.we,1);
      printf ("cmdout addr:%d\n",peek (c.io.cmdout.bits.addr));

      for (i <- 0 to 3) {
        // low bit
        printf ("wdata to DRAM:%d\n",peek(c.io.wdataToDRAM.bits));

        expect (c.io.wdataToDRAM.valid,1);
        step (wtime)

        // high bit
        printf ("wdata to DRAM:%d\n",peek(c.io.wdataToDRAM.bits));
        expect (c.io.wdataToDRAM.valid,1);
        step (wtime);
      }
      print ("end of write back\n");
      print ("start of read for write\n");
      // data from dram is always 0 in test
      readRoutine (addr,0);
      print ("end of read for write\n");
      print ("end of write test\n");
    }
    
  // now, req,grant is ignored
    poke(c.io.grant,true);

    // expect 1024 and 1025 are in the same line
    writeTest (1024,1234567890,true);
    readTest  (1024,1234567890,false);
    writeTest (1025,1234567890,false);
    readTest  (1025,1234567890,false);

    // expect working as 2 nowWay
    writeTest (1024,234567890,false);
    writeTest (2048,123456789,true);
    readTest  (2048,123456789,false);
    readTest  (1024,234567890,false);
  }
}
