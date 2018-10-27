// See LICENSE for license details.

package treadle.executable

import org.objectweb.asm.{Label, MethodVisitor}
import org.objectweb.asm.Opcodes._
import treadle.utils.{BitMasks, BitUtils}

trait IntExpressionResult extends ExpressionResult {
  def apply(): Int
  val expressionCompiler: Option[MethodVisitor => Unit] = None
}

case class GetIntConstant(n: Int) extends IntExpressionResult {
  def apply(): Int = n
  override val expressionCompiler = Some({mv: MethodVisitor => {
    mv.visitLdcInsn(n)
    ()
  }})
}

case class ToBig(f: FuncInt) extends BigExpressionResult {
  def apply(): Big = Big(f())
}

case class ToLong(f: FuncInt) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class LongToBig(f: FuncLong) extends BigExpressionResult {
  def apply(): Big = BigInt(f())
}

case class LongToInt(f: FuncLong) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class BigToLong(f: FuncBig) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class ToInt(f: FuncBig) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class AddInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() + f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
  case (Some(e1), Some(e2)) =>
    Some({mv: MethodVisitor => {
      e1(mv)
      e2(mv)
      mv.visitInsn(IADD)
    }})
  case _ => None
  }
}

case class SubInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() - f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        e1(mv)
        e2(mv)
        mv.visitInsn(ISUB)
      }})
    case _ => None
  }
}

case class MulInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() * f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        e1(mv)
        e2(mv)
        mv.visitInsn(IMUL)
      }})
    case _ => None
  }
}

case class DivInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = {
    val divisor = f2()
    if(divisor == 0) {
      0
    }
    else {
      f1() / divisor
    }
  }
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(IDIV)
      }})
    case _ => None
  }
}

case class RemInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = {
    val modulus = f2()
    if(modulus == 0) {
      0
    }
    else {
      f1() % modulus
    }
  }
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(IREM)
      }})
    case _ => None
  }
}

case class MuxInts(condition: FuncInt, trueClause: FuncInt, falseClause: FuncInt, condExprCompiler: Option[MethodVisitor => Unit] = None, trueExprCompiler: Option[MethodVisitor => Unit] = None, falseExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
  override val expressionCompiler = (condExprCompiler, trueExprCompiler, falseExprCompiler) match {
    // case (Some(c), Some(t), Some(f)) =>
    //   Some({mv: MethodVisitor => {
    //     // TODO check div by 0
    //     val tLabel = new Label()
    //     val endLabel = new Label()
    //     mv.visitLdcInsn(0)
    //     c(mv)
    //     mv.visitJumpInsn(IF_ICMPGT, tLabel)
    //     f(mv)
    //     mv.visitJumpInsn(GOTO, endLabel)
    //     mv.visitLabel(tLabel)
    //     mv.visitFrame(F_SAME, 0, null, 0, null)
    //     t(mv)
    //     mv.visitLabel(endLabel)
    //     mv.visitFrame(F_SAME, 0, null, 1, Array(INTEGER))
    //     mv.visitInsn(NOP)
    //   }})
    case _ => None
  }
}

case class EqInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = if(f1() == f2()) 1 else 0
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    //case (Some(e1), Some(e2)) =>
    //  Some({mv: MethodVisitor => {
    //    val eqLabel = new Label()
    //    val doneLabel = new Label()
    //    e1(mv)
    //    e2(mv)
    //    mv.visitJumpInsn(IF_ICMPEQ, eqLabel)
    //    mv.visitLdcInsn(0)
    //    mv.visitJumpInsn(GOTO, doneLabel)
    //    mv.visitLabel(eqLabel)
    //    mv.visitFrame(F_SAME, 0, null, 0, null)
    //    mv.visitLdcInsn(1)
    //    mv.visitLabel(doneLabel)
    //    mv.visitFrame(F_SAME, 0, null, 1, null)
    //    mv.visitInsn(NOP)
    //  }})
    case _ => None
  }
}

case class NeqInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = if(f1() != f2()) 1 else 0
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    // case (Some(e1), Some(e2)) =>
    //   Some({mv: MethodVisitor => {
    //     val neqLabel = new Label()
    //     val doneLabel = new Label()
    //     e1(mv)
    //     e2(mv)
    //     mv.visitJumpInsn(IF_ICMPNE, neqLabel)
    //     mv.visitLdcInsn(0)
    //     mv.visitJumpInsn(GOTO, doneLabel)
    //     mv.visitLabel(neqLabel)
    //     mv.visitFrame(F_SAME, 0, null, 0, null)
    //     mv.visitLdcInsn(1)
    //     mv.visitLabel(doneLabel)
    //     mv.visitFrame(F_SAME, 0, null, 1, null)
    //     mv.visitInsn(NOP)
    //   }})
    case _ => None
  }
}

case class LtInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = if(f1() < f2()) 1 else 0
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    // case (Some(e1), Some(e2)) =>
    //   Some({mv: MethodVisitor => {
    //     val ltLabel = new Label()
    //     val doneLabel = new Label()
    //     e1(mv)
    //     e2(mv)
    //     mv.visitJumpInsn(IFLT, ltLabel)
    //     mv.visitLdcInsn(0)
    //     mv.visitJumpInsn(GOTO, doneLabel)
    //     mv.visitLabel(ltLabel)
    //     mv.visitLdcInsn(1)
    //     mv.visitLabel(doneLabel)
    //     mv.visitInsn(NOP)
    //   }})
    case _ => None
  }
}

case class LeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() <= f2()) 1 else 0
}

case class GtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class GeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() >= f2()) 1 else 0
}

case class AsUIntInts(f1: FuncInt, width: Int, f1ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  private val bitMasks = BitMasks.getBitMasksInts(width)

  def apply(): Int = f1() & bitMasks.allBitsMask
  override val expressionCompiler = f1ExprCompiler match {
    case Some(e) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e(mv)
        mv.visitLdcInsn(bitMasks.allBitsMask)
        mv.visitInsn(IAND)
      }})
    case _ => None
  }
}

case class AsSIntInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val bitMasks = BitMasks.getBitMasksInts(width)

  def apply(): Int = {
    val value = f1()
    if(value < 0) {
      value
    }
    else {
      if(bitMasks.isMsbSet(value)) {
        (value & bitMasks.allBitsMask) - bitMasks.nextPowerOfTwo
      }
      else {
        value & bitMasks.allBitsMask
      }
    }
  }
}

case class AsClockInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() == 0) 0 else 1
}

case class ShlInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(ISHL)
      }})
    case _ => None
  }
}

case class ShrInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() >> f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(ISHR)
      }})
    case _ => None
  }
}

case class DshlInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(ISHL)
      }})
    case _ => None
  }
}

case class DshrInts(f1: FuncInt, f2: FuncInt, f1ExprCompiler: Option[MethodVisitor => Unit] = None, f2ExprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = f1() >> f2()
  override val expressionCompiler = (f1ExprCompiler, f2ExprCompiler) match {
    case (Some(e1), Some(e2)) =>
      Some({mv: MethodVisitor => {
        // TODO check div by 0
        e1(mv)
        e2(mv)
        mv.visitInsn(ISHR)
      }})
    case _ => None
  }
}

case class NegInts(f1: FuncInt, exprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  def apply(): Int = - f1()
  override val expressionCompiler = exprCompiler match {
    case Some(e) => Some({mv: MethodVisitor => {
      mv.visitLdcInsn(0)
      e(mv)
      mv.visitInsn(ISUB)
    }})
    case _ => None
  }
}

case class NotInts(f1: FuncInt, width: Int, exprCompiler: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask
  def apply(): Int = (~ f1()) & mask
  override val expressionCompiler = exprCompiler match {
    case Some(e) => Some({mv: MethodVisitor => {
      e(mv)
      mv.visitInsn(INEG)
      mv.visitLdcInsn(mask)
      mv.visitInsn(IAND)
    }})
    case _ => None
  }
}

case class AndInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() & f2()) & mask
}

case class OrInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() | f2()) & mask
}

case class XorInts(f1: FuncInt, f2: FuncInt, resultWidth: Int) extends IntExpressionResult {
  private val mask = BitUtils.makeMaskInt(resultWidth)

  def apply(): Int = (f1() ^ f2()) & mask
}

/**
  * are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val bitMask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    if((f1() & bitMask) == bitMask) { 1 } else {0 }
  }
}

/**
  * are any bits set
  * @param f1 value to be `or` reduced
  * @param width result bit size
  */
case class OrrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & mask
    if(uInt > 0) { 1 } else { 0 }
  }
}

/**
  * are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size
  */
case class XorrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  private val mask = BitMasks.getBitMasksInts(width).allBitsMask

  def apply(): Int = {
    val uInt = f1() & mask
    (0 until width).map(n => (uInt >> n) & 1).reduce(_ ^ _)
  }
}

case class CatInts(f1: FuncInt, f1Width: Int, f2: FuncInt, f2Width: Int) extends IntExpressionResult {
  private val mask1 = BitMasks.getBitMasksInts(f1Width).allBitsMask
  def apply(): Int = {
    ((f1() & mask1) << f2Width) | f2()
  }
}

case class BitsInts(f1: FuncInt, high: Int, low: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask = (1 << ((high - low) + 1)) - 1

  def apply(): Int = {
    (f1() >> low) & mask
  }
}

case class HeadInts(f1: FuncInt, takeBits: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask = (1 << takeBits) - 1
  private val shift = originalWidth - takeBits

  def apply(): Int = {
    (f1() >> shift) & mask
  }
}

case class TailInts(f1: FuncInt, toDrop: Int, originalWidth: Int, e1: Option[MethodVisitor => Unit] = None) extends IntExpressionResult {
  private val mask: Int = (1 << (originalWidth - toDrop)) - 1

  def apply(): Int = {
    f1() & mask
  }
  override val expressionCompiler = e1 match {
    case Some(e) => Some({mv: MethodVisitor => {
      e(mv)
      mv.visitLdcInsn(mask)
      mv.visitInsn(IAND)
    }})
    case _ => None
  }
}

case class IsPosEdge(symbol: Symbol, symbolPreviousValue: Symbol, dataStore: DataStore) extends IntExpressionResult {
  def apply(): Int = {
    if(dataStore.intData(symbol.index) == 1 && dataStore.intData(symbolPreviousValue.index) == 0) {
      1
    }
    else {
      0
    }
  }
}

case class UndefinedInts(width: Int) {
  val maxValue: Int = 1 << width
  def apply(): Int = {
    //TODO: (chick) parameterize to random|0|current
    treadle.random.nextInt(maxValue)
  }
}

