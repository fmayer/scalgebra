/*
(C) 2010 by Florian Mayer
 */

package scalgebra

object Complex {
  def fromPolar(r: Double, phi: Double): Complex = {
    new Complex(
      r * math.cos(phi), r * math.sin(phi), r, phi % math.Pi
    ) with ComplexBinomial with AbstractComplexPolar
  }
  
  def fromBinomial(a: Double, b: Double): Complex = {
    new Complex(
      a, b, math.sqrt(math.pow(a, 2) + math.pow(b, 2)), math.tanh(b / a)
    ) with ComplexPolar with AbstractComplexBinomial
  }
}

abstract class Complex(
  val a: Double, val b: Double, val r: Double, val phi: Double
) {
  def +(other: Complex): Complex
  def -(other: Complex): Complex
  def *(other: Complex): Complex
  def /(other: Complex): Complex
  def pow(other: Double): Complex
  def rt(other: Double): Complex
}

// Why are we doing this? Consider the case of two complex numbers being
// multiplicated (x * y) whereas x is known to have been produced from
// polar coordinates; if the whole calculation is performed in polar
// coordinates, at most one (possibly lossy) conversion has to be done.
trait ComplexBinomial extends Complex {
  override def toString = "%f + %fi".format(a, b)

  override def +(other: Complex): Complex = {
    Complex.fromBinomial(a + other.a, b + other.b)
  }

  override def -(other: Complex): Complex = {
    Complex.fromBinomial(a - other.a, b - other.b)
  }

  override def *(other: Complex) = {
    Complex.fromBinomial(
      a * other.a - b * other.b,
      a * other.b + b * other.a
    )
  }

  override def /(other: Complex) = {
    val divisor = math.pow(other.a, 2) + math.pow(other.b, 2)
    Complex.fromBinomial(
      (a * other.a + b * other.b) / divisor,
      (b * other.a - a * other.b) / divisor
    )
  }

  override def rt(other: Double): Complex = {
    Complex.fromBinomial(1, 1)
  }
}

// Mix this in if you want ComplexBinomial mixed in _after_ ComplexPolar.
trait AbstractComplexBinomial extends ComplexBinomial {
  abstract override def pow(other: Double): Complex = {
    other match {
      case 2.0 => Complex.fromBinomial(
          math.pow(a, 2) + math.pow(b, 2),
          a * b
        )
      case n => super.pow(other);
    }
  }

  abstract override def rt(other: Double) = {
    other match {
      case 2.0 => Complex.fromBinomial(a * a - b * b, a * b + b * a);
      case n => super.rt(n);
    }
  }
}


trait ComplexPolar extends Complex  {
  override def toString = "(%f; %f rad)".format(r, phi)
  
  override def *(other: Complex) = {
    Complex.fromBinomial(
      a * other.a - b * other.b,
      a * other.b + b * other.a
    )
  }

  override def /(other: Complex) = {
    val divisor = math.pow(other.a, 2) + math.pow(other.b, 2)
    Complex.fromBinomial(
      (a * other.a + b * other.b) / divisor,
      (b * other.a - a * other.b) / divisor
    )
  }

  override def pow(other: Double): Complex = {
    Complex.fromPolar(math.pow(r, other), phi * other)
  }
  
  override def rt(other: Double): Complex = {
    Complex.fromPolar(math.sqrt(r), phi)
  }
}

// Mix this in if you want ComplexBinomial mixed in _after_ ComplexPolar.
// Despite there being no implementation it makes sense to supply it so that
// client code can mix it in where appropriate without needing to change its
// source once methods are supplied (see the Complex object).
trait AbstractComplexPolar extends ComplexPolar {

}
