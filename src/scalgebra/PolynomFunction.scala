/*
 * (C) 2010 by Florian Mayer
 */

package scalgebra

import scalgebra.Complex._

import scala.collection.Seq
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet


class PolynomFunction(var fun: Seq[(Double, Double)]) {
  fun = fun.sortBy(-_._2).filter(_._1 != 0)
  val mp = fun.map(x => (x._2, x._1)).toMap

  def pow(x: Double, n: Double) = math.pow(x, n)
  def pow(x: Complex, n: Double) = x.pow(n)

  def getn(n: Double) = if (mp.contains(n)) mp(n) else 0.

  def order =
    fun(0)._2
  
  override def toString = {
    if (!isEmpty) {
      fun.map(x => "%fx^%f".format(x._1, x._2)).reduceLeft(_ + " + " + _)
    }
    else {
      ""
    }
  }

  // FIXME: Get me out of this class.
  def abs(x: Double): Double = {
    if (x >= 0) {
      x;
    } else {
      -x;
    }
  }

  def derivative = {
    val newfun = for ((x, n) <- fun if n > 0) yield (x * n, n-1);
    new PolynomFunction(newfun);
  }
  
  def value(x: Complex): Complex =
    fun.map(a => x.pow(a._2) * a._1).reduceLeft(_ + _)

  def value(x: Double): Double =
    fun.map(a => math.pow(x, a._2) * a._1).reduceLeft(_ + _)

  def roots(f: PolynomFunction) {

  }

  def +(other: PolynomFunction): PolynomFunction = {
    var nmp = HashMap.empty ++ mp
    for ((k, v) <- other.mp) {
      nmp.update(k, mp.getOrElse(k, 0.) + other.mp.getOrElse(k, 0.))
    }
    new PolynomFunction(nmp.toSeq.map(a => (a._2, a._1)))
  }

  def -(other: PolynomFunction) = {
    this + (other * (-1))
  }

  def *(other: Double) : PolynomFunction = {
    new PolynomFunction(fun.map(a => (other * a._1, a._2)))
  }

  def *(other: PolynomFunction) : PolynomFunction = {
    val nfun = new ListBuffer[(Double, Double)]
    for ((a, n) <- other.fun) {
      for ((sa, sn) <- fun) {
        nfun += ((a*sa, n + sn))
      }
    }
    new PolynomFunction(nfun)
  }

  def /(other: PolynomFunction): Option[PolynomFunction] = {
    if (other.order > order) {
      None
    } else {
      var result = new PolynomFunction(List.empty)
      var rest = this;
      while (!rest.isEmpty) {
        var by =
          new PolynomFunction(
            List(
              (rest.fun(0)._1 / other.fun(0)._1,
               rest.fun(0)._2 - other.fun(0)._2)
            )
          )
        result += by;
        rest -= by * other;
      }
      Some(result)
    }
  }

  def newton(start: Complex, epsilon: Complex) {
    var x = start
    var nx = Complex.fromBinomial(0, 0)

    var s = new HashSet[Complex]

    do {
      nx = x
      x = x - value(x) / derivative.value(x);
      if (s.contains(x) || x == Double.NegativeInfinity) {
        return None
      }
    } while (abs(nx.a - x.a) > epsilon.a || abs(nx.b - x.b) > epsilon.b);
    Some(x)
  }

  def newton(start: Double, epsilon: Double): Option[Double] = {
    var x = start
    var nx = 0.;
    
    var s = new HashSet[Double]

    do {
      nx = x
      x = x - value(x) / derivative.value(x);
      if (s.contains(x) || x == Double.NegativeInfinity) {
        return None
      }
    } while (abs(nx - x) > epsilon);
    Some(x)
  }

  def solveLin() = -getn(0) / getn(1)
  
  def solveQuad() = {
    val rt = getn(1) * getn(1) - 4 * getn(2) * getn(0);
    if (rt > 0) {
      List(
        -getn(1) + math.sqrt(rt) / (2 * getn(2)),
        -getn(1) - math.sqrt(rt) / (2 * getn(2))
      )
    } else {
      List(
        Complex.fromBinomial(
          -getn(1) / (2 * getn(2)),
          math.abs(rt) / (2 * getn(2))
        ),
        Complex.fromBinomial(
          -getn(1) / (2 * getn(2)),
          -math.abs(rt) / (2 * getn(2))
        )
      )
    }
  }
  
  def isEmpty = fun.isEmpty
}
