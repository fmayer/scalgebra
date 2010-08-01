/*
 * (C) 2010 by Florian Mayer
 */

package scalgebra

import scala.collection.Seq
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer


class PolynomFunction(var fun: Seq[(Double, Double)]) {
  fun = fun.sortBy(-_._2).filter(_._1 != 0)
  val mp = fun.map(x => (x._2, x._1)).toMap

  def getn(n: Double) = mp(n)

  def order =
    fun(0)._2
  
  override
  def toString = {
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
  
  def value(x: Double) = 
    fun.map(a => a._1 * math.pow(x, a._2)).reduceLeft(_ + _)

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

  def newton(start: Double, epsilon: Double) {
    val d = derivative;
    var h = start + 1;
    var x = start;
    var a: Double = 0.;
    while (abs(x - h) < epsilon) {
      h = x;
      x = (x-value(x)) / d.value(h);
    }
    x
  }
  
  def isEmpty = fun.isEmpty
}
