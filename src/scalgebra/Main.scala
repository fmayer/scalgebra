/*
(C) 2010 by Florian Mayer
 */

package scalgebra

import scalgebra.ComplexPrelude._
import scalgebra.ComplexGlobals._


object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val a = 1

    val test = Complex.fromBinomial(1, 2)
    println(test)
    val tast = 1 + 2 * i

    assert(tast == test)
    
    val fun = new PolynomFunction(List((1, 2), (-8, 1), (15, 0), (0, 230)))
    val ofun = new PolynomFunction(List((1, 1), (-5, 0)))
    
    println(fun)

    (fun / ofun) match {
      case Some(x) => println(x)
      case None =>
    }
  }

}
