/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalgebra

import scala.collection.mutable.ListBuffer

abstract class Expr

case class Var(name: String) extends Expr
case class Const(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(op: String, arg: Expr) extends Expr
case class BinOp(op: String, left: Expr, right: Expr) extends Expr
case class MulOp(op: String, operants: Seq[Expr]) extends Expr


object Parser {
  // Inspired by Programming in Scala by Odersky et al.
  def simplifyAll(expr: Expr): Expr = {
    expr match {
      case UnOp("-", UnOp("-", e)) => simplifyAll(e)
      case UnOp("+", e) => simplifyAll(e)
      case UnOp("-", Number(a)) => Number(-a)
      case UnOp(op, e) => UnOp(op, simplifyAll(e))

      case BinOp(op, e, BinOp(op2, f, g)) if op == op2 =>
        simplifyAll(
          flatten(
            BinOp(op, simplifyAll(e),
                  BinOp(op2, simplifyAll(f), simplifyAll(g)
              )
            )
          )
        )
      
      case BinOp("+", e, Number(0)) => simplifyAll(e)
      case BinOp("+", Number(0), e) => simplifyAll(e)
      case BinOp("*", e, Number(1)) => simplifyAll(e)
      case BinOp("*", Number(1), e) => simplifyAll(e)

      case BinOp("*", Number(a), Number(b)) => Number(a * b)
      case BinOp("+", Number(a), Number(b)) => Number(a + b)
      case BinOp("-", Number(a), Number(b)) => Number(a - b)

      case BinOp(op, l, r) => BinOp(op, simplifyAll(l), simplifyAll(l))

      case MulOp(op, lst) => 
          simplifyMul(MulOp(op, lst.map(simplifyAll)))
      

      case _ => expr
    }
  }

  def flattenAll(Op: String, expr: Expr): List[Expr] = {
    expr match {
      case BinOp(Op, a, b) => flattenAll(Op, a) ++ flattenAll(Op, b)
      case e => List(e)
    }
  }

  def flatten(expr: BinOp): MulOp = {
    MulOp(expr.op, flattenAll(expr.op, expr))
  }

  def deepSimplify(expr: Expr) {
    var oexpr = expr
    var nexpr: Expr = null

    while (oexpr != nexpr) {
      oexpr = nexpr
      nexpr = simplifyAll(nexpr)
    }

    nexpr
  }

  def simplifyMul(expr: MulOp): Expr = {
    var total = 0.0
    var newops = ListBuffer[Expr]()

    for (operant <- expr.operants) {
      operant match {
        case Number(a) => total += a
        case e => newops.append(e)
      }
    }
    
    if (newops.isEmpty) {
      Number(total)
    } else if (newops.length == 2) {
      BinOp(expr.op, Number(total), newops(0))
    } else {
      MulOp(expr.op, Number(total) :: newops.toList)
    }
  }
}
