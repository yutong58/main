package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    for (expr <- prog.main.exprs)
      tcExpr(expr)
    for (cls <- prog.classes){
      for (meth <- cls.methods){
        for (expr <- meth.exprs){
          tcExpr(expr)
        }
        val reTpe = tcExpr(meth.retExpr,meth.retType.getType)
        
        //check override 
        if (meth.overrides){
          val methSymbol = meth.getSymbol
          val ormethSymbol = methSymbol.overridden.get
          if (!methSymbol.getType.isSubTypeOf(ormethSymbol.getType))
            Reporter.error("return type not match")
          var i = 0
          for ( i <- 0 to  methSymbol.argList.size-1){
            if (!methSymbol.argList.apply(i).getType.isSubTypeOf(ormethSymbol.argList.apply(i).getType))
              Reporter.error("parameter types not match")
          }
        }
      }
    }
    def getMethodSymbol(classSymbol: ClassSymbol, methodName: String) : Option[MethodSymbol] = {
      if (classSymbol.methods.contains(methodName))
        return classSymbol.methods.get(methodName)
      else if (classSymbol.parent.isDefined)
        return getMethodSymbol(classSymbol.parent.get,methodName)
      else 
        return None
    }
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case id: Identifier => 
          id.getType
        case wl: While => 
          tcExpr(wl.cond, TBoolean)
          tcExpr(wl.body,TUnit)
          TUnit
          
          //The type of an if expression is the least upper bound of the types 
          //of the two branches. Its conditional expression must have type Boolean.
        case ifstmt: If => 
          tcExpr(ifstmt.expr, TBoolean)
          val branch1 = tcExpr(ifstmt.thn)
          if (ifstmt.els.isDefined){
            val branch2 = tcExpr(ifstmt.els.get)
            if (branch1.isSubTypeOf(branch2))
              branch1
            else branch2
          }
          else TUnit
          
        case assign: Assign => 
          tcExpr(assign.expr, assign.id.getType)
          TUnit
        case println: Println => 
          tcExpr(println.expr, TString,TInt,TBoolean)
          TUnit
        case block : Block => 
          for (expr <- block.exprs)
            tcExpr(expr)
           if (block.exprs.isEmpty)
             TUnit
           else
             tcExpr(block.exprs.last)
        case minus : Minus => 
          tcExpr(minus.lhs, TInt)
          tcExpr(minus.rhs, TInt)
          TInt
        case plus : Plus => 
          val tplhs = tcExpr(plus.lhs,TInt,TString)
          if (tplhs.isSubTypeOf(TInt))
            tcExpr(plus.rhs,TInt,TString)
          else {
            tcExpr(plus.rhs,TInt,TString)
            TString
          }
          
        case times : Times =>
          tcExpr(times.lhs, TInt)
          tcExpr(times.rhs, TInt)
          TInt
        case div : Div => 
          tcExpr(div.lhs, TInt)
          tcExpr(div.rhs, TInt)
          TInt
          
        case and : And => 
          tcExpr(and.lhs, TBoolean)
          tcExpr(and.rhs, TBoolean)
          TBoolean
        case or : Or => 
          tcExpr(or.lhs, TBoolean)
          tcExpr(or.rhs, TBoolean)
          TBoolean
        case lessthan : LessThan => 
          tcExpr(lessthan.lhs, TInt)
          tcExpr(lessthan.rhs, TInt)
          TBoolean
        case equals : Equals => 
          val tplhs = tcExpr(equals.lhs)
          if (tplhs.isSubTypeOf(TInt) || tplhs.isSubTypeOf(TString) || tplhs.isSubTypeOf(TBoolean) || tplhs.isSubTypeOf(TUnit))
           tcExpr(equals.rhs, tplhs)
          else if (!tcExpr(equals.rhs).isInstanceOf[TAnyRef])
            Reporter.error("cannot compare class type with premetive types")
            //tcExpr(equals.rhs, tplhs.asInstanceOf[TAnyRef].classSymbol.getType)
          TBoolean
        case methodcall : MethodCall => 
          val objTpe = tcExpr(methodcall.obj)
          if (!objTpe.isInstanceOf[TAnyRef])
            Reporter.error("no such class")
          val method = getMethodSymbol(objTpe.asInstanceOf[TAnyRef].classSymbol,methodcall.meth.value)
          //objTpe.asInstanceOf[TAnyRef].classSymbol.methods.get(methodcall.meth.value)
          if (method.isDefined){
            if (methodcall.args.size != method.get.argList.size){
              Reporter.error("number of parameters not match")
              expected.head
            }
            else {
              var i = 0
              for ( i <- 0 to  methodcall.args.size-1)
                tcExpr(methodcall.args.apply(i), method.get.argList.apply(i).getType)
              method.get.getType
            }           
          }
          else {
            Reporter.error("no such method")
            expected.head
          }
          
        case intlit : IntLit => TInt
        case stringlit : StringLit => TString
        case tr : True => TBoolean
        case fal : False => TBoolean
        case thiscase : This => thiscase.getType
        //case nullcase : Null => TUnit
        case newcase : New => 
          newcase.tpe.getType
        case not : Not => 
          tcExpr(not.expr)
        case _ =>  TError
      } // TODO: Compute type for each kind of expression


      // Check result and return a valid type in case of error
      val finalType = 
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
      expr.setType(finalType)
      finalType
    }

    prog
  }

}