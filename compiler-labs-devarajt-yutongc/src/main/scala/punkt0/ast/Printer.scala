package punkt0
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = 
  {
    var sb:StringBuilder = new StringBuilder()

    val main = t.asInstanceOf[Program].main
    
    var tabCounter = 0

    def printMain = {
      sb.append("object ")
      printIdentifier(main.obj)
      sb.append(" extends ")
      sb.append(main.parent.value)
      startBlock
      putTabs
      printVars(main.vars)
      printExpressions(main.exprs)
      endBlock
     }
    
    def putTabs = {
      val tabs = "\t"*tabCounter
      sb.append(tabs)
    }

    def startBlock = {
      sb.append(" {\n")
      tabCounter += 1

    }
    def endBlock = {
      tabCounter -= 1
      putTabs
      sb.append("}\n")
    }

    def printClasses(classDecls: List[ClassDecl]) = {
      classDecls.map(classDecl => {
        printClass(classDecl)
        putTabs
      })
    }

    def printClass(classDecl: ClassDecl) = {
      sb.append("class ")
      printIdentifier(classDecl.id)
      classDecl.parent match {
        case Some(pr) => {
          sb.append(" extends ")
          printIdentifier(pr)
        }
        case None => 
      }
      
      startBlock
      printVars(classDecl.vars)
      printMethods(classDecl.methods)
      endBlock
    }

    def printMethods(methodDecls: List[MethodDecl]) = {
      methodDecls.map(method => {
        putTabs
        printMethod(method)
      })
    }

    def parseType(tpe:TypeTree): String = tpe match {
      case IntType() => "Int"
      case StringType() => "String"
      case BooleanType() => "Boolean"
      case UnitType() => "Unit"
      case Identifier(_) => {
        //println("printing parseType")
        //println(tpe.asInstanceOf[Identifier])
        printIdentifier(tpe.asInstanceOf[Identifier])
        ""
        }
     // case _ => "NoSuchType"
    }

    def printMethod(method: MethodDecl): Unit ={
      if (method.overrides){
        sb.append("override ")
      }
      sb.append("def ")
      printIdentifier(method.id)
      sb.append(" (")
      if(!method.args.isEmpty){
        printIdentifier(method.args.head.id)
        sb.append(" : "+parseType(method.args.head.tpe))
        method.args.tail.map(x => {
          sb.append(", ")
          printIdentifier(x.id)
          sb.append(" : " + parseType(x.tpe))
        })
      }
      sb.append(") : ")
      sb.append(parseType(method.retType)+ " = ")
      startBlock
      printVars(method.vars)
      printExpressions(method.exprs)
      putTabs
      printExpression(method.retExpr)
      putTabs
      
      sb.append("\n")
      endBlock
    }

    def printVars(varDecls: List[VarDecl]) = {
      varDecls.map(varDecl => {
        putTabs
        printVar(varDecl)
      })
    }

    def printVar(varDecl: VarDecl) = {
      sb.append("var ")
      printIdentifier(varDecl.id)
      sb.append(" : ")
      sb.append(parseType(varDecl.tpe))
      sb.append(" = ")
      printExpression(varDecl.expr)
      sb.append("\n")
    }


    def printExpressions(expressions:List[ExprTree]) = {
      expressions.map(expression => {
        putTabs
        printExpression(expression)
        sb.append("\n")
      })
    }


    def printIdentifier(id: Identifier) = {
      //println(id + "#" + id.getSymbol.id )//+ "(" + id.line + ":" + id.column + ")")
      //println("********************current result: \n"+ sb)
      sb.append(id.value)// + "#" + id.getSymbol.id)
    }

    def printMethodCallArguments(args:List[ExprTree]) = {
      if(args.length != 0){
        printExpression(args.head)
        if(args.length != 1){
          for(arg <- args.tail){
            sb.append(", ")
            printExpression(arg)
          }
        }


      }


    }



    def printExpression(expression:ExprTree):String = {
      expression match {
        case While(_,_) => {
          val whileNode = expression.asInstanceOf[While]
          sb.append("while ")
          printExpression(whileNode.cond)
          printExpression(whileNode.body)
          ""
        }

        case If(_,_,_) => {
          val ifNode = expression.asInstanceOf[If]
          sb.append("if (")
          printExpression(ifNode.expr)
          sb.append(") ")
          printExpression(ifNode.thn)
          sb.append("\n")
          if(ifNode.els != None){
            putTabs
            sb.append("else ")
            printExpression(ifNode.els.get)
          }
          ""
        }

        case Assign(_,_) => {
          val assignNode = expression.asInstanceOf[Assign]

          printIdentifier(assignNode.id)
          sb.append(" = ")
          printExpression(assignNode.expr)
          ""
        }

        case Println(_) => {
          val printNode = expression.asInstanceOf[Println]
          sb.append("println(")
          //println("println: " + printNode.expr)
          printExpression(printNode.expr)
          sb.append(")")

          ""
        }


        case Block(_) => {
          val blockNode = expression.asInstanceOf[Block]
          startBlock
          printExpressions(blockNode.exprs)
          endBlock
          ""
        }
        case Plus(_,_) => {
          val plusNode = expression.asInstanceOf[Plus]
          //sb.append("(")
          sb.append(printExpression(plusNode.lhs))
          sb.append(" + ")
          sb.append(printExpression(plusNode.rhs))
          //sb.append(")")
          ""
        }
        case Minus(_,_) => {
          val minusNode = expression.asInstanceOf[Minus]
          //sb.append("(")
          sb.append(printExpression(minusNode.lhs))
          sb.append(" - ")
          sb.append(printExpression(minusNode.rhs))
          //sb.append(")")
          ""
        }
        case Times(_,_) => {
          val timesNode = expression.asInstanceOf[Times]
          //sb.append("(")
          sb.append(printExpression(timesNode.lhs))
          sb.append(" * ")
          sb.append(printExpression(timesNode.rhs))
          //sb.append(")")
          ""
        }
        case Div(_,_) => {
          val divNode = expression.asInstanceOf[Div]
          //sb.append("(")
          sb.append(printExpression(divNode.lhs))
          sb.append(" / ")
          sb.append(printExpression(divNode.rhs))
          //sb.append(")")
          ""
        }
        case And(_,_) => {
          val andNode = expression.asInstanceOf[And]
          //sb.append("(")
          sb.append(printExpression(andNode.lhs))
          sb.append(" && ")
          sb.append(printExpression(andNode.rhs))
          //sb.append(")")
          ""
        }
        case Or(_,_) => {
          val orNode = expression.asInstanceOf[Or]
          //sb.append("(")
          sb.append(printExpression(orNode.lhs))
          sb.append("||")
          sb.append(printExpression(orNode.rhs))
          //sb.append(")")
          ""
        }
        case LessThan(_,_) => {
          val ltNode = expression.asInstanceOf[LessThan]
          //sb.append("(")
          sb.append(printExpression(ltNode.lhs))
          sb.append(" < ")
          sb.append(printExpression(ltNode.rhs))
          //sb.append(")")
          ""
        }
        case Equals(_,_) => {
          val equalsNode = expression.asInstanceOf[Equals]
          //sb.append("(")
          sb.append(printExpression(equalsNode.lhs))
          sb.append(" == ")
          sb.append(printExpression(equalsNode.rhs))
          //sb.append(")")
          ""
        }
        
        case MethodCall(_,_,_) => {
          val mcNode = expression.asInstanceOf[MethodCall]
          //println("printing methodcall obj: " + mcNode.obj)
          printExpression(mcNode.obj)
          sb.append("."+mcNode.meth.value)//+"#??(")
          printMethodCallArguments(mcNode.args)
          sb.append(")")
          
          ""
        }
        case IntLit(_) => {
          val intNode = expression.asInstanceOf[IntLit]
          sb.append(intNode.value)
          ""
        }
        case StringLit(_) => {
          val strNode = expression.asInstanceOf[StringLit]
          sb.append("\""+strNode.value+"\"")
          ""
        }
        case True() => {
          sb.append("true")
          ""
        }
        case False() => {
          sb.append("false")
          ""
        }
        case Identifier(_) => {
          val idNode = expression.asInstanceOf[Identifier]
            printIdentifier(idNode)
          ""
        }
        case This() => {
          sb.append("this")
          ""
        }
        
        case Null() => {
          sb.append("null")
          ""
        }
        
        case New(_) =>{
          val newNode = expression.asInstanceOf[New]
          sb.append("new ")
          sb.append(parseType(newNode.tpe))
          sb.append("()")
          ""
        }
        case notNode @ Not(_) => {
          sb.append("!(")
          printExpression(notNode.expr)
          sb.append(")")
          ""
        }

      }
     
    }

    def printType(nodeType: TypeTree) = nodeType match {
      case IntType() => {
        sb.append("Int")
      }
      case BooleanType() => {
        sb.append("Bool")
      }
      case StringType() => {
        sb.append("String")
      }
      case UnitType() => {
        sb.append("Unit")
      }
      case Identifier(_) => {
        val idNode = nodeType.asInstanceOf[Identifier]
        sb.append(idNode.value)
      }
    }
    printClasses(t.asInstanceOf[Program].classes)
    printMain
    sb.toString()
  }
}

