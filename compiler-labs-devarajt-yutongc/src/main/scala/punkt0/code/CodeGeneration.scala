package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {
        var varMap = Map[Int,Int]()
        var currentMethod:MethodSymbol = null
        var nRecursionStart = ""
    def appendString(ch: CodeHandler, expr: ExprTree) : Unit = {
          expr.getType match {
          case TBoolean | TInt => ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
          case TString  => ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
          case _ => sys.error("unexpected type for append string")
          }
    }
    def convertType(tpe: Type) : String = {
      tpe match {
        case TInt => "I"
        case TString => "Ljava/lang/String;"
        case TBoolean => "Z"
        case TUnit => "V"
        case TAnyRef(cls) => "L"+cls.name+";"
        case _ => ""
      }
    }
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classSymbol = ct.getSymbol
      val classFile = new ClassFile(classSymbol.name, classSymbol.parent.map((_.name)))
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      for (variable <- ct.vars){
        val variableName = variable.id.value
        classFile.addField(convertType(variable.tpe.getType),variableName)
      }
      for (method <- ct.methods){//addMethod(retType,name,args)
        val methodSymbol = method.getSymbol
        currentMethod = methodSymbol
        val m = classFile.addMethod(convertType(method.retType.getType),methodSymbol.name,method.args.map(x => convertType(x.tpe.getType)))
        generateMethodCode(m.codeHandler,method)
        
      }
      classFile.writeToFile(dir + "/" + classFile.className + ".class")
    }

    
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      varMap = varMap.empty
      var index = 1
      for (arg <- mt.args){
        varMap += arg.id.getSymbol.id -> index
        
        index += 1
      }
      nRecursionStart = ch.getFreshLabel("nRecursionStart") //the label gets refreshed for each method
      ch << Label(nRecursionStart)
      for (variable <- mt.vars){
        varMap += variable.id.getSymbol.id -> ch.getFreshVar
        generateExpressionCode(ch,Assign(variable.id,variable.expr))
        
        //index += 1
      }
       
      
      for (expr <- mt.exprs){
        generateExpressionCode(ch,expr)
        if (expr.getType != TUnit)
          ch << POP
      }
      ////////extension: tail recursion
      generateRetExprCode(ch,mt,mt.retExpr)
      
      mt.retExpr.getType match {
        case TInt | TBoolean => ch << IRETURN
        case TAnyRef(_) | TString => ch << ARETURN
        case _ => ch << RETURN
      }
      //println("_______________________" + mt.id.value + "______________________________")
      //ch.print
      ch.freeze
    }
    def generateRetExprCode(ch:CodeHandler, mt:MethodDecl, retExpr: ExprTree):Unit = {
      retExpr match { //only three special cases need to take care: methodCall, block and if statement
        case mc:MethodCall => //if at first the last expression is a methodcall, the function might be infinite and cause stack overflow, but it's not handled in codegen 
          if (mc.meth.value.equals(currentMethod.name) ){ // if it is method call
            mc.obj match {
              case This() => //the object has to be this in case of recursive call
                for ( i <- 0 to  mc.args.size-1)              
                  generateExpressionCode(ch,Assign(mt.args.apply(i).id, mc.args.apply(i))) //reassign parameters
                ch<< Goto(nRecursionStart)// go back to the start of the function
              case _ => generateExpressionCode(ch,mc)
            }
          }
          else generateExpressionCode(ch,mc)// for other cases, just do it normally
          
        case block: Block => // else if it is block, check the last expression
          if (!block.exprs.isEmpty){
            for (i <- 0 to  block.exprs.size-2)
              generateExpressionCode(ch,block.exprs.apply(i))
             generateRetExprCode(ch,mt,block.exprs.last) //call generateRetExprCode again for the last expression in block, because it can be anything including ifstmt and methodcall
           }
           
        case ifstmt: If =>  // if the last expression is an if statement
          val nElse = ch.getFreshLabel("nElse")
          val nExit = ch.getFreshLabel("nExit")
          generateExpressionCode(ch,ifstmt.expr)
          ch << IfEq(nElse)
           //check then body
         generateRetExprCode(ch,mt,ifstmt.thn)
          ch << Goto(nExit)
          ch << Label(nElse)
          //check else body
          if (ifstmt.els.isDefined)
            generateRetExprCode(ch,mt,ifstmt.els.get)
          ch << Label(nExit)
          
        case _ => generateExpressionCode(ch,retExpr) //if the expr passed in is just a normal expression, do it normally
      }
    }
    
    def generateExpressionCode(ch : CodeHandler, expr : ExprTree): Unit = {
      expr match {
        case id: Identifier => //class variable  | method variable 
         /* if (varMap.contains(id.getSymbol.id)){
            id.getType match { 
              case TInt | TBoolean => ch << ILoad(varMap.get(id.getSymbol.id).get)
              case TString | TAnyRef(_) => ch << ALoad(varMap.get(id.getSymbol.id).get)
              case _ => 
                }
          }
          else if (currentMethod.classSymbol.members.contains(id.value)){
            id.getType match { 
            case TInt | TBoolean => ch << ILoad(0) << GetField(currentMethod.classSymbol.name, id.value,convertType(id.getType))
            case TString | TAnyRef(_) => ch << ALoad(0) << GetField(currentMethod.classSymbol.name, id.value,convertType(id.getType))
            case _ =>
            }
          }
          else if (currentMethod.classSymbol.methods.contains(id.value)){
            ch << invokespecial(currentMethod.classSymbol.name,currentMethod.name,)
          }
          //else _ =>*/
          id.getType match {
        //Aload(0) is to load this, as the class we are getting from
        case _ if !varMap.contains(id.getSymbol.id) =>
          ch << ALoad(0) <<
          GetField(currentMethod.classSymbol.name, id.value, convertType(id.getType))
        case TInt | TBoolean =>
          ch << ILoad(varMap.get(id.getSymbol.id).get)
        case TString | TAnyRef(_)  =>
          ch << ALoad(varMap.get(id.getSymbol.id).get)
      }
        case wl: While => // wl.cond body
          val nStart = ch.getFreshLabel("nStart")
          val nExit = ch.getFreshLabel("nExit")
          ch << Label(nStart)
          generateExpressionCode(ch,wl.cond)
          ch << IfEq(nExit)
          generateExpressionCode(ch,wl.body)
          ch << Goto(nStart)
          ch << Label(nExit)
          
        case ifstmt: If => 
          val nElse = ch.getFreshLabel("nElse")
          val nExit = ch.getFreshLabel("nExit")
          generateExpressionCode(ch,ifstmt.expr)
          ch << IfEq(nElse)
          generateExpressionCode(ch,ifstmt.thn)
          ch << Goto(nExit)
          ch << Label(nElse)
          if (ifstmt.els.isDefined)
            generateExpressionCode(ch,ifstmt.els.get)
          ch << Label(nExit)
          
        case assign: Assign => 
          assign.id.getType match {
            case _ if !varMap.contains(assign.id.getSymbol.id) =>
              ch << ALoad(0)
              generateExpressionCode(ch,assign.expr)
              ch << PutField(currentMethod.classSymbol.name, assign.id.value,convertType(assign.id.getType))
            case TInt | TBoolean => 
              generateExpressionCode(ch,assign.expr)
              ch << IStore(varMap.get(assign.id.getSymbol.id).get)
            case TString | TAnyRef(_) => 
              generateExpressionCode(ch,assign.expr)
              ch << AStore(varMap.get(assign.id.getSymbol.id).get)
            
          }
          
        case println: Println => 
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateExpressionCode(ch,println.expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "("+convertType(println.expr.getType)+")V")
        case block : Block => 
          for (expr <- block.exprs)
            generateExpressionCode(ch,expr)
          
        case minus : Minus => 
          generateExpressionCode(ch,minus.lhs)
          generateExpressionCode(ch,minus.rhs)
          ch << ISUB
          
        case plus : Plus => 
          if (plus.getType.isSubTypeOf(TString)){
            ch << DefaultNew("java/lang/StringBuilder")
            generateExpressionCode(ch,plus.lhs)
            appendString(ch,plus.lhs)
            generateExpressionCode(ch,plus.rhs)
            appendString(ch,plus.rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          }
          else {
            generateExpressionCode(ch,plus.lhs)
            generateExpressionCode(ch,plus.rhs)
            ch << IADD
          }
          
        case times : Times =>
          generateExpressionCode(ch,times.lhs)
          generateExpressionCode(ch,times.rhs)
          ch << IMUL
            
        case div : Div => 
          generateExpressionCode(ch,div.lhs)
          generateExpressionCode(ch,div.rhs)
          ch << IDIV
          
        case and : And => 
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          generateExpressionCode(ch,and.lhs)
          ch << IfEq(nExit)
          generateExpressionCode(ch,and.rhs)
          ch << Goto(nTrue)
          ch << Label(nExit)
          ch << ICONST_0
          ch << Label(nTrue)
          
        case or : Or => 
          val nExit = ch.getFreshLabel("nExit");
          val nTrue = ch.getFreshLabel("nTrue")
          generateExpressionCode(ch,or.lhs)
          ch << IfEq(nExit)
          ch << ICONST_1
          ch << Goto(nTrue)
          ch << Label(nExit)
          generateExpressionCode(ch,or.rhs)
          ch << Label(nTrue)
        
        case lessthan : LessThan => 
           val nExit = ch.getFreshLabel("nExit");
           val nTrue = ch.getFreshLabel("nTrue")
           generateExpressionCode(ch,lessthan.lhs)
           generateExpressionCode(ch,lessthan.rhs)
           ch << If_ICmpLt(nTrue)
           ch << ICONST_0
           ch << Goto(nExit)
           ch << Label(nTrue)
           ch << ICONST_1
           ch << Label(nExit)
           
        case equals : Equals => 
         val nExit = ch.getFreshLabel("nExit");
        val nTrue = ch.getFreshLabel("nTrue")
        generateExpressionCode(ch,equals.lhs)
        generateExpressionCode(ch,equals.rhs)
        equals.lhs.getType match {
          case TInt | TBoolean => ch << If_ICmpEq(nTrue)
          case _ => ch << If_ACmpEq(nTrue)
        }
        ch << ICONST_0
        ch << Goto(nExit)
        ch << Label(nTrue)
        ch << ICONST_1
        ch << Label(nExit)
        
        case methodcall : MethodCall => 
          generateExpressionCode(ch,methodcall.obj)
          val cls = methodcall.obj.getType.asInstanceOf[TAnyRef].classSymbol
          val meth = cls.lookupMethod(methodcall.meth.value).get
          for (arg <- methodcall.args)
            generateExpressionCode(ch,arg)
          val args = meth.argList.map(arg => {
            convertType(arg.getType)
          })
          var argConcat = "("
          for (arg <- args) {
            argConcat = argConcat + arg
          }
          argConcat = argConcat + ")"
          argConcat = argConcat + convertType(methodcall.getType)
          ch << InvokeVirtual(methodcall.obj.getType.toString, methodcall.meth.value, argConcat)
        
        case intlit : IntLit => ch << Ldc(intlit.value)
        case stringlit : StringLit => ch << Ldc(stringlit.value)
        case tr : True => ch << ICONST_1
        case fal : False => ch << ICONST_0
        case thiscase : This => ch << ALOAD_0
        case nullcase : Null => ch << ACONST_NULL
        case newcase : New => 
          ch << DefaultNew(newcase.tpe.value)
        case not : Not => 
          ch << ICONST_1
          generateExpressionCode(ch,not.expr)
          ch << ISUB
        case _ =>  
      }
    }
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main declaration
    val mainClass = new ClassFile(prog.main.obj.value)
    mainClass.setSourceFile(sourceName)
    mainClass.addDefaultConstructor
    var ch = mainClass.addMainMethod.codeHandler
    varMap = varMap.empty
      for (arg <- prog.main.vars){
        val varSym = arg.getSymbol
        mainClass.addField(convertType(varSym.getType), varSym.name)
      }
     for (arg <- prog.main.vars){
        varMap += (arg.getSymbol.id -> ch.getFreshVar)
      }
      for (expr <- prog.main.exprs){
        generateExpressionCode(ch,expr)
        if (expr.getType != TUnit)
          ch << POP
      }
      
      ch << RETURN
      //ch.print
      ch.freeze
      mainClass.writeToFile(outDir + "/" + prog.main.obj.value + ".class")
  }

}
