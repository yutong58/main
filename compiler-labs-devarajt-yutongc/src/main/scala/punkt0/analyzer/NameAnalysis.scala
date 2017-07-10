package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Phase[Program, Program] {
  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    
    // Step 1: Collect symbols in declarations

      var gs = new GlobalScope
      
      //setup main class: parent vars exprs
      gs.mainClass = new ClassSymbol(prog.main.obj.value).setPos(prog.main)
      prog.main.obj.setSymbol(gs.mainClass)
      if (!prog.main.parent.value.equals("App"))
        error("main class has to entend App")
        // vars in main
      for (mb <- prog.main.vars){
          if (gs.mainClass.members.contains(mb.id.value))
            error("duplicate variable")
          gs.mainClass.members += mb.id.value -> new VariableSymbol(mb.id.value).setPos(mb)
          //mb.id.setSymbol(gs.mainClass.members.get(mb.id.value).get)
      }
        //parent of main
       // gs.mainClass.parent = Some(new ClassSymbol(prog.main.parent.value).setPos(prog.main.parent))
        //prog.main.parent.setSymbol(gs.mainClass.parent.get)
      
      //collect class symbols
      for (cls <- prog.classes){
        if (gs.classes.contains(cls.id.value))
          error("duplicate class")
        val classSymbol = new ClassSymbol(cls.id.value).setPos(cls)
        classSymbol.setType(TAnyRef(classSymbol))
        gs.classes += (cls.id.value -> classSymbol)
        
        // add class members 
        for (mb <- cls.vars){
          if (classSymbol.members.contains(mb.id.value))
            error("duplicate class members")
          if (classSymbol.name.equals(mb.id.value))
            error("class members cannot have same name as class")
          classSymbol.members += mb.id.value -> new VariableSymbol(mb.id.value).setPos(mb)
        }
        
        //add class methods: params, members, argList. overriden will be handled later 
        for (method <- cls.methods){
          if (classSymbol.name.equals(method.id.value))
            error("method cannot have same name as class")
          if (classSymbol.methods.contains(method.id.value))
            error("duplicate class method")
          val methodSymbol = new MethodSymbol(method.id.value,classSymbol).setPos(method)
          //methodSymbol.setType(method.retType)
          //add method parameters
          var argList = List[VariableSymbol]()
          for (arg <- method.args){
            if (classSymbol.name.equals(arg.id.value))
              error("method parameters cannot have same name as class")
            if (methodSymbol.params.contains(arg.id.value))
              error("duplicate method parameter")
            val argSymbol = new VariableSymbol(arg.id.value).setPos(arg)
            methodSymbol.params += arg.id.value -> argSymbol
            argList :+= argSymbol
          }
          methodSymbol.argList = argList
          
          //add method members 
          for (mb <- method.vars){
            if (classSymbol.name.equals(mb.id.value))
              error("method members cannot have same name as class")
            if (methodSymbol.members.contains(mb.id.value))
              error("duplicate method member")
            if (methodSymbol.params.contains(mb.id.value))
              error("method member duplicate with parameter")
            val mbSymbol = new VariableSymbol(mb.id.value).setPos(mb)
            methodSymbol.members += (mb.id.value -> mbSymbol)
          }
          
          //println(method.id.value + " variables: "+methodSymbol.members)
          //add method to class
          classSymbol.methods += (method.id.value -> methodSymbol)
        }
        //add class to global scope
        gs.classes += (cls.id.value -> classSymbol)
      }
    
    //take care of parents
    for (cls <- prog.classes){
      if (cls.parent.isDefined){
        if (gs.classes.contains(cls.id.value))
          gs.classes.get(cls.id.value).get.parent = gs.classes.get(cls.parent.get.value)
        else
          error("parent class is not defined")
      }
    }
    
    
    //check loop
    for (cls <- prog.classes){
      var curr = gs.classes.get(cls.id.value) //curr is classsymbol
      var parentLink = List[Option[ClassSymbol]]()
      parentLink :+= curr
      while (curr.get.parent.isDefined){
       curr = gs.classes.get(curr.get.parent.get.name)
       if (parentLink.contains(curr))
         fatal("class loop")
       parentLink :+= curr
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    //println("classes: "+ gs.classes)
    for (cls <- prog.classes){
      val classSymbol = gs.classes.get(cls.id.value).get
      //println("classSymbol:" + classSymbol.name)
      //set class parent
      if (cls.parent.isDefined){
        val parentSymbol = gs.classes.get(cls.parent.get.value)
        cls.parent.get.setSymbol(parentSymbol.get)
        classSymbol.parent = parentSymbol
      }
      
      //set vars: tpe,expr
      for (variable <- cls.vars){
        variable.tpe match {
          case id: Identifier => setClassIdentifier(id)//,null,classSymbol)
          case _ =>
        }
         setExpr(variable.expr,null,classSymbol)
         val variableSymbol = classSymbol.members.get(variable.id.value).get
         variableSymbol.setType(variable.tpe.getType)
        variable.id.setSymbol(variableSymbol).setType(variable.tpe.getType)
      }
      //set methods : overriden, args,vars,retType,expr,retExpr
       for (method <- cls.methods){
         
         val methodSymbol = classSymbol.methods.get(method.id.value).get
         //println("step2: methodsymbol member: " + methodSymbol.members)
         //override
         
          val parentSymbol = {
           if (cls.parent.isDefined)
             gs.classes.get(cls.parent.get.value)
           else
             None
         }
          val overrideMethodSymbol = getOverridenMethod(method.id.value,parentSymbol) 
          if (method.overrides){
            if (!cls.parent.isDefined)
             error("super class is not declared")
            if (!overrideMethodSymbol.isDefined)
              error("there is no such method in super classes")
            if (overrideMethodSymbol.isDefined && overrideMethodSymbol.get.argList.size != methodSymbol.argList.size)
              error("number of parameters doesn't match")
          }
          else if (overrideMethodSymbol != None)
              error("cannot have the same name as methods in super classes") 
          methodSymbol.overridden = overrideMethodSymbol
          
         //args,vars
         for (arg <- method.args){
           arg.tpe match {
              case id: Identifier => setClassIdentifier(id)//,null,classSymbol)
              case _ =>
           }
           val argSymbol = methodSymbol.params.get(arg.id.value).get
           argSymbol.setType(arg.tpe.getType)
           arg.id.setSymbol(argSymbol).setType(arg.tpe.getType)
         }
         for (variable <- method.vars){
           variable.tpe match {
              case id: Identifier => setClassIdentifier(id)//,methodSymbol,classSymbol)
              case _ =>
            }
         setExpr(variable.expr,methodSymbol,classSymbol)
         val variableSymbol = methodSymbol.members.get(variable.id.value).get
         variableSymbol.setType(variable.tpe.getType)
        variable.id.setSymbol(variableSymbol).setType(variable.tpe.getType)
       }
         //retType
         method.retType match{
           case rt: Identifier => setClassIdentifier(rt)//rt.setSymbol(gs.classes.get(rt.value).get)
           case _ => 
         }
         //expr, retExpr
         for (expr <- method.exprs){
           setExpr(expr,methodSymbol,classSymbol)
         }  
         //println("printing return expr: " + method.retExpr)
         setExpr(method.retExpr,methodSymbol,classSymbol)
         
         method.setSymbol(methodSymbol)
         methodSymbol.setType(method.retType.getType)
         method.id.setSymbol(methodSymbol).setType(method.retType.getType)
       }//method
         cls.setSymbol(classSymbol)
         cls.id.setSymbol(classSymbol)
    }//class
    
    //main var expr
    for (variable <- prog.main.vars){
      variable.tpe match {
              case id: Identifier => setClassIdentifier(id)//,null,gs.mainClass)
              case _ =>
            }
      setExpr(variable.expr,null,gs.mainClass)
      val variableSymbol = gs.mainClass.members.get(variable.id.value).get
      variableSymbol.setType(variable.tpe.getType)
        variable.id.setSymbol(variableSymbol).setType(variable.tpe.getType)
    }
    for (expr <- prog.main.exprs)
      setExpr(expr,null,gs.mainClass)
      
      
    def setExpr(expr: ExprTree, methodSymbol: MethodSymbol, classSymbol: ClassSymbol): Unit = {
      expr match {
        case id: Identifier => 
          var symbol = getSymbol(id.value,methodSymbol,classSymbol)
          //println("symbol: " + symbol)
          if (symbol != None){
            //println("printing symbol.get" + symbol.get.name)
            id.setSymbol(symbol.get)
            //println("after setting symbol " +  id.value + " #"+id.getSymbol.id)
          }
          else 
            error("undeclaired identifier: " + id.value)
        case wl: While => 
          setExpr(wl.cond,methodSymbol,classSymbol)
          setExpr(wl.body,methodSymbol,classSymbol)
        case ifstmt: If => 
          setExpr(ifstmt.expr,methodSymbol,classSymbol)
          setExpr(ifstmt.thn,methodSymbol,classSymbol)
          if (ifstmt.els.isDefined)
            setExpr(ifstmt.els.get,methodSymbol,classSymbol)
        case assign: Assign => 
          if (methodSymbol != null){
             if(methodSymbol.params.contains(assign.id.value))
               error("cannot rewrite pamameter")
          }
            
          setExpr(assign.id,methodSymbol,classSymbol)
          setExpr(assign.expr,methodSymbol,classSymbol)
        case println: Println => 
          setExpr(println.expr,methodSymbol,classSymbol)
        case block : Block => 
          for (expr <- block.exprs)
            setExpr(expr,methodSymbol,classSymbol)
        case minus : Minus => 
          setExpr(minus.lhs,methodSymbol,classSymbol)
          setExpr(minus.rhs,methodSymbol,classSymbol)
        case plus : Plus => 
          setExpr(plus.lhs,methodSymbol,classSymbol)
          setExpr(plus.rhs,methodSymbol,classSymbol)
        case times : Times =>
          setExpr(times.lhs,methodSymbol,classSymbol)
          setExpr(times.rhs,methodSymbol,classSymbol)
        case div : Div => 
          setExpr(div.lhs,methodSymbol,classSymbol)
          setExpr(div.rhs,methodSymbol,classSymbol)
        case and : And => 
          setExpr(and.lhs,methodSymbol,classSymbol)
          setExpr(and.rhs,methodSymbol,classSymbol)
        case or : Or => 
          setExpr(or.lhs,methodSymbol,classSymbol)
          setExpr(or.rhs,methodSymbol,classSymbol)
        case lessthan : LessThan => 
          setExpr(lessthan.lhs,methodSymbol,classSymbol)
          setExpr(lessthan.rhs,methodSymbol,classSymbol)
        case equals : Equals => 
          setExpr(equals.lhs,methodSymbol,classSymbol)
          setExpr(equals.rhs,methodSymbol,classSymbol)
        case methodcall : MethodCall => 
          setExpr(methodcall.obj,methodSymbol,classSymbol)
          for (expr <- methodcall.args)
            setExpr(expr,methodSymbol,classSymbol)
        case intlit : IntLit => 
        case stringlit : StringLit => 
        case tr : True => 
        case fal : False => 
        case thiscase : This => 
          thiscase.setSymbol(classSymbol)
          thiscase.setType(classSymbol.getType)
        case nullcase : Null => 
        case newcase : New => 
          setClassIdentifier(newcase.tpe)
        case not : Not => 
          setExpr(not.expr,methodSymbol,classSymbol)
        case _ => 
      }
    }
    def setClassIdentifier(id: Identifier) : Unit = {
      val idName = id.value
      if (gs.classes.contains(idName))
          id.setSymbol(gs.classes.get(idName).get)
      else 
        error("class not defined")
    }

    def getSymbol(idName: String, methodSymbol: MethodSymbol, classSymbol: ClassSymbol) : Option[Symbol]={
      //println("finding " + idName + "in class " + classSymbol.name)
      if (methodSymbol != null){
        //println("first")
        //println(methodSymbol.members)
        if (methodSymbol.params.contains(idName))
           methodSymbol.params.get(idName)
        else if (methodSymbol.members.contains(idName))
           methodSymbol.members.get(idName)
        else if (classSymbol.members.contains(idName))
         classSymbol.members.get(idName)
        else if (classSymbol.methods.contains(idName))
          classSymbol.methods.get(idName)
        else if (classSymbol.parent.isDefined)
          getSymbol(idName,methodSymbol,classSymbol.parent.get)
        else if (gs.classes.contains(idName))
          gs.classes.get(idName)
        else None
      }
        else {
          if (classSymbol.members.contains(idName))
             classSymbol.members.get(idName)
          else if (classSymbol.methods.contains(idName))
            classSymbol.methods.get(idName)
          else if (gs.classes.contains(idName))
            gs.classes.get(idName)
          else None
        }
      

    }
    
    def getOverridenMethod(methodName: String, parentSymbol: Option[ClassSymbol]) : Option[MethodSymbol] = {
      if (parentSymbol.isDefined){
        if (parentSymbol.get.methods.contains(methodName))
          parentSymbol.get.methods.get(methodName)
        else if (parentSymbol.get.parent.isDefined)
          getOverridenMethod(methodName, parentSymbol.get.parent)
        else 
          None
      }
      else None
    }
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints
    prog
  }

}