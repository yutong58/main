package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        
        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
        if (currentToken.kind == EOF)
          readToken
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }
////////////////////////////////////////////////////start here 
    def parseGoal: Program = {
      val firstOfExpression = List(IF,OVERRIDE,INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS,NULL, NEW, BANG, LPAREN,PRINTLN, IDKIND, IF, WHILE, LBRACE,INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS,NULL, NEW, BANG, LPAREN)
      
      //TODO: define expression = intlit | id | expr(+|/)expr
      //val firstOfExpression = 
      //List(INTLITKIND, STRLITKIND, TRUE, FALSE, 
      //IDKIND, THIS,NULL, NEW, BANG, LPAREN)
      //priority: !, */div, +/-,<|==, &&,||
      def want(kind: TokenKind): Unit = {
        if (currentToken.kind != kind)
          expected(kind)
      }
      def expression : ExprTree = { //term; termlist
          var e = expression2
          while(currentToken.kind == OR ){
              readToken
              e = Or(e,expression2).setPos(e)
          }//while
          // println("expression: " + e)        
          e
      }//def
      
       def expression2 : ExprTree = { //term; termlist
          var e = condition
          while(currentToken.kind == AND){
              readToken
              e = And(e,condition).setPos(e)
          }//while
          // println("expression: " + e)        
          e
      }//def
      
      def condition : ExprTree = {
          var e = compare
          while(currentToken.kind == LESSTHAN || currentToken.kind == EQUALS){    
            if (currentToken.kind == LESSTHAN){
              readToken
              e = LessThan(e,compare).setPos(e)
            }
            else{
              readToken
              e = Equals(e,compare).setPos(e)
            }
          }
        e
      }
      
    def compare : ExprTree = {
          var e = term
          while(currentToken.kind == PLUS || currentToken.kind == MINUS){    
            if (currentToken.kind == PLUS){
              readToken
              e = Plus(e,term).setPos(e)
            }
            else{
              readToken
              e = Minus(e,term).setPos(e)
            }
          }
        e
      }
  
  def term : ExprTree = {
          var e = factor
          while(currentToken.kind == TIMES || currentToken.kind == DIV){    
            if (currentToken.kind == TIMES){
              readToken
              e = Times(e,factor).setPos(e)
            }
            else{
              readToken
              e = Div(e,factor).setPos(e)
            }
          }
        e
      }
    
  def factor : ExprTree = {
           var prev : ExprTree = null
            currentToken.kind match{
            
            case INTLITKIND => {
              prev = IntLit(currentToken.asInstanceOf[INTLIT].value).setPos(currentToken)
              readToken
            }
            case IDKIND => {
              var id = Identifier(currentToken.asInstanceOf[ID].value).setPos(currentToken)
              readToken
              if (currentToken.kind == EQSIGN){
                readToken
                var expr_temp : ExprTree = expression
                prev = Assign(id,expr_temp).setPos(id)
              }
              else 
                prev = id
              prev
            }
            case STRLITKIND => {
              prev = StringLit(currentToken.asInstanceOf[STRLIT].value).setPos(currentToken)
              readToken
            }
            case BANG => {
              var pos = currentToken
              readToken
              prev = Not(factor).setPos(pos)
            }
            case LPAREN => {
              var pos = currentToken
              readToken
              prev = expression.setPos(pos)
              eat(RPAREN)
            }
            case TRUE => {
             prev = True().setPos(currentToken)
              readToken
            }
            case FALSE => {
              prev = False().setPos(currentToken)
              readToken
            }
            case THIS => {
              prev = This().setPos(currentToken)
              readToken
            }
            case NEW => {
              var pos = currentToken
              readToken
              want(IDKIND)
              var id = Identifier(currentToken.asInstanceOf[ID].value)
              readToken
              eat(LPAREN)
              eat(RPAREN)
              prev = New(id).setPos(pos) 
            }//case new
            case NULL => {
              prev = Null().setPos(currentToken) 
              readToken
            }
         case PRINTLN => {
          val pos = currentToken
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          prev = Println(expr).setPos(pos)
        }
         
         case IF => { 
          val pos = currentToken
          readToken
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val body = expression
          var elseStm : Option[ExprTree] = None
          if (currentToken.kind == ELSE){
            readToken
            elseStm = Some(expression)
          }
          prev = If(cond,body,elseStm).setPos(pos)
        }
        
        //while (expression) expression
        //While(cond: ExprTree, body: ExprTree)
        case WHILE => {
          val pos = currentToken
          readToken
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val body = expression
          prev = While(cond,body).setPos(pos)
        }
        
        //{(expression(;Expression)*)?}
        case LBRACE => {
          val pos = currentToken
          readToken
          var expressionList : List[ExprTree] = List()
           if (firstOfExpression.contains(currentToken.kind)){
               expressionList = expressionList ::: List(expression)
             }
             while(currentToken.kind == SEMICOLON){
               eat(SEMICOLON)
               expressionList = expressionList ::: List(expression)             
             }

          eat(RBRACE)
          prev = Block(expressionList).setPos(pos)
        }
         case _ => expected(NULL,STRLITKIND, INTLITKIND, TRUE, FALSE, IDKIND, NEW, BANG, LPAREN)
          } //match
              expression_extend(prev)
      }//factor
      
      def expression_extend(prev: ExprTree) : ExprTree = {
        var result = prev
        while (currentToken.kind == DOT) {
              readToken
              var pos = currentToken
              want(IDKIND)
              var meth = Identifier(currentToken.asInstanceOf[ID].value)
              readToken
          
              eat(LPAREN)
              
          //MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree])
          var expressionList : List[ExprTree] = List()
           if (firstOfExpression.contains(currentToken.kind)){
               expressionList = expressionList ::: List(expression)
             }
           while(currentToken.kind == COMMA){
             eat(COMMA)
             expressionList = expressionList ::: List(expression)             
           }

          eat(RPAREN)
           result = MethodCall(result,meth,expressionList)
           //result = expression_extend(result)
       }//if 
        result
      }
      
      
      
      //TODO: define main: MainDecl, classes: List[ClassDecl]
      
      //Program(main,classes)
      //ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], 
      //methods: List[MethodDecl])
      //class Identifier ( extends Identifier )? { ( VarDeclaration ) * ( MethodDeclaration ) * }
      def ClassDeclare : ClassDecl = {
        var pos = currentToken
        eat(CLASS)
        var parent : Option[Identifier] = None
        want(IDKIND)
          var id = Identifier(currentToken.asInstanceOf[ID].value)
          readToken
          if (currentToken.kind == EXTENDS){
            readToken
            want(IDKIND)
            parent = Some(Identifier(currentToken.asInstanceOf[ID].value))
            readToken  
          }//if extends
            eat(LBRACE)
            
            var vars = List[VarDecl]()
            var methods = List[MethodDecl]()
            
            while (currentToken.kind == VAR){
              var vartemp : VarDecl = VarDeclare
              vars = vars ::: List(vartemp)
            }
            while (currentToken.kind == DEF || currentToken.kind == OVERRIDE){
              var methodtemp = MethodDeclare
              methods = methods ::: List(methodtemp)
            }
            eat(RBRACE)
            
            val cls = ClassDecl(id,parent,vars,methods)
            cls
       
      }//def classdeclare
      
      //tpe: TypeTree, id: Identifier, expr: ExprTree
      def VarDeclare : VarDecl = {
        var pos = currentToken
        eat(VAR)
        want(IDKIND)
        var id = Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        eat(COLON)
        //typecheck
              
        var tpe = types
        eat(EQSIGN)
        var expr = expression
        eat(SEMICOLON)
        val vardel = VarDecl(tpe,id,expr).setPos(pos)
      //  println("variable declaration: " + vardel)
        vardel
      }//def vardeclare
      
      def types : TypeTree = {
        var pos = currentToken
      
        currentToken.kind match {
          case BOOLEAN => {readToken; BooleanType().setPos(pos)}
          case INT => {readToken; IntType().setPos(pos)}
          //case STRLITKIND => {readToken; StringType().setPos(pos)}
          case STRING => {readToken; StringType().setPos(pos)}
          case IDKIND =>{readToken; Identifier(pos.asInstanceOf[ID].value).setPos(pos)}
          case UNIT => {readToken; UnitType().setPos(pos)}
          case _ => expected(BOOLEAN,INT,IDKIND,UNIT)
        }
    
      }//def types
      
      //overrides: Boolean, retType: TypeTree, 
      //id: Identifier, args: List[Formal], vars: List[VarDecl], 
      //exprs: List[ExprTree], retExpr: ExprTree)
      def MethodDeclare : MethodDecl = {
        var pos = currentToken
        var overrides = currentToken.kind == OVERRIDE
        if (overrides)
          readToken
          
        eat(DEF)
        want(IDKIND)
          var id = Identifier(currentToken.asInstanceOf[ID].value)
          readToken
          
          eat(LPAREN)
          var args = List[Formal]()
          
          if (currentToken.kind == IDKIND){
            var formaltemp : Formal = formal
            args = args ::: List(formaltemp)
            while(currentToken.kind == COMMA){
              readToken
              formaltemp = formal
              args = args ::: List(formaltemp)
            }//while
          }//if
          
          eat(RPAREN)
          eat(COLON)
          var retType = types
          eat(EQSIGN)
          eat(LBRACE)
          
          var vars = List[VarDecl]()
          
          while (currentToken.kind == VAR){
            var vartemp = VarDeclare
            vars = vars ::: List(vartemp)
          }//while
          
          var exprs = List[ExprTree]()
          
          var exprtemp : ExprTree = expression
          var retExprs : ExprTree = exprtemp
          
          exprs = exprs ::: List(exprtemp)
          if (currentToken.kind == SEMICOLON)
            eat(SEMICOLON)
          while (firstOfExpression.contains(currentToken.kind)){
            exprtemp = expression
            if (currentToken.kind == SEMICOLON){
              eat(SEMICOLON)
              if (currentToken.kind == RBRACE)
                expected(PRINTLN, IDKIND, IF, WHILE, LBRACE,INTLITKIND, STRLITKIND, TRUE, FALSE, IDKIND, THIS,NULL, NEW, BANG, LPAREN)
                }
            
            exprs = exprs ::: List(exprtemp)
            retExprs = exprtemp //get the last expression
          }
          var new_exprs = exprs.take(exprs.length -1 )
          eat(RBRACE)
          val methoddecl = MethodDecl(overrides,retType,id,args,vars,new_exprs,retExprs).setPos(pos)
         // println("method: " + methoddecl)
          methoddecl
      }//method declare
      
      //(tpe: TypeTree, id: Identifier)
      def formal : Formal = {
        var pos = currentToken
        want(IDKIND)
          var id = Identifier(currentToken.asInstanceOf[ID].value)
          readToken
          eat(COLON)
          var tpe = types
          val form = Formal(tpe,id).setPos(pos)
    //      println("formal: " + form)
          form
      }//formal
      
      //(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree])
      //object Identifier extends Identifier { ( VarDeclaration ) * Expression ( ; Expression ) * }
      def mainDeclare : MainDecl = {
        var pos = currentToken
        eat(OBJECT)
        want(IDKIND)
        var obj = Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        eat(EXTENDS)
        want(IDKIND)
        var parent = Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        eat(LBRACE)
        var vars = List[VarDecl]()
        var exprs = List[ExprTree]()
        while (currentToken.kind == VAR){
          var vartemp = VarDeclare
          vars = vars ::: List(vartemp)
        }
        exprs = exprs:::List(expression)
        while (currentToken.kind == SEMICOLON){
          readToken
          exprs = exprs:::List(expression)
        }
        eat(RBRACE)
        val main = MainDecl(obj,parent,vars,exprs).setPos(pos)
        main
      }//main
      
      //Program(main: MainDecl, classes: List[ClassDecl]) extends Tree
      
      var main : MainDecl = null
      var classes = List[ClassDecl]()
      
        
      while(currentToken.kind == CLASS){
          classes = classes ::: List(ClassDeclare)  
      }
      want(OBJECT)
      main = mainDeclare
      eat(EOF)
      Program(main,classes)
      
  }//parsegoal
    readToken
    val tree = parseGoal
    terminateIfErrors
    tree

  }//run
  }//object