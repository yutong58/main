package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._
  
  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    //println("runing")
    
    
    // TODO: implement this method
    
    //construct keywords map
    def keywordType(kw: String): Token = {
      kw match {
        case "Int" => new Token(INT);
          case "object" => new Token(OBJECT);
          case "class" => new Token(CLASS); 
          case "def" => new Token(DEF);
         case  "override" => new Token(OVERRIDE);
        case   "var" => new Token(VAR);
        case   "Unit" => new Token(UNIT);
        case   "String" => new Token(STRING);
        case   "extends" => new Token(EXTENDS);
        case   "Boolean" => new Token(BOOLEAN);
         case  "while" => new Token(WHILE);
        case   "if" => new Token(IF);
         case  "else" => new Token(ELSE);
        case   "true" => new Token(TRUE);
        case   "false" => new Token(FALSE);
        case   "this" => new Token(THIS);
        case   "null" => new Token(NULL);
          case "new" => new Token(NEW);
          case "println" => new Token(PRINTLN); 
        case   _ => new Token(IDKIND)
    }}
    //check end of line and end of file 
    def isEOL : Boolean = {
        if (readNext == '\n' || readNext == '\r') true else false 
      }
      
    def isEOF : Boolean = !source.hasNext
    
    def isSpecialSymbol(ch: Char): Boolean = {
          var ss = Set(':',';','.','=','!','(',')','{','}','<','+','-','*','&','|','/',',')
          var res = ss(ch)
          res
        }
    def readNext(): Char = {
      var ch = -1.toChar
      if (source.hasNext)
        ch = source.next
      //print(ch)
      ch
    }
    var curr = readNext
    var EOFFound = false
    var endChar = -1.toChar
    new Iterator[Token] {
    
      def hasNext : Boolean = !EOFFound

      def next : Token = {
        var token: Token = new Token(BAD)

        //check empty or next line or end of file
        while (curr.isWhitespace)
          curr = readNext  
        var initial_pos = source.pos
        if (curr.toByte == -1){
          token = new Token(EOF)
          EOFFound = true
        }
        
        //check keywords and identifiers 
        else if (curr.isLetter){          
          var b = new StringBuffer
          while (curr.isLetter || curr.isDigit || curr == '_'){
            b.append(curr)
            curr = readNext
          }
          token = keywordType(b.toString)
          if (token.kind.equals(IDKIND))
            token = new ID(b.toString)
            
        }
        
        //check integer
        else if (curr == '0') {
          curr = readNext
          token = new INTLIT(0)
        }
        
        else if (curr.isDigit && curr != '0'){
          //println("cheking int")
          var k = 0
          while(curr.isDigit && curr != '|'){
            k = 10 * k + curr.toString.toInt
            curr = readNext
          }
          token = new INTLIT(k)
        }
        
        //check string 
        else if (curr == '"'){
          //println("checking string")
          var str = new StringBuffer
          curr = readNext
          while (curr != '"'){
            str.append(curr)
            curr = readNext
          }
          if (curr == '"'){
             token = new STRLIT(str.toString)
             curr = readNext
          }
        }
        
        //check symbols 
        else if (isSpecialSymbol(curr)){
          token = curr match {
            case ':' => curr = readNext; new Token(COLON)
            case ';' => curr = readNext; new Token(SEMICOLON)
            case '.' => curr = readNext; new Token(DOT)
            case ',' => curr = readNext; new Token(COMMA)
            case '=' =>{
              curr = readNext
              if (curr == '='){curr = readNext; new Token(EQUALS)}
              else new Token(EQSIGN)
            }
            case '!' => curr = readNext; new Token(BANG)
            case '(' => curr = readNext; new Token(LPAREN)
            case ')' => curr = readNext; new Token(RPAREN)
            case '{' => curr = readNext; new Token(LBRACE)
            case '}' => curr = readNext; new Token(RBRACE)
            case '<' => curr = readNext; new Token(LESSTHAN)
            case '+' => curr = readNext; new Token(PLUS)
            case '-' => curr = readNext; new Token(MINUS)
            case '*' => curr = readNext; new Token(TIMES)
            case '&' => { //check and 
              curr = readNext
              if (curr == '&') {curr = readNext; new Token(AND)}
              else new Token(BAD)
            }
            case '|' => {//check or
              curr = readNext
              if (curr == '|') {curr = readNext; new Token(OR)}
              else
                {new Token(BAD)}
            }
            case '/' =>{ 
              //println("checking div and comments"); 
              curr = readNext;
              if (curr == '/'){ // skip command
                while (curr != '\n' && source.hasNext)
                  curr = readNext 
              return next // /* /**/ */
              }else if (curr == '*'){ // starts with /*, skip command block, search for */
                curr = readNext
                while (curr.toByte != -1){
                  if (curr == '*'){
                     curr = readNext
                     if (curr == '/'){
                       curr = readNext
                       return next
                     }//if
                  }//if
                  else curr = readNext
                }//while
                token
             }//elseif 
            else new Token(DIV)// simply divide 
           } 
        }//match
        }//if special symbol
        else curr = readNext
        token.setPos(f,initial_pos)
         if(token.kind == BAD){ 
           println("bad token")
            Reporter.error("Bad token found", token)
         }
    
        token
      }//next
      /*var initial = readNext
      var end_pos = source.pos
      
      var token : Token =  new Token(EOF).setPos(f,end_pos)
          println("EOF" + token.posString)*/
      
      
    } // new token iterator
  }// run
}// object lexer




