package punkt0

import java.io.File
import lexer._
import ast._
import analyzer._
import code._
import java.io.FileReader



object Main {
  
  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)
      
      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)
        
      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--ast" :: args =>
      ctx = ctx.copy(doAST = true)
      processOption(args)
      
      case "--print" :: args => 
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)
        
      case "--symid" :: args => 
        ctx = ctx.copy(doSymbolIds = true)
        processOption(args)
        
      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)
   
      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      displays the tokens")
    println(" --ast         displays the AST")
    println(" --print       pretty print")
    println(" --symid       pretty print with #id")
    println(" -d <outdir>   generates class files in the specified directory")
    
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)
    
    // TODO: run lexer phase
    // val filename = new File("C:/Users/Yutong/Desktop/punkt0-parser-stubs-v2/testprograms/lab3/valid/Calendar.p0")
   // val iter = Lexer.run(filename)(ctx)
  
   if (ctx.doTokens){
      val iter = Lexer.run(ctx.file.head)(ctx)
      while (iter.hasNext){
      val n = iter.next
      println(n+"("+n.line+":"+n.column+")")
      } 
   }
    
   else if (ctx.doAST){
     val Phase = Lexer andThen Parser
     val ast = Phase.run(ctx.file.head)(ctx)
     println(ast)
   }
   
   else if (ctx.doPrintMain){
     val Phase = Lexer andThen Parser
     val ast = Phase.run(ctx.file.head)(ctx)
     println(Printer.apply(ast))
   }
    
    else if (ctx.doSymbolIds){
     val Phase = Lexer andThen Parser andThen NameAnalysis
     val ast = Phase.run(ctx.file.head)(ctx)
     println(PrinterID.apply(ast))
   }
   else {
     val Phase = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking andThen CodeGeneration
     Phase.run(ctx.file.head)(ctx)
   }
    
    if (Reporter.errors)
      System.exit(1)
     
    
    
    
  }

}




