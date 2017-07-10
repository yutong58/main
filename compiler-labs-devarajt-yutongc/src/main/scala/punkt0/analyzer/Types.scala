package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case TAnyRef(_) => true
      case _ => false
    }
    override def toString = "Int"
  }
  
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case TAnyRef(_) => true
      case _ => false
    }
    override def toString = "Boolean"
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case TAnyRef(_) => true
      case _ => false
    }
    override def toString = "String"
  }
    case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case TAnyRef(_) => true
      case _ => false
    }
    override def toString = "Unit"
  }

   
  // TODO: Complete by creating necessary types

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case obj: TAnyRef =>
        var temp = false
        
        if(obj.classSymbol == classSymbol)
          temp = true
        else if(obj == anyRef)
           temp = true
        else if(classSymbol.parent != None)
          temp = classSymbol.parent.get.getType.isSubTypeOf(obj)
          
        temp
        
      case _ =>
        false
          
    }
    
      override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
