package project.CTL

abstract sealed class CTL(){


  def and (x:CTL) : CTL = {
    return And(this,x)
  }

  def and (x:Int) : CTL = {
    return And(this,Atom(x))
  }

  def or(x:CTL) : CTL = {
    return Or(this,x) 
  }
  def or(x:Int) : CTL = {
    return Or(this,Atom(x)) 
  }

  def ->(x:CTL) : CTL = {
    return Implies(this,x) 
  }
  def ->(x:Int) : CTL = {
    return Implies(this,Atom(x)) 
  }
  def unary_! : CTL = {
    return Neg(this )
  }



}
case class T() extends CTL()
case class F() extends CTL()
case class Atom(x:Int) extends CTL()
case class Neg(x:CTL) extends CTL()
case class And(l:CTL , r:CTL) extends CTL()
case class Or(l:CTL , r:CTL) extends CTL()
case class Implies(l:CTL , r:CTL) extends CTL()
case class AX(x:CTL) extends CTL()
case class EX(x:CTL) extends CTL()
case class AU(l:CTL,r:CTL) extends CTL()
case class EU(l:CTL,r:CTL) extends CTL()
case class EF(x:CTL) extends CTL()
case class EG(x:CTL) extends CTL()
case class AF(x:CTL) extends CTL()
case class AG(x:CTL) extends CTL()



