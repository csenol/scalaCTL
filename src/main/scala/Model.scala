package project.CTL

import scala.collection.immutable._

class Model(val M:Set[State]) {
  
  type S = Set[State];

  def |=(s:State,t:CTL) :Boolean = {
    return SAT(t)(s)

  }

  def SAT(p1:CTL) : S = {
    
    p1 match {
      case T() => return M;
      case F() =>  return new HashSet[State]()
      case Atom(x) => return M.filter(_.atoms(x))
      case Neg(x) => return M diff SAT(x)
      case And(l,r) => return SAT(l) intersect SAT(r)
      case Or(l,r) => return SAT(l) union SAT(r)
      case Implies(l,r) => return SAT(Or(Neg(l),r))
      case AX(x) => return SAT(Neg(EX(Neg(x))))
      case EX(x) => return SATex(x)
      case AU(l,r) => return SAT(Neg(Or(EU(Neg(r),And(Neg(l),Neg(r))), EG(Neg(r))) ))
      case EU(l,r) => return SATeu(l,r)
      case EF(x) => return SAT(EU(T(),x))
      case EG(x) => return  SAT(Neg(AF(Neg(x))))
      case AF(x) => return SATaf(x)
      case AG(x) => return SAT(Neg(EF(Neg(x))))
    }

  }

  def SATex(p1:CTL) : S = {
    return PREe(SAT(p1));
  }

  def SATaf(p1:CTL) : S = {
    var x= M;
    var y= SAT(p1);
    while(x != y){
      x=y;
      y= y union PREa(y)

    }

    return y;
  }

  def SATeu(p1:CTL,p2:CTL) : S = {
    var w=SAT(p1);
    var x= M;
    var y = SAT(p2)
    
    while(x != y){
      x=y;
      y= y union (w intersect PREe(y))

    }


    return y ;
  }


  def PREe(m:S) : S = {
    var res = new HashSet[State]();
    for(s <- m) {
      res  = res ++ s.incomings
    }

    return res;
  }


  def PREa(m:S) : S = {
    var res = new HashSet[State]();
    if(m.isEmpty)
      return res;
    val x = m.first
    
    for(s <- x.incomings){
      if(s.outgoings==m)
	res = res + s
    }
    
    return res;
  }




}
