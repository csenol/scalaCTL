package project.CTL

import scala.collection.mutable._

class State(val atoms:scala.collection.immutable.Set[Int]){

  type Model = Set[State];
  var outgoings = new HashSet[State]();
  var incomings = new HashSet[State]();
  def ->(x:State) : State = {
    this.outgoings += x;
    x.incomings += this;
    return x;
  }


  override def toString() : String = {
    var res = atoms.toString;
       
    return res;
  }



}
