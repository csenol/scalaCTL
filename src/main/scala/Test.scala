package project.CTL

object Test extends App {

implicit def intToCtl(x:Int) : CTL = {return Atom(x)}

//p=0
//q=1
//r=2

val p=0
val q=1
val r=2


val s0=new State(Set(p,q))
val s1=new State(Set(q,r))
val s2=new State(Set(r))

val M = new Model(Set(s0,s1,s2))

s0->s2->s2;
s0->s1->s0;
s1->s2;


val t1 = p and q
val t2 = !r
val t3 = T()
val t4 = EX(q and r)
val t5 = !AX(q and r)
val t6 = !EF(p and r)
val t7 = EG(r)
val t8 = AF(r)
val t9 = EU(p and q , r)
val t10 = AU(p,r)
val t11 = AG((p or q or r)-> EF(EG (r)) )

val res = List(M|=(s0,t1) , M|=(s0,t2), M|=(s0,t3), M|=(s0,t4), M|=(s0,t5), M|=(s0,t6), M|=(s2,t7), M|=(s0,t8), M|=(s0,t9), M|=(s0,t10), M|=(s0,t11) )

println(res.size)
res.foreach(println(_))


}
