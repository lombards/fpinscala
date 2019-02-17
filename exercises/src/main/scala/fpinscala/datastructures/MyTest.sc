import fpinscala.datastructures.{Cons, List, Nil}
import fpinscala.datastructures.List.{init,init1,init2}

val numbers = List.apply(1,2,3,4,5)
val c_init = init(numbers)
val c_init1 = init1(numbers)
val c_init2 = init2(numbers)


val a = Cons(1, Nil)
val b = Cons(2, a)
val c = Cons(3, b)

c.tail