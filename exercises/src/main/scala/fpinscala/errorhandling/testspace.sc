import fpinscala.errorhandling.None

import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter



sealed trait Option[+A] {


  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) => if (f(x)) this else None
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }




  //Exercise 4.3
  //Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa, bb)))

  // Exercise 4.4
  //Write a function sequence that combines a list of Options into one Option containing a list of all the Some values
  // in the original list. If the original list contains None even once, the result of the function should be None;
  // otherwise the result should be Some with a list of all the values. Here is its signature:[3]

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //book
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }

  //def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}


val myList1 = List[Option[String]](Some("Hello"), Some("world"))
val myList2 = List[Option[String]](None, Some("world"))
val myList3 = List[Option[String]](Some("world"), None)
val myList4 = List[Option[String]](None, None)


val myseq = sequence(myList1)