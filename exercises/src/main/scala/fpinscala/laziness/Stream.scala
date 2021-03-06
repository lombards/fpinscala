package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty;
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n < 0 => t().drop(n-1)
    case _ => this
  }

  // Exercise 5.3
  //Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5.4
  //Implement forAll, which checks that all elements in the Stream match a given predicate.
  // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a,b) => p(a) && b)
  }

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B] (f: A => B) : Stream[B] = {
    foldRight(Empty[B])((a,b) => cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty[A])((h,t) => if (p(h)) cons(h, t) else t)
  }

  //Exercise 5.13
  //Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
  // The zipAll function should continue the traversal as long as either stream has more elements—it uses Option
  // to indicate whether each stream has been exhausted.
  def mapViaUnfold[B] (f: A => B) : Stream[B] = {
    foldRight(Empty[B])((a,b) => cons(f(a), b))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???


  def append[B>:A](s: => Stream[B]): Stream[B] = ???
  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  val fibs = {
    def go(n: Int, m: Int): Stream[Int] =
      cons(n, go(m, n+m))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  // Exercise 5.12
  //Write fibs, from, constant, and ones in terms of unfold.[8]
  def onesViaUnfold():Stream[Int] = {
    unfold(1)(_ => Some(1,1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a,a))
  }

  def fromViaUnfold(i: Int): Stream[Int] = {
    unfold(i)(i => Some(i, i + 1))
  }

  def fibViaUnfold(): Stream[Int] ={
    unfold((0, 1)){ case (n,m) => Some(n, (m, n+m))}
  }


}