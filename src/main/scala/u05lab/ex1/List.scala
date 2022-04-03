package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] =
    val i=Iterator.from(0)
    this.map((_,i.next))

  def partition(pred: A => Boolean): (List[A], List[A]) =
    (this.filter(pred), this.filter(!pred(_)))

  def partition2(pred: A => Boolean): (List[A], List[A]) =
    foldLeft ( (List[A](),List[A]()) ) ((lists,h) => (lists, pred(h)) match
      case ((a,b), true) => (a.append(h :: Nil()),b)
      case ((a,b), false) => (a, b.append(h :: Nil()))
  )

  def span(pred: A => Boolean): (List[A], List[A]) =
    foldLeft ((List[A](),List[A]())) ((lists,he) => (lists, pred(he)) match
      case ((a, h::t), _) => (a, h :: t.append(he :: Nil()))
      case ((a,Nil()), true) => (a.append(he :: Nil()), Nil())
      case ((a,Nil()), false) => (a,he::Nil())
    )

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A,A)=>A): A = this match
    case h :: Nil() => h
    case h :: t => op(h,t.reduce(op))
    case Nil() => throw new UnsupportedOperationException()
  
  def takeRight(n: Int): List[A] = this.reverse() match
    case h :: t if n>0 => t.reverse().takeRight(n-1).append(h :: Nil())
    case _ => Nil()

  def collect[B](f: PartialFunction[A,B]): List[B] = this match
    case h :: t if f.isDefinedAt(h) => f(h) :: (t collect f)
    case _ :: t => t collect f
    case _ => Nil()

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x<1 || x>3 => x-1 }) // Cons(9, Cons(39, Nil()))
