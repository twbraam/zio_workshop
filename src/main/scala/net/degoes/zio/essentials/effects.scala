// Copyright(C) 2019 - John A. De Goes. All rights reserved.

package net.degoes.zio
package essentials
import net.degoes.zio.essentials.effects.Console.{ReadLine, Return, WriteLine}
import net.degoes.zio.essentials.effects.unsafeRun
import net.degoes.zio.essentials.zio_types.Example.unsafeRun
import net.degoes.zio.essentials.zio_types.{UIO, putStrLn}
import zio.{DefaultRuntime, Task}

import scala.util.Try

object effects {

  /**
   * `Console` is an immutable data structure that describes a console program
   * that may involve reading from the console, writing to the console, or
   * returning a value.
   */
  sealed trait Console[A] { self =>

    /**
     * Implement `flatMap` for every type of `Console[A]` to turn it into a
     * `Console[B]` using the function `f`.
     */
    final def flatMap[B](f: A => Console[B]): Console[B] =
      self match {
        case Console.ReadLine(next)        => ReadLine(line => next(line).flatMap(f))
        case Console.WriteLine(line, next) => WriteLine(line, next.flatMap(f))
        case Console.Return(value)         => f(value())
      }

    final def map[B](f: A => B): Console[B] = flatMap(f andThen (Console.succeed(_)))

    final def *>[B](that: Console[B]): Console[B] = (self zip that).map(_._2)

    final def <*[B](that: Console[B]): Console[A] = (self zip that).map(_._1)

    /**
     * Implement the `zip` function using `flatMap` and `map`.
     */
    final def zip[B](that: Console[B]): Console[(A, B)] = self.flatMap(a => that.map(b => (a, b)))
  }
  object Console {
    final case class ReadLine[A](next: String => Console[A])      extends Console[A]
    final case class WriteLine[A](line: String, next: Console[A]) extends Console[A]
    final case class Return[A](value: () => A)                    extends Console[A]

    /**
     * Implement the following helper functions:
     */
    final val readLine: Console[String]              = ReadLine(succeed(_))
    final def writeLine(line: String): Console[Unit] = WriteLine(line, succeed())
    final def succeed[A](a: => A): Console[A]        = Return(() => a)
  }

  /**
   * Using the helper functions, write a program that just returns a unit value.
   */
  val unit: Console[Unit] = Console.succeed()

  /**
   * Using the helper functions, write a program that just returns the value 42.
   */
  val fortyTwo: Console[Int] = Console.succeed(42)

  /**
   * Using the helper functions, write a program that asks the user for their name.
   */
  val askName: Console[Unit] = Console.writeLine("What is your name?")

  /**
   * Using the helper functions, write a program that reads a line of input from
   * the user.
   */
  val readName: Console[String] = Console.readLine

  /**
   * Write a function that greets the user by the specified name.
   */
  def greetUser(name: String): Console[Unit] =
    Console.writeLine("Hello " + name)

  /***
   * Using `flatMap` and the preceding three functions, write a program that
   * asks the user for their name, reads their name, and greets them.
   */
  val sayHello: Console[Unit] =
    askName.flatMap(_ => readName.flatMap(name => greetUser(name)))

  /**
   * Write a program that reads from the console then parse the given input into int if it possible
   * otherwise it returns None
   */
  val readInt: Console[Option[Int]] =
  Console.readLine.map(str => Try(str.toInt).toOption)

  /**
   * Implement the following effectful procedure, which effectfully interprets
   * the description of a given `Console[A]` into an `A`.
   */
  @scala.annotation.tailrec
  def unsafeRun[A](program: Console[A]): A =
    program match {
      case Return(thunk) =>
        thunk()
      case WriteLine(line, next) =>
        println(line)
        unsafeRun(next)
      case ReadLine(next) =>
        val line = scala.io.StdIn.readLine()
        unsafeRun(next(line))
    }

  /**
   * Implement the following combinator `collectAll` that transforms a list of
   * console programs into a console program that returns a list of collected
   * results of the individual programs.
   */
  def collectAll[A](programs: List[Console[A]]): Console[List[A]] =
    programs.foldLeft[Console[List[A]]](Console.succeed(List[A]())) { (console, program) =>
      for {
        as <- console
        a  <- program
      } yield as :+ a
    }

  /**
   * Implement the `foreach` function, which iterates over the values in a list,
   * passing every value to a body, which effectfully computes a `B`, and
   * collecting all such `B` values in a list.
   */
  def foreach[A, B](values: List[A])(body: A => Console[B]): Console[List[B]] =
    collectAll(values.map(body))

  /**
   * Using `Console.writeLine` and `Console.readLine`, map the following
   * list of strings into a list of programs, each of which prints out its
   * question and reads the answer.
   */
  val questions =
    List(
      "What is your name?",
      "Where where you born?",
      "Where do you live?",
      "What is your age?",
      "What is your favorite programming language?"
    )
  val answers: List[Console[String]] = questions map {q =>
    Console.writeLine(q).flatMap(_ => Console.readLine)
  }

  /**
   * Using `collectAll`, transform `answers` into a program that returns
   * a list of strings.
   */
  val answers2: Console[List[String]] = collectAll(answers)

  /**
   * Now using only `questions` and `foreach`, write a program that is
   * equivalent to `answers2`.
   */
  val answers3: Console[List[String]] =
    foreach(questions) { question =>
      Console.writeLine(question) *> Console.readLine
    }

  /**
   * Implement the missing methods of Thunk.
   */
  class Thunk[A](val unsafeRun: () => A) {
    def map[B](ab: A => B): Thunk[B]             = new Thunk(() => ab(unsafeRun()))
    def flatMap[B](afb: A => Thunk[B]): Thunk[B] = new Thunk(() => afb(unsafeRun()).unsafeRun())
    def either: Thunk[Either[Throwable, A]]      = new Thunk(() => Try(unsafeRun()).toEither)
  }
  object Thunk {
    def succeed[A](a: => A): Thunk[A]   = new Thunk(() => a)
    def fail[A](t: Throwable): Thunk[A] = new Thunk(() => throw t)
  }

  /**
   * Build the version of printLn and readLn
   * then make a simple program base on that.
   */
  def printLn(line: String): Thunk[Unit] = Thunk.succeed(println(line))
  def readLn: Thunk[String]              = Thunk.succeed(scala.io.StdIn.readLine())

  val thunkProgram: Thunk[Unit] = printLn("Please write your name").flatMap(_ => readLn).map(line => println(line))

  val thunkProgram2: Thunk[Unit] = for {
    _ <- printLn("Please write your name")
    name <- readLn
    _ <- printLn("Hello " + name)
  } yield Thunk.succeed()
}
