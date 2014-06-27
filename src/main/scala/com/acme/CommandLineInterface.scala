package com.acme

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import org.joda.time.LocalDate
import org.parboiled2.ParseError

object CommandLineInterface extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter expression > ")
    Console.out.flush()

    StdIn.readLine() match {
      case "" =>
      case line =>
        val parser = new DateParser(line)
        parser.InputLine.run() match {
          case Success(result)        => println(s"Result: ${result.toDate(new LocalDate())}")
          case Failure(e: ParseError) => println(s"Invalid expression: ${parser.formatError(e)}")
          case Failure(e)             => println(s"Unexpected error: ${e}")
        }
        repl()
    }
  }
}
