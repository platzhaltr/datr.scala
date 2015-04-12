package org.platzhaltr

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success}

import org.joda.time.{LocalDate,LocalDateTime}
import org.parboiled2.{ParseError, ErrorFormatter}

object CommandLineInterface extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    StdIn.readLine("---\nEnter expression > ") match {
      case "" =>
      case line =>
        println(SimpleInterface.read(line))
        repl()
    }
  }
}
