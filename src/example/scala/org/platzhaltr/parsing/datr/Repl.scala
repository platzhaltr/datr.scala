package org.platzhaltr.parsing.datr

import scala.annotation.tailrec
import scala.io.StdIn

object Repl {
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
