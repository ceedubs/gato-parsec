package io.chrisdavenport.gatoparsec
package bench

import implicits._
import Combinator._

import cats.implicits._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scala.collection.immutable.Queue

@State(Scope.Benchmark)
class ParserBench {
  // TODO oops
  val a = elem[Char].filter(_ === 'a')
  val b = elem[Char].filter(_ === 'b')
  val aOrB: Parser[Char, Char] = a | b

  @Benchmark
  def streamingSmallReadSmallChunks(): Unit = {
    val data = Iterator.unfold(0)(i => if (i === 10000) None else Some((Queue('a', 'b'), i + 1)))
    val res = data.foldLeft(Parser.parse(many(aOrB), Queue.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res.feedMany(Queue.empty) match {
      case ParseResult.Done(_, _) => true
      case _ => false
    }
    assert(isDone)
  }

  @Benchmark
  def streamingBigReadSmallChunksCount(): Unit = {
    val bigNum = 10000
    val data = Iterator.unfold(0)(i => if (i === bigNum) None else Some((Queue('a','b'), i + 1)))
    val p = count(bigNum * 2 - 1, elem[Char].void) ~> elem[Char]
    val res = data.foldLeft(Parser.parse(p, Queue.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res match {
      case ParseResult.Done(_, 'b') => true
      case _ => false
    }
    assert(isDone)
  }

  @Benchmark
  def streamingBigReadSmallChunksTake(): Unit = {
    val bigNum = 10000
    val data = Iterator.unfold(0)(i => if (i === bigNum) None else Some((Queue('a','b'), i + 1)))
    val p = take[Char](bigNum * 2 - 1).void ~> elem[Char]
    val res = data.foldLeft(Parser.parse(p, Queue.empty))((parseResult, input) =>
      parseResult.feedMany(input)
    )
    val isDone = res match {
      case ParseResult.Done(_, 'b') => true
      case _ => false
    }
    assert(isDone)
  }
}
