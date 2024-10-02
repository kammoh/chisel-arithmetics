package adders

import chisel3._


trait PipelinableAdder[T <: Data] extends Adder[T] {

  def buildFirstLayer(a: Seq[T], b: Seq[T], c: Option[T]): Seq[(T, T)]

  /// depth >= 1
  def buildLayer(depth: Int, prev: Seq[(T, T)]) : Seq[(T, T)]
}