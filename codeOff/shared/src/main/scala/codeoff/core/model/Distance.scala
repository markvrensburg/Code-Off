package codeoff.core.model

object Distance {

  def manhattan(l1: Location, l2: Location): Int =
    Math.abs(l1.x - l2.x) + Math.abs(l1.y - l2.y)

  def euclidean(l1: Location, l2: Location): Double =
    Math.sqrt(Math.pow(l1.x - l2.x, 2) + Math.pow(l1.y - l2.y, 2))
}
