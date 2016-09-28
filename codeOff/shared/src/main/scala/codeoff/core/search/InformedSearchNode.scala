package codeoff.core.search

case class InformedSearchNode[A, B](state: A, action: B, g: Double, h: Double) {
  lazy val eval = g + h
}

