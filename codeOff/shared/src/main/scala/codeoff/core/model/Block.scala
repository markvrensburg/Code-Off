package codeoff.core.model

final case class Block[A](location: Location, entity: A) {

  lazy val tupled: (Location, A) = location -> entity
}
