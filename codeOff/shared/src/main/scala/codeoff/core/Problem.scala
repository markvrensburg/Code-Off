package codeoff.core

case class ProblemLocation(problemID: ProblemID, location: String)

case class ProblemLocations(problemLocations: List[ProblemLocation])

