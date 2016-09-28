package codeoff

object Problem {

  sealed trait DSL[A]

  //case class RetrieveInput[A](s: Source[A]) extends DSL[A]

  /**
    * String => FileLocation  //  get working directory
    * FileLocation => File    //  resolve file in working directory
    * File => Iterator[String]  //  read file -> line by line
    * Iterator[String] => A     //  decode file contents into problem model
    * A => B                    //  solve the problem
    * B => Iterator[String]     //  encode/serialize problem solution
    * String => FileLocation  //  get working directory
    * FileLocation => File    //  resolve file in working directory
    * Iterator[String] => File  //  write file -> line by line
    */

}
