package week6

object books {

  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : List[week6.books.Book] = List(Book(Structure and Interpretation of 
                                                  //| Computer Programs,List(Abelson, Harald, Sussman, Gerald J.)), Book(Introduct
                                                  //| ion to Functional Programming,List(Bird, Richard, Wadler, Phil)), Book(Effec
                                                  //| tive Java,List(Bloch, Joshua)), Book(Effective Java 2,List(Bloch, Joshua)), 
                                                  //| Book(Java Puzzlers,List(Bloch, Joshua, Gafter, Neal)), Book(Programming in S
                                                  //| cala,List(Odersky, Martin, Spoon, Lex, Venners, Bill)))

  for (b <- books; a <- b.authors if a startsWith "Bird,") yield b.title
                                                  //> res0: List[String] = List(Introduction to Functional Programming)
  for (b <- books; a <- b.authors if a startsWith "Bloch,") yield b.title
                                                  //> res1: List[String] = List(Effective Java, Effective Java 2, Java Puzzlers)
  for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res2: List[String] = List(Bloch, Joshua, Bloch, Joshua, Bloch, Joshua)

  // adding distinct to eliminate duplicates
  // or change data structure to Set instead of List.

}