package week6

object ForToQuery {
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

  //for (b <- books; a <- b.authors if a startsWith ”Bird”)
  //yield b.title

  // revise previous books/author/title query "for" statement
  // translate into query
  // two leading generators lead to a flatMap

  // first step pull books in front of flatMap
  books flatMap (b =>
    // continuing first step
    // for (a <- b.authors if a startsWith "Bird") yield b.title)
    
    // reduce again.  It is a generator followed by a filter
    // pull the filter into the generator
    // for (a <- b.authors withFilter (a => a startsWith "Bird")) yield b.title
    
    // and again
    // still have a for expression to translate.
    // a single generator translates into a map
    b.authors withFilter (a => a startsWith "Bird") map (y => y.title)
    )
}