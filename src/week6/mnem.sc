package week6
import scala.io.Source

object mnem {
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt").getLines
                                                  //> in  : Iterator[String] = non-empty iterator
  // make it a list. allow only letters, i.e. no hypens
  val words = in.toList filter (_ forall (_.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
  // invert map, multiple letter to single number
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)

  // map a word to the digit string
  def wordCode(word: String): String =
    // use map as a function of a map method
    word.toUpperCase map charCode                 //> wordCode: (word: String)String

  wordCode("JAVA")                                //> res0: String = 5282

  // map digit to words.  Groups lists of words that have the same code

  // to address "key not found error" make the map "total"
  // if a key does not have a word, return an empty sequence
  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq() //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 29
                                                  //| 237638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> Li
                                                  //| st(allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86
                                                  //| 8437 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3
                                                  //| 364646489 -> List(femininity), 3987267346279 -> List(extraordinary), 785539
                                                  //| 7 -> List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 3
                                                  //| 86583 -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 
                                                  //| 847827 -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curl
                                                  //| icue), 84863372658 -> List(thunderbolt), 46767833 -> List(imported), 264374
                                                  //| 64 -> List(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(
                                                  //| spoolers), 46636233 -> List(homemade), 7446768759 -> List(rigorously), 7464
                                                  //| 4647 -> List(ringings), 633738 -> List(offset), 847825 -> List(visual), 772
                                                  //| 832 -> List(Pravda), 47
                                                  //| Output exceeds cutoff limit.

  // return all ways to encode a number as a list of words
  // return a set of lists of strings
  def encode(number: String): Set[List[String]] =
    // take care of the boundry cases first: what do with an empty number
    // return not the empty set, rather the empty phrase
    if (number.isEmpty) Set(List())
    else {
      // how many characters should be take from this number to form the first word?
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        // iterate with the rest of the number
        rest <- encode(number drop split)
      } yield word :: rest // the first word, followed by the rest of the words
    }.toSet // since a range was defined, convert to a set
                                                  //> encode: (number: String)Set[List[String]]

  // encode("7225247386")
  // present as phrases instead of lists of strings
  def translate(number: String): Set[String] =
    // separate the list of words with spaces.
    // apply a make string op to separate words with spaces
    encode(number) map (_ mkString " ")           //> translate: (number: String)Set[String]

  translate("7225247386")                         //> res1: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)

}