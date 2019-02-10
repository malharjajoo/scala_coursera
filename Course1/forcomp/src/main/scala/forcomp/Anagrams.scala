package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy(x => x).map(p => (p._1, p._2.length)).toList.sortWith(_._1 < _._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List[(Char,Int)]())
    case (ch, n)::xs =>
      val temp = combinations(xs)

     val check = for ( i <- 0 to n ) yield {
       if (i == 0) temp
       else
         for (elem <- temp) yield {
           (ch, i) :: elem
         }
     }

      val check2 = check.foldLeft(List(List[(Char,Int)]()))((x,y) => x++y)
      check2.toSet.toList
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {

    def subHelper(acc:Map[Char,Int], y_elem:(Char,Int) ): Map[Char,Int] =
    {
        val (ch,i) = y_elem
        val temp = acc(ch) - i

        if(temp == 0) acc-ch // cannot use -= on immutable map ...
        else acc.updated(ch, acc(ch) - i)
    }

    val acc = x.toMap
    y.foldLeft(acc)(subHelper).toList.sortWith(_._1 < _._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */

  // permutes a word
  def permute(word:Word, sentence:Sentence):List[Sentence] = {

    def helper(acc:List[Sentence], splitIndices:List[Int]): List[Sentence] = splitIndices match {
      case Nil => acc
      case i::is => val (first_half:Sentence, second_half:Sentence) = sentence.splitAt(i)

                    val currentSentence:Sentence = first_half ++ List(word) ++ second_half
                     helper(acc ++ List(currentSentence), is)
    }

    val acc = List[Sentence]()
    helper(acc, (0 to sentence.length).toList )

  }

  def myHelper(remainingOccurence:Occurrences):List[Sentence] = {

    // Sorry for using "var" Scala, Imperative languages will always be my first love :P .
    var acc: List[Sentence] = List[Sentence]()

    // find all possible words from remaining occurrences.
    val subsets = combinations(remainingOccurence)
    //println("remainingOccurences=",remainingOccurence)

    // For each subset (word)
    for (subset <- subsets) {

      val k = dictionaryByOccurrences.get(subset)


      //println("current subset =",subset)
      // if current subset is not a valid word,
      // then don't need to do anything about remaining characters
      // simply move on to next subset
      if(k.isDefined)
      {

          val meaningfulWords = k.get
          val remainder = subtract(remainingOccurence, subset)



        //println("meaningfulWords =",meaningfulWords)
        //println("remainder =",remainder)

          // if current subset is meaningful and there are no remaining characters
          if (remainder.isEmpty) {
            //println("1")
            acc = acc ++ List(meaningfulWords)
          }

          // if current subset is meaningful and remaining occurrences is not empty,
          // then remaining occurences must return a list of sentences otherwise skip current subset ....
          else
          {
            //println("2")
            val remainingSentence: List[Sentence] = myHelper(remainder)
          //println("remaining sentence=",remainingSentence)
            // this means that remaining characters were recognized by dictionary.
            // If this is not the case, we skip current subset (and remaining characters)
            if(remainingSentence != List(List(Nil)) ) {

              // permute meaningful words with remaining sentence.
              for (myword <- meaningfulWords) {
                for (sent <- remainingSentence) {
                  acc = acc ++ permute(myword, sent)
                }
              }

            }

          }

         //println("acc =", acc)

      }
      //else println("No meaningful word found in dictionary..")

    }

    return acc
  }


  def sentenceAnagrams(s: Sentence): List[Sentence] = s match {

    case Nil => List(Nil)
    case _ =>
       val occurList = sentenceOccurrences(s)
       val res = myHelper(occurList)
      // HACK. Need to get rid of this at some point ....
      res.filter(x => sentenceOccurrences(x) == occurList )
      }
}
