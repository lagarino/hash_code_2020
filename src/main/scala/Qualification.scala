import java.io.{File, FileWriter}

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Qualification {
  def main(args: Array[String]): Unit = {
    val fileNames = List("a_example.txt", "b_read_on.txt", "c_incunabula.txt", "d_tough_choices.txt",
      "e_so_many_books.txt", "f_libraries_of_the_world.txt")
    fileNames.foreach(run)
  }

  var lines: Iterator[String] = _

  def run (filename: String): Unit = {
    lines = Source.fromFile(filename).getLines()
    val firstLine = readLine()
    val (_, nLibraries, days) = (firstLine.head, firstLine.tail.head, firstLine.tail.tail.head)
    val books: Map[Int, Book] = readLine().zipWithIndex.map { case (score, index) => (index, Book(index, score))}.toMap
    val libraries: List[Library] = (0 until nLibraries).map { index => loadLibrary(index, books) }.toList

    val librariesToPrint = ListBuffer[Library]()

    var remainingDays = days
    var previousLibrary = libraries.maxBy(_.calculateScore(remainingDays))
    var i = 0
    while (remainingDays > 0 && i < nLibraries) {
      println(s"Printing library $i with remaining days $remainingDays")
      remainingDays -= previousLibrary.signUpDays
      librariesToPrint.addOne(previousLibrary.copy())
      i += 1

      previousLibrary = libraries.maxBy { library =>
        library.removeBooks(previousLibrary.books)
        library.calculateScore(remainingDays)
      }
    }

    val file = new File(s"$filename.out")
    val writer = new FileWriter(file)
    writer.write(librariesToPrint.filterNot(_.books.isEmpty).size + "\n")
    librariesToPrint.foreach(l => printLibrary(writer, l))
    writer.flush()
    writer.close()
  }

  private def printLibrary(writer: FileWriter, l: Library): Unit = {
    if (l.books.nonEmpty) {
      writer.write(s"${l.index} ${l.books.size}\n")
      writer.write(l.books.map(_.index).mkString(" ") + "\n")
    }
  }

  private def loadLibrary(index: Int, existingBooks: Map[Int, Book]): Library = {
    val firstLine = readLine()
    val (_, signUpDays, booksPerDay) = (firstLine.head, firstLine.tail.head, firstLine.tail.tail.head)
    val books: Seq[Book] = readLine().map(i => existingBooks(i))
    Library(index, books.toList.sortBy(- _.index), signUpDays, booksPerDay)
  }

  private def readLine(): List[Int] =
    lines.next().split(" ").toList.map(_.toInt)

  case class Book(index: Int, score: Int)

  case class Library(index: Int, var books: List[Book], signUpDays: Int, booksPerDay: Int) {

    def calculateScore(daysLeft: Int): Double = {
      val booksItCanSend = (daysLeft - signUpDays) * booksPerDay
      val booksToSend = books.take(booksItCanSend)

      val potentialScore = booksToSend.foldLeft(0L) { case (acc: Long, b: Book) => acc + b.score }
      val timeToSend: Double = (signUpDays * 2) + (booksToSend.size.toDouble / booksPerDay.toDouble)
      if (timeToSend == 0 || timeToSend > daysLeft)
        0
      else
        potentialScore.toDouble / timeToSend
    }

    def removeBooks(booksToRemove: List[Book]): Unit =
      books = books.filterNot(booksToRemove.contains)
  }

}
