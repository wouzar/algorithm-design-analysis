
/** Object contains methods for merge sorting algorithm */
object MergeSortImplementation {
  
  /** 
   *  Method takes interval [s, f) of array a for sorting. 
   *  If length of subarray less than threshold, then we apply 
   *  regular sorting and inversion's calculus, else we apply 
   *  merge sorting and calculate inversions crossed by two subarrays */
  def mergeSort(a: Array[Int], s: Int, f: Int, threshold: Int = 3): (Array[Int], BigInt) = {
    if (f - s <= threshold) {
      var inv = 0
      var (x, y) = (s, 0)
      while (x < f) {
        y = x + 1
        while (y < f) {
          if (a(x) > a(y)) inv += 1
          y += 1
        }
        x += 1
      }
      (a.slice(s, f).sorted, inv)
    } else {
      val mid = (f + s) / 2
      var (l, r) = (mergeSort(a, s, mid), mergeSort(a, mid, f))
      merge(l, r)
    }
  }
  
  /** Method merges two subarrays into one and counts number of inversions */
  def merge(a: (Array[Int], BigInt), b: (Array[Int], BigInt)): (Array[Int], BigInt) = {
    var inv = 0
    val n = a._1.length + b._1.length
    var (i, j, k) = (0, 0, 0)
    var c = new Array[Int](n)
    while (k < a._1.length + b._1.length) {
      if (i == a._1.length) { c(k) = b._1(j); k += 1; j += 1 }
      else if (j == b._1.length || a._1(i) < b._1(j)) { c(k) = a._1(i); k += 1; i += 1 }
      else { c(k) = b._1(j); k += 1; j += 1; inv += a._1.length - i }
    }
    (c, inv + a._2 + b._2)
  }
  
}


/** Contains additional methods */
object Utils {
  
  import sys.process._
  import java.net.URL
  import java.io.File

  /** Downloads file with name filename from url */
  def fileDownloader(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }
  
}


/** Main program */
object Main extends App {
  
  import MergeSortImplementation._
  import Utils._
  import scala.io.Source

  /** Preparing data */
  fileDownloader("datasource.random.url", "data.txt")
  val listOfNumbers = Source.fromFile("data.txt").getLines.toArray.map(_.toInt)
  
  /** Sorting and inversion's calculus */
  val (arr, inv: BigInt) = mergeSort(listOfNumbers, 0, listOfNumbers.length)
  
  /** Number of inversions */
  println(inv)
  
}