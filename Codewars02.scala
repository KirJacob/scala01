package tutorialspoint

import scala.util.control.Breaks._

object SomeTask02 {
  /*
  "abc#d##c"      ==>  "ac"
  "abc##d######"  ==>  ""
  "#######"       ==>  ""
  ""              ==>  ""
      var lst01: List[Int] = List(1, 2, 3, 4)
    var lst02: List[Int] = List(6)
    var lst03: List[Int] = lst01++lst02
    println(lst03)
    var lst04: List[Int] = lst03.take(3) ++ lst03.takeRight(1)
    println(lst04)
   */

  def cleanString(s: String): String = {
    var resultList: List[String] = s.split("").toList
    var tempList: List[String] = resultList
    var index: Int = 0
    while (resultList.length - index > 0) {
      tempList = resultList
      if (resultList(index) == "#") {
        resultList = resultList.take(index - 1) ++ resultList.takeRight(resultList.length - index - 1)
        if (index > 0) index = index - 1
      } else index = index + 1
    }
    resultList.mkString("")
  }

  def duplicateCount(str: String): Int = {
    var initList: List[String] = str.split("").map(_.toLowerCase).toList
    var uniqueValuesList: List[String] = List()
    var duplicatesList: List[String] = List()
    var duplicatesCount: Int = 0
    for (i <- initList.indices) {
      var currentElement: String = initList(i)
      if ((i > 0) && uniqueValuesList.contains(currentElement)) {
        if (!duplicatesList.contains(currentElement)) {
          duplicatesList = duplicatesList ++ List(currentElement)
          duplicatesCount = duplicatesCount + 1
        }
      }
      if (!uniqueValuesList.contains(currentElement)) uniqueValuesList = uniqueValuesList ++ List(currentElement)
    }
    duplicatesCount
  }

  def divisorsSumMinusOne(number: Long): Long = {
    Range.Long(2, 1 + number / 2, 1).filter(x => number % x == 0).sum
  }

  def buddy(start: Long, limit: Long): String = {
    var firstNumber: Long = 0
    var secondNumber: Long = 0
    var result: String = ""
    breakable {
      for (i <- start to limit) {
        var sumFirst: Long = divisorsSumMinusOne(i)
        var sumSecond: Long = -1
        if (sumFirst > i) {
          if (sumFirst != 0) sumSecond = divisorsSumMinusOne(sumFirst)
          println(s"number=${i} sumFirst=${sumFirst} sumSecond=${sumSecond} result=${sumSecond == i}")
          if (sumSecond == i) {
            firstNumber = i
            secondNumber = sumFirst
            break()
          }
        }
      }
    }
    if (firstNumber == 0 && secondNumber == 0) result = "Nothing"
    else result = "[" + firstNumber + ", " + secondNumber + "]"
    result
  }

  def attempt(n: Long, from: Int, to: Int): Long = {
    var listTemp: List[String] = n.toString.split("").toList
    var listSize: Int = listTemp.size
    var fromVal = listTemp(from)
    listTemp = listTemp.take(from) ++ listTemp.takeRight(listSize - from - 1)
    listTemp = listTemp.take(to) ++ List(fromVal) ++ listTemp.takeRight(listSize - 1 - to)
    listTemp.mkString("").toLong
  }

  def smallest(n: Long): Array[Long] = {
    var smallest: Long = 999999999999999999L
    var lineSize: Int = n.toString.length
    var iSaved: Long = -1
    var jSaved: Long = -1
    for (i <- 0 until lineSize)
      for (j <- 0 until lineSize) {
        if (i != j) {
          var temp: Long = attempt(n, i, j)
          if (temp<smallest) {
            println(smallest)
            smallest = temp
            iSaved = i
            jSaved = j
          }
        }
      }
    Array(smallest, iSaved, jSaved)
  }

  def main(args: Array[String]) {
    //    println(s"=${cleanString("abc#d##c")}")
    //    println(s"=${cleanString("abc####d##c#")}")
    //    println(duplicateCount("aaabcdead"))
    //    println(divisorsSumMinusOne(8))
    //    println(buddy(10, 50))
    //    println(buddy(57345, 90061))
    //    println(buddy(1071625, 1103735))
    //    println(attempt(123456, 5, 2))
    println("INITIAL=" + 261235)

    /*
    //          println(s"temp=${temp} i=${i} j=${j} smallest=${smallest} compare=${temp<smallest}")
    testing(261235, Array(126235, 2, 0))
    testing(256687587015L, Array(25668758715L, 9, 0))
    testing(935855753L, Array(358557539, 0, 8))
    testing(285365,  Array(238565, 3, 1))
     */
    println(smallest(285365).toList)


  }
}
