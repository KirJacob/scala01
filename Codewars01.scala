package tutorialspoint

object SomeTask {

  def multiply(n: Int): Int = {
    if (n % 2 == 0) n * 8 else n * 9
  }

  def makeNegative(n: Int): Int = {
    if (n >= 0) n * (-1) else n
  }

  def maps(xs: List[Int]): List[Int] = {
    xs.map(_ * 2)
  }

  def positiveSum(arr: Array[Int]): Int = {
    var result = 0
    for (i <- arr.indices)
      if (arr(i) > 0)
        result = result + arr(i)
    result
  }

  //RemoveFirstAndLastCharacters
  def removeChars(s: String): String = {
    s.substring(1, s.length - 1)
  }

  //not my but best solution
  def positiveSum2(arr: Array[Int]): Int = {
    arr.filter(x => x > 0).sum
  }

  def reverseWords(str: String): String = {
    var arrStr: Array[String] = str.split(" ")
    arrStr = arrStr.reverse
    arrStr.mkString(" ")
  }

  //not my but best solution
  def reverseWords2(str: String): String =
    str.split(' ').reverse.mkString(" ")

  def noSpace(s: String): String = {
    var result: String = ""
    for (i <- 0 until s.length)
      if (s(i).toString != " ") result = result + s(i).toString
    result
  }

  //not my but best solution
  def noSpace2(s: String): String = s.filter(_ != ' ')

  def opposite(number: Double): Double = (-1) * number

  def summation(n: Int): Int = {
    var result: Int = 0
    for (i <- 1 to n)
      result = result + i
    result
  }

  //not my but best solution
  def summation2(n: Int): Int = (1 to n).sum

  def points(games: Vector[String]): Int = {
    var scores: Int = 0
    for (x <- games.indices) {
      val res: String = games(x)
      val firstNum: Int = res.substring(0, res.indexOf(":")).toInt
      val secondNum: Int = res.substring(res.indexOf(":") + 1, res.length).toInt
      var scoreIncrement = 0
      if (firstNum == secondNum) scoreIncrement = 1
      else if (firstNum > secondNum) scoreIncrement = 3
      scores = scores + scoreIncrement
      println(String.format("calculate score for %s => %s, %s", games(x), scoreIncrement, firstNum > secondNum))
    }
    scores
  }

  def countSheep(sheep: Array[Boolean]): Int = sheep.toList.filter(_ == true).length

  //not my but best solution
  def countSheep2(sheep: Array[Boolean]): Int = sheep.count(_ == true)

  //not my but best solution
  def removeEveryOther[T](list: List[T]): List[T] = {
    list.grouped(2).map(_.head).toList
  }

  def twice_as_old(dad: Int, son: Int) = {
    var result: Int = 0
    var year = -1 * son
    var diff: Int = -1
    while (diff != 0) {
      var sonCurr: Int = son + year
      var dadCurr: Int = dad + year
      diff = sonCurr * 2 - dadCurr
      println(s"year=${year} son=${sonCurr} dad=${dadCurr} diff=${diff}")
      if (diff == 0) {
        result = year
      }
      year = year + 1
    }
    Math.abs(result)
  }

  def xo(str: String): Boolean =
    str.split("").count(x => x.toLowerCase.equals("x")) == str.split("").count(x => x.toLowerCase.equals("o"))

  def getCount(inputStr: String): Int = {
    inputStr.split("").count(x =>
      x.equals("a") || x.equals("o") || x.equals("e") ||
        x.equals("u") || x.equals("i")
    )
  }

  //numbers.sorted.take(2).sum
  def sumTwoSmallest(numbers: List[Int]): Int = {
    var numSorted:List[Int] = numbers.filter(x=>x>0).sorted
    numSorted(0) + numSorted(1)
  }

  def reverse(str: String): String = {
    var count:Int = 0
    val result: List[String] = str.split(" ").toList.map(x=>{
      count = count + 1
      if (count%2 ==0) x.reverse else x
    })
    result.mkString(" ")
  }

  def cleanString(s: String): String = {
    var initList: List[String] = s.split("").toList
    var resultList: List[String] = initList
    for (i<-initList.indices){
      if (initList(i).equals("#")){
        resultList = resultList.drop(i)
      } else{

      }
      println("i=>" + i + "|" + resultList)
    }
    resultList.mkString("")
  }


  //not my but best solution


  //  HELPER
  //  println((1 to 3).toList.map(_*2).sum)


  def main(args: Array[String]) {
    //array initialize
    //var arrStr:Array[String] = Array[String]("1","2")
    //    println(reverseWords("ab cdf egh q"))
    //    println(noSpace("asa sa saqw q"))
    //    println(summation(8))
    //    println(points(Vector("2:1","3:3","11:12")))
    //    println(countSheep(sheep))
    //    var someList:List[Int] = List(1,3,2,10)
    //    println(removeEveryOther(someList))
    //    println("RESULT=" + twice_as_old(55, 30))
    //    println(getCount("absdsdae"))
    //    println(sumTwoSmallest(List(1,32,-11,345)))
//    println(reverse("abd wed rty sdsdsd a"))
//    println(cleanString("asssa#dsd#ds##sx#"))
    var lst:List[Int] = List(1,2,3,4)
    var lstTemp:List[Int] = List(1,2,3,4)
    println(lstTemp)
    println(lstTemp)
//    lstTemp = lstTemp.drop(1)
//    println(lstTemp)

    //    var resultList:List[Int] = someList.take(2) ++ someList.drop(3)
    //    println(resultList)


  }

}
