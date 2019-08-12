package tutorialspoint

object SomeTask03 {
  
//https://www.codewars.com/kata/twice-linear/train/scala

  //[1, 3, 4, 7, 9, 10, 13, 15, 19, 21, 22, 27, ...]
  //y = 2 * x + 1
  //z = 3 * x + 1
  //1 -> 3 ,4
  //3 -> 6, 10
  //4 -> 9, 13
  //

  def dblLinear2(n: Int): Int = {
    var index: Int = 0
    var usedIndex: Int = 0
    var result: Int = 0
    var uSequence: List[Int] = List(1)
    var root: Int = 1
    var tempList:List[Int] = List()
    do {
      root = uSequence(usedIndex)
//      println(s"index=${index} usedIndex=${usedIndex} uSequence=${uSequence} root=${root}")
      index = index + 1
      tempList = List()
      if (!uSequence.contains(2 * root + 1)) tempList = tempList ++ List(2 * root + 1)
      if (!uSequence.contains(3 * root + 1)) tempList = tempList ++ List(3 * root + 1)
      uSequence = uSequence ++ tempList
      uSequence = uSequence.sorted
      usedIndex = usedIndex + 1
    }while(index < n)
    uSequence(n)
  }

  def dblLinear(n: Int): Int = {
    var index: Int = 0
    var usedIndex: Int = 0
    var result: Int = 0
    var alternative: List[Int] = List(1)
    var rootA: Int = 1
    var adjustedIndex: Int = 0
    var correction: Int = 0

    do {
      adjustedIndex = index - usedIndex - correction
      rootA = alternative(adjustedIndex)
      index = index + 2
      var twiceRoot: Int = 2 * rootA + 1
      var threeRoot: Int = 3 * rootA + 1
      if (!alternative.contains(twiceRoot)) alternative = twiceRoot :: alternative else correction = correction + 1
      if (!alternative.contains(threeRoot)) alternative = threeRoot :: alternative else correction = correction + 1
      alternative = alternative.sorted(Ordering.Int.reverse)
      usedIndex = usedIndex + 1
//      println(s"index=${index} usedIndex=${usedIndex} rootA=${rootA} alternative=${alternative} correction=${correction} adjusted=${adjustedIndex}")
    } while (index < 2*n )
    alternative(index - n - correction)
  }

  def main(args: Array[String]): Unit = {
    println("lets code 4th level")
    var parameter: Int = 30
    println(dblLinear2(parameter))
    println(dblLinear(parameter))
  }

}
