package edu.knoldus

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    def search(arr: Array[Int], target: Int, low: Int, high: Int): Int = {

      if (low > high) return -1
      val middle = low + (high - low) / 2

      arr match {
        case(arr:Array[Int]) if (arr(middle) == target) => middle
        case(arr:Array[Int]) if (arr(middle) < target) => search(arr, target, middle + 1, high)
        case(arr:Array[Int]) if (arr(middle) > target) => search(arr, target, low, middle - 1)
      }
    }
    val res = search(array, elem, 0, array.length - 1)
    if (res==(-1)) false
    else true
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if (array.isEmpty) false
    else if (array.head == elem) true
    else linearSearch(array.tail, elem)
  }

}
