package edu.knoldus

import scala.util.control.Breaks.{break, breakable}

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
          for ( j <- 1 until array.length ) {
            breakable {
              for ( i <- (1 to j).reverse ) {
                if (array(i-1) < array(i)) {
                  break
                } else {
                  val temp = array(i)
                  array(i) = array(i-1)
                  array(i-1) = temp
                }
              }
            }
          }
        array
  }


  def selectionSort(array: Array[Int]): Array[Int] = {
      def selSort(arr: Array[Int], i1: Int, i2: Int) = {
          val temp = arr(i1)
          arr(i1) = arr(i2)
          arr(i2) = temp
      }
    for (i <- 0 until array.size - 1)
      selSort(array, i, (i + 1 until array.size).foldLeft(i)((currMin, index) =>
          if (array(index) < array(currMin)) index
          else currMin)
          )
    array
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    for(i <- 0 until array.length-1){
      for(j <- 0 until array.length-1-i){
        if(array(j)>array(j + 1)){
          val temp = array(j)
          array(j) = array(j + 1)
          array(j + 1) = temp
         }
        }
      }
    array
    }
  }
