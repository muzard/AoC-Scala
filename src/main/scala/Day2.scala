package Day2


import scala.collection.mutable
import scala.io.Source

def getLines: Vector[String] =
  val path = "C:\\Users\\arttu\\ideaProjects\\AdventOfCode\\src\\files\\Day2.txt"

  val lineIterator = Source.fromFile(path)
  try
    lineIterator.getLines().toVector
  finally
    lineIterator.close()

def linesToNumVectors: mutable.Buffer[Vector[Int]] =
  val numVectors = mutable.Buffer[Vector[Int]]()

  for line <- getLines do
    val splitLine = line.split(" ")

    val tempBuffer = mutable.Buffer[Int]()
    for char <- splitLine do
      tempBuffer += char.toInt

    numVectors += tempBuffer.toVector
    tempBuffer.clear()

  numVectors

def isAscending(numVec: Vector[Int]): Boolean =
  var isInOrder = true
  var lastNum = 0
  for num <- numVec do
    if !(num > lastNum) then
      isInOrder = false
    else
      lastNum = num
  isInOrder

def isDescending(numVec: Vector[Int]): Boolean =
  var isInOrder = true
  var lastNum = numVec.max + 1
  for num <- numVec do
    if !(num < lastNum) then
      isInOrder = false
      // break here
    else
      lastNum = num
  isInOrder


def checkVector(numVec: Vector[Int]): Boolean =
  var isSafe = true

  if !(isAscending(numVec) || isDescending(numVec)) then
    isSafe = false
  else
    for i <- 1 until numVec.size do
      if Math.abs(numVec(i) - numVec(i - 1)) > 3 then isSafe = false
  isSafe

def checkAllVectors: Int =
  var safeLines = 0
  for vector <- linesToNumVectors do
    if checkVector(vector) then safeLines += 1
  safeLines