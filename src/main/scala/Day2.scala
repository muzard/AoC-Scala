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
def sliceVector(vector: Vector[Int], removableIndex: Int): Vector[Int] =
    Vector(vector.slice(0,removableIndex), vector.slice(removableIndex + 1, vector.size)).flatten

def isAscending(numVec: Vector[Int], firstTime: Boolean = true): (Boolean, Vector[Int]) =
  var isSafe = true
  var infractionCounter = 0
  var infractionIndex = 0
  for i <- 1 until numVec.size do
    if !(numVec(i) > numVec(i-1)) then
      isSafe = false
      infractionCounter += 1
      infractionIndex = i
  if !isSafe && infractionCounter == 1 && firstTime then
    val newVec = sliceVector(numVec, infractionIndex)
    (isAscending(newVec, false).apply(0), newVec)
  else
    (isSafe, numVec)

def isDescending(numVec: Vector[Int], firstTime: Boolean = true): (Boolean, Vector[Int]) =
  var isSafe = true
  var infractionCounter = 0
  var infractionIndex = 0
  for i <- 1 until numVec.size do
    if !(numVec(i) < numVec(i-1)) then
      isSafe = false
      infractionCounter += 1
      infractionIndex = i
  if !isSafe && infractionCounter == 1 && firstTime then
    val newVec = sliceVector(numVec, infractionIndex)
    (isAscending(newVec, false).apply(0), newVec)
  else
    (isSafe, numVec)


def checkLevels(numVec: Vector[Int], isFirst: Boolean = true): Boolean =
  var isSafe = true
  var infractions = 0
  var infractionIndex = 0
  for i <- 1 until numVec.size do
    if Math.abs(numVec(i) - numVec(i - 1)) > 3 then
      isSafe = false
      infractions += 1
      infractionIndex = i
  if !isSafe && infractions == 1 && isFirst then
    checkLevels(sliceVector(numVec, infractionIndex), false)
  else
    isSafe

def checkVector(numVec: Vector[Int]): Boolean =
  var isSafe = true

  val ascending = isAscending(numVec)
  val descending = isDescending(numVec)

  if ascending(0) && numVec == ascending(1) then
    isSafe = checkLevels(numVec, true)
  else if ascending(0) && numVec != ascending(1) then
    isSafe = checkLevels(ascending(1), false)
  else if descending(0) && numVec == descending(1) then
    isSafe = checkLevels(numVec, true)
  else if descending(0) && numVec != descending(1) then
    isSafe = checkLevels(descending(1), false)
  else
    isSafe = false
  isSafe

def checkAllVectors: Int =
  linesToNumVectors.count(checkVector)