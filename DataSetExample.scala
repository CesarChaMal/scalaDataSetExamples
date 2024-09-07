// Define DataPipe and DataPipe2 for compatibility
case class DataPipe[X, Y](f: X => Y) {
  def apply(x: X): Y = f(x)
}

case class DataPipe2[X, Y, Z](f: (X, Y) => Z) {
  def apply(x: X, y: Y): Z = f(x, y)
}

// DataSet class
class DataSet[X](val data: Iterable[X]) {
  self =>

  // Filter method: keep elements that satisfy a predicate
  def filter(pipe: DataPipe[X, Boolean]): DataSet[X] = {
    DataSet[X](self.data.filter(pipe(_)))
  }

  // FilterNot method: remove elements that satisfy a predicate
  def filterNot(pipe: DataPipe[X, Boolean]): DataSet[X] = {
    DataSet[X](self.data.filterNot(pipe(_)))
  }

  // Map method: apply a function to each element and return a new DataSet
  def map[Y](pipe: DataPipe[X, Y]): DataSet[Y] = {
    DataSet[Y](data.map(pipe(_)))
  }

  // FlatMap method: apply a function that returns a collection of values and flatten the result
  def flatMap[Y](pipe: DataPipe[X, Iterable[Y]]): DataSet[Y] = {
    DataSet[Y](data.flatMap(pipe(_)))
  }
}

// Companion object for DataSet
object DataSet {
  def apply[X](data: Iterable[X]): DataSet[X] = new DataSet(data)
}

// Main object to demonstrate the usage of the DataSet methods
object DataSetExample {

  def main(args: Array[String]): Unit = {

    // Example for filter: keep only even numbers
    val dataset = DataSet(List(1, 2, 3, 4, 5, 6))
    val evenPipe = DataPipe((x: Int) => x % 2 == 0) // Function to check if number is even
    val evenNumbers = dataset.filter(evenPipe)
    println(s"Filtered even numbers: ${evenNumbers.data}") // Output: List(2, 4, 6)

    // Example for filterNot: remove even numbers
    val oddNumbers = dataset.filterNot(evenPipe)
    println(s"Filtered odd numbers: ${oddNumbers.data}") // Output: List(1, 3, 5)

    // Example for map: multiply each number by 2
    val doublePipe = DataPipe((x: Int) => x * 2) // Function to multiply number by 2
    val doubledNumbers = dataset.map(doublePipe)
    println(s"Doubled numbers: ${doubledNumbers.data}") // Output: List(2, 4, 6, 8, 10, 12)

    // Example for flatMap: return a list of the number and its square
    val pairPipe = DataPipe((x: Int) => List(x, x * x)) // Function to return list of number and its square
    val numberPairs = dataset.flatMap(pairPipe)
    println(s"Number pairs (number and its square): ${numberPairs.data}") // Output: List(1, 1, 2, 4, 3, 9, 4, 16, 5, 25, 6, 36)
  }
}
