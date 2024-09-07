object DataSetExample {

  // Define the DataPipe type alias
  case class DataPipe[X, Y](f: X => Y) extends (X => Y) {
    def apply(x: X): Y = f(x)
  }

  // Define the DataSet class with logging
  class DataSet[X](val data: Iterable[X]) {
    
    // Map function: apply a transformation to each element in the dataset
    def map[Y](pipe: DataPipe[X, Y]): DataSet[Y] = {
      println(s"Applying map transformation with pipe: $pipe")
      new DataSet(data.map(pipe(_)))
    }

    // FlatMap function: apply a transformation that produces a collection of elements
    def flatMap[Y](pipe: DataPipe[X, Iterable[Y]]): DataSet[Y] = {
      println(s"Applying flatMap transformation with pipe: $pipe")
      new DataSet(data.flatMap(pipe(_)))
    }

    // Filter function: keep only elements that match the predicate
    def filter(pipe: DataPipe[X, Boolean]): DataSet[X] = {
      println(s"Applying filter with predicate: $pipe")
      new DataSet(data.filter(pipe(_)))
    }

    // FilterNot function: remove elements that match the predicate
    def filterNot(pipe: DataPipe[X, Boolean]): DataSet[X] = {
      println(s"Applying filterNot with predicate: $pipe")
      new DataSet(data.filterNot(pipe(_)))
    }

    // Print each element in the dataset
    def foreach(action: X => Unit): Unit = {
      println(s"Iterating over dataset elements:")
      data.foreach(action)
    }
  }

  // Companion object to create DataSet easily
  object DataSet {
    def apply[X](data: Iterable[X]): DataSet[X] = new DataSet(data)
  }

  def main(args: Array[String]): Unit = {
    // Sample DataSet
    println("Creating dataset with numbers 1 to 5")
    val dataset = DataSet(List(1, 2, 3, 4, 5))

    // Example usage of map
    println("\n--- Example: map ---")
    val doublePipe = DataPipe((x: Int) => x * 2)
    println("Mapping each element by doubling its value")
    val doubledDataset = dataset.map(doublePipe)
    doubledDataset.foreach(println) // Output: 2, 4, 6, 8, 10

    // Example usage of flatMap with Iterable[Int]
    println("\n--- Example: flatMap ---")
    val pairPipe = DataPipe((x: Int) => List(x, x * 2): Iterable[Int])
    println("Using flatMap to generate pairs of each element and its double")
    val numberPairs = dataset.flatMap(pairPipe)
    numberPairs.foreach(println) // Output: 1, 2, 2, 4, 3, 6, 4, 8, 5, 10

    // Example usage of filter
    println("\n--- Example: filter ---")
    val evenPipe = DataPipe((x: Int) => x % 2 == 0)
    println("Filtering even numbers")
    val evenNumbers = dataset.filter(evenPipe)
    evenNumbers.foreach(println) // Output: 2, 4

    // Example usage of filterNot
    println("\n--- Example: filterNot ---")
    println("Filtering out even numbers (keeping odd numbers)")
    val oddNumbers = dataset.filterNot(evenPipe)
    oddNumbers.foreach(println) // Output: 1, 3, 5
  }
}
