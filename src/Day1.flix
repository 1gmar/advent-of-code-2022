namespace Day1
{
    def readCaloryLines(input: String): List[List[String]] =
        input
        |> String.splitOn({substr = "\n\n"})
        |> List.map(String.splitOn({substr = "\n"}))

    def sumElfCalories(calories: List[String]): Int32 =
        calories 
        |> List.flatMap(x -> Result.toList(Int32.parse(10, x)))
        |> List.sum

    pub def solutionPart1(input: String): Int32 =
        input
        |> readCaloryLines
        |> List.map(sumElfCalories)
        |> List.maximum
        |> Option.getWithDefault(0)
    
    pub def solutionPart2(input: String): Int32 =
        input
        |> readCaloryLines
        |> List.map(sumElfCalories)
        |> List.sortWith(x -> y -> if (x < y) GreaterThan else if (x > y) LessThan else EqualTo)
        |> List.take(3)
        |> List.sum
}
