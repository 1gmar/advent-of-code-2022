use Day1.{solutionPart1, solutionPart2};

def input1(): String = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

@Test
def part1TestCase1(): Bool = Assert.eq(24000, solutionPart1(input1()))

@Test
def part1TestCase2(): Bool \ IO = 
    File.read("resources/day1.txt")
    |> Result.map(input -> Assert.eq(69836, solutionPart1(input)))
    |> Result.getWithDefault(false)

@Test
def part2TestCase1(): Bool = Assert.eq(45000, solutionPart2(input1()))

@Test
def part2TestCase2(): Bool \ IO =
    File.read("resources/day1.txt")
    |> Result.map(input -> Assert.eq(207968, solutionPart2(input)))
    |> Result.getWithDefault(false)