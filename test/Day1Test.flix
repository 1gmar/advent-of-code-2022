namespace Day1Test
{
    use Day1.{solutionPart1, solutionPart2};

    def smallInput(): String = "1000
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
    def part1TestCase1(): Bool = Assert.eq(24000, solutionPart1(smallInput()))

    @Test
    def part1TestCase2(): Bool \ IO = assertWith("resources/day1.txt", input -> Assert.eq(69836, solutionPart1(input)))

    @Test
    def part2TestCase1(): Bool = Assert.eq(45000, solutionPart2(smallInput()))

    @Test
    def part2TestCase2(): Bool \ IO = assertWith("resources/day1.txt", input -> Assert.eq(207968, solutionPart2(input)))
}
