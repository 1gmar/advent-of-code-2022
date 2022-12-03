namespace DayTest2
{
    use Day2.{solutionPart1, solutionPart2};
    
    def smallInput(): String = "A Y
B X
C Z"

    @Test
    def part1TestCase1(): Bool = Assert.eq(15, solutionPart1(smallInput()))

    @Test
    def part1TestCase2(): Bool \ IO = testFileInput("resources/day2.txt", input -> Assert.eq(12645, solutionPart1(input)))

    @Test
    def part2TestCase1(): Bool = Assert.eq(12, solutionPart2(smallInput()))

    @Test
    def part2TestCase2(): Bool \ IO = testFileInput("resources/day2.txt", input -> Assert.eq(11756, solutionPart2(input)))
}
