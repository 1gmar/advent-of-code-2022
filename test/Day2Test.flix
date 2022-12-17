namespace Day2Test
{
    use Day2.{solutionPart1, solutionPart2};
    
    def smallInput(): String = "A Y
B X
C Z"

    @Test
    def part1SmallData(): Bool = Assert.eq(15, solutionPart1(smallInput()))

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day2.txt", input -> Assert.eq(12645, solutionPart1(input)))

    @Test
    def part2SmallData(): Bool = Assert.eq(12, solutionPart2(smallInput()))

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day2.txt", input -> Assert.eq(11756, solutionPart2(input)))
}
