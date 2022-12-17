namespace Day4Test
{
    use Day4.{solutionPart1, solutionPart2};

    def smallInput(): String = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

    @Test
    def part1SmallData(): Bool = Assert.eq(2, solutionPart1(smallInput()))

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day4.txt", input -> Assert.eq(524, solutionPart1(input)))

    @Test
    def part2SmallData(): Bool = Assert.eq(4, solutionPart2(smallInput()))

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day4.txt", input -> Assert.eq(798, solutionPart2(input)))
}