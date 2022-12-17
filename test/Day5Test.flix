namespace Day5Test
{
    use Day5.{solutionPart1, solutionPart2};

    def smallInput(): String = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

    @Test
    def part1SmallData(): Bool = Assert.eq("CMZ", solutionPart1(smallInput()))

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day5.txt", input -> Assert.eq("LBLVVTVLP", solutionPart1(input)))

    @Test
    def part2SmallData(): Bool = Assert.eq("MCD", solutionPart2(smallInput()))

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day5.txt", input -> Assert.eq("TPFFBDRJD", solutionPart2(input)))
}