namespace Day3Test
{
    use Day3.{solutionPart1, solutionPart2};

    def smallInput(): String = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

    @Test
    def part1SmallData(): Bool = Assert.eq(157, solutionPart1(smallInput()))

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day3.txt", input -> Assert.eq(7889, solutionPart1(input)))

    @Test
    def part2SmallData(): Bool = Assert.eq(70, solutionPart2(smallInput()))

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day3.txt", input -> Assert.eq(2825, solutionPart2(input)))
}
