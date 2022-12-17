namespace Day6Test
{
    use Day6.{solutionPart1, solutionPart2};
    
    @Test
    def part1SmallData(): Bool =
        def smallTestCases(): List[(Int32, String)]
            =  (7, "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
            :: (5, "bvwbjplbgvbhsrlpgdmjqwftvncz")
            :: (6, "nppdvjthqldpwncqszvftbrmjlhg")
            :: (10, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
            :: (11, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
            :: Nil;
        List.forAll(match (expected, input) -> Assert.eq(expected, solutionPart1(input)), smallTestCases())

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day6.txt", input -> Assert.eq(1480, solutionPart1(input)))

    @Test
    def part2SmallData(): Bool =
        def smallTestCases(): List[(Int32, String)]
            =  (19, "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
            :: (23, "bvwbjplbgvbhsrlpgdmjqwftvncz")
            :: (23, "nppdvjthqldpwncqszvftbrmjlhg")
            :: (29, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
            :: (26, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
            :: Nil;
        List.forAll(match (expected, input) -> Assert.eq(expected, solutionPart2(input)), smallTestCases())

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day6.txt", input -> Assert.eq(2746, solutionPart2(input)))
}
