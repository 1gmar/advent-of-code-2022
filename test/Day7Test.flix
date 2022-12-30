namespace Day7Test
{
    use Day7.{solutionPart1, solutionPart2}

    def smallInput(): String = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

    @Test
    def part1SmallData(): Bool = Assert.eq(95_437, solutionPart1(smallInput()))

    @Test
    def part1BigData(): Bool \ IO = assertWith("resources/day7.txt", input -> Assert.eq(1_390_824, solutionPart1(input)))

    @Test
    def part2SmallData(): Bool = Assert.eq(24_933_642, solutionPart2(smallInput()))

    @Test
    def part2BigData(): Bool \ IO = assertWith("resources/day7.txt", input -> Assert.eq(7_490_863, solutionPart2(input)))
}
