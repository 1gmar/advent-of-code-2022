pub class DayTest[day] with Day[day]
{
    pub def testCasesPart1(test: day): Map[Int64, String]
    pub def testCasesPart2(test: day): Map[Int64, String]
}

def testPart(test: day, part: day -> String -> Int64, testCases: day -> Map[Int64, String]): Bool with DayTest[day] = 
    Map.forAll(expected -> input -> Assert.eq(expected, part(test)(input)), testCases(test))

def testPart1(test: dayTest): Bool with DayTest[dayTest] = testPart(test, Day.part1, DayTest.testCasesPart1)
def testPart2(test: dayTest): Bool with DayTest[dayTest] = testPart(test, Day.part2, DayTest.testCasesPart2)