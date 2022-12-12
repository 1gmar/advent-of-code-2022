namespace Day4
{
    type alias Range = {from = Int32, to = Int32}

    def parseRange(rangeStr: String): Option[Range] = match String.splitOn({substr = "-"}, rangeStr)
    {
        case x :: y :: Nil =>
            use Option.flatMap;
            let* low = Int32.parse(10, x) |> Result.toOption;
            let* high = Int32.parse(10, y) |> Result.toOption;
            Some({from = low, to = high})
        case _             => None
    }
    
    def toAssignmentPair(inputLine: String): Option[(Range, Range)] = match String.splitOn({substr = ","}, inputLine)
    {
        case x :: y :: Nil =>
            use Option.flatMap;
            let* range1 = parseRange(x);
            let* range2 = parseRange(y);
            Some((range1, range2))
        case _             => None
    }

    def fullyOverlappingRanges(pair: (Range, Range)): Bool =
        let (range1, range2) = pair;
        range1.from <= range2.from and range1.to >= range2.to
        or
        range1.from >= range2.from and range1.to <= range2.to
    
    def overlappingRanges(pair: (Range, Range)): Bool =
        let (range1, range2) = pair;
        not (range1.to < range2.from or range1.from > range2.to)

    pub def solutionPart1(input: String): Int32 =
        input
        |> String.lines
        |> List.flatMap(toAssignmentPair >> Option.toList)
        |> List.count(fullyOverlappingRanges)

    pub def solutionPart2(input: String): Int32 =
        input
        |> String.lines
        |> List.flatMap(toAssignmentPair >> Option.toList)
        |> List.count(overlappingRanges)
}
