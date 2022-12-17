namespace Day6
{
    def findFirstConsecutiveDistinctChars(dataStream: String, size: Int32): Int32 =
        def loop(window, index) = (
            let newWindow = dataStream |> String.charAt(index) |> c -> c :: window;
            let isStartOfPacket = newWindow |> List.toSet |> Set.size == size;
            if (isStartOfPacket) index + 1
            else loop(if (index < size - 1) newWindow else List.take(size - 1, newWindow), index + 1)
        );
        loop(Nil, 0)

    pub def solutionPart1(input: String): Int32 = findFirstConsecutiveDistinctChars(input, 4)
    pub def solutionPart2(input: String): Int32 = findFirstConsecutiveDistinctChars(input, 14)
}
