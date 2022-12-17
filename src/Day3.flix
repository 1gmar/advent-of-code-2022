namespace Day3
{
    def splitRucksackContent(rucksackContent: String): (String, String) =
        String.splitAt(String.length(rucksackContent) / 2, rucksackContent)

    def findCommonItemTypes(rucksackCompartments: (String, String)): List[Char] =
        let toSet = String.toList >> List.toSet;
        let (fstContent, sndContent) = rucksackCompartments;
        let commonItemTypes = Set.intersection(toSet(fstContent), toSet(sndContent));
        Set.toList(commonItemTypes)

    def toPriority(itemType: Char): Int32 =
        let itemTypeIntCode = Char.toInt32(itemType);
        if (Char.isUpperCase(itemType)) itemTypeIntCode - 38 else itemTypeIntCode - 96
    
    def groupBy3(rucksacks: List[String]): List[(String, String, String)] =
        def fillGroups(list, accumulator) = match list
        {
            case x :: y :: z :: _ => fillGroups(List.drop(3, list), (x, y, z) :: accumulator)
            case _                => accumulator
        };
        fillGroups(rucksacks, Nil)

    def findBadge(groupRucksacks: (String, String, String)): List[Char] =
        let toSet = String.toList >> List.toSet;
        let (fstRucksack, sndRucksack, thirdRucksack) = groupRucksacks;
        let badge = toSet(fstRucksack) `Set.intersection` toSet(sndRucksack) |> Set.intersection(toSet(thirdRucksack));
        Set.toList(badge)

    pub def solutionPart1(input: String): Int32 =
        input
        |> String.lines
        |> List.flatMap(splitRucksackContent >> findCommonItemTypes >> List.map(toPriority))
        |> List.sum

    pub def solutionPart2(input: String): Int32 =
        input
        |> String.lines
        |> groupBy3
        |> List.flatMap(findBadge >> List.map(toPriority))
        |> List.sum
}
