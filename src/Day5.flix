namespace Day5
{
    type alias CrateStack = List[Char]
    type alias CrateStacks = Map[Int32, CrateStack]
    type alias MoveInstruction = {move = Int32, from = Int32, to = Int32}
    type alias CraneProgram = (CrateStacks, List[MoveInstruction])
    
    def updateCrateStacks(crateLine: String, idAndIndex: (Int32, Int32), crateStacks: CrateStacks): CrateStacks =
        let (stackId, index) = idAndIndex;
        let crate = String.charAt(index, crateLine);
        if (Char.isWhiteSpace(crate)) crateStacks else Map.adjust(stack -> crate :: stack, stackId, crateStacks)

    def collectCrateStacks(stackIdsAndIndices: List[(Int32, Int32)], crateLine: String, crateStacks: CrateStacks): CrateStacks =
        List.foldRight(updateCrateStacks(crateLine), crateStacks, stackIdsAndIndices)

    def parseCrateStacks(stacksInput: String): Option[CrateStacks] =
        let stackLines = String.lines(stacksInput);
        forM(
            crateStackLines <- List.init(stackLines);
            stackIdsLine <- List.last(stackLines);
            stackIds <- String.words(stackIdsLine) |> List.traverse(Int32.parse(10) >> Result.toOption)
        )
        yield (
            let stackIdIndices = stackIdsLine |> String.findIndices(Char.isDigit);
            List.foldRight(collectCrateStacks(stackIds `List.zip` stackIdIndices), List.toMapWith(_ -> Nil, stackIds), crateStackLines)
        )

    def toMoveInstruction(tokens: List[String]): Option[MoveInstruction] = match tokens
    {
        case moveStr :: fromStr :: toStr :: Nil =>
            forM(
                moveInt <- Int32.parse(10, moveStr) |> Result.toOption;
                fromInt <- Int32.parse(10, fromStr) |> Result.toOption;
                toInt <- Int32.parse(10, toStr) |> Result.toOption
            )
            yield {move = moveInt, from = fromInt, to = toInt}
        case _                                  => None
    }

    def parseMovements(movementInput: String): List[MoveInstruction] =
        movementInput
        |> String.lines
        |> List.map(String.words >> List.filter(String.forAll(Char.isDigit)) >> toMoveInstruction)
        |> List.flatMap(Option.toList)
    
    def parseInput(input: String): Option[CraneProgram] = match String.splitOn({substr = "\n\n"}, input)
    {
        case stacks :: movements :: Nil => parseCrateStacks(stacks) |> Option.map(s -> (s, parseMovements(movements)))
        case _                          => None
    }
    
    def runRearrengements(moveStrategy: (CrateStack, CrateStack) -> CrateStack, craneProgram: CraneProgram): CrateStacks = 
        def runRearrangement(crateStacks, instruction) = (
            let moveQuantity = instruction.move;
            let movedCrates = Map.get(instruction.from, crateStacks) |> Option.toList |> List.flatMap(List.take(moveQuantity));
            crateStacks
            |> Map.adjust(List.drop(moveQuantity), instruction.from)
            |> Map.adjust(stack -> moveStrategy(stack, movedCrates), instruction.to)
        );
        let (crateStacks, moveInstructions) = craneProgram;
        List.foldLeft(runRearrangement, crateStacks, moveInstructions)

    def peekTopCrates(crateStacks: CrateStacks): Option[String] =
        Map.toList(crateStacks) 
        |> List.sortBy(fst)
        |> List.traverse(snd >> List.head >> Option.map(Char.toString))
        |> Option.map(List.join(""))
    
    pub def solutionPart1(input: String): String =
        input
        |> parseInput
        |> Option.map(runRearrengements(List.foldLeft((xs, x) -> x :: xs)))
        |> Option.flatMap(peekTopCrates)
        |> Option.getWithDefault("")

    pub def solutionPart2(input: String): String =
        input
        |> parseInput
        |> Option.map(runRearrengements(List.foldRight((x, xs) -> x :: xs)))
        |> Option.flatMap(peekTopCrates)
        |> Option.getWithDefault("")
}
