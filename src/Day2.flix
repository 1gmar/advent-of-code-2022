sealed class Score[a]
{
    pub def score(it: a): Int32
}

enum Outcome
{
    case Win, Loss, Draw
}

enum Shape with Eq, Order, ToString, Boxable
{
    case Rock, Paper, Scissors
}

instance Score[Shape]
{
    pub def score(shape: Shape): Int32 = match shape
    {
        case Rock     => 1
        case Paper    => 2
        case Scissors => 3
    }
}

instance Score[Outcome]
{   
    pub def score(outcome: Outcome): Int32 = match outcome
    {
        case Win  => 6
        case Loss => 0
        case Draw => 3
    }
}

namespace Day2
{
    def gameRules(): #{Win(Shape, Shape), Loss(Shape, Shape), Draw(Shape, Shape)} = #{
        Win(Rock, Scissors).
        Win(Scissors, Paper).
        Win(Paper, Rock).
        Loss(x, y) :- Win(y, x).
        Draw(x, x) :- Win(x, _).
    }

    def parseOpponentShape(token: Char): Option[Shape] = match token
    {
        case 'A' => Some(Rock)
        case 'B' => Some(Paper)
        case 'C' => Some(Scissors)
        case _   => None
    }

    def parseShape(token: Char): Option[Shape] = match token
    {
        case 'X' => Some(Rock)
        case 'Y' => Some(Paper)
        case 'Z' => Some(Scissors)
        case _   => None
    }

    def parseOutcome(token: Char): Option[Outcome] = match token
    {
        case 'X' => Some(Loss)
        case 'Y' => Some(Draw)
        case 'Z' => Some(Win)
        case _   => None
    }

    def parseRound(row: String): Option[(Shape, Shape)] = match String.splitOn({substr = " "}, row)
    {
        case opponentShapeStr :: shapeStr :: Nil => 
            use Option.flatMap;
            let* opponentShape = parseOpponentShape(String.charAt(0, opponentShapeStr));
            let* shape = parseShape(String.charAt(0, shapeStr));
            Some((opponentShape, shape))
        case _                                   => None
    }

    def parseStrat(row: String): Option[(Shape, Outcome)] = match String.splitOn({substr = " "}, row)
    {
        case opponentShapeStr :: outcomeStr :: Nil => 
            use Option.flatMap;
            let* opponentShape = parseOpponentShape(String.charAt(0, opponentShapeStr));
            let* shape = parseOutcome(String.charAt(0, outcomeStr));
            Some((opponentShape, shape))
        case _                                     => None
    }

    def playRound(playerShapes: (Shape, Shape)): (Outcome, Shape) =
        def isNotEmpty(list: List[Bool]): Bool = List.isEmpty(list) |> Bool.not;
        let (a, b) = playerShapes;
        if (isNotEmpty(query gameRules() select true from Win(b, a))) (Win, b) 
        else if (isNotEmpty(query gameRules() select true from Loss(b, a))) (Loss, b) 
        else (Draw, b)

    def playStrat(strategy: (Shape, Outcome)): Option[(Outcome, Shape)] = 
        let (opponentShape, outcome) = strategy;
        let queryResult = match outcome
            {
                case Win  => query gameRules() select winningShape from Win(winningShape, opponentShape)
                case Loss => query gameRules() select throwShape from Loss(throwShape, opponentShape)
                case Draw => opponentShape :: Nil
            };
        Option.map(shape -> (outcome, shape), List.head(queryResult))
    
    def computeScore(roundResult: (Outcome, Shape)): Int32 =
        let (outcome, shape) = roundResult;
        Score.score(outcome) + Score.score(shape)
    
    pub def solutionPart1(input: String): Int32 = 
        input
        |> String.lines
        |> List.map(parseRound >> Option.map(playRound) >> Option.map(computeScore))
        |> List.flatMap(Option.toList)
        |> List.sum
    
    pub def solutionPart2(input: String): Int32 =
        input
        |> String.lines
        |> List.map(parseStrat >> Option.flatMap(playStrat) >> Option.map(computeScore))
        |> List.flatMap(Option.toList)
        |> List.sum
}
