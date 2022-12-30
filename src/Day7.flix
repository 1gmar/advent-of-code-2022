namespace Day7
{
    use Bool.{not};
    use List.{drop, dropWhile, filter, find, foldRight, length, map, sort, sum, takeWhile};
    use String.{startsWith, words};
    
    type alias Name = String
    type alias Size = Int32
    enum FSItem
    {
        case File(Name, Size),
        case Directory(Name, Size, List[FSItem])
    }
    enum FSCrumb(Name, List[FSItem], List[FSItem])
    type alias FSZipper = (FSItem, List[FSCrumb])

    def toSize(item: FSItem): Size = match item
    {
        case Directory(_, size, _) => size
        case File(_, size)         => size
    }

    def goUp(zipper: FSZipper): FSZipper = match zipper
    {
        case (_, Nil)                              => zipper
        case (fsItem, FSCrumb(name, ls, rs) :: bs) =>
            let items = ls ++ fsItem :: rs;
            let size = map(toSize, items) |> sum;
            (Directory(name, size, items), bs)
    }

    def goToRoot(zipper: FSZipper): FSZipper = match zipper
    {
        case (_, Nil) => zipper
        case _        => goUp(zipper) |> goToRoot
    }

    def goDownTo(dir: Name, zipper: FSZipper): FSZipper =
        def break(predicate, items) = (takeWhile(predicate >> not, items), dropWhile(predicate >> not, items));
        def nameIs(name, item) = match item
        {
            case Directory(fname, _, _) => fname == name
            case File(fname, _)         => fname == name
        };
        match zipper
        {
            case (Directory(name, _, items), bs) => match break(nameIs(dir), items)
            {
                case (ls, item :: rs) => (item, FSCrumb(name, ls, rs) :: bs)
                case _                => zipper
            }
            case _                               => zipper
        }

    def parseTerminalOutput(inputLines: List[String]): FSItem =
        def processCommand(zipper, line) = match words(line)
        {
            case "cd" :: "/" :: Nil  => goToRoot(zipper)
            case "cd" :: ".." :: Nil => goUp(zipper)
            case "cd" :: dir :: Nil  => goDownTo(dir, zipper)
            case _                   => zipper
        };
        def insertDirEntry(line, fsItem) = match fsItem
        {
            case Directory(name, dirSize, items) => match words(line)
            {
                case "dir" :: dir :: Nil    => Directory(name, dirSize, Directory(dir, 0, Nil) :: items)
                case sizeStr :: file :: Nil => Int32.parse(10, sizeStr)
                    |> Result.map(size -> Directory(name, dirSize + size, File(file, size) :: items))
                    |> Result.getWithDefault(fsItem)
                case _                      => fsItem
            }
            case _                               => fsItem
        };
        def processLsOutput(lines, zipper) = (
            let (fsItem, crumbs) = zipper;
            let outputLines = lines |> takeWhile(startsWith({prefix = "$"}) >> not);
            let updatedItem = foldRight(insertDirEntry, fsItem, outputLines);
            (drop(length(outputLines), lines), (updatedItem, crumbs))
        );
        def loop(lines, zipper) = match lines
        {
            case line :: ls if startsWith({prefix = "$"}, line) => 
                String.drop(1, line) |> processCommand(zipper) |> zpr -> loop(ls, zpr)
            case _ :: _ => processLsOutput(lines, zipper) |> match (ls, zpr) -> loop(ls, zpr)
            case Nil    => goToRoot(zipper) |> fst
        };
        loop(drop(1, inputLines), (Directory("/", 0, Nil), Nil))
    
    def getDirSizes(root: FSItem): List[Int32] =
        def collectDirSizes(item, sizes) = match item
        {
            case Directory(_, size, items) =>
                let innerSizes = foldRight(collectDirSizes, Nil, items);
                size :: sizes ++ innerSizes
            case _                         => sizes
        };
        foldRight(collectDirSizes, Nil, root :: Nil)
    
    pub def solutionPart1(input: String): Int32 =
        String.lines(input)
        |> parseTerminalOutput
        |> getDirSizes
        |> filter(size -> size <= 100_000)
        |> sum

    pub def solutionPart2(input: String): Int32 =
        let root = String.lines(input) |> parseTerminalOutput;
        let totalSize = toSize(root);
        getDirSizes(root)
        |> sort
        |> find(size -> size >= Int32.max(0, 30_000_000 - (70_000_000 - totalSize)))
        |> Option.getWithDefault(0)
}
