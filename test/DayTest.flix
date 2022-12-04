pub def assertWith(filePath: String, assertion: String -> Bool): Bool \ IO = 
    File.read(filePath)
    |> Result.map(assertion)
    |> Result.getWithDefault(false)