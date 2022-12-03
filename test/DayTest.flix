pub def testFileInput(filePath: String, assertion: String -> Bool): Bool \ IO = 
    File.read(filePath)
    |> Result.map(assertion)
    |> Result.getWithDefault(false)