open System.IO

[<EntryPointAttribute>]
let main args =
    let filePath = args.[0]

    let readLines = File.ReadLines(filePath)

    for line in readLines do
        printfn "%s" line

    0
