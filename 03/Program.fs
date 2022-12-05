open System.IO

exception InvalidItem of string

let rucksackToCompartments (rucksack: string) =
    let compartmentLength = String.length rucksack / 2
    (rucksack.Substring(0, compartmentLength), rucksack.Substring(compartmentLength, compartmentLength))

let rec findDuplicate (a: string, b: string) = b[b.IndexOfAny(a.ToCharArray())]

let itemPriority item : int =
    match item with
    | c when c >= 'a' && c <= 'z' -> int item - int 'a' + 1
    | c when c >= 'A' && c <= 'Z' -> int item - int 'A' + 27
    | _ -> raise (InvalidItem(sprintf "Invalid item '%c'" item))

[<EntryPointAttribute>]
let main args =
    let filePath = args.[1]

    let readLines = File.ReadLines(filePath)
    let mutable totalPriority = 0

    for line in readLines do
        let compartmentA, compartmentB = rucksackToCompartments line

        totalPriority <- totalPriority + ((compartmentA, compartmentB) |> findDuplicate |> itemPriority)

    printfn "Total duplicate item priority: %i" totalPriority

    0
