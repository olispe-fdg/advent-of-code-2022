open System.IO
open System.Collections.Generic
open MoreLinq

exception InvalidItem of string

let rucksackToCompartments (rucksack: string) =
    let compartmentLength = String.length rucksack / 2
    (rucksack.Substring(0, compartmentLength), rucksack.Substring(compartmentLength, compartmentLength))

let findDuplicate (a: string, b: string) = b[b.IndexOfAny(a.ToCharArray())]

let itemPriority item : int =
    match item with
    | c when c >= 'a' && c <= 'z' -> int item - int 'a' + 1
    | c when c >= 'A' && c <= 'Z' -> int item - int 'A' + 27
    | _ -> raise (InvalidItem(sprintf "Invalid item '%c'" item))

let uniqueChars (str: string) = str.ToCharArray() |> Set.ofArray

let findBadge (rucksacks: seq<Set<char>>) =
    rucksacks |> Set.intersectMany |> Set.minElement

let getDuplicatesPriority (rucksacks: IEnumerable<string>) =
    let mutable totalPriority = 0

    for rucksack in rucksacks do
        let compartmentA, compartmentB = rucksackToCompartments rucksack
        totalPriority <- totalPriority + ((compartmentA, compartmentB) |> findDuplicate |> itemPriority)

    totalPriority


let getBadgesPriority (rucksacks: IEnumerable<string>) =
    let mutable totalPriority = 0

    for group in rucksacks.Batch(3) do
        let groupUniqueChars =
            seq {
                for rucksack in group do
                    yield uniqueChars rucksack
            }

        totalPriority <- totalPriority + (groupUniqueChars |> findBadge |> itemPriority)

    totalPriority

[<EntryPointAttribute>]
let main args =
    let filePath = args.[1]

    let readLines = File.ReadLines(filePath)

    let duplicatesPriority = getDuplicatesPriority readLines
    let badgesPriority = getBadgesPriority readLines

    printfn "Total duplicate item priority: %i" duplicatesPriority
    printfn "Total badges priority: %i" badgesPriority

    0
