import os
import algorithm
import nre
import strutils

let fileName = paramStr(1)
let f = open(fileName)

type
    Stack = seq[string]
    Stacks = array[9, Stack]

let stackExpr = re"(?:(?:    )|(?:\[([A-Z])\]))"
let moveExpr = re"move ([0-9]+) from ([0-9]+) to ([0-9]+)"

var stacks: Stacks

# Read header
var line: string
while f.readLine(line):
    var stackIdx = 0
    var matchFound = false

    for match in line.findIter(stackExpr):
        if 0 in match.captureBounds:
            stacks[stackIdx].add(match.captures[0])

        stackIdx += 1
        matchFound = true

    if not matchFound: break

# We read crates top to bottom, reverse to get top items at end of seq
for stack in stacks.mitems:
    stack.reverse()

# Perform moves
proc moveCrate(self: var Stacks, fromStack: int, toStack: int) =
    self[toStack].add(self[fromStack].pop())

while f.readLine(line):
    let match = line.match(moveExpr)
    if match.isNone: continue

    let
        count = match.get.captures[0].parseInt
        fromStack = match.get.captures[1].parseInt - 1
        toStack = match.get.captures[2].parseInt - 1

    for i in 1..count:
        stacks.moveCrate(fromStack, toStack)

# Print stack tops
var output = ""
for stack in stacks:
    if stack.len > 0:
        output &= stack[^1]

echo output
