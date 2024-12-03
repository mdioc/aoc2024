import Aoc2024
open Utils

def runAocPart : Option Utils.AocPart -> IO Nat
  | some Utils.AocPart.dayOnePartOne => DayOne.partOne
  | some Utils.AocPart.dayOnePartTwo => DayOne.partTwo
  | none => pure 0

def main : IO Unit := do
  IO.print "Enter a AOC day/part in format d<daynum>p<partnum>: "
  let input <- Utils.getInput
  let part := Utils.parseAocPart input
  let output <- runAocPart part
  IO.println s!"Answer is: {output}"
