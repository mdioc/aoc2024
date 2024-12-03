import Aoc2024.Utils
open Std.Internal.Parsec.String

namespace DayOne

private def input : String := "Aoc2024/d1.txt"

def parse : Parser (Nat × Nat) := do
  let left <- Utils.stringToNat
  _ <- ws
  let right <- Utils.stringToNat
  pure (left, right)

def readToListTuple : IO (List Nat × List Nat) := do
  let lines <- Utils.readLines input
  let parsedLines <- lines.mapM (fun line => IO.ofExcept (parse.run line))
  pure parsedLines.unzip

def partOne : IO Nat := do
  let (listL, listR) <- readToListTuple 
  let sortedL := (listL.map Int.ofNat) |> Utils.sortList
  let sortedR := (listR.map Int.ofNat) |> Utils.sortList
  let zipped := List.zipWith (fun left right => Int.natAbs (left - right)) sortedL sortedR
  return zipped.sum

def partTwo : IO Nat := do
  let (listL, listR) <- readToListTuple
  let countMult := listL.map (fun num => num * listR.count num)
  return countMult.sum

end DayOne
