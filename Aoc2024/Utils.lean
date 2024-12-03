import Std
open Std.Internal
open Std.Internal.Parsec.String

namespace Utils

inductive AocPart where 
  | dayOnePartOne
  | dayOnePartTwo

def parseAocPart : String -> Option AocPart 
  | "d1p1" => some AocPart.dayOnePartOne
  | "d1p2" => some AocPart.dayOnePartTwo
  | _ => none

def getInput : IO String := do
  let stdin <- IO.getStdin
  let input <- stdin.getLine 
  pure input.trimRight

def readLines (s : String) : IO (List String) := do
  let path := System.mkFilePath [s]
  let lines <- IO.FS.lines path 
  pure lines.toList

partial def sortList : List Int -> List Int
  | [] => []
  | [x] => [x]
  | h::t => 
    let lesser := List.filter (· < h) t
    let greater := List.filter (· >= h) t
    (sortList lesser) ++ [h] ++ (sortList greater)

def digitToNat : Parser Nat := do
  return (<- digit).toNat - 48

def stringToNat : Parser Nat := do
  return (<- Parsec.many1 digitToNat).foldl (fun num d => 10 * num + d) 0

end Utils
