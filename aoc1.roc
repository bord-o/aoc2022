app "aoc1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [pf.Stdout, "aoc1_data.txt" as puzzleInput : Str]
    provides [main] to pf

# testinputs = [
#     "1abc2",
#    "pqr3stu8vwx",
#    "a1b2c3d4e5f",
#    "treb7uchet",
#]

realinputs = 
    Str.split puzzleInput "\n"


onlyNums = \s -> 
    digits = ["0", "1","2","3", "4","5","6","7","8","9"]
    s
        |> Str.graphemes
        |> List.keepIf (\char -> digits |> List.contains char) 
        |> Str.joinWith ""

firstAndLast = \s ->
    chars = s |> Str.graphemes
    first = chars |> List.get  0
    last = chars |> List.get (List.len chars - 1)
    when (first, last) is 
        (Ok f, Ok l) -> (f, l)
        _ -> crash "Bad first and last"

parseInts = \(f, l) -> 
    when Str.concat f l |> Str.toU64 is
    Ok i -> i
    _ -> crash "error w/ conversion"

ans = \l ->
    state, elem <- List.walk l 0
    elem |> onlyNums |> firstAndLast |> parseInts |> Num.add state


main = 
    realinputs |> ans |> Num.toStr |> Stdout.line
