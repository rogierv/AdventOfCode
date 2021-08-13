open Utils

[<EntryPoint>]
let main argv =
    let input = IO.readAllLines "Day5a.txt" 
    let solution = Day5.countNiceStringsPart2 input
    printfn "%O" solution
    0