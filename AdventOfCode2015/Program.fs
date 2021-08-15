module Run
open Utils

[<EntryPoint>]
let main argv =
    let input = IO.readAllLines "Day6a.txt" 
    let m = Array2D.zeroCreate 1000 1000
    let solution = Day6.executeInstructions input m
    printfn "%O" solution
    0