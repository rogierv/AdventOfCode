open System

let m: int[,] = Array2D.zeroCreate 5 5

let toggle (x:int) = if x = 0 then 1 else 0

type instruction = TurnOn | Toggle | TurnOff

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.StartsWith(p))
    else
        None

let getAction s = match s with
                    | Prefix "turn on" rest -> TurnOn
                    | Prefix "toggle" rest -> Toggle
                    | Prefix "turn off" rest -> TurnOff

let execute instruction (x1, y1, x2, y2) m = 
    for i in x1 .. x2 do
        for j in y1 .. y2 do
            match instruction with
            | TurnOn -> Array2D.set m j i 1
            | TurnOff -> Array2D.set m j i 0
            | Toggle -> Array2D.get m j i |> toggle |> Array2D.set m j i
    m

execute Toggle (2,2,2,2) m

m

let test = "turn on 0,0 through 2,2"
//let instructions = [|"turn on 887,9 through 959,629";
//"turn on 454,398 through 844,448";
//"turn off 539,243 through 559,965";
//"turn off 370,819 through 676,868";
//"turn off 145,40 through 370,997"|]

let instructions = [|"turn on 0,0 through 4,4";
"toggle on 1,1 through 3,3"|]

getAction test

open System.Text.RegularExpressions
let getValues text = 
    let values = Regex.Matches(text, "\d+")
    (values.[0].Value |> int, values.[1].Value|> int, values.[2].Value|> int, values.[3].Value|> int)

let executeInstruction instruction m = execute (getAction instruction) (getValues instruction) m

let executeInstructions (listOfInstructions:string[]) m = 
    listOfInstructions 
    |> Seq.map (fun instruction -> executeInstruction instruction m)
    |> Seq.last
    |> Seq.cast<int> |> Seq.sum

executeInstructions instructions m