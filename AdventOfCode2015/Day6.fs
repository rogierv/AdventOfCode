module Day6
    open System.Text.RegularExpressions
        
    type instruction = TurnOn | Toggle | TurnOff

    let  private toggle (x:int) = if x = 0 then 1 else 0
    
    let private increase (current:int) (value:int) = current + value
    let private decrease (current:int) = if current = 0 then 0 else current - 1

    let private (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.StartsWith(p))
        else
            None
    
    let private getAction s = match s with
                        | Prefix "turn on" rest -> TurnOn
                        | Prefix "toggle" rest -> Toggle
                        | Prefix "turn off" rest -> TurnOff
    
    let private execute instruction (x1, y1, x2, y2) m = 
        for i in x1 .. x2 do
            for j in y1 .. y2 do
                match instruction with
                | TurnOn -> Array2D.set m j i 1
                | TurnOff -> Array2D.set m j i 0
                | Toggle -> Array2D.get m j i |> toggle |> Array2D.set m j i
        m


    let private execute2 instruction (x1, y1, x2, y2) m = 
        for i in x1 .. x2 do
            for j in y1 .. y2 do
                match instruction with
                | TurnOn ->  Array2D.get m j i |> increase 1 |> Array2D.set m j i
                | TurnOff -> Array2D.get m j i |> decrease |> Array2D.set m j i
                | Toggle -> Array2D.get m j i |> increase 2 |> Array2D.set m j i
        m

    let private getValues text = 
        let values = Regex.Matches(text, "\d+")
        (values.[0].Value |> int, values.[1].Value|> int, values.[2].Value|> int, values.[3].Value|> int)

    let private executeInstruction instruction m = execute2 (getAction instruction) (getValues instruction) m

    let executeInstructions (listOfInstructions:string[]) m = 
        listOfInstructions 
        |> Seq.map (fun instruction -> executeInstruction instruction m)
        |> Seq.last
        |> Seq.cast<int> |> Seq.sum