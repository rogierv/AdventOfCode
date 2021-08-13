module Day3
    let calculateHousesVisited input = 
        input |> Seq.scan (fun (x,y) move -> 
           match move with
           | '^' -> (x + 1, y)
           | '>' -> (x, y + 1)
           | 'v' -> (x - 1, y)
           | '<' -> (x, y - 1)
           | _ -> (0, 0)) (0,0) 
    
    let private foldToString list = list |> Seq.fold (fun state (_, m) -> state + m.ToString()) ""
    
    let private santaMoves input = 
        input 
        |> Seq.indexed 
        |> Seq.filter (fun (i, _) -> i % 2 = 0) 
        |> foldToString
    
    let private roboSantaMoves input =
        input 
        |> Seq.indexed 
        |> Seq.filter (fun (i, _) -> i % 2 <> 0) 
        |> foldToString
        
    let totalHousesVisited input = santaMoves input |> Seq.append (roboSantaMoves input) |> Seq.distinct |> Seq.length