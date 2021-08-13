module Day1  
    let findFloor (input:string) =
        input 
            |> Seq.fold (fun counter instruction -> if instruction = '(' then counter + 1 else counter - 1) 0

    let findBasement input =
        let rec find (s:string) pos lvl =
            match lvl with
            | -1 -> pos
            | _  -> match s.[pos] with
                    | '(' -> find s (pos+1) (lvl+1)
                    | _   -> find s (pos+1) (lvl-1)
        find input 0 0 