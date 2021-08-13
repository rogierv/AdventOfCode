module Day5
    type Result<'TSuccess,'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure

    let bind switchFunction twoTrackInput =
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failure f -> Failure f

    let private hasThreeVowels input =
        input 
        |> Seq.fold (fun a b -> if ("aeiou".Contains(b.ToString())) then a+1 else a) 0 
        |> fun x -> if x >= 3 then Success input else Failure $"is naughty because it contains only {x} vowel"

    let private hasLetterAppearsTwice (input:string) =
        let rec recursive (part:string) =
            if part.Length <= 1 then Failure "is naughty because it has no double letter" else
                match part with
                | x when x.[0] = x.[1] -> Success input
                | _ -> recursive part.[1..]
        recursive input

    let private doesNotContain (input:string) =
        let rec recursive (part:string) =
            if part.Length <= 1 then Success input else
                match part.[..1] with
                | x when List.contains x ["ab";"cd";"pq";"xy"] -> Failure $"is naughty because it contains the string {x}"
                | _ -> recursive part.[1..]
        recursive input
        
    let private containsPairOfTwoLetters (input:string) =
        let rec recursive (part:string) =
            if part.Length <= 1 then Failure $"is naughty because it no pair that appears twice" else
                match part.[..1] with
                | x when part.[2..].IndexOf(x) >= 0 -> Success input
                | _ -> recursive part.[1..]
        recursive input
    
    let private containsSameLetterWithOneInBetween (input:string) =
        let rec recursive (part:string) =
            if part.Length <= 2 then Failure $"is naughty because ..." else
                match part.[..2] with
                | x when x.[0] = x.[2] -> Success input
                | _ -> recursive part.[1..]
        recursive input

    let private combinedValidation = hasThreeVowels >> bind hasLetterAppearsTwice >> bind doesNotContain
    let private combinedValidationPart2 = containsPairOfTwoLetters >> bind containsSameLetterWithOneInBetween

    let private parseToValue input =
        match combinedValidation input with
        | Success msg -> 1
        | Failure msg -> printfn $"{input} - {msg}"; 0

    let private parseToValuePart2 input =
        match combinedValidationPart2 input with
        | Success msg -> 1
        | Failure msg -> printfn $"{input} - {msg}"; 0
    
    let countNiceStrings (array:string[]) =
        array |> Array.toList |> List.filter (fun str -> parseToValue str = 1) |> List.length

    let countNiceStringsPart2 (array:string[]) =
        array |> Array.toList |> List.filter (fun str -> parseToValuePart2 str = 1) |> List.length


