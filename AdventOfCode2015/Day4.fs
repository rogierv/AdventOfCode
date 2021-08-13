module Day4
    open System.Security.Cryptography
    open System.Text

    let private md5 (data : string) : string =
        use md5 = MD5.Create()
        let byteArray = Encoding.ASCII.GetBytes(data)
        (StringBuilder(), md5.ComputeHash(byteArray))
        ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
        |> string

    let rec calc key value =
        let hash = md5 $"{key}{value}"
        match hash with
        | h when h.StartsWith("000000") -> value
        | _ -> calc key (value+1)