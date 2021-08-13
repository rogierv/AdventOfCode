module Day2
    open System
    
    let private getDimensions (dimensions:string) = 
        dimensions.Split('x') 
        |> Seq.map Int32.Parse 
        |> Seq.toList
        |> fun dim -> [dim.[0]; dim.[1]; dim.[2]]
    
    let private calculateSurface (l:int list) =
        let surface = [l.[0]*l.[1]; l.[1]*l.[2]; l.[2]*l.[0]]
        List.min(surface) + (List.sum(surface) * 2)
    
    let private calculateWrappingPaper input =
        input 
        |> getDimensions
        |> calculateSurface
    
    let calculateTotalWrappingPaper input =
        input 
        |> Seq.map (fun dim -> calculateWrappingPaper dim)
        |> Seq.sum

    let private wrapRibbon l = (List.sum(l) - List.max(l)) * 2
    let private wrapBow l = List.fold (*) 1 l
    let private wrap l = wrapRibbon l + wrapBow l

    let private calculateWrapRibbon input =
        input
        |> getDimensions
        |> wrap

    let calculateTotalRibbon input =
        input
        |> Seq.map (fun dim -> calculateWrapRibbon dim)
        |> Seq.sum