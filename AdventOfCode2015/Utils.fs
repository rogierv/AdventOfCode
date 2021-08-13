namespace Utils

module IO =
    let public readLines filePath = System.IO.File.ReadLines(filePath) |> Seq.head
    let public readAllLines filePath = System.IO.File.ReadAllLines(filePath)