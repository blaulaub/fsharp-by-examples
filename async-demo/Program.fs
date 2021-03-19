open System

module Blub =

    type Kind =
    | OnlyFiles
    | OnlyDirs
    | OnlyChilds

    let content kind dir =
        match kind with
        | OnlyFiles -> IO.Directory.GetFiles dir
        | OnlyDirs -> IO.Directory.GetDirectories dir
        | OnlyChilds -> IO.Directory.GetFileSystemEntries dir

    let contentSeq kind dir = content kind dir |> Seq.map (fun x -> x)


open Blub

[<EntryPoint>]
let main argv =

    // get the current directory
    Environment.CurrentDirectory
    |> ignore

    // get files in the current directory
    Environment.CurrentDirectory
    |> contentSeq OnlyFiles
    |> ignore

    // get files in the current directory
    Environment.CurrentDirectory
    |> contentSeq OnlyDirs
    |> ignore

    // get almost everything in the current directory
    Environment.CurrentDirectory
    |> contentSeq OnlyChilds
    |> Seq.map IO.FileInfo
    |> Seq.iter (fun x -> printfn "%A %A" x x.Attributes)

    // get everything, including the directory itself and its parent
    Environment.CurrentDirectory |> IO.DirectoryInfo |> (fun x -> printfn "%A %A" x x.Attributes)
    Environment.CurrentDirectory |> IO.DirectoryInfo |> (fun x -> x.Parent) |> (fun x -> printfn "%A %A" x x.Attributes)

    0