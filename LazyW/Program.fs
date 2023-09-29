namespace LazyW

module Program = 
    open System
    open System.IO

    open LazyW
    open Streams

    [<EntryPoint>]
    let main(args) =  
        let func = 
            if args |> Array.tryHead = Some("compress") then
                Some(compress)
            else if args |> Array.tryHead = Some("decompress") then
                Some(rBytes >> decompress)
            else if args |> Array.tryHead = Some("roundtrip") then
                Some(compress >> decompress)
            else
                None

        let inputFilePath = args |> Array.tryItem 1
        let outputFilePath = args |> Array.tryItem 2

        if func.IsSome then
            let inputStream = 
                if inputFilePath.IsSome && inputFilePath.Value <> "-" then 
                    File.OpenRead inputFilePath.Value :> Stream
                else
                    let inner = Console.OpenStandardInput()
                    let outer = new MemoryStream()
                    inner.CopyTo(outer)
                    inner.Dispose()
                    outer
            let outputStream = 
                if outputFilePath.IsSome && outputFilePath.Value <> "-" then 
                    File.Create outputFilePath.Value :> Stream
                else
                    Console.OpenStandardOutput()
            func.Value >> wBytes >> using outputStream |> using inputStream
            +0
        else 
            eprintfn "Please specify at least one argument (compress/decompress/roundtrip), and optionally 2 file paths for input and output, respectively."
            -1