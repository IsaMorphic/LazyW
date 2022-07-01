open System
open System.IO
open System.IO.Compression

let rBytes fpath = 
    File.ReadAllBytes (fpath)

let wBytes fPath (bytes : seq<byte>) = 
    let stream = File.Create(fPath)
    for x in bytes do stream.WriteByte (x)
    stream.Dispose ()

let rec lzwCompress (dict : Map<string, uint>) (bytes : array<char>) (index : int)  = 
    let longestOrNone = (seq { 
        for l in (dict.Keys |> Seq.map String.length |> Seq.max).. -1 .. 0 do 
            yield bytes[index..index + l] |> String.Concat
        } 
    |> Seq.tryFind dict.ContainsKey)

    if longestOrNone.IsNone then 
        Seq.empty
    else
        let longest = longestOrNone.Value
        seq {
            yield dict.Item longest
            yield! lzwCompress 
                (if dict.Count < 4096 then 
                    let newKey = longest + (bytes.[index + longest.Length + 1] |> string)
                    dict.Add (newKey, dict.Count |> uint) else dict)
                bytes (index + longest.Length)
            }

let packInts (ints : seq<uint>) = 
    ints |> Seq.chunkBySize 2 |> 
    Seq.map (
        fun arr -> 
            let x = arr.[0]
            let y = if arr.Length < 2 then 0u else arr.[1]
            seq { x &&& 255u; (x >>> 8) ||| (y &&& 15u <<< 4); y >>> 4; } 
            |> Seq.map byte
        )
    |> Seq.concat

let dict = [| 0u .. 255u |] |> 
    Array.map (fun x -> (x |> char |> string, x)) |>
    Map.ofArray

(rBytes "./in_.bin" 
|> Array.map char 
|> lzwCompress dict) 0 
|> packInts 
|> wBytes "./out.bin"