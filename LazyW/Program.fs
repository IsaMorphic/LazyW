open System
open System.IO

let rBytes fpath = 
    File.ReadAllBytes (fpath)

let wBytes fPath (bytes : seq<byte>) = 
    let stream = File.Create(fPath)
    for x in bytes do stream.WriteByte (x)
    stream.Dispose ()

let rec lzwCompress (dict : Map<string, uint>) (index : int) (bytes : array<char>) = 
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
                    let newKey = longest + (bytes.[index + longest.Length] |> string)
                    dict.Add (newKey, dict.Count |> uint) 
                    else dict
                    )
                (index + longest.Length) bytes
            }

let packInts (ints : seq<uint>) = 
    ints |> Seq.chunkBySize 2 |> 
    Seq.map (
        fun arr -> 
            let x = arr.[0]
            let y = if arr.Length < 2 then 0u else arr.[1]
            seq { (x &&& 255u); (x >>> 8) ||| ((y &&& 15u) <<< 4); (y >>> 4); } 
            |> Seq.map byte
        )
    |> Seq.concat

let unpackInts (bytes : seq<byte>) = 
    bytes |> Seq.chunkBySize 3 |>
    Seq.map (
        fun arr -> 
            let a = arr.[0] |> uint
            let b = (if arr.Length < 2 then 0uy else arr.[1]) |> uint
            let c = (if arr.Length < 3 then 0uy else arr.[2]) |> uint
            seq { (a ||| ((b &&& 15u) <<< 8)); ((b >>> 4) ||| (c <<< 4)); }
        )
    |> Seq.concat

let rec lzwDecompress (dict : Map<uint, seq<char>>) (ints : seq<uint>) = 
    Seq.scan (
        fun (prev : seq<char>, dict : Map<uint,seq<char>>) nextInt ->
            if nextInt < (uint dict.Count) then
                let w = dict.Item nextInt;
                let nextStr = w |> Seq.head |> Seq.singleton |> Seq.append prev
                w, (if dict.Count < 4096 && not (prev |> Seq.isEmpty) then dict.Add (uint dict.Count, nextStr) else dict)
            else
                let v = prev |> Seq.head |> Seq.singleton |> Seq.append prev
                v, (if dict.Count < 4096 then dict.Add (uint dict.Count, v) else dict)
        ) (Seq.empty, dict) ints |> Seq.collect fst

let rDict = [| 0u .. 255u |] |> 
    Array.map (fun x -> (x |> char |> string, x)) |>
    Map.ofArray

let wDict = [| 0u .. 255u |] |> 
    Array.map (fun x -> (x, x |> char |> Seq.singleton)) |>
    Map.ofArray

// Compression
rBytes "./in_.bin" 
|> Array.map char
|> lzwCompress rDict 0
|> packInts

// Decompression
|> unpackInts 
|> lzwDecompress wDict
|> Seq.map byte
|> wBytes "./out.bin"

// NOTE: Lazy sequence logic is used for everything except reading the file, 
//       functional roundtrip can be computed 1 byte at a time!