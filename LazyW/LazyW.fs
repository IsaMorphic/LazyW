module LazyW

open System
open System.IO

let rec lzwCompress (dict : Map<string, uint>) (index : int) (bytes : Stream) = 
    let maxLen = dict.Keys |> Seq.map String.length |> Seq.max
    let buff = Array.zeroCreate maxLen
    let longestOrNone = (seq { 
        for l in maxLen .. -1 .. 0 do
            bytes.Seek(index, SeekOrigin.Begin) |> ignore
            let read = bytes.Read(buff.AsSpan().Slice(0, l))
            yield buff[..read - 1] |> Array.map char |> String.Concat
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
                    let newKey = longest + (bytes.ReadByte() |> char |> string)
                    dict.Add (newKey, dict.Count |> uint) 
                    else dict
                    )
                (index + longest.Length) bytes
            }

let rec lzwDecompress (dict : Map<uint, seq<byte>>) (ints : seq<uint>) = 
    Seq.scan (
        fun (prev : seq<byte>, dict : Map<uint,seq<byte>>) nextInt ->
            if nextInt < (uint dict.Count) then
                let w = dict.Item nextInt;
                let nextStr = w |> Seq.head |> Seq.singleton |> Seq.append prev
                w, (if dict.Count < 4096 && not (prev |> Seq.isEmpty) then 
                    dict.Add (uint dict.Count, nextStr) else dict)
            else
                let v = prev |> Seq.head |> Seq.singleton |> Seq.append prev
                v, (if dict.Count < 4096 then 
                    dict.Add (uint dict.Count, v) else dict)
        ) (Seq.empty, dict) ints |> Seq.collect fst

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

let compress stream = 
    let rDict = [| 0u .. 255u |] |> 
        Array.map (fun x -> (x |> char |> string, x)) |>
        Map.ofArray
    stream |> lzwCompress rDict 0 |> packInts

let decompress bytes =
    let wDict = [| 0u .. 255u |] |> 
        Array.map (fun x -> (x, x |> byte |> Seq.singleton)) |>
        Map.ofArray
    bytes |> unpackInts |> lzwDecompress wDict
