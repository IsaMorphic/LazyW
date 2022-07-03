module Streams

open System.IO

let rBytes (stream : Stream) = 
    Seq.init (stream.Length |> int) (
        fun pos -> 
            stream.Seek(pos, SeekOrigin.Begin) |> ignore
            stream.ReadByte() |> byte
        )

let wBytes (bytes : seq<byte>) (stream : Stream) = 
    for x in bytes do stream.WriteByte(x)

