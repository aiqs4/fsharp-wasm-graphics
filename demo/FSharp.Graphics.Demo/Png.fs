/// Pure F# PNG encoder – no external dependencies.
///
/// Produces valid PNG files using zlib "stored" (non-compressed) blocks so
/// that no Huffman/LZ77 implementation is required.  The output is larger
/// than a compressed PNG but byte-for-byte identical in pixel data.


/// TODO: obsolete and now in FSharp.Grapihs.Host.Png !!!!
module FSharp.Graphics.Demo.Png

// ─────────────────────────────────────────────────────────────────────────────
// CRC-32
// ─────────────────────────────────────────────────────────────────────────────

/// CRC-32 table using the standard IEEE polynomial 0xEDB88320.
let private crcTable =
    Array.init 256 (fun n ->
        let mutable c = uint32 n
        for _ = 0 to 7 do
            if c &&& 1u <> 0u
            then c <- 0xEDB88320u ^^^ (c >>> 1)
            else c <- c >>> 1
        c)

let private updateCrc (crc: uint32) (data: byte[]) (off: int) (len: int) =
    let mutable c = crc ^^^ 0xFFFFFFFFu
    for i = off to off + len - 1 do
        c <- crcTable.[int (byte c ^^^ data.[i])] ^^^ (c >>> 8)
    c ^^^ 0xFFFFFFFFu

let private crc32 (data: byte[]) (off: int) (len: int) = updateCrc 0u data off len

// ─────────────────────────────────────────────────────────────────────────────
// Adler-32 (zlib checksum)
// ─────────────────────────────────────────────────────────────────────────────

let private adler32 (data: byte[]) (off: int) (len: int) =
    let mutable s1 = 1u
    let mutable s2 = 0u
    for i = off to off + len - 1 do
        s1 <- (s1 + uint32 data.[i]) % 65521u
        s2 <- (s2 + s1)              % 65521u
    (s2 <<< 16) ||| s1

// ─────────────────────────────────────────────────────────────────────────────
// Buffer builder helpers
// ─────────────────────────────────────────────────────────────────────────────

let private writeUInt32BE (buf: byte[]) (off: int) (v: uint32) =
    buf.[off]   <- byte (v >>> 24)
    buf.[off+1] <- byte (v >>> 16)
    buf.[off+2] <- byte (v >>>  8)
    buf.[off+3] <- byte  v
    off + 4

let private writeUInt16LE (buf: byte[]) (off: int) (v: uint16) =
    buf.[off]   <- byte  v
    buf.[off+1] <- byte (v >>> 8)
    off + 2

// ─────────────────────────────────────────────────────────────────────────────
// PNG chunk writer
// ─────────────────────────────────────────────────────────────────────────────

/// Write a PNG chunk into a ResizeArray<byte>.
let private writeChunk (out: ResizeArray<byte>) (chunkType: byte[]) (data: byte[]) =
    let len = data.Length
    let lenBuf = Array.zeroCreate 4
    writeUInt32BE lenBuf 0 (uint32 len) |> ignore
    out.AddRange lenBuf
    out.AddRange chunkType
    out.AddRange data
    // CRC covers chunk type + data
    let crcInput = Array.append chunkType data
    let crcVal   = crc32 crcInput 0 crcInput.Length
    let crcBuf   = Array.zeroCreate 4
    writeUInt32BE crcBuf 0 crcVal |> ignore
    out.AddRange crcBuf

// ─────────────────────────────────────────────────────────────────────────────
// DEFLATE stored-block wrapper
// ─────────────────────────────────────────────────────────────────────────────

/// Wrap raw data in a zlib stream using only non-compressed DEFLATE blocks
/// (RFC 1951 §3.2.4 "stored" type 00).
let private zlibWrap (data: byte[]) : byte[] =
    // zlib header: CMF=0x78 (deflate, window=32KB), FLG makes CMF*256+FLG divisible by 31.
    let cmf = 0x78uy  // deflate, 32k window
    let flg = 0x01uy  // check bits (0x7801 mod 31 = 0)

    // Split data into 65535-byte DEFLATE stored blocks.
    let blockSize = 65535
    let numBlocks = (data.Length + blockSize - 1) / blockSize |> max 1
    // Each block: BFINAL(1) + BTYPE(1) + LEN(2) + NLEN(2) + data = 5 + data bytes
    let deflateLen = numBlocks * 5 + data.Length
    let buf = Array.zeroCreate (2 + deflateLen + 4)  // header(2) + deflate + adler32(4)
    buf.[0] <- cmf
    buf.[1] <- flg
    let mutable off = 2
    let mutable dataOff = 0
    for b = 0 to numBlocks - 1 do
        let remaining = data.Length - dataOff
        let len  = uint16 (min blockSize remaining)
        let bfinal = if b = numBlocks - 1 then 0x01uy else 0x00uy
        buf.[off] <- bfinal   // BFINAL | BTYPE=00 (stored)
        off <- off + 1
        off <- writeUInt16LE buf off len
        off <- writeUInt16LE buf off (len ^^^ 0xFFFFus)  // NLEN = one's complement
        Array.blit data dataOff buf off (int len)
        off    <- off    + int len
        dataOff <- dataOff + int len
    // Adler-32 big-endian
    let a32 = adler32 data 0 data.Length
    writeUInt32BE buf off a32 |> ignore
    buf

// ─────────────────────────────────────────────────────────────────────────────
// PNG encoder
// ─────────────────────────────────────────────────────────────────────────────

/// Encode a width×height RGBA pixel array (row-major, 4 bytes per pixel) as a
/// PNG byte array.
let encode (width: int) (height: int) (pixels: byte[]) : byte[] =
    if pixels.Length < width * height * 4 then
        failwith "pixel buffer too small"

    let out = ResizeArray<byte>()

    // PNG signature
    out.AddRange [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy
                    0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]

    // IHDR
    let ihdr = Array.zeroCreate 13
    writeUInt32BE ihdr 0 (uint32 width)  |> ignore
    writeUInt32BE ihdr 4 (uint32 height) |> ignore
    ihdr.[8]  <- 8uy   // bit depth
    ihdr.[9]  <- 6uy   // colour type = RGBA
    ihdr.[10] <- 0uy   // compression
    ihdr.[11] <- 0uy   // filter
    ihdr.[12] <- 0uy   // interlace = none
    writeChunk out (System.Text.Encoding.ASCII.GetBytes "IHDR") ihdr

    // Build raw image data with PNG filter-byte 0 (None) on each row.
    let stride    = width * 4
    let rawLen    = height * (1 + stride)
    let rawData   = Array.zeroCreate rawLen
    for row = 0 to height - 1 do
        rawData.[row * (1 + stride)] <- 0uy   // filter byte
        Array.blit pixels (row * stride) rawData (row * (1 + stride) + 1) stride

    // IDAT (zlib-wrapped)
    let compressed = zlibWrap rawData
    writeChunk out (System.Text.Encoding.ASCII.GetBytes "IDAT") compressed

    // IEND
    writeChunk out (System.Text.Encoding.ASCII.GetBytes "IEND") [||]

    out.ToArray()
