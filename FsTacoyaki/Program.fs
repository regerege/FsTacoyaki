(*
http://www.gamedesign.jp/flash/tacoyaki/tacoyaki.html
5x5=25の裏と表のコインを同じ色に統一するゲーム
コインをクリックすると斜め4方向に反転する。

●○○○○○●○○○○○●○○○○○●○○○○○●
○○○○○●○○○○○●○○○○○●○●○○○●○
○○○○○○○○○○●○○○●○●○●○○○●○○
○○○○○○○○○●○○○●○●○●○○○●○○○
○○○○●○○○●○○○●○○○●○○○●○○○○
○●○○○○○●○○○○○●○○○○○●○○○●○
●○○○○○●○○○○○●○●○○○●○○○●○●
○○○○○●○○○●○●○●○○○●○○○●○●○
○○○○●○○○●○●○●○○○●○○○●○●○○
○○○●○○○●○○○●○○○●○○○○○●○○○
○○●○○○○○●○○○○○●○○○●○○○●○○
○●○○○○○●○●○○○●○○○●○●○●○○○
●○○○●○●○●○○○●○○○●○●○●○○○●
○○○●○●○●○○○●○○○●○●○○○○○●○
○○●○○○●○○○●○○○○○●○○○○○●○○
○○○●○○○○○●○○○●○○○●○○○●○○○
○○●○●○○○●○○○●○●○●○○○●○○○○
○●○●○○○●○○○●○●○●○○○●○○○○○
●○●○○○●○○○●○●○○○○○●○○○○○●
○●○○○●○○○○○●○○○○○●○○○○○●○
○○○○●○○○●○○○●○○○●○○○●○○○○
○○○●○○○●○●○●○○○●○○○○○○○○○
○○●○○○●○●○●○○○●○○○○○○○○○○
○●○○○●○●○○○○○●○○○○○●○○○○○
●○○○○○●○○○○○●○○○○○●○○○○○●
*)
open System
open System.Text.RegularExpressions

let rec input() =
    printf "xy: "
    let t = Console.ReadLine()
    if t.Length = 2 && Regex.IsMatch(t, "[1-5]{2}") then
        1 <<< (int t.[0] - 49 + ((int t.[1] - 49) * 5))
    else
        printfn "Please enter twice the coordinates of 1~5."
        input()

type TacoyakiInput =
    | None
    | Position of int
type TacoyakiConfig = {
        Tacoyaki : int
        Input : TacoyakiInput
    }
let mask =
    [
        0x1041041
        0x00820a2
        0x0004544
        0x0008a88
        0x0111110
        0x0820822
        0x1041445
        0x008a88a
        0x0115114
        0x0222208
        0x0410444
        0x08288a8
        0x1151151
        0x02a2282
        0x0444104
        0x0208888
        0x0511510
        0x0a22a20
        0x1445041
        0x0882082
        0x0111110
        0x022a200
        0x0454400
        0x08a0820
        0x1041041
    ]
let inline nzc (bit:int) =
    if bit = 0 then 0
    else
        let rec find n c =
            if n &&& 1 = 1 then c
            else find (n >>> 1) (c+1)
        find bit 0
/// get reversal target
let inline reverse (n:int) =
    n |||
    (n <<< 4) ||| (n <<< 6) ||| (n <<< 8) ||| (n <<< 12) ||| (n <<< 16) ||| (n <<< 18) ||| (n <<< 20) ||| (n <<< 24) |||
    (n >>> 4) ||| (n >>> 6) ||| (n >>> 8) ||| (n >>> 12) ||| (n >>> 16) ||| (n >>> 18) ||| (n >>> 20) ||| (n >>> 24) &&& mask.[nzc n]
let inline calc bit n =
    let n = reverse n &&& 0x1FFFFFF
    printfn "%A" n
    bit ^^^ n
    
let inline display (bit:int) =
    Console.Clear()
    let coins = [| '○'; '●'; |]
    let rec f m =
        printf "%c" coins.[(bit >>> m &&& 0x00000001)]
        if m % 5 = 0 then printfn ""
        if 0 < m then f (m-1)
    f 24

let rec gameloop (config:TacoyakiConfig) =
    let tacoyaki =
        match config.Input with
        | TacoyakiInput.Position n -> calc config.Tacoyaki n
        | TacoyakiInput.None -> config.Tacoyaki
    display tacoyaki
    if tacoyaki = 0 || tacoyaki = 0x1FFFFFF then
        printfn "Clear!"
        Console.ReadKey() |> ignore
    else
        gameloop
            { config with
                Tacoyaki = tacoyaki
                Input = TacoyakiInput.Position <| input()
            }

let rand = new System.Random()
gameloop { Tacoyaki = rand.Next(1,24); Input = TacoyakiInput.None }
