// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open FSharp.Data
open Base
open Boll
open BollRange
open Gpu
open StockData
open Order

let beginTime = DateTime.Now
let gpu = new Gpu(false)
printfn "------ Begin Now ------"

let CheckIsToBuy (code : StockCode) =
    try
        //printfn "Process %s" (code.CodeDisplay())
        let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.CodeDisplay() + ".csv"))
        let closePrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)
        if closePrices.Length < 100 then
            None
        else
            let boll = BollConfig(99, Float 2.0).Calculate(gpu, closePrices)
            let bollRange = BollRangeConfig(boll).Calculate(gpu, closePrices)
            let v = bollRange.range[bollRange.range.Length - 1]
            if v < Float 0.065 then
                //printfn "Process %s" (code.CodeDisplay())
                let message = String.Format("Price is : {0}, Boll range is: {1}", closePrices[closePrices.Length - 1], v)
                Some (Order.WillBuy (code, message))
            else
                None
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            None

let StockCodes = StockCodesProvider.Load(Path.Combine(StockDataPath, "stock_codes.csv"))
let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, "../history_k_data.csv"))

let orders =
    StockCodes.Rows |> Seq.toArray |> Array.map (fun c -> StockCode(c.Code, c.Code_name))
    |> Array.filter (fun c ->
        match c.prefix with
        | BJ -> false
        | _ -> true)
    //|> Array.Parallel.map CheckIsToBuy
    |> Array.map CheckIsToBuy
    |> Array.filter (fun x ->
        match x with
        | Some _ -> true
        | None -> false)
    |> Array.map (fun x ->
        match x with
        | Some s -> s
        | None -> failwith "impossible")


gpu.Dispose()
printfn "-------------------------------"
printfn "Cost time : %A" (DateTime.Now - beginTime)
printfn " Find out %A orders." orders.Length
for i in orders do
    printfn "Order %A" i

