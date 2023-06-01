// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open FSharp.Data
open Base
open Boll
open BollRange
open SuperSmootherFilter
open Trending
open Gpu
open StockData
open CandlestickChartPattern
open Order
open Filter
open PrettyPrint
open ScottPlot

let gpu = new Gpu(false)

let CheckIsToBuy1(code : StockCode) =
    try
        let stock = StockData(code)
        let last = stock.Get(StockIndex(stock))
        if stock.Length < 100 then
            None
        else
            None
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            None

let CheckIsToBuy (code : StockCode) =
    try
        //printfn "Process %s" (code.CodeDisplay())
        let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.FileName()))
        let closePrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)
        if closePrices.Length < 100 then
            None
        else
            let boll = BollConfig(20, Float 2.0).Calculate(gpu, closePrices)
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

let CheckAllStocks (skip_code30 : bool) (checkFunc : StockCode -> Order option) =
    let beginTime = DateTime.Now
    printfn "-------------------------------"
    printfn "------ Begin Now ------"

    let StockCodes = StockCodesProvider.Load(Path.Combine(StockDataPath, "stock_codes.csv"))

    let orders =
        StockCodes.Rows |> Seq.toArray |> Array.map (fun c -> StockCode(c.Code, c.Code_name))
        |> (fun arr ->  // filter code doesn't needed
            arr |>
            if skip_code30 then
                Array.filter (fun c ->
                    match c.prefix with
                    | BJ -> false
                    | _ -> not (c.code.StartsWith "30"))
            else
                Array.filter (fun c ->
                    match c.prefix with
                    | BJ -> false
                    | _ -> true))
        //|> Array.Parallel.map CheckIsToBuy
        |> Array.map checkFunc
        |> Array.filter (fun x ->
            match x with
            | Some _ -> true
            | None -> false)
        |> Array.map (fun x ->
            match x with
            | Some s -> s
            | None -> failwith "impossible")


    printfn "-------------------------------"
    printfn "Cost time : %A" (DateTime.Now - beginTime)
    printfn " Find out %A orders." orders.Length
    for i in orders do
        printfn "Order %A" i

let CheckSSF() =
    let code = StockCode("sh.000001", "上证指数")
    let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.FileName()))
    let closePrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)
    let ssf5 = SuperSmootherFilterConfig(5).Calculate(gpu, closePrices)
    let ssf10 = SuperSmootherFilterConfig(10).Calculate(gpu, closePrices)
    let boll = BollConfig(20, Float 2.0).Calculate(gpu, closePrices)
    let bollRange = BollRangeConfig(boll).Calculate(gpu, closePrices)

    let plt = ScottPlot.Plot(5000, 1000)
    //plt.AddSignal(boll.bot) |> ignore
    //plt.AddSignal(boll.mid) |> ignore
    //plt.AddSignal(boll.top) |> ignore
    plt.AddSignal(closePrices, color=Drawing.Color.Blue) |> ignore
    let signal = plt.AddSignal(ssf5.ssf, color=Drawing.Color.Red)
    plt.AddSignal(ssf10.ssf, color=Drawing.Color.RosyBrown) |> ignore
    plt.SaveFig("SSF.png")


let CheckTrending() =
    let code = StockCode("sh.600000", "上证指数")
    let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.FileName()))
    let closePrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)
    let ssf5 = SuperSmootherFilterConfig(5).Calculate(gpu, closePrices)


    let trending = TrendingConfig(2).Calculate(gpu, ssf5.ssf)
    let line1 = trending.maxIndex |> Array.map (fun x -> float ssf5.ssf[x])
    let line2 = trending.minIndex |> Array.map (fun x -> float ssf5.ssf[x])

    let plt = ScottPlot.Plot(5000, 1000)
    plt.AddSignal(ssf5.ssf, color=Drawing.Color.Black) |> ignore
    plt.AddScatterPoints(trending.maxIndex |> Array.map(fun x -> float x), line1, color=Drawing.Color.Red) |> ignore
    plt.AddScatterPoints(trending.minIndex |> Array.map(fun x -> float x), line2, color=Drawing.Color.Blue) |> ignore
    plt.SaveFig("Trending.png")

let CheckBoll (maxPrice:Float) (period:int) (code : StockCode) =
    try
        //printfn "Process %s" (code.CodeDisplay())
        let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.FileName()))
        let closePrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)
        if closePrices.Length < period + 1 then
            None
        else
            let boll = BollConfig(period, Float 2.0).Calculate(gpu, closePrices)
            let v = closePrices[closePrices.Length - 1]
            if v < maxPrice && v > boll.bot[boll.bot.Length - 1] && v < boll.top[boll.top.Length - 1] then
                //printfn "Process %s" (code.CodeDisplay())
                let message = String.Format("Price is : {0,6:C2}, Boll range is: [{1,6:F2}:{2,6:F2}:{3,6:F2}]", closePrices[closePrices.Length - 1], boll.bot[boll.bot.Length - 1], boll.mid[boll.mid.Length - 1], boll.top[boll.top.Length - 1])
                Some (Order.WillBuy (code, message))
            else
                None
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            None


            
// translate [open, close] to some pattern, use these pattern to buy or sell
//      #1 if body contain in yesterday in different state, buy or sell
let FindOutHammer (maxPrice:Float) (daysAgo : int) (code : StockCode) =
    try
        let Stocks = StocksProvider.Load(Path.Combine(StockDataPath, code.FileName()))
        let arr = Stocks.Rows |> Seq.toArray
        let today = arr.Length - daysAgo - 1
        let yestoday = today - 1

        if today < 1 then
            None
        else
            //let highPrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.High)
            let lowPrices = arr |> Array.map (fun x -> Float x.Low)
            let openPrices = arr |> Array.map (fun x -> Float x.Open)
            let closePrices = arr |> Array.map (fun x -> Float x.Close)
            // check the pattern
            let b = closePrices[today] >= openPrices[today] &&
                    closePrices[today] < openPrices[yestoday] &&
                    openPrices[today] >= closePrices[yestoday] &&
                    lowPrices[today] <= lowPrices[yestoday]
            let v = closePrices[today]
            if v < maxPrice && b then
                let message = String.Format("Price is : {0,6:C2}, because of 'hammer line'.", closePrices[closePrices.Length - 1])
                Some (Order.WillBuy (code, message))
            else
                None
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            None

let FindOutRollUp (maxPrice:Float) (daysAgo : int) (code : StockCode) =
    try
        let Stocks = StockData(code)
        let arr = Stocks.Rows
        
        let todayIdx = StockIndex(Stocks) - daysAgo
        let yestodayIdx = todayIdx - 1

        if todayIdx.Index < 1 then
            None
        else
            //let highPrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.High)
            //let lowPrices = arr |> Array.map (fun x -> Float x.Low)

            let yestoday = Stocks.Get(yestodayIdx)
            let today = Stocks.Get(todayIdx)
            // check the pattern
            let b = today.Open < today.Close &&
                    today.Open < yestoday.Close &&
                    today.Close <= yestoday.Open &&
                    today.Close > yestoday.Close &&
                    yestoday.Close < yestoday.Open &&
                    ((today.Close - today.Open) > 0.035F) &&
                    ((today.Close - today.Open) / (yestoday.Open - yestoday.Close)) > 0.3F
            let v = today.Close
            if v < maxPrice && b then
                let message = String.Format("Price is : {0,6:C2}, because of 'light of night'.", today.Close)
                Some (Order.WillBuy (code, message))
            else
                None
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            None

let FindOutRollUp1 (maxPrice:Float) (daysAgo : int) (stock : StockData) =
    try
        let todayIdx = StockIndex(stock) - daysAgo
        let yestodayIdx = todayIdx - 1

        if todayIdx.Index < 1 then
            Order.Invalid
        else
            //let highPrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.High)
            //let lowPrices = arr |> Array.map (fun x -> Float x.Low)

            let yestoday = stock.Get(yestodayIdx)
            let today = stock.Get(todayIdx)
            // check the pattern
            let b = today.Open < today.Close &&
                    today.Open < yestoday.Close &&
                    today.Close <= yestoday.Open &&
                    today.Close > yestoday.Close &&
                    yestoday.Close < yestoday.Open &&
                    ((today.Close - today.Open) > 0.035F) &&
                    ((today.Close - today.Open) / (yestoday.Open - yestoday.Close)) > 0.3F
            let v = today.Close
            if v < maxPrice && b then
                let message = String.Format("Price is : {0,6:C2}, because of 'light of night'.", today.Close)
                Order.WillBuy (stock.Code, message)
            else
                Order.Invalid
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            Order.Invalid

let FindOutUpUpUp (maxPrice:Float) (daysAgo : int) (stock : StockData) =
    try
        let todayIdx = StockIndex(stock) - daysAgo
        //let yestodayIdx = todayIdx - 1

        //let highPrices = Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.High)
        //let lowPrices = arr |> Array.map (fun x -> Float x.Low)

        //let yestoday = stock.Get(yestodayIdx)
        let today = stock.Get(todayIdx)
        // check the pattern
        //let b = today.Open < today.Close &&
        //        today.Open < yestoday.Open &&
        //        today.Close > yestoday.Open &&
        //        yestoday.Open - yestoday.Close > 0.035F
        //let b = IsPiercingLine stock todayIdx
        //let b = IsHigherVolumeIn2Day 2.5f stock todayIdx
        //let b = IsHigherVolume 2.5f stock todayIdx
                //&& IsMorningStar stock todayIdx
                //&& IsPiercingLine stock todayIdx
        //let b = IsUpGap stock todayIdx
        let b = IsMorningStar stock todayIdx
        let v = today.Close
        if v < maxPrice && b then
            let message = String.Format("Price is : {0,6:C2}, because of 'light of night'.", today.Close)
            Order.WillBuy (stock.Code, message)
        else
            Order.Invalid
    with
        | :? System.Exception as e ->
            //printfn "%s" (code.ToString() + e.GetType().ToString())
            Order.Invalid

let BackTrace (skip_code30 : bool) (skipDays:int) (strategy : int -> StockData -> Order) (code : StockCode) =
    let beginTime = DateTime.Now
    printfn "---------------------------------"
    printfn "------ Begin BackTrace Now ------"
    printfn "-------%A-------" code

    let StockCodes = StockCodesProvider.Load(Path.Combine(StockDataPath, "stock_codes.csv"))

    let Stocks = StockData(code)
    
    let orders =
        [|skipDays..Stocks.Length - 1|] |> Array.map (fun i -> (i, strategy i Stocks)) |>
        Array.filter (fun (i, o) ->
            match o with
            | Invalid -> false
            | _ -> true)

    let prices = orders |> Array.map (fun (i, o) ->
        let last = StockIndex(Stocks)
        let buyIdx = last - i + 1
        let sellIdx = buyIdx + 3
        let sell = Stocks.Get(sellIdx)
        let buyPrice = Stocks.Get(buyIdx).Open
        //let buyPrice = (Stocks.Get(buyIdx).Open + Stocks.Get(buyIdx + 1).Open + Stocks.Get(buyIdx + 2).Open) / 3.0f
        //let sellPrice = max sell.Open sell.Close
        let sellPrice = [|1..3|] |> Array.map (fun i ->
            let sell = Stocks.Get(buyIdx+i)
            (sell.Open, sell.Close)) |> Array.unzip ||> Array.append |> Array.max
        buyIdx, sellIdx, buyPrice, sellPrice)
    if prices.Length > 0 then
        let profit = prices |> Array.map(fun (_,_,b,s) -> s - b) |> Array.sum
        let profitPercent =
            prices |> Array.map(fun (_,_,b,s) -> (s - b) / b)
                |> Array.average
                //|> Array.sum
                |> fun x -> 100.0f * x
        let successTrade = prices |> Array.map(fun (_,_,b,s) -> if (s-b) > 0.0001f then 1.0f else 0.0f) |> Array.sum |> fun x -> (100.0f * x / (Float prices.Length))

        printfn "---------------------------------"
        printfn "Cost time : %A" (DateTime.Now - beginTime)
        printfn " Find out %A orders." orders.Length
        Console.WriteLine(String.Format("Trade {0} times, Total profit : {1,8:C2} ({2,5}%) with success ({3,5}%)", orders.Length, profit, profitPercent, successTrade))
        for i,j,b,s in prices do
            let message = String.Format("Date: {0} Buy Price is : {1,6:C2}, sell price is : {2,6:C2}, profit: {3,6:C2}", Stocks.Get(i).Date, b, s, s - b)
            Console.WriteLine message
        profit, profitPercent
    else
        printfn "---------------------------------"
        printfn "Cost time : %A" (DateTime.Now - beginTime)
        printfn " Find out %A orders." orders.Length
        0.0f, 0.0f

//CheckTrending() |> ignore
//CheckAllStocks true (CheckBoll 10.0f 60)
//for i in 0..1 do
    //printf "Check %A ago\n" i
    //CheckAllStocks true (FindOutHammer 50.0f i)
    //CheckAllStocks true (FindOutRollUp 50.0f i)

let FindOutStocks (skip_code30 : bool) (strategy : StockData -> Order) =
    let beginTime = DateTime.Now
    printfn "---------------------------------"
    printfn "------ Begin BackTrace Now ------"

    let StockCodes = StockCodesProvider.Load(Path.Combine(StockDataPath, "stock_codes.csv"))

    let orders =
        StockCodes.Rows |> Seq.toArray |> Array.map (fun c -> StockCode(c.Code, c.Code_name))
        |> FilterNotNeededCode
        //|> Array.Parallel.map CheckIsToBuy
        |> Array.map (fun c -> strategy (StockData(c)))
        |> Array.filter (fun x ->
            match x with
            | Invalid _ -> false
            | _ -> true)


    HtmlPrintOrders orders
    printfn "-------------------------------"
    printfn "Cost time : %A" (DateTime.Now - beginTime)
    printfn " Find out %A Stocks." orders.Length
    for i in orders do
        printfn "Order %A" i
    orders

let trace = BackTrace true 6 (FindOutUpUpUp 50.0f)

//let record = FindOutStocks true (FindOutRollUp1 20.0f 0) |> Array.map (fun o ->
//    match o with
//    | WillBuy(c,_) -> trace c
//    | _ -> failwith "Impossible")
let record = FindOutStocks true (FindOutUpUpUp 20.0f 0) |> Array.map (fun o ->
    match o with
    | WillBuy(c,_) -> trace c
    | _ -> failwith "Impossible")
let mutable profitSum = 0.0F
let mutable profitPerSum = 0.0F
record |> Array.iter (fun (p, pp) ->
    
    profitSum <- profitSum + if pp > 0.0F then 1.0F else 0.0F
    profitPerSum <- profitPerSum + pp
)
Console.WriteLine(profitSum / Float record.Length)
Console.WriteLine(profitPerSum)

gpu.Dispose()
