namespace TestStockBoll

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Base
open Boll
open Gpu
open StockData

type CpuBoll =
    val mid : Float[]
    val top : Float[]
    val bot : Float[]
    new(m, t, b) =
        {
            mid = m
            top = t
            bot = b
        }

type CpuBollConfig =
    val period : int
    val devfactor : Float
    new(p, d) =
        {
            period = p
            devfactor = d
        }
    member this.Calculate(data : Float[]) =
        let t = data |> Array.windowed this.period
        let average = t |> Array.map (fun x -> x |> Array.average)
        let StdDev i d =
            let avg = average[i]
            let std = d |> Array.map (fun x -> (x - avg) ** 2.0f)
                    |> Array.average
                    |> MathF.Sqrt
            std
        let std = data
                |> Array.windowed this.period
                |> Array.mapi StdDev
        let top = average
                |> Array.mapi (fun i x -> x + std[i] * this.devfactor)
        let bot = average
                |> Array.mapi (fun i x -> x - std[i] * this.devfactor)
        CpuBoll(average, top, bot)

[<TestClass>]
type TestBoll () =
    let stockCodes = [|
        ("sh.600000", "浦发银行")
        ("sh.601825", "沪农商行")
        ("sh.601880", "辽港股份")
    |]

    member this.CheckResult(cpuBoll:CpuBoll, gpuBoll:Boll) =
        let equalMid = gpuBoll.mid
                    |> Array.zip cpuBoll.mid
                    |> Array.forall (fun (x, y) -> Math.Abs(x - y) < 0.0001f)
        let equalTop = gpuBoll.top
                    |> Array.zip cpuBoll.top
                    |> Array.forall (fun (x, y) -> Math.Abs(x - y) < 0.0001f)
        let equalBot = gpuBoll.bot
                    |> Array.zip cpuBoll.bot
                    |> Array.forall (fun (x, y) -> Math.Abs(x - y) < 0.0001f)
        equalMid && equalTop && equalBot

    [<TestMethod>]
    member this.TestSimplest () =
        let data = [| 1.0f; 2.0f; 3.0f; 4.0f; 5.0f |]
        let cpuBollConfig = CpuBollConfig(3, 1.0f)
        let cpuBoll = cpuBollConfig.Calculate(data)
        let gpuBoll = BollConfig(3, 1.0f).Calculate(new Gpu(false), data)
        
        Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll));

    member this.LoadStockData(code : StockCode) =
        let Stocks = StocksProvider.Load(Path.Combine(StockDataPath,"../../../../StocksData/", code.CodeDisplay() + ".csv"))
        Stocks.Rows |> Seq.toArray |> Array.map (fun x -> Float x.Close)

    [<TestMethod>]
    member this.TestSingleThreadGpuBoll() =
        let codes = stockCodes
                    |> Array.map (fun (x,y) -> StockCode(x,y))

        let gpu = new Gpu(false)
        codes |> Array.iter (fun code ->
            let data = this.LoadStockData(code)
            
            do
                let cpuBoll = CpuBollConfig(99, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(99, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))

            do
                let cpuBoll = CpuBollConfig(21, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(21, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))

            do
                let cpuBoll = CpuBollConfig(17, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(17, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))
        )

    [<TestMethod>]
    member this.TestSingleThreadBollOnCpu() =
        let codes = stockCodes
                    |> Array.map (fun (x,y) -> StockCode(x,y))

        let gpu = new Gpu(true)
        codes |> Array.iter (fun code ->
            let data = this.LoadStockData(code)
            
            do
                let cpuBoll = CpuBollConfig(17, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(17, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))

            do
                let cpuBoll = CpuBollConfig(21, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(21, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))

            do
                let cpuBoll = CpuBollConfig(99, 2.0f).Calculate(data)
                let gpuBoll = BollConfig(99, 2.0f).Calculate(gpu, data)
                Assert.IsTrue(this.CheckResult(cpuBoll, gpuBoll))
        )