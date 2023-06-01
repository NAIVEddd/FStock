module Boll
open ILGPU
open ILGPU.Runtime
open ILGPU.Algorithms
open System
open Base
open Gpu

type Boll =
    struct
        val mid : Float[]
        val top : Float[]
        val bot : Float[]
        new(_mid, _top, _bot) =
            {
                mid = _mid
                top = _top
                bot = _bot
            }
    end

type BollConfig =
    struct
        val period : int
        val devfactor : Float
        new(_period, _devfactor) =
            {
                period = _period
                devfactor = _devfactor
            }
        static member DoCalc (index:Index1D) (boll:BollConfig)
            (dataIn:ArrayView<Float>) (stdDev:ArrayView<Float>) (average:ArrayView<Float>)
            (topBand:ArrayView<Float>) (botBand:ArrayView<Float>) =
            // calculate average
            for i in 0..boll.period-1 do
                average[index] <- average[index] + dataIn[index.X + i]
            average[index] <- average[index] / (Float boll.period)

            // calculate standard deviation
            //for i in 0..boll.period-1 do
            //    let diff = dataIn[index.X + i] - average[index]
            //    stdDev[index] <- stdDev[index] + diff * diff
            //stdDev[index] <- MathF.Sqrt(stdDev[index] / (Float boll.period))
            let mutable stdDev = 0.f
            for i in 0..boll.period-1 do
                let diff = dataIn[index.X + i] - average[index]
                stdDev <- stdDev + diff * diff
            stdDev <- MathF.Sqrt(stdDev / (Float boll.period))

            // calculate top and bottom bands
            topBand[index] <- average[index] + boll.devfactor * stdDev//[index]
            botBand[index] <- average[index] - boll.devfactor * stdDev//[index]
    
        member this.Calculate(gpu: Gpu, closePrices:Float[]) =
            let resLen = closePrices.Length - this.period + 1
            use bufferSource = gpu.accelerator.Allocate1D<Float>(closePrices)
            use bufferTemp = gpu.accelerator.Allocate1D<Float>(resLen)
            use bufferAverage = gpu.accelerator.Allocate1D<Float>(resLen)
            use bufferTop = gpu.accelerator.Allocate1D<Float>(resLen)
            use bufferBottom = gpu.accelerator.Allocate1D<Float>(resLen)

            use stream = gpu.MakeStream()
            let kernel = gpu.accelerator.LoadAutoGroupedKernel<
                Index1D, BollConfig, ArrayView<Float>, ArrayView<Float>, ArrayView<Float>, ArrayView<Float>, ArrayView<Float>>(BollConfig.DoCalc)
            kernel.Invoke(stream, resLen, this, bufferSource.View, bufferTemp.View, bufferAverage.View, bufferTop.View, bufferBottom.View)
            stream.Synchronize()
        
            Boll(bufferAverage.GetAsArray1D(), bufferTop.GetAsArray1D(), bufferBottom.GetAsArray1D())
    end
