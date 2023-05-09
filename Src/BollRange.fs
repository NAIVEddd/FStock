module BollRange
open ILGPU
open ILGPU.Runtime
open Base
open Boll
open Gpu

type BollRange =
    struct
        val range : Float[]
        new(_range) =
            {
                range = _range
            }
    end

type BollRangeConfig =
    struct
        val boll : Boll
        new(_boll) =
            {
                boll = _boll
            }
        static member DoCalc (index:Index1D) (price:ArrayView<Float>) (top:ArrayView<Float>) (bot:ArrayView<Float>) (range:ArrayView<Float>) =
            range[index.X] <- (top[index.X] - bot[index.X]) / price[index.X]

        member this.Calculate(gpu:Gpu, closePrices:Float[]) =
            let startIndex = closePrices.Length - this.boll.mid.Length

            use bufferSource = gpu.accelerator.Allocate1D(closePrices[startIndex..])
            use bufferTop = gpu.accelerator.Allocate1D(this.boll.top)
            use bufferBot = gpu.accelerator.Allocate1D(this.boll.bot)
            use bufferRange = gpu.accelerator.Allocate1D<Float>(this.boll.mid.Length)

            use stream = gpu.MakeStream()
            let kernel = gpu.accelerator.LoadAutoGroupedKernel<
                Index1D, ArrayView<Float>, ArrayView<Float>, ArrayView<Float>, ArrayView<Float>>(BollRangeConfig.DoCalc)
            kernel.Invoke(stream, this.boll.mid.Length, bufferSource.View, bufferTop.View, bufferBot.View, bufferRange.View)
            stream.Synchronize()

            BollRange(bufferRange.GetAsArray1D())
    end