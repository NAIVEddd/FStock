module SuperSmootherFilter
open ILGPU
open ILGPU.Runtime
open System
open Base
open Gpu


type SuperSmootherFilter =
    struct
        val ssf : Float[]
        new(_ssf) =
            {
                ssf = _ssf
            }
    end

type SuperSmootherFilterConfig =
    struct
        val period : int
        new(_period) =
            {
                period = _period
            }

        // John Ehlers' Super Smoother Filter
        member this.Calculate(gpu: Gpu, closePrices:Float[]) =
            let f = (1.414f * MathF.PI / (Float this.period))
            let a1 = MathF.Exp(-f)
            let b1 = 2.0f * a1 * MathF.Cos(f)
            let c2 = b1
            let c3 = -a1 * a1
            let c1 = 1.0f - c2 - c3
            
            let ssf = Array.zeroCreate<Float> closePrices.Length
            for i in 2..closePrices.Length-1 do
                ssf[i] <- c1 * (closePrices[i] + closePrices[i - 1]) / 2.0f + c2 * ssf[i - 1] + c3 * ssf[i - 2]
            SuperSmootherFilter(ssf)
    end
