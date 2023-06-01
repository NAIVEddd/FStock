module Trending
open ILGPU
open ILGPU.Runtime
open ILGPU.Algorithms
open System
open Base
open Gpu

type Trending =
    struct
        val minIndex : int[]
        val maxIndex : int[]
        new(_min, _max) =
            {
                minIndex = _min
                maxIndex = _max
            }
    end

type TrendingConfig =
    struct
        val period : int
        new(_p) =
            {
                period = _p
            }
        member this.Calculate(gpu: Gpu, closePrices:Float[]) =
            let incrementArray = Array.zeroCreate<int> closePrices.Length
            let decrementArray = Array.zeroCreate<int> closePrices.Length
            for i in 1..closePrices.Length-1 do
                if closePrices[i] > closePrices[i-1] then
                    incrementArray[i] <- 1
                else
                    decrementArray[i] <- 1
            for i in 1..closePrices.Length-1 do
                incrementArray[i] <-
                    if incrementArray[i] = 0 then
                        0
                    else
                        incrementArray[i] + incrementArray[i-1]
                decrementArray[i] <-
                    if decrementArray[i] = 0 then
                        0
                    else
                        decrementArray[i] + decrementArray[i-1]
            let period = this.period
            let maxIndex = incrementArray |>
                            Array.mapi (fun i x ->
                                if i = 0 then
                                    0
                                else
                                    if x = 0 && incrementArray[i - 1] > period then
                                        i - 1
                                    else
                                        0) |>
                            Array.filter (fun x -> x <> 0)
            let minIndex = decrementArray |>
                            Array.mapi (fun i x ->
                                if i = 0 then
                                    0
                                else
                                    if x = 0  && decrementArray[i - 1] > period then
                                        i - 1
                                    else
                                        0) |>
                            Array.filter (fun x -> x <> 0)
            Trending(minIndex, maxIndex)
    end
