module Gpu
open ILGPU
open ILGPU.Runtime
open ILGPU.Runtime.Cuda
open System

[<Struct>]
type Gpu =
    val context : Context
    val gpu : Device
    val accelerator : Accelerator
    new(preferedCpu:bool) =
        let context = Context.CreateDefault()
        
        let gpu = context.GetPreferredDevice(preferedCpu)
        let accelerator = gpu.CreateAccelerator(context)
        //let accelerator = context.CreateCudaAccelerator(0)
        {
            context = context
            gpu = gpu
            accelerator = accelerator
        }

    member this.MakeStream() =
        this.accelerator.CreateStream()
    member this.Dispose() =
        this.accelerator.Dispose()
        this.context.Dispose()
    interface IDisposable with
        member this.Dispose() = this.Dispose()
