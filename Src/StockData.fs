module StockData
open System
open System.IO
open Base

type StockPrefix =
    | BJ
    | SH
    | SZ

let StockPrefix(s : string) =
    match s with
    | "bj" -> StockPrefix.BJ
    | "sh" -> StockPrefix.SH
    | "sz" -> StockPrefix.SZ
    | _ -> failwith ("StockPrefix not support: " + s)

let BaoStockPrefixDisplay (p:StockPrefix) =
    match p with
    | BJ -> "bj"
    | SH -> "sh"
    | SZ -> "sz"

type StockCode =
    struct
        val prefix : StockPrefix
        val code : string
        val name : string
        new(_p, _c, _n) =
            {
                prefix = _p
                code = _c
                name = _n
            }
        new(_pc:String, _n) =
            let s = _pc.Split('.')
            assert(s.Length = 2)
            {
                prefix = StockPrefix(s[0])
                code = s[1]
                name = _n
            }
        member this.Path =
            Path.Combine(StockDataPath, this.FileName())
        member this.FileName() =
            String.Format("{0}.{1}.csv", BaoStockPrefixDisplay this.prefix, this.code)
        override this.ToString() =
            String.Format(" {0} {1}", this.code, this.name)
    end

type StockRow =
    val Date   : DateTime
    val High   : Float
    val Low    : Float
    val Open   : Float
    val Close  : Float
    val Volume : Int64
    new(data : StocksProvider.Row) =
        {
            Date = data.Date
            High = Float data.High
            Low = Float data.Low
            Open = Float data.Open
            Close = Float data.Close
            Volume = match data.Volume with
                     | Some(v) -> v
                     | None -> 0
        }

type StockIndex =
    struct
        val Index : int
        new(idx) =
            {
                Index = idx
            }
        new(stock:StockData) =
            {
                Index = stock.Length - 1
            }
        member this.Get() = this.Get(0)
        // get the idx-th row away from current index
        //    eg: get yestoday stock is : Get(-1)
        //        get tomorrow stock is : Get(1)    (should't use this in strategy...)
        member this.Get(idx : int) =
            assert(not (idx < -this.Index) || idx > 0)
            StockIndex(this.Index + idx)
        static member (+) (this : StockIndex, idx : int) = this.Get(idx)
        static member (-) (this : StockIndex, idx : int) = this.Get(-idx)
    end

and StockData =
    val Code : StockCode
    val Rows : StockRow array
    static member Load(code : StockCode) =
        let rows = StocksProvider.Load(code.Path).Rows
        StockData(code, rows)
    new(code : StockCode) =
        let rows = StocksProvider.Load(code.Path).Rows
        StockData(code, rows)
    new(code : StockCode, rows : StockRow array) =
        {
            Code = code
            Rows = rows
        }
    new(code : StockCode, rows : StocksProvider.Row seq) =
        {
            Code = code
            Rows = rows |> Seq.toArray |> Array.map (fun r -> StockRow(r))
        }
    member this.GetSlice(startIdx, endIdx) =
        let s = defaultArg startIdx 0
        let e = defaultArg endIdx this.Rows.Length
        StockData(this.Code, this.Rows[s..e])
    member this.Align(length) =
        let s = this.Length - length
        if s > 0 then
            this[s..this.Rows.Length]
        else
            this
    member this.Length = this.Rows.Length
    member this.IsValidIndex(idx) = idx >= 0 && idx < this.Length
    member this.Get(idx : StockIndex) = this.Rows[idx.Index]