module StockData
open System
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
        member this.CodeDisplay() =
            String.Format("{0}.{1}", BaoStockPrefixDisplay this.prefix, this.code)
        override this.ToString() =
            String.Format("{0}.{1}", this.code, this.name)
    end

type StockRow =
    {
        Date   : DateTime
        High   : Float
        Low    : Float
        Open   : Float
        Close  : Float
        Volume : Float
    }

type StockData =
    int * StockCode
