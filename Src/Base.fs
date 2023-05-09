﻿module Base
open FSharp.Data

type Float = float32
let Float(f:float) = float32 f

let StockDataPath = "..\\StocksData"
type StockCodesProvider =
    CsvProvider<"..\\StocksData\\stock_codes.csv">
type StocksProvider =
    CsvProvider<"..\\StocksData\\sh.600000.csv",
        CacheRows=false,
        Schema="Open=float,Close=float,Low=float,High=float,Volume=Int64">