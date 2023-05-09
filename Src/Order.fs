module Order
open Base
open StockData

type Order =
    | Buy of StockCode * Float * int    // price and buy amount
    | Sell of StockCode * Float * int
    | WillBuy of StockCode * string
    | WillSell of StockCode * string

type OrderRecord =
    int
