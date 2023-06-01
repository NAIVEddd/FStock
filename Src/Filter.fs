module Filter
open Base
open StockData

let FilterNone x = x

let FilterCode30 =
    Array.filter (fun (c:StockCode) -> not (c.code.StartsWith "30"))

let FilterCode688 =
    Array.filter (fun (c:StockCode) -> not (c.code.StartsWith "688"))

let FilterCodeBJ =
    Array.filter (fun (c:StockCode) -> c.prefix <> BJ)

let FilterNameST =
    Array.filter (fun (c:StockCode) -> not ((c.name.StartsWith "*") || (c.name.StartsWith "ST")))

let FilterNotNeededCode x =
    x |> FilterCode30 |> FilterCode688 |> FilterCodeBJ |> FilterNameST