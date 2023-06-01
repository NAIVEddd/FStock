module PrettyPrint
open System
open System.IO
open Order

let RawHtmlHead = """
<!DOCTYPE html>
<html>
<head>
    <title>Stock Code list</title>
    <style>
        .category-content ul {
            list-style: none;
            padding: 0;
            margin: 0;
        }
        .category-content li {
            margin-bottom: 10px;
        }
        .category-content li a {
            display: block;
            padding: 10px;
            background-color: #f5f5f5;
            border: 1px solid #ddd;
            color: #333;
            text-decoration: none;
            font-size: 14px;
        }
        .category-content li a:hover {
            background-color: #eee;
        }
    </style>
</head>
<body>
    <div class="category">
        <div class="category-content" id="stock-list" style="display: block;">
            <ul>
"""
let RawHtmlTail = """
            </ul>
        </div>
    </div>
</body>
</html>
"""

let ToLi (order:Order) =
    match order with
    | WillBuy(c,_) ->
        String.Format ("<li><a href=\"https://data.eastmoney.com/stockdata/{0}.html\" target=\"_blank\">{1}</a></li>", c.code, c.name)
    | failwith -> "Not supported"

let HtmlPrintOrders (orders:Order array) =
    let li = orders |> Array.map ToLi |> String.Concat
    let html = String.Format ("{0}{1}{2}",RawHtmlHead,li,RawHtmlTail)
    use outputFile = new StreamWriter("OrderList.html", false)
    outputFile.WriteLine(html);
