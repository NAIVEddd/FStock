module CandlestickChartPattern
open System
open Base
open StockData

let IsWhite (data:StockData) (index:StockIndex) =
    let today = data.Get(index)
    today.Close > today.Open

let IsBlack (data:StockData) (index:StockIndex) =
    let today = data.Get(index)
    today.Close < today.Open

let IsMorningStar (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index-2) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        let beforeYestoday = data.Get(index - 2)
        let isBeforeYestodayDown = IsBlack data (index - 2)
        let isTodayUp = IsWhite data index
        let yestodayHigh = max yestoday.Open yestoday.Close
        let isYestodayBelowBeforeYestoday = yestodayHigh <= beforeYestoday.Close
        let isYestodayBelowToday = yestodayHigh <= today.Open
        let isHighUp = (today.Close - today.Open) / (beforeYestoday.Open - beforeYestoday.Close) > 0.5f
        isBeforeYestodayDown && isTodayUp && isHighUp &&
        isYestodayBelowBeforeYestoday && isYestodayBelowToday
    else
        false

let IsPiercingLine (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index-1) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        let isYestodayDown = yestoday.Close < yestoday.Open
        let isTodayUp = today.Close > today.Open
        let isTodayCloseAboveYestodayMiddle = today.Close >= (yestoday.Close + yestoday.Open) / 2.0F
        let isTodayOpenBelowYestodayClose = today.Open <= yestoday.Close
        let isTodayCloseBelowYestodayOpen = today.Close <= yestoday.Open
        let isBigDown = yestoday.Open - yestoday.Close > yestoday.Open * 0.021F
        let isBigUp = today.Close - today.Open > today.Open * 0.01F
        isYestodayDown && //isBigDown &&
        isTodayUp && isBigUp &&
        isTodayCloseAboveYestodayMiddle &&
        isTodayOpenBelowYestodayClose &&
        isTodayCloseBelowYestodayOpen
    else
        false

let IsHaveGap (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index - 1) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        today.High < yestoday.Low ||    // down gap
        today.Low > yestoday.High       // up gap
    else
        false

let IsUpGap (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index - 1) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        IsWhite data index &&
        today.Low > yestoday.High
    else
        false

let IsDownGap (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index - 1) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        IsBlack data index &&
        today.High < yestoday.Low
    else
        false

let IsHigherVolume (proportion:Float) (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index - 1) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        let v = Float ((float today.Volume) / ( float yestoday.Volume))
        v <> infinityf && v <> nanf && v > proportion
    else
        false

let IsHigherVolumeIn2Day (proportion:Float) (data:StockData) (index:StockIndex) =
    if data.IsValidIndex (index.Index - 2) then
        let today = data.Get(index)
        let yestoday = data.Get(index - 1)
        let beforeYestoday = data.Get(index - 2)
        let v1 = Float (float today.Volume / float beforeYestoday.Volume)
        let v2 = Float (float yestoday.Volume / float beforeYestoday.Volume)
        v1 <> infinityf && v2 <> infinityf &&
        v1 <> nanf && v2 <> nanf &&
        v1 > proportion && v2 > proportion
    else
        false