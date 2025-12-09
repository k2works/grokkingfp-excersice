namespace Ch02

/// チップ計算の例
/// グループサイズに応じたチップ率を計算
module TipCalculator =

    /// グループサイズに応じたチップ率を計算
    /// 6人以上 → 20%
    /// 1-5人 → 10%
    /// 0人 → 0%
    let getTipPercentage (names: string list) : int =
        let size = List.length names
        if size > 5 then 20
        elif size > 0 then 10
        else 0

    /// パターンマッチングを使用したチップ率計算
    let getTipPercentageMatch (names: string list) : int =
        match List.length names with
        | n when n > 5 -> 20
        | n when n > 0 -> 10
        | _ -> 0

    /// チップ額を計算
    let calculateTip (billAmount: decimal) (names: string list) : decimal =
        let tipRate = decimal (getTipPercentage names) / 100m
        billAmount * tipRate

    /// 一人当たりの支払額を計算（チップ込み）
    let calculatePerPerson (billAmount: decimal) (names: string list) : decimal option =
        let size = List.length names
        if size = 0 then
            None
        else
            let tip = calculateTip billAmount names
            let total = billAmount + tip
            Some (total / decimal size)
