namespace Ch02

/// ショッピングカートの例
/// 純粋関数によるビジネスロジックの実装
module ShoppingCart =

    // ============================================
    // 純粋関数版ショッピングカート
    // ============================================

    /// カート内の商品リストから割引率を計算
    /// Book が含まれていれば 5%、それ以外は 0%
    let getDiscountPercentage (items: string list) : int =
        if List.contains "Book" items then 5
        else 0

    /// 複数の条件による割引計算
    let getAdvancedDiscountPercentage (items: string list) : int =
        let hasBook = List.contains "Book" items
        let hasElectronics = List.exists (fun item ->
            item = "Laptop" || item = "Phone" || item = "Tablet") items
        let itemCount = List.length items

        match hasBook, hasElectronics, itemCount with
        | true, true, _ -> 15      // Book + 電子機器 → 15%
        | true, _, n when n >= 5 -> 10  // Book + 5個以上 → 10%
        | true, _, _ -> 5          // Book のみ → 5%
        | _, true, n when n >= 3 -> 7   // 電子機器 + 3個以上 → 7%
        | _, _, n when n >= 10 -> 5     // 10個以上 → 5%
        | _ -> 0                   // それ以外 → 0%

    // ============================================
    // イミュータブルなカート操作
    // ============================================

    /// 商品を追加した新しいカートを返す
    let addItem (items: string list) (item: string) : string list =
        item :: items

    /// 商品を削除した新しいカートを返す
    let removeItem (items: string list) (item: string) : string list =
        items |> List.filter (fun i -> i <> item)

    /// カートの合計金額を計算
    let calculateTotal (prices: Map<string, decimal>) (items: string list) : decimal =
        items
        |> List.choose (fun item -> Map.tryFind item prices)
        |> List.sum

    /// 割引後の合計金額を計算
    let calculateDiscountedTotal (prices: Map<string, decimal>) (items: string list) : decimal =
        let total = calculateTotal prices items
        let discountRate = decimal (getDiscountPercentage items) / 100m
        total * (1m - discountRate)
