namespace Ch03

/// 第3章: イミュータブルなデータ操作
/// リストのスライスと結合操作を学ぶ
module ImmutableOperations =

    // ============================================
    // 基本的なリスト操作
    // ============================================

    /// リストの先頭n個の要素を取得
    let firstN (n: int) (list: 'a list) : 'a list =
        list |> List.truncate n

    /// リストの末尾n個の要素を取得
    let lastN (n: int) (list: 'a list) : 'a list =
        let skipCount = max 0 (List.length list - n)
        list |> List.skip skipCount

    /// リストの最初の2要素を取得
    let firstTwo (list: 'a list) : 'a list =
        firstN 2 list

    /// リストの最後の2要素を取得
    let lastTwo (list: 'a list) : 'a list =
        lastN 2 list

    /// リストのスライス（開始インデックスから終了インデックスまで）
    let slice (startIndex: int) (endIndex: int) (list: 'a list) : 'a list =
        list
        |> List.skip startIndex
        |> List.truncate (endIndex - startIndex)

    // ============================================
    // 要素の追加
    // ============================================

    /// リストの末尾に要素を追加（新しいリストを返す）
    let appended (element: 'a) (list: 'a list) : 'a list =
        list @ [element]

    /// リストの末尾に複数の要素を追加
    let appendedAll (elements: 'a list) (list: 'a list) : 'a list =
        list @ elements

    // ============================================
    // リスト変換パターン
    // ============================================

    /// 最初の2要素を末尾に移動
    let moveFirstTwoToEnd (list: 'a list) : 'a list =
        let first = firstTwo list
        let rest = list |> List.skip 2
        rest @ first

    /// 最後の要素の前に新しい要素を挿入
    let insertBeforeLast (element: 'a) (list: 'a list) : 'a list =
        let withoutLast = list |> List.truncate (List.length list - 1)
        let last = list |> lastN 1
        withoutLast @ [element] @ last

    /// 中央に要素を挿入
    let insertAtMiddle (element: 'a) (list: 'a list) : 'a list =
        let middle = List.length list / 2
        let before = list |> List.truncate middle
        let after = list |> List.skip middle
        before @ [element] @ after


/// 旅程の再計画
module Itinerary =

    /// 指定した都市の前に新しい都市を挿入
    let replan (plan: string list) (newCity: string) (beforeCity: string) : string list =
        let index =
            plan
            |> List.tryFindIndex (fun c -> c = beforeCity)
            |> Option.defaultValue (List.length plan)
        let citiesBefore = plan |> List.truncate index
        let citiesAfter = plan |> List.skip index
        citiesBefore @ [newCity] @ citiesAfter


/// 文字列操作
module StringOperations =

    /// 名前を省略形に変換（例: "Alonzo Church" -> "A. Church"）
    let abbreviate (name: string) : string =
        let initial = name.Substring(0, 1)
        let separatorIndex = name.IndexOf(' ')
        if separatorIndex < 0 then
            name
        else
            let lastName = name.Substring(separatorIndex + 1)
            $"{initial}. {lastName}"

    /// 文字列の最初のn文字を取得
    let firstNChars (n: int) (s: string) : string =
        if n >= s.Length then s
        else s.Substring(0, n)

    /// 文字列の最後のn文字を取得
    let lastNChars (n: int) (s: string) : string =
        if n >= s.Length then s
        else s.Substring(s.Length - n)
