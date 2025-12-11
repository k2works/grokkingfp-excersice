namespace Ch05

/// 第5章: flatMap とネスト構造
/// List.collect（flatMap相当）とシーケンス式を学ぶ
module FlatMapOperations =

    // ============================================
    // 基本データ型
    // ============================================

    /// 本を表すレコード型
    type Book = {
        Title: string
        Authors: string list
    }

    /// 映画を表すレコード型
    type Movie = {
        Title: string
    }

    /// 点を表すレコード型
    type Point = {
        X: int
        Y: int
    }

    // ============================================
    // flatten - ネストしたリストを平坦化
    // ============================================

    /// ネストしたリストを平坦化
    let flatten (lists: 'a list list) : 'a list =
        lists |> List.concat

    // ============================================
    // flatMap (List.collect) - map + flatten
    // ============================================

    /// 本のリストから著者のリストを取得
    let getAllAuthors (books: Book list) : string list =
        books |> List.collect (fun book -> book.Authors)

    // ============================================
    // flatMap によるリストサイズの変化
    // ============================================

    /// 各要素を複製（サイズ増加）
    let duplicate (numbers: int list) : int list =
        numbers |> List.collect (fun i -> [i; i + 10])

    /// 各要素を2倍にしてリストに（サイズ維持）
    let mapToList (numbers: int list) : int list =
        numbers |> List.collect (fun i -> [i * 2])

    /// 偶数のみをフィルタ（サイズ減少）
    let filterEven (numbers: int list) : int list =
        numbers |> List.collect (fun i ->
            if i % 2 = 0 then [i] else [])

    // ============================================
    // 映画のレコメンデーション
    // ============================================

    /// 著者から映画化作品を取得
    let bookAdaptations (author: string) : Movie list =
        if author = "Tolkien" then
            [{ Title = "An Unexpected Journey" }; { Title = "The Desolation of Smaug" }]
        else
            []

    /// 本のリストからレコメンデーションを生成
    let getRecommendations (books: Book list) : string list =
        books
        |> List.collect (fun book ->
            book.Authors
            |> List.collect (fun author ->
                bookAdaptations author
                |> List.map (fun movie ->
                    $"You may like {movie.Title}, because you liked {author}'s {book.Title}")))

    /// シーケンス式を使ったレコメンデーション生成
    let getRecommendationsSeq (books: Book list) : string list =
        [ for book in books do
          for author in book.Authors do
          for movie in bookAdaptations author do
          yield $"You may like {movie.Title}, because you liked {author}'s {book.Title}" ]

    // ============================================
    // 円内の点の判定
    // ============================================

    /// 点が指定した半径の円内にあるかを判定
    let isInside (point: Point) (radius: int) : bool =
        radius * radius >= point.X * point.X + point.Y * point.Y

    /// 全組み合わせを生成
    let allCombinations (radiuses: int list) (points: Point list) : string list =
        [ for r in radiuses do
          for point in points do
          yield $"Point({point.X},{point.Y}) is within a radius of {r}: {isInside point r}" ]

    /// 条件を満たす組み合わせのみを生成（ガード式）
    let insidePointsOnly (radiuses: int list) (points: Point list) : string list =
        [ for r in radiuses do
          for point in points do
          if isInside point r then
              yield $"Point({point.X},{point.Y}) is within a radius of {r}" ]

    // ============================================
    // シーケンス式と型
    // ============================================

    /// リストから始まるシーケンス式 → リストを返す
    let listComprehension (xs: int list) (ys: int list) : int list =
        [ for x in xs do
          for y in ys do
          yield x * y ]

    /// セットから始まるシーケンス式 → リストを返す（F#ではSetの直接内包表記はない）
    let setToListComprehension (xs: Set<int>) (ys: int list) : int list =
        [ for x in xs do
          for y in ys do
          yield x * y ]

    // ============================================
    // 追加のユーティリティ
    // ============================================

    /// 3つのリストの全組み合わせを生成
    let allTripleCombinations (xs: int list) (ys: int list) (zs: int list) : int list =
        [ for x in xs do
          for y in ys do
          for z in zs do
          yield x + y + z ]

    /// flatMap を使った偶数フィルタ（if 式なし）
    let filterEvenWithFlatMap (numbers: int list) : int list =
        numbers |> List.collect (fun n ->
            if n % 2 = 0 then [n] else [])

    /// Option をフィルタ的に使う
    let filterWithOption (predicate: 'a -> bool) (list: 'a list) : 'a list =
        list |> List.collect (fun x ->
            if predicate x then [x] else [])
