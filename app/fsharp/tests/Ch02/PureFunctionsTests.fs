namespace Ch02.Tests

open Xunit
open Ch02.PureFunctions

/// 第2章: PureFunctions のテスト
module PureFunctionsTests =

    [<Fact>]
    let ``increment は数値を1増やす`` () =
        Assert.Equal(7, increment 6)
        Assert.Equal(1, increment 0)
        Assert.Equal(-5, increment -6)
        Assert.Equal(System.Int32.MaxValue, increment (System.Int32.MaxValue - 1))

    [<Fact>]
    let ``add は2つの数値を加算する`` () =
        Assert.Equal(5, add 2 3)
        Assert.Equal(0, add -5 5)
        Assert.Equal(-10, add -3 -7)

    [<Fact>]
    let ``getFirstCharacter は最初の文字を返す`` () =
        Assert.Equal('H', getFirstCharacter "Hello")
        Assert.Equal('1', getFirstCharacter "123")

    [<Fact>]
    let ``double は数値を2倍にする`` () =
        Assert.Equal(4, double 2)
        Assert.Equal(0, double 0)
        Assert.Equal(-10, double -5)

    [<Fact>]
    let ``isEven は偶数判定を行う`` () =
        // 偶数
        Assert.True(isEven 0)
        Assert.True(isEven 2)
        Assert.True(isEven 4)
        Assert.True(isEven -2)

        // 奇数
        Assert.False(isEven 1)
        Assert.False(isEven 3)
        Assert.False(isEven -3)

        // 境界値
        Assert.False(isEven System.Int32.MaxValue)
        Assert.True(isEven System.Int32.MinValue)

    [<Fact>]
    let ``wordScore は 'a' を除外した文字数を返す`` () =
        Assert.Equal(3, wordScore "Scala")  // "Scl" → 3
        Assert.Equal(8, wordScore "function") // 'a' なし → 8
        Assert.Equal(0, wordScore "")        // 空文字 → 0
        Assert.Equal(0, wordScore "aaa")     // すべて 'a' → 0

    [<Fact>]
    let ``wordScoreIgnoreCase は大文字小文字を区別せず 'a' を除外`` () =
        Assert.Equal(3, wordScoreIgnoreCase "Scala")
        Assert.Equal(3, wordScoreIgnoreCase "SCALA")
        Assert.Equal(0, wordScoreIgnoreCase "AaAa")

    [<Fact>]
    let ``bonusScore は7文字以上で5点を返す`` () =
        Assert.Equal(5, bonusScore "function")  // 8文字 → 5
        Assert.Equal(5, bonusScore "abcdefg")   // 7文字 → 5
        Assert.Equal(0, bonusScore "abcdef")    // 6文字 → 0
        Assert.Equal(0, bonusScore "")          // 0文字 → 0

    [<Fact>]
    let ``totalScore は wordScore と bonusScore の合計`` () =
        // "functional" → 'a' を1つ除外で 9文字 + ボーナス5 = 14
        Assert.Equal(14, totalScore "functional")
        // "Scala" → 'a' 2つ除外で 3文字 + ボーナス0 = 3
        Assert.Equal(3, totalScore "Scala")
        // "programming" → 'a' 1つ除外で 10文字 + ボーナス5 = 15
        Assert.Equal(15, totalScore "programming")

    // ============================================
    // 純粋関数の参照透過性テスト
    // ============================================

    [<Fact>]
    let ``純粋関数は同じ入力に対して常に同じ出力を返す`` () =
        // 複数回呼び出しても同じ結果
        let result1 = wordScore "Scala"
        let result2 = wordScore "Scala"
        let result3 = wordScore "Scala"

        Assert.Equal(result1, result2)
        Assert.Equal(result2, result3)

    [<Fact>]
    let ``参照透過性: 式をその結果で置き換え可能`` () =
        // wordScore "Scala" は 'a' 2つ除外で 3 を返す
        // wordScore "Java" は 'a' 2つ除外で 2 を返す
        let total1 = wordScore "Scala" + wordScore "Java"
        let total2 = 3 + 2  // 直接値で置き換え

        Assert.Equal(total1, total2)
