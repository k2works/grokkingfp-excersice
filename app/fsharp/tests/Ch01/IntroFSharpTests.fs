namespace Ch01.Tests

open Xunit
open Ch01.IntroFSharp

/// 第1章: IntroFSharp のテスト
module IntroFSharpTests =

    [<Fact>]
    let ``increment は数値を1増やす`` () =
        Assert.Equal(8, increment 7)
        Assert.Equal(1, increment 0)
        Assert.Equal(0, increment -1)

    [<Fact>]
    let ``getFirstCharacter は最初の文字を返す`` () =
        Assert.Equal('H', getFirstCharacter "Hello")
        Assert.Equal('F', getFirstCharacter "F#")
        Assert.Equal('a', getFirstCharacter "abc")

    [<Fact>]
    let ``wordScore は文字数を返す`` () =
        Assert.Equal(5, wordScore "Hello")
        Assert.Equal(2, wordScore "F#")
        Assert.Equal(0, wordScore "")

    [<Fact>]
    let ``wordScoreImperative と wordScoreFunctional は同じ結果を返す`` () =
        let words = ["Hello"; "World"; "F#"; "Functional"; ""]
        for word in words do
            Assert.Equal(wordScoreImperative word, wordScoreFunctional word)

    [<Fact>]
    let ``scoreUpperCase は大文字変換後の長さを返す`` () =
        Assert.Equal(5, scoreUpperCase "hello")
        Assert.Equal(5, scoreUpperCase "HELLO")
        Assert.Equal(5, scoreUpperCase "HeLLo")

    [<Fact>]
    let ``processWord はトリム、小文字化、スペース除去を行う`` () =
        Assert.Equal("hello", processWord "  Hello  ")
        Assert.Equal("helloworld", processWord "Hello World")
        Assert.Equal("fsharp", processWord " F Sharp ")

    [<Fact>]
    let ``add は2つの数値を加算する`` () =
        Assert.Equal(3, add 1 2)
        Assert.Equal(0, add -5 5)
        Assert.Equal(10, addExplicit 3 7)

    [<Fact>]
    let ``identity は入力をそのまま返す`` () =
        Assert.Equal(42, identity 42)
        Assert.Equal("hello", identity "hello")
        Assert.Equal(true, identity true)

    [<Fact>]
    let ``部分適用が正しく動作する`` () =
        Assert.Equal(8, addFive 3)
        Assert.Equal(5, addFive 0)
        Assert.Equal(35, addTenAndTwenty 5)
