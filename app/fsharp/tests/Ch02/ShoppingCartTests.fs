namespace Ch02.Tests

open Xunit
open Ch02.ShoppingCart

/// 第2章: ShoppingCart のテスト
module ShoppingCartTests =

    [<Fact>]
    let ``getDiscountPercentage は Book があれば 5% を返す`` () =
        Assert.Equal(5, getDiscountPercentage ["Book"])
        Assert.Equal(5, getDiscountPercentage ["Apple"; "Book"])
        Assert.Equal(5, getDiscountPercentage ["Book"; "Pen"; "Notebook"])

    [<Fact>]
    let ``getDiscountPercentage は Book がなければ 0% を返す`` () =
        Assert.Equal(0, getDiscountPercentage [])
        Assert.Equal(0, getDiscountPercentage ["Apple"])
        Assert.Equal(0, getDiscountPercentage ["Pen"; "Notebook"])

    [<Fact>]
    let ``getAdvancedDiscountPercentage の複合条件テスト`` () =
        // Book + 電子機器 → 15%
        Assert.Equal(15, getAdvancedDiscountPercentage ["Book"; "Laptop"])
        Assert.Equal(15, getAdvancedDiscountPercentage ["Book"; "Phone"; "Apple"])

        // Book + 5個以上 → 10%
        Assert.Equal(10, getAdvancedDiscountPercentage ["Book"; "A"; "B"; "C"; "D"])

        // Book のみ → 5%
        Assert.Equal(5, getAdvancedDiscountPercentage ["Book"; "Apple"; "Pen"])

        // 電子機器 + 3個以上 → 7%
        Assert.Equal(7, getAdvancedDiscountPercentage ["Laptop"; "A"; "B"])

        // 10個以上 → 5%
        Assert.Equal(5, getAdvancedDiscountPercentage ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J"])

        // それ以外 → 0%
        Assert.Equal(0, getAdvancedDiscountPercentage ["Apple"; "Pen"])

    [<Fact>]
    let ``addItem は新しいリストに商品を追加する`` () =
        let cart = ["Apple"]
        let newCart = addItem cart "Banana"

        Assert.Equal<string list>(["Banana"; "Apple"], newCart)
        // 元のカートは変更されない（イミュータブル）
        Assert.Equal<string list>(["Apple"], cart)

    [<Fact>]
    let ``removeItem は商品を除いた新しいリストを返す`` () =
        let cart = ["Apple"; "Banana"; "Apple"]
        let newCart = removeItem cart "Apple"

        Assert.Equal<string list>(["Banana"], newCart)
        // 元のカートは変更されない
        Assert.Equal<string list>(["Apple"; "Banana"; "Apple"], cart)

    [<Fact>]
    let ``calculateTotal は合計金額を計算する`` () =
        let prices = Map.ofList [
            ("Apple", 100m)
            ("Banana", 80m)
            ("Book", 1500m)
        ]

        Assert.Equal(0m, calculateTotal prices [])
        Assert.Equal(100m, calculateTotal prices ["Apple"])
        Assert.Equal(180m, calculateTotal prices ["Apple"; "Banana"])
        Assert.Equal(1680m, calculateTotal prices ["Apple"; "Banana"; "Book"])
        // 存在しない商品は無視される
        Assert.Equal(100m, calculateTotal prices ["Apple"; "Unknown"])

    [<Fact>]
    let ``calculateDiscountedTotal は割引後の合計を計算する`` () =
        let prices = Map.ofList [
            ("Apple", 100m)
            ("Book", 1000m)
        ]

        // Book なし → 割引なし
        Assert.Equal(100m, calculateDiscountedTotal prices ["Apple"])

        // Book あり → 5% 割引
        let total = calculateDiscountedTotal prices ["Apple"; "Book"]
        Assert.Equal(1045m, total) // 1100 * 0.95 = 1045
