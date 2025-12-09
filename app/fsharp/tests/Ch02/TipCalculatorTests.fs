namespace Ch02.Tests

open Xunit
open Ch02.TipCalculator

/// 第2章: TipCalculator のテスト
module TipCalculatorTests =

    [<Fact>]
    let ``getTipPercentage は6人以上で 20% を返す`` () =
        Assert.Equal(20, getTipPercentage ["A"; "B"; "C"; "D"; "E"; "F"])
        Assert.Equal(20, getTipPercentage ["A"; "B"; "C"; "D"; "E"; "F"; "G"])

    [<Fact>]
    let ``getTipPercentage は1-5人で 10% を返す`` () =
        Assert.Equal(10, getTipPercentage ["A"])
        Assert.Equal(10, getTipPercentage ["A"; "B"])
        Assert.Equal(10, getTipPercentage ["A"; "B"; "C"; "D"; "E"])

    [<Fact>]
    let ``getTipPercentage は0人で 0% を返す`` () =
        Assert.Equal(0, getTipPercentage [])

    [<Fact>]
    let ``getTipPercentageMatch は getTipPercentage と同じ結果を返す`` () =
        let testCases = [
            []
            ["A"]
            ["A"; "B"; "C"]
            ["A"; "B"; "C"; "D"; "E"]
            ["A"; "B"; "C"; "D"; "E"; "F"]
            ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"]
        ]

        for names in testCases do
            Assert.Equal(
                getTipPercentage names,
                getTipPercentageMatch names
            )

    [<Fact>]
    let ``calculateTip はチップ額を正しく計算する`` () =
        // 6人以上 → 20%
        Assert.Equal(200m, calculateTip 1000m ["A"; "B"; "C"; "D"; "E"; "F"])

        // 1-5人 → 10%
        Assert.Equal(100m, calculateTip 1000m ["A"; "B"])

        // 0人 → 0%
        Assert.Equal(0m, calculateTip 1000m [])

    [<Fact>]
    let ``calculatePerPerson は一人当たりの支払額を計算する`` () =
        // 2人で 1000円、チップ 10% → 1100円 / 2 = 550円
        match calculatePerPerson 1000m ["A"; "B"] with
        | Some amount -> Assert.Equal(550m, amount)
        | None -> Assert.Fail("Should return Some")

        // 6人で 1200円、チップ 20% → 1440円 / 6 = 240円
        match calculatePerPerson 1200m ["A"; "B"; "C"; "D"; "E"; "F"] with
        | Some amount -> Assert.Equal(240m, amount)
        | None -> Assert.Fail("Should return Some")

    [<Fact>]
    let ``calculatePerPerson は0人で None を返す`` () =
        match calculatePerPerson 1000m [] with
        | Some _ -> Assert.Fail("Should return None")
        | None -> Assert.True(true)
