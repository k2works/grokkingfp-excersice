package ch06

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TvShowParserTest : FunSpec({

    context("例外を使うパース") {
        test("parseShowUnsafe は正しい形式をパースできる") {
            parseShowUnsafe("Breaking Bad (2008-2013)") shouldBe TvShow("Breaking Bad", 2008, 2013)
        }

        test("parseShowUnsafe は不正な形式で例外をスローする") {
            shouldThrow<StringIndexOutOfBoundsException> { parseShowUnsafe("The Wire 2002-2008") }
            shouldThrow<StringIndexOutOfBoundsException> { parseShowUnsafe("Chernobyl (2019)") }
        }
    }

    context("小さな関数") {
        test("extractName は括弧の前の名前を返す") {
            extractName("The Wire (2002-2008)") shouldBe "The Wire"
        }

        test("extractName は名前がなければ null を返す") {
            extractName("(2022)") shouldBe null
            extractName("The Wire 2002-2008") shouldBe null
        }

        test("extractYearStart は開始年を返す") {
            extractYearStart("The Wire (2002-2008)") shouldBe 2002
        }

        test("extractYearStart は開始年がなければ null を返す") {
            extractYearStart("The Wire (-2008)") shouldBe null
            extractYearStart("The Wire (oops-2008)") shouldBe null
            extractYearStart("Chernobyl (2019)") shouldBe null
        }

        test("extractYearEnd は終了年を返す") {
            extractYearEnd("The Wire (2002-2008)") shouldBe 2008
        }

        test("extractYearEnd は終了年がなければ null を返す") {
            extractYearEnd("The Wire (2002-)") shouldBe null
            extractYearEnd("The Wire (2002-oops)") shouldBe null
        }

        test("extractSingleYear は単年を返す") {
            extractSingleYear("Chernobyl (2019)") shouldBe 2019
        }

        test("extractSingleYear はダッシュがあると null を返す") {
            extractSingleYear("The Wire (2002-2008)") shouldBe null
        }

        test("parseYear は数値文字列を Int にする") {
            parseYear(" 2019 ") shouldBe 2019
            parseYear("oops") shouldBe null
        }
    }

    context("parseShow") {
        test("正しい形式をパースできる") {
            parseShow("Breaking Bad (2008-2013)") shouldBe TvShow("Breaking Bad", 2008, 2013)
        }

        test("単年の番組は ?: によるフォールバックでパースできる") {
            parseShow("Chernobyl (2019)") shouldBe TvShow("Chernobyl", 2019, 2019)
        }

        test("不正な形式は null を返す") {
            parseShow("The Wire 2002-2008") shouldBe null
            parseShow("(2008-2013)") shouldBe null
            parseShow("Invalid") shouldBe null
        }

        test("parseShowWithLet も同じ結果を返す") {
            parseShowWithLet("Breaking Bad (2008-2013)") shouldBe TvShow("Breaking Bad", 2008, 2013)
            parseShowWithLet("Chernobyl (2019)") shouldBe TvShow("Chernobyl", 2019, 2019)
            parseShowWithLet("The Wire 2002-2008") shouldBe null
        }
    }

    context("エラーハンドリング戦略") {
        val rawShows = listOf(
            "Breaking Bad (2008-2013)",
            "The Wire 2002 2008",
            "Mad Men (2007-2015)",
        )

        test("Best-effort 戦略はパースできたものだけを返す") {
            parseShowsBestEffort(rawShows) shouldBe listOf(
                TvShow("Breaking Bad", 2008, 2013),
                TvShow("Mad Men", 2007, 2015),
            )
        }

        test("All-or-nothing 戦略はすべて成功すればリストを返す") {
            parseShowsAllOrNothing(listOf("Breaking Bad (2008-2013)", "Mad Men (2007-2015)")) shouldBe listOf(
                TvShow("Breaking Bad", 2008, 2013),
                TvShow("Mad Men", 2007, 2015),
            )
        }

        test("All-or-nothing 戦略は 1 つでも失敗すれば null を返す") {
            parseShowsAllOrNothing(rawShows) shouldBe null
        }

        test("All-or-nothing 戦略は空リストなら空リストを返す") {
            parseShowsAllOrNothing(emptyList()) shouldBe emptyList()
        }

        test("parseShowsAllOrNothingFold も同じ結果を返す") {
            parseShowsAllOrNothingFold(listOf("Breaking Bad (2008-2013)")) shouldBe
                listOf(TvShow("Breaking Bad", 2008, 2013))
            parseShowsAllOrNothingFold(rawShows) shouldBe null
        }
    }
})
