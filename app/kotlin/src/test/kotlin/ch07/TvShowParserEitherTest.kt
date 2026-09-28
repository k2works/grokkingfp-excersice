package ch07

import arrow.core.left
import arrow.core.nonEmptyListOf
import arrow.core.right
import ch06.TvShow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TvShowParserEitherTest : FunSpec({

    context("小さな関数（Either 版）") {
        test("extractName は名前を Right で返す") {
            extractName("The Wire (2002-2008)") shouldBe "The Wire".right()
        }

        test("extractName は失敗理由を Left で返す") {
            extractName("(2022)") shouldBe "Can't extract name from: (2022)".left()
        }

        test("extractYearStart は開始年を Right で返す") {
            extractYearStart("The Wire (2002-2008)") shouldBe 2002.right()
        }

        test("extractYearStart は抽出とパースの失敗を区別する") {
            extractYearStart("The Wire (-2008)") shouldBe
                "Can't extract start year from: The Wire (-2008)".left()
            extractYearStart("The Wire (oops-2008)") shouldBe "Can't parse start year: 'oops'".left()
        }

        test("extractYearEnd は終了年を返す") {
            extractYearEnd("The Wire (2002-2008)") shouldBe 2008.right()
            extractYearEnd("The Wire (2002-)") shouldBe "Can't extract end year from: The Wire (2002-)".left()
        }

        test("extractSingleYear は単年を返す") {
            extractSingleYear("Chernobyl (2019)") shouldBe 2019.right()
            extractSingleYear("The Wire (2002-2008)") shouldBe
                "Can't extract single year from: The Wire (2002-2008)".left()
        }

        test("parseYear は toIntOrNull の結果を Either に変換する") {
            parseYear("2019", "year") shouldBe 2019.right()
            parseYear("abc", "year") shouldBe "Can't parse year: 'abc'".left()
        }
    }

    context("parseShow（either { } DSL 版）") {
        test("正しい形式をパースできる") {
            parseShow("Breaking Bad (2008-2013)") shouldBe TvShow("Breaking Bad", 2008, 2013).right()
        }

        test("単年の番組は getOrElse によるフォールバックでパースできる") {
            parseShow("Chernobyl (2019)") shouldBe TvShow("Chernobyl", 2019, 2019).right()
        }

        test("名前がなければその理由を Left で返す") {
            parseShow("(2008-2013)") shouldBe "Can't extract name from: (2008-2013)".left()
        }

        test("年がパースできなければフォールバック側の理由を Left で返す") {
            parseShow("Bad Show (abc-2020)") shouldBe
                "Can't extract single year from: Bad Show (abc-2020)".left()
        }
    }

    context("parseShowWithFlatMap（flatMap + recover 版）") {
        test("DSL 版と同じ結果を返す") {
            listOf(
                "Breaking Bad (2008-2013)",
                "Chernobyl (2019)",
                "(2008-2013)",
                "Bad Show (abc-2020)",
                "The Wire 2002-2008",
            ).forEach { raw ->
                parseShowWithFlatMap(raw) shouldBe parseShow(raw)
            }
        }
    }

    context("エラーハンドリング戦略（Either 版）") {
        val rawShows = listOf(
            "Breaking Bad (2008-2013)",
            "The Wire 2002 2008",
            "Mad Men (2007-2015)",
            "(2019)",
        )
        val validShows = listOf("Breaking Bad (2008-2013)", "Mad Men (2007-2015)")
        val parsedValidShows = listOf(
            TvShow("Breaking Bad", 2008, 2013),
            TvShow("Mad Men", 2007, 2015),
        )

        test("Best-effort 戦略はパースできたものだけを返す") {
            parseShowsBestEffort(rawShows) shouldBe parsedValidShows
        }

        test("All-or-nothing 戦略はすべて成功すれば Right を返す") {
            parseShowsAllOrNothing(validShows) shouldBe parsedValidShows.right()
        }

        test("All-or-nothing 戦略は最初のエラーを Left で返す") {
            parseShowsAllOrNothing(rawShows) shouldBe "Can't extract name from: The Wire 2002 2008".left()
        }

        test("mapOrAccumulate はすべて成功すれば Right を返す") {
            parseShowsCollectErrors(validShows) shouldBe parsedValidShows.right()
        }

        test("mapOrAccumulate はすべてのエラーを NonEmptyList に集める") {
            parseShowsCollectErrors(rawShows) shouldBe nonEmptyListOf(
                "Can't extract name from: The Wire 2002 2008",
                "Can't extract name from: (2019)",
            ).left()
        }
    }
})
