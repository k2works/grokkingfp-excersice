package ch09;

import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static ch09.CurrencyExchange.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第9章: 通貨交換レートのテスト
 */
@DisplayName("第9章: 通貨交換レート")
class CurrencyExchangeTest {

    @Nested
    @DisplayName("trending")
    class TrendingTest {

        @Test
        @DisplayName("上昇トレンドを検出")
        void detectsUptrend() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.82),
                    BigDecimal.valueOf(0.83)
            );
            assertThat(trending(rates)).isTrue();
        }

        @Test
        @DisplayName("上昇していない場合は false")
        void notUptrend() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.84),
                    BigDecimal.valueOf(0.83)
            );
            assertThat(trending(rates)).isFalse();
        }

        @Test
        @DisplayName("横ばいは false")
        void flatIsNotUptrend() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.81)
            );
            assertThat(trending(rates)).isFalse();
        }

        @Test
        @DisplayName("要素が1つ以下は false")
        void singleElementNotUptrend() {
            assertThat(trending(List.of(BigDecimal.ONE))).isFalse();
            assertThat(trending(List.empty())).isFalse();
        }
    }

    @Nested
    @DisplayName("trendingDown")
    class TrendingDownTest {

        @Test
        @DisplayName("下降トレンドを検出")
        void detectsDowntrend() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.83),
                    BigDecimal.valueOf(0.81)
            );
            assertThat(trendingDown(rates)).isTrue();
        }

        @Test
        @DisplayName("下降していない場合は false")
        void notDowntrend() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.83)
            );
            assertThat(trendingDown(rates)).isFalse();
        }
    }

    @Nested
    @DisplayName("isStable")
    class IsStableTest {

        @Test
        @DisplayName("すべて同じ値なら安定")
        void allSameIsStable() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.85)
            );
            assertThat(isStable(rates)).isTrue();
        }

        @Test
        @DisplayName("異なる値があれば不安定")
        void differentValuesNotStable() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.85),
                    BigDecimal.valueOf(0.86)
            );
            assertThat(isStable(rates)).isFalse();
        }

        @Test
        @DisplayName("3つ未満は不安定")
        void lessThanThreeNotStable() {
            assertThat(isStable(List.of(BigDecimal.ONE, BigDecimal.ONE))).isFalse();
        }
    }

    @Nested
    @DisplayName("extractSingleCurrencyRate")
    class ExtractSingleCurrencyRateTest {

        @Test
        @DisplayName("存在する通貨のレートを抽出")
        void extractsExistingRate() {
            var table = exchangeTable(Currency.USD).unsafeRun();
            Option<BigDecimal> rate = extractSingleCurrencyRate(table, Currency.EUR);
            assertThat(rate.isDefined()).isTrue();
        }

        @Test
        @DisplayName("存在しない通貨は None")
        void returnsNoneForMissing() {
            var table = exchangeTable(Currency.USD).unsafeRun();
            Option<BigDecimal> rate = extractSingleCurrencyRate(table, Currency.USD); // USD to USD is not in table
            assertThat(rate.isEmpty()).isTrue();
        }
    }

    @Nested
    @DisplayName("exchangeTable")
    class ExchangeTableTest {

        @Test
        @DisplayName("為替テーブルを取得")
        void getsExchangeTable() {
            var table = exchangeTable(Currency.USD).unsafeRun();
            assertThat(table.containsKey(Currency.EUR)).isTrue();
            assertThat(table.containsKey(Currency.GBP)).isTrue();
            assertThat(table.containsKey(Currency.JPY)).isTrue();
        }

        @Test
        @DisplayName("IO は遅延実行される")
        void ioIsDeferred() {
            var io = exchangeTable(Currency.USD);
            assertThat(io.toString()).isEqualTo("IO(...)");
        }
    }

    @Nested
    @DisplayName("rate")
    class RateTest {

        @Test
        @DisplayName("特定の通貨ペアのレートを取得")
        void getsCurrencyPairRate() {
            Option<BigDecimal> usdToEur = rate(Currency.USD, Currency.EUR).unsafeRun();
            assertThat(usdToEur.isDefined()).isTrue();

            // レートは妥当な範囲にある
            BigDecimal value = usdToEur.get();
            assertThat(value.doubleValue()).isBetween(0.7, 1.0);
        }
    }

    @Nested
    @DisplayName("findTrendInRates")
    class FindTrendInRatesTest {

        @Test
        @DisplayName("上昇トレンドを発見")
        void findsTrendInRates() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.80),
                    BigDecimal.valueOf(0.79),
                    BigDecimal.valueOf(0.81),  // ここから
                    BigDecimal.valueOf(0.82),  // 上昇トレンド [0.79, 0.81, 0.82]
                    BigDecimal.valueOf(0.83),
                    BigDecimal.valueOf(0.82)
            );

            // 最初の上昇トレンドは [0.79, 0.81, 0.82] なので 0.82 が返る
            Option<BigDecimal> trend = findTrendInRates(rates, 3);
            assertThat(trend.isDefined()).isTrue();
            assertThat(trend.get()).isEqualTo(BigDecimal.valueOf(0.82));
        }

        @Test
        @DisplayName("トレンドがない場合は None")
        void noTrendReturnsNone() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.80),
                    BigDecimal.valueOf(0.82),
                    BigDecimal.valueOf(0.81),
                    BigDecimal.valueOf(0.83),
                    BigDecimal.valueOf(0.82)
            );

            Option<BigDecimal> trend = findTrendInRates(rates, 3);
            assertThat(trend.isEmpty()).isTrue();
        }

        @Test
        @DisplayName("ウィンドウサイズ2で検出")
        void windowSizeTwo() {
            List<BigDecimal> rates = List.of(
                    BigDecimal.valueOf(0.80),
                    BigDecimal.valueOf(0.81)
            );

            Option<BigDecimal> trend = findTrendInRates(rates, 2);
            assertThat(trend.isDefined()).isTrue();
            assertThat(trend.get()).isEqualTo(BigDecimal.valueOf(0.81));
        }
    }

    @Nested
    @DisplayName("fetchRates")
    class FetchRatesTest {

        @Test
        @DisplayName("指定した数のレートを取得")
        void fetchesSpecifiedCount() {
            List<BigDecimal> rates = fetchRates(Currency.USD, Currency.EUR, 5).unsafeRun();
            assertThat(rates).hasSize(5);
        }

        @Test
        @DisplayName("各レートは妥当な範囲")
        void ratesAreInRange() {
            List<BigDecimal> rates = fetchRates(Currency.USD, Currency.EUR, 10).unsafeRun();
            for (BigDecimal rate : rates) {
                assertThat(rate.doubleValue()).isBetween(0.5, 1.5);
            }
        }
    }

    @Nested
    @DisplayName("ratesStream")
    class RatesStreamTest {

        @Test
        @DisplayName("レートのストリームを生成")
        void generatesRatesStream() {
            var stream = ratesStream(Currency.USD, Currency.EUR);
            var firstFive = stream.take(5).toList();
            assertThat(firstFive).hasSize(5);
        }
    }
}
