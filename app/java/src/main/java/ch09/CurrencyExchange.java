package ch09;

import ch08.IO;
import io.vavr.collection.List;
import io.vavr.collection.Map;
import io.vavr.collection.HashMap;
import io.vavr.collection.Queue;
import io.vavr.control.Option;

import java.math.BigDecimal;
import java.util.Random;

/**
 * 第9章: 通貨交換レートの例
 *
 * ストリーム処理を使ってリアルタイムの為替レートを監視し、
 * トレンドを検出する例。
 */
public final class CurrencyExchange {

    private CurrencyExchange() {}

    // ============================================
    // データ型
    // ============================================

    /**
     * 通貨コード
     */
    public enum Currency {
        USD, EUR, GBP, JPY, CHF
    }

    // ============================================
    // 純粋関数（副作用なし）
    // ============================================

    /**
     * レートが上昇トレンドかどうかを判定
     * 直近のレートが連続して上昇していれば true
     */
    public static boolean trending(List<BigDecimal> rates) {
        if (rates.size() <= 1) {
            return false;
        }
        return rates.zip(rates.drop(1))
                .forAll(pair -> pair._2().compareTo(pair._1()) > 0);
    }

    /**
     * レートが下降トレンドかどうかを判定
     */
    public static boolean trendingDown(List<BigDecimal> rates) {
        if (rates.size() <= 1) {
            return false;
        }
        return rates.zip(rates.drop(1))
                .forAll(pair -> pair._2().compareTo(pair._1()) < 0);
    }

    /**
     * レートが安定しているかどうかを判定
     * すべてのレートが同じなら true
     */
    public static boolean isStable(List<BigDecimal> rates) {
        if (rates.size() < 3) {
            return false;
        }
        return rates.distinct().size() == 1;
    }

    /**
     * 為替テーブルから特定の通貨のレートを抽出
     */
    public static Option<BigDecimal> extractSingleCurrencyRate(
            Map<Currency, BigDecimal> table,
            Currency currency) {
        return table.get(currency);
    }

    // ============================================
    // 外部 API のシミュレーション
    // ============================================

    private static final Random random = new Random();

    /**
     * 為替レートテーブルを取得（シミュレーション）
     * 実際のアプリケーションでは外部 API を呼び出す
     */
    private static Map<Currency, BigDecimal> simulateExchangeTable(Currency from) {
        // ベースレートにランダムな変動を加える
        double variation = 0.95 + random.nextDouble() * 0.1; // 0.95 ~ 1.05

        return switch (from) {
            case USD -> HashMap.of(
                    Currency.EUR, BigDecimal.valueOf(0.85 * variation),
                    Currency.GBP, BigDecimal.valueOf(0.73 * variation),
                    Currency.JPY, BigDecimal.valueOf(110.0 * variation),
                    Currency.CHF, BigDecimal.valueOf(0.92 * variation)
            );
            case EUR -> HashMap.of(
                    Currency.USD, BigDecimal.valueOf(1.18 * variation),
                    Currency.GBP, BigDecimal.valueOf(0.86 * variation),
                    Currency.JPY, BigDecimal.valueOf(129.0 * variation),
                    Currency.CHF, BigDecimal.valueOf(1.08 * variation)
            );
            case GBP -> HashMap.of(
                    Currency.USD, BigDecimal.valueOf(1.37 * variation),
                    Currency.EUR, BigDecimal.valueOf(1.16 * variation),
                    Currency.JPY, BigDecimal.valueOf(150.0 * variation),
                    Currency.CHF, BigDecimal.valueOf(1.26 * variation)
            );
            case JPY -> HashMap.of(
                    Currency.USD, BigDecimal.valueOf(0.0091 * variation),
                    Currency.EUR, BigDecimal.valueOf(0.0078 * variation),
                    Currency.GBP, BigDecimal.valueOf(0.0067 * variation),
                    Currency.CHF, BigDecimal.valueOf(0.0084 * variation)
            );
            case CHF -> HashMap.of(
                    Currency.USD, BigDecimal.valueOf(1.09 * variation),
                    Currency.EUR, BigDecimal.valueOf(0.93 * variation),
                    Currency.GBP, BigDecimal.valueOf(0.79 * variation),
                    Currency.JPY, BigDecimal.valueOf(119.0 * variation)
            );
        };
    }

    // ============================================
    // IO を使った API ラッパー
    // ============================================

    /**
     * 為替テーブルを取得（IO でラップ）
     */
    public static IO<Map<Currency, BigDecimal>> exchangeTable(Currency from) {
        return IO.delay(() -> simulateExchangeTable(from));
    }

    /**
     * 特定の通貨ペアのレートを取得
     */
    public static IO<Option<BigDecimal>> rate(Currency from, Currency to) {
        return exchangeTable(from)
                .map(table -> extractSingleCurrencyRate(table, to));
    }

    // ============================================
    // ストリーム処理
    // ============================================

    /**
     * 為替レートの無限ストリームを生成
     */
    public static LazyStream<IO<Option<BigDecimal>>> ratesStream(
            Currency from,
            Currency to) {
        return LazyStream.continually(() -> rate(from, to));
    }

    /**
     * レートを取得して値のみを抽出（None を除外）
     */
    public static IO<List<BigDecimal>> fetchRates(
            Currency from,
            Currency to,
            int count) {

        List<IO<Option<BigDecimal>>> ios = ratesStream(from, to)
                .take(count * 2)  // 余分に取得（None を考慮）
                .toList();

        return IO.sequence(ios)
                .map(options -> options
                        .flatMap(opt -> opt.toList())
                        .take(count));
    }

    /**
     * 上昇トレンドを検出するまでレートを取得
     *
     * @param windowSize トレンド判定に使うウィンドウサイズ
     * @param maxAttempts 最大試行回数
     */
    public static IO<Option<BigDecimal>> waitForUptrend(
            Currency from,
            Currency to,
            int windowSize,
            int maxAttempts) {

        return fetchRates(from, to, maxAttempts)
                .map(rates -> {
                    // スライディングウィンドウでトレンドを検出
                    LazyStream<Queue<BigDecimal>> windows = LazyStream.fromList(rates)
                            .sliding(windowSize);

                    for (Queue<BigDecimal> window : windows) {
                        List<BigDecimal> windowList = window.toList();
                        if (trending(windowList)) {
                            return Option.some(windowList.last());
                        }
                    }
                    return Option.none();
                });
    }

    /**
     * トレンドを検出したら交換を実行
     *
     * @param amount 交換する金額
     * @param windowSize トレンド判定に使うウィンドウサイズ
     * @param maxAttempts 最大試行回数
     */
    public static IO<Option<BigDecimal>> exchangeIfTrending(
            BigDecimal amount,
            Currency from,
            Currency to,
            int windowSize,
            int maxAttempts) {

        return waitForUptrend(from, to, windowSize, maxAttempts)
                .map(maybeRate -> maybeRate.map(rate -> rate.multiply(amount)));
    }

    // ============================================
    // ユーティリティ
    // ============================================

    /**
     * 指定したレートシーケンスからトレンドを検出
     * （テスト用の純粋関数版）
     */
    public static Option<BigDecimal> findTrendInRates(
            List<BigDecimal> rates,
            int windowSize) {

        LazyStream<Queue<BigDecimal>> windows = LazyStream.fromList(rates)
                .sliding(windowSize);

        for (Queue<BigDecimal> window : windows) {
            List<BigDecimal> windowList = window.toList();
            if (trending(windowList)) {
                return Option.some(windowList.last());
            }
        }
        return Option.none();
    }
}
