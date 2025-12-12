(ns ch09.lazy-sequences-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch09.lazy-sequences :as ls]))

;; =============================================================================
;; 遅延シーケンスの基本テスト
;; =============================================================================

(deftest lazy-range-tests
  (testing "lazy-range generates sequence"
    (is (= '(1 2 3 4 5) (ls/lazy-range 1 6)))
    (is (= '() (ls/lazy-range 5 5)))
    (is (= '() (ls/lazy-range 10 5)))))

(deftest process-stream-tests
  (testing "process-stream filters, maps and takes"
    (is (= '(1 9 25 49 81) (ls/process-stream (range))))))

;; =============================================================================
;; take-while/drop-while のテスト
;; =============================================================================

(deftest take-while-positive-tests
  (testing "take-while-positive"
    (is (= '(1 2 3) (ls/take-while-positive [1 2 3 -1 4 5])))
    (is (= '() (ls/take-while-positive [-1 2 3])))
    (is (= '(1 2 3) (ls/take-while-positive [1 2 3])))))

(deftest skip-small-values-tests
  (testing "skip-small-values"
    (is (= '(5 6 7) (ls/skip-small-values 5 [1 2 3 4 5 6 7])))
    (is (= '(1 2 3) (ls/skip-small-values 0 [1 2 3])))
    (is (= '() (ls/skip-small-values 10 [1 2 3])))))

;; =============================================================================
;; スライディングウィンドウのテスト
;; =============================================================================

(deftest sliding-window-tests
  (testing "sliding-window"
    (is (= '((1 2 3) (2 3 4) (3 4 5)) (ls/sliding-window 3 [1 2 3 4 5])))
    (is (= '((1 2) (2 3) (3 4) (4 5)) (ls/sliding-window 2 [1 2 3 4 5])))
    (is (= '() (ls/sliding-window 5 [1 2 3])))))

;; =============================================================================
;; トレンド検出のテスト
;; =============================================================================

(deftest trending-tests
  (testing "trending? - uptrend"
    (is (true? (ls/trending? [0.81 0.82 0.83])))
    (is (true? (ls/trending? [1 2 3 4 5]))))

  (testing "trending? - not uptrend"
    (is (false? (ls/trending? [0.81 0.84 0.83])))
    (is (false? (ls/trending? [5 4 3 2 1])))
    (is (false? (ls/trending? [1 1 1]))))

  (testing "trending? - insufficient data"
    (is (false? (ls/trending? [1])))
    (is (false? (ls/trending? [])))))

(deftest declining-tests
  (testing "declining? - downtrend"
    (is (true? (ls/declining? [0.83 0.82 0.81])))
    (is (true? (ls/declining? [5 4 3 2 1]))))

  (testing "declining? - not downtrend"
    (is (false? (ls/declining? [0.81 0.82 0.83])))
    (is (false? (ls/declining? [1 1 1])))))

(deftest stable-tests
  (testing "stable? - within tolerance"
    (is (true? (ls/stable? [1.0 1.01 1.02 1.01] 0.02)))
    (is (true? (ls/stable? [5.0 5.0 5.0] 0.0))))

  (testing "stable? - exceeds tolerance"
    (is (false? (ls/stable? [1.0 1.05 1.0] 0.01)))))

;; =============================================================================
;; 為替レートストリームのテスト
;; =============================================================================

(deftest get-rate-tests
  (testing "get-rate"
    (is (= 0.85 (ls/get-rate :USD :EUR)))
    (is (= 110.0 (ls/get-rate :USD :JPY)))
    (is (nil? (ls/get-rate :USD :XYZ)))))

(deftest simulate-rate-change-tests
  (testing "simulate-rate-change stays within bounds"
    (let [rate 1.0
          changes (repeatedly 100 #(ls/simulate-rate-change rate))]
      ;; 変動は ±0.5% 以内
      (is (every? #(and (> % 0.99) (< % 1.01)) changes)))))

(deftest rate-stream-tests
  (testing "rate-stream generates infinite sequence"
    (let [rates (take 5 (ls/rate-stream :USD :EUR))]
      (is (= 5 (count rates)))
      (is (= 0.85 (first rates))))))

;; =============================================================================
;; find-trend のテスト
;; =============================================================================

(deftest find-trend-tests
  (testing "find-trend detects trends"
    (let [stream [1 2 3 4 5]
          trends (ls/find-trend 3 stream)]
      (is (= 3 (count trends)))
      (is (every? :trending? trends)))))

(deftest first-uptrend-tests
  (testing "first-uptrend finds first uptrend"
    (let [stream [5 4 3 4 5 6]
          result (ls/first-uptrend 3 stream)]
      (is (some? result))
      (is (true? (:trending? result))))))

;; =============================================================================
;; reductions と移動平均のテスト
;; =============================================================================

(deftest moving-average-tests
  (testing "moving-average"
    (is (= [2 3 4 5 6 7 8 9] (ls/moving-average 3 [1 2 3 4 5 6 7 8 9 10])))
    (is (= [3/2 5/2 7/2] (ls/moving-average 2 [1 2 3 4])))))

;; =============================================================================
;; トランスデューサーのテスト
;; =============================================================================

(deftest process-with-transducer-tests
  (testing "process-with-transducer"
    (is (= [1 9 25 49 81] (ls/process-with-transducer (range 20))))))

(deftest lazy-process-with-transducer-tests
  (testing "lazy-process-with-transducer"
    (is (= '(1 9 25 49 81) (seq (ls/lazy-process-with-transducer (range 20)))))))

;; =============================================================================
;; ログストリーム処理のテスト
;; =============================================================================

(deftest filter-errors-tests
  (testing "filter-errors"
    (let [logs [{:level :info :message "msg1"}
                {:level :error :message "msg2"}
                {:level :warn :message "msg3"}
                {:level :error :message "msg4"}]
          errors (ls/filter-errors logs)]
      (is (= 2 (count errors)))
      (is (every? #(= :error (:level %)) errors)))))

(deftest count-by-level-tests
  (testing "count-by-level"
    (let [logs [{:level :info} {:level :error} {:level :info} {:level :warn}]
          counts (ls/count-by-level logs)]
      (is (= 2 (:info counts)))
      (is (= 1 (:error counts)))
      (is (= 1 (:warn counts))))))

(deftest recent-errors-tests
  (testing "recent-errors"
    (let [logs [{:level :info} {:level :error :message "e1"}
                {:level :error :message "e2"} {:level :error :message "e3"}]
          errors (ls/recent-errors 2 logs)]
      (is (= 2 (count errors)))
      (is (= "e1" (:message (first errors)))))))

;; =============================================================================
;; センサーデータストリームのテスト
;; =============================================================================

(deftest generate-sensor-reading-tests
  (testing "generate-sensor-reading within variance"
    (let [readings (repeatedly 100 #(ls/generate-sensor-reading 50 10))]
      (is (every? #(and (>= % 45) (<= % 55)) readings)))))

(deftest sensor-stream-tests
  (testing "sensor-stream generates infinite sequence"
    (let [readings (take 10 (ls/sensor-stream 50 10))]
      (is (= 10 (count readings))))))

(deftest detect-anomalies-tests
  (testing "detect-anomalies finds outliers"
    (let [stream [50 55 60 30 45 70 50]
          anomalies (take 10 (ls/detect-anomalies 15 stream))]
      (is (= [30 70] (vec anomalies))))))

(deftest alert-on-anomaly-tests
  (testing "alert-on-anomaly generates alerts"
    (let [stream [50 55 80 20 50]  ;; 80と20は閾値20を超える異常値（|80-50|=30>20, |20-50|=30>20）
          alerts (ls/alert-on-anomaly 20 2 stream)]
      (is (= 2 (count alerts)))
      (is (every? #(= :anomaly (:type %)) alerts)))))

;; =============================================================================
;; バッチ処理のテスト
;; =============================================================================

(deftest batch-sum-tests
  (testing "batch-sum"
    (is (= '(6 15 24 10) (ls/batch-sum 3 [1 2 3 4 5 6 7 8 9 10])))
    (is (= '(10 26 42) (ls/batch-sum 4 [1 2 3 4 5 6 7 8 9 10 11 12])))))

(deftest process-in-batches-tests
  (testing "process-in-batches"
    (let [result (ls/process-in-batches 2 count [1 2 3 4 5])]
      (is (= '(2 2 1) result)))))

;; =============================================================================
;; パイプライン処理のテスト
;; =============================================================================

(deftest pipeline-tests
  (testing "pipeline composition"
    (let [p (ls/pipeline
             (partial filter odd?)
             (partial map #(* % 2))
             (partial take 3))]
      (is (= '(2 6 10) (p (range 20)))))))

(deftest my-pipeline-tests
  (testing "my-pipeline"
    (is (= '(2 6 10 14 18) (ls/my-pipeline (range 20))))))

;; =============================================================================
;; eduction のテスト
;; =============================================================================

(deftest efficient-process-tests
  (testing "efficient-process with eduction"
    (is (= [1 9 25 49 81] (into [] (ls/efficient-process (range 20)))))))

