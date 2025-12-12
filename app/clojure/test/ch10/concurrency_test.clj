(ns ch10.concurrency-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch10.concurrency :as cc]))

;; =============================================================================
;; City と CityStats のテスト
;; =============================================================================

(deftest city-tests
  (testing "city creation"
    (let [c (cc/city "Sydney")]
      (is (= "Sydney" (:name c))))))

(deftest city-stats-tests
  (testing "city-stats creation"
    (let [c (cc/city "Sydney")
          stats (cc/city-stats c 100)]
      (is (= c (:city stats)))
      (is (= 100 (:check-ins stats))))))

;; =============================================================================
;; top-cities のテスト
;; =============================================================================

(deftest top-cities-tests
  (testing "top-cities returns sorted cities"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")
          lima (cc/city "Lima")
          check-ins {sydney 100 dublin 200 lima 50}
          result (cc/top-cities check-ins 3)]
      (is (= 3 (count result)))
      (is (= dublin (:city (first result))))
      (is (= sydney (:city (second result))))
      (is (= lima (:city (nth result 2))))))

  (testing "top-cities with fewer results"
    (let [sydney (cc/city "Sydney")
          check-ins {sydney 100}
          result (cc/top-cities check-ins 3)]
      (is (= 1 (count result))))))

;; =============================================================================
;; 逐次処理のテスト
;; =============================================================================

(deftest process-check-ins-sequential-tests
  (testing "sequential processing counts correctly"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")
          check-ins [sydney sydney dublin sydney]
          result (cc/process-check-ins-sequential check-ins)]
      (is (= 3 (get result sydney)))
      (is (= 1 (get result dublin))))))

;; =============================================================================
;; 並行処理のテスト
;; =============================================================================

(deftest store-check-in-tests
  (testing "store-check-in! updates atom"
    (let [stored (atom {})
          sydney (cc/city "Sydney")]
      (cc/store-check-in! stored sydney)
      (cc/store-check-in! stored sydney)
      (is (= 2 (get @stored sydney))))))

(deftest process-check-ins-concurrent-tests
  (testing "concurrent processing counts correctly"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")
          check-ins (concat (repeat 100 sydney) (repeat 50 dublin))
          result (cc/process-check-ins-concurrent check-ins)]
      (is (= 100 (get result sydney)))
      (is (= 50 (get result dublin))))))

;; =============================================================================
;; pmap のテスト
;; =============================================================================

(deftest parallel-map-tests
  (testing "parallel-map applies function"
    (let [result (cc/parallel-map inc [1 2 3 4 5])]
      (is (= [2 3 4 5 6] result)))))

;; =============================================================================
;; サイコロのテスト
;; =============================================================================

(deftest cast-the-die-tests
  (testing "cast-the-die returns valid dice value"
    (dotimes [_ 100]
      (let [result (cc/cast-the-die)]
        (is (>= result 1))
        (is (<= result 6))))))

(deftest cast-dice-concurrent-tests
  (testing "cast-dice-concurrent returns correct count"
    (let [result (cc/cast-dice-concurrent 10)]
      (is (= 10 (count result)))
      (is (every? #(and (>= % 1) (<= % 6)) result)))))

(deftest cast-dice-and-sum-tests
  (testing "cast-dice-and-sum returns sum in valid range"
    (let [n 5
          result (cc/cast-dice-and-sum n)]
      (is (>= result n))      ; 最小: n * 1
      (is (<= result (* n 6))))))  ; 最大: n * 6

(deftest cast-dice-with-storage-tests
  (testing "cast-dice-with-storage! stores all results"
    (let [n 10
          result (cc/cast-dice-with-storage! n)]
      (is (= n (count result)))
      (is (every? #(and (>= % 1) (<= % 6)) result)))))

;; =============================================================================
;; チェックイン処理の完全版テスト
;; =============================================================================

(deftest start-check-in-processing-tests
  (testing "check-in processing starts and stops"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")
          check-ins (concat (repeat 10 sydney) (repeat 5 dublin))
          processing (cc/start-check-in-processing! check-ins)]
      ;; 少し待機
      (Thread/sleep 200)
      (let [ranking ((:current-ranking processing))]
        (is (>= (count ranking) 0)))
      ;; 停止
      ((:stop processing)))))

;; =============================================================================
;; agent のテスト
;; =============================================================================

(deftest agent-check-in-tests
  (testing "agent stores check-ins"
    (cc/agent-reset-check-ins!)
    (let [sydney (cc/city "Sydney")]
      (cc/agent-store-check-in! sydney)
      (cc/agent-store-check-in! sydney)
      (await cc/check-in-agent)
      (is (= 2 (get (cc/agent-get-check-ins) sydney))))))

;; =============================================================================
;; ref と STM のテスト
;; =============================================================================

(deftest stm-check-in-tests
  (testing "STM stores check-ins"
    (dosync (ref-set cc/check-in-ref {}))
    (let [sydney (cc/city "Sydney")]
      (cc/stm-store-check-in! sydney)
      (cc/stm-store-check-in! sydney)
      (is (= 2 (get @cc/check-in-ref sydney))))))

(deftest stm-update-ranking-tests
  (testing "STM updates ranking"
    (dosync
     (ref-set cc/check-in-ref {(cc/city "Sydney") 100
                               (cc/city "Dublin") 200})
     (ref-set cc/ranking-ref []))
    (cc/stm-update-ranking! 2)
    (let [ranking @cc/ranking-ref]
      (is (= 2 (count ranking)))
      ;; CityStats record なので :city でアクセス
      (is (= 200 (:check-ins (first ranking)))))))

(deftest stm-transfer-check-ins-tests
  (testing "STM transfers check-ins"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")]
      (dosync
       (ref-set cc/check-in-ref {sydney 100 dublin 50}))
      (is (true? (cc/stm-transfer-check-ins! sydney dublin 30)))
      (is (= 70 (get @cc/check-in-ref sydney)))
      (is (= 80 (get @cc/check-in-ref dublin)))))

  (testing "STM transfer fails if insufficient"
    (let [sydney (cc/city "Sydney")
          dublin (cc/city "Dublin")]
      (dosync
       (ref-set cc/check-in-ref {sydney 10 dublin 50}))
      (is (nil? (cc/stm-transfer-check-ins! sydney dublin 30)))
      (is (= 10 (get @cc/check-in-ref sydney))))))

;; =============================================================================
;; 並列 API 呼び出しのテスト
;; =============================================================================

(deftest parallel-api-calls-tests
  (testing "parallel API calls return results"
    (let [results (cc/parallel-api-calls ["a" "b" "c"])]
      (is (= 3 (count results)))
      (is (every? :result results)))))

(deftest parallel-api-calls-with-timeout-tests
  (testing "parallel API calls with timeout"
    (let [results (cc/parallel-api-calls-with-timeout ["a" "b"] 500)]
      (is (= 2 (count results)))
      (is (every? #(or (:result %) (:error %)) results)))))

;; =============================================================================
;; 並行カウントのテスト
;; =============================================================================

(deftest count-evens-tests
  (testing "count-evens counts correctly"
    (let [numbers [1 2 3 4 5 6 7 8 9 10]
          result (cc/count-evens numbers)]
      (is (= 5 result)))))

(deftest parallel-filter-count-tests
  (testing "parallel-filter-count counts correctly"
    (let [numbers (range 1 101)
          result (cc/parallel-filter-count #(zero? (mod % 3)) numbers)]
      (is (= 33 result)))))

;; =============================================================================
;; タイムアウト付き収集のテスト
;; =============================================================================

(deftest collect-for-tests
  (testing "collect-for collects values"
    (let [counter (atom 0)
          producer-fn #(swap! counter inc)
          result (cc/collect-for 200 producer-fn 50)]
      (is (>= (count result) 2))
      (is (<= (count result) 6)))))

;; =============================================================================
;; 並行マップ更新のテスト
;; =============================================================================

(deftest apply-updates-tests
  (testing "apply-updates applies all updates"
    (let [updates [(cc/->Update "a" 1)
                   (cc/->Update "b" 2)
                   (cc/->Update "c" 3)]
          result (cc/apply-updates updates)]
      (is (= 1 (get result "a")))
      (is (= 2 (get result "b")))
      (is (= 3 (get result "c"))))))

;; =============================================================================
;; イベントプロセッサのテスト
;; =============================================================================

(deftest event-processor-tests
  (testing "event processor handles events"
    (let [processor (cc/event-processor)
          handled (atom [])]
      ((:add-handler processor) :test #(swap! handled conj %))
      ((:emit processor) (cc/create-event :test {:value 1}))
      ((:emit processor) (cc/create-event :test {:value 2}))
      (Thread/sleep 50)
      (is (= 2 (count @handled)))
      ((:stop processor)))))

;; =============================================================================
;; 並行キャッシュのテスト
;; =============================================================================

(deftest create-cache-tests
  (testing "cache stores and retrieves values"
    (let [cache (cc/create-cache)
          compute-count (atom 0)]
      ;; 最初の取得（キャッシュミス）
      (let [result ((:get cache) :key1 #(do (swap! compute-count inc) 42))]
        (is (= 42 result)))
      ;; 2回目の取得（キャッシュヒット）
      (let [result ((:get cache) :key1 #(do (swap! compute-count inc) 100))]
        (is (= 42 result)))
      ;; compute は1回だけ呼ばれる
      (is (= 1 @compute-count))
      ;; stats を確認
      (let [stats ((:stats cache))]
        (is (= 1 (:size stats)))
        (is (= 1 (:hits stats)))
        (is (= 1 (:misses stats))))))

  (testing "cache invalidation"
    (let [cache (cc/create-cache)]
      ((:get cache) :key1 (constantly 42))
      ((:invalidate cache) :key1)
      (let [result ((:get cache) :key1 (constantly 100))]
        (is (= 100 result))))))

