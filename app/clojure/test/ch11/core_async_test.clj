(ns ch11.core-async-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.core.async :as async
             :refer [chan go go-loop <! >! <!! >!! close!
                     timeout alts! alts!! put! take!]]
            [ch11.core-async :as ca]))

;; =============================================================================
;; チャネル作成のテスト
;; =============================================================================

(deftest create-channel-tests
  (testing "create-channel creates a channel"
    (let [ch (ca/create-channel)]
      (is (some? ch))
      (close! ch))))

(deftest create-buffered-channel-tests
  (testing "create-buffered-channel creates buffered channel"
    (let [ch (ca/create-buffered-channel 10)]
      (is (some? ch))
      ;; バッファがあるので put! が即座に成功
      (put! ch 1)
      (put! ch 2)
      (is (= 1 (<!! ch)))
      (is (= 2 (<!! ch)))
      (close! ch))))

;; =============================================================================
;; プロデューサー/コンシューマーのテスト
;; =============================================================================

(deftest simple-producer-consumer-tests
  (testing "producer and consumer work together"
    (let [ch (chan 10)
          _ (ca/simple-producer ch [1 2 3 4 5])
          result (<!! (ca/simple-consumer ch))]
      (is (= [1 2 3 4 5] result)))))

;; =============================================================================
;; 非同期操作のテスト
;; =============================================================================

(deftest async-operation-tests
  (testing "async-operation returns result"
    (let [result-ch (ca/async-operation 42 50)
          result (<!! result-ch)]
      (is (= 42 (:value result)))
      (is (true? (:processed result))))))

(deftest parallel-async-operations-tests
  (testing "parallel operations return results"
    (let [result-ch (ca/parallel-async-operations [1 2 3] 50)]
      ;; 少し待機して結果を収集
      (Thread/sleep 200)
      (let [results (ca/collect-n result-ch 3)]
        (is (= 3 (count results)))
        (is (every? :processed results))))))

;; =============================================================================
;; City と top-cities のテスト
;; =============================================================================

(deftest ca-city-tests
  (testing "city creation"
    (let [c (ca/city "Sydney")]
      (is (= "Sydney" (:name c))))))

(deftest ca-top-cities-tests
  (testing "top-cities returns sorted cities"
    (let [sydney (ca/city "Sydney")
          dublin (ca/city "Dublin")
          check-ins {sydney 100 dublin 200}
          result (ca/top-cities check-ins 2)]
      (is (= 2 (count result)))
      (is (= dublin (:city (first result)))))))

;; =============================================================================
;; パイプラインのテスト
;; =============================================================================

(deftest map-pipeline-tests
  (testing "map-pipeline transforms values"
    (let [in-ch (chan 10)
          out-ch (ca/map-pipeline inc in-ch)]
      (put! in-ch 1)
      (put! in-ch 2)
      (put! in-ch 3)
      (close! in-ch)
      (is (= 2 (<!! out-ch)))
      (is (= 3 (<!! out-ch)))
      (is (= 4 (<!! out-ch))))))

(deftest filter-pipeline-tests
  (testing "filter-pipeline filters values"
    (let [in-ch (chan 10)
          out-ch (ca/filter-pipeline even? in-ch)]
      (put! in-ch 1)
      (put! in-ch 2)
      (put! in-ch 3)
      (put! in-ch 4)
      (close! in-ch)
      (is (= 2 (<!! out-ch)))
      (is (= 4 (<!! out-ch))))))

(deftest reduce-pipeline-tests
  (testing "reduce-pipeline reduces values"
    (let [in-ch (chan 10)
          result-ch (ca/reduce-pipeline + 0 in-ch)]
      (put! in-ch 1)
      (put! in-ch 2)
      (put! in-ch 3)
      (close! in-ch)
      (is (= 6 (<!! result-ch))))))

;; =============================================================================
;; Fan-out/Fan-in のテスト
;; =============================================================================

(deftest fan-in-tests
  (testing "fan-in merges channels"
    (let [ch1 (chan 10)
          ch2 (chan 10)
          out-ch (ca/fan-in [ch1 ch2])]
      (put! ch1 1)
      (put! ch2 2)
      (let [r1 (<!! out-ch)
            r2 (<!! out-ch)
            results (set [r1 r2])]
        (is (= #{1 2} results)))
      (close! ch1)
      (close! ch2)
      (close! out-ch))))

;; =============================================================================
;; タイムアウトのテスト
;; =============================================================================

(deftest with-timeout-tests
  (testing "with-timeout returns value if available"
    (let [ch (chan 1)]
      (put! ch 42)
      (let [result (<!! (ca/with-timeout ch 100 :default))]
        (is (= 42 result))
        (close! ch))))

  (testing "with-timeout returns default on timeout"
    (let [ch (chan)]
      (let [result (<!! (ca/with-timeout ch 50 :default))]
        (is (= :default result))
        (close! ch)))))

;; =============================================================================
;; ワーカープールのテスト
;; =============================================================================

(deftest worker-pool-tests
  (testing "worker pool processes work"
    (let [work-ch (chan 10)
          result-ch (chan 10)]
      (ca/create-worker-pool 3 work-ch result-ch)
      ;; ワークを送信
      (put! work-ch (fn [] 1))
      (put! work-ch (fn [] 2))
      (put! work-ch (fn [] 3))
      ;; 結果を取得
      (let [results (repeatedly 3 #(<!! result-ch))
            result-values (set (map :result results))]
        (is (= #{1 2 3} result-values)))
      (close! work-ch)
      (close! result-ch))))

;; =============================================================================
;; pub/sub のテスト
;; =============================================================================

(deftest pub-sub-tests
  (testing "pub/sub delivers messages"
    (let [ps (ca/create-pub-sub)
          sub-ch ((:subscribe ps) :topic1)]
      ((:start ps))
      ((:publish ps) :topic1 "hello")
      (let [msg (<!! sub-ch)]
        (is (= "hello" msg)))
      ((:close ps)))))

;; =============================================================================
;; レートリミッターのテスト
;; =============================================================================

(deftest rate-limiter-tests
  (testing "rate limiter controls access rate"
    (let [limiter (ca/create-rate-limiter 10)
          start-time (System/currentTimeMillis)]
      ;; 3つのトークンを取得
      ((:acquire limiter))
      ((:acquire limiter))
      ((:acquire limiter))
      (let [elapsed (- (System/currentTimeMillis) start-time)]
        ;; 200ms 以上経過しているはず（10/秒 = 100ms 間隔）
        (is (>= elapsed 200))))))

;; =============================================================================
;; セマフォのテスト
;; =============================================================================

(deftest semaphore-tests
  (testing "semaphore limits concurrency"
    (let [sem (ca/create-semaphore 2)
          counter (atom 0)
          max-concurrent (atom 0)]
      ;; 5つのタスクを実行
      (dotimes [_ 5]
        (future
          ((:with-permit sem)
           (fn []
             (swap! counter inc)
             (swap! max-concurrent max @counter)
             (Thread/sleep 50)
             (swap! counter dec)))))
      ;; 待機
      (Thread/sleep 400)
      ;; 同時実行数は2以下
      (is (<= @max-concurrent 2)))))

;; =============================================================================
;; イベントソーシングのテスト
;; =============================================================================

(deftest event-sourced-entity-tests
  (testing "event sourcing tracks state"
    (let [entity (ca/event-sourced-entity
                  {:count 0}
                  (fn [state event]
                    (case (:type event)
                      :increment (update state :count inc)
                      :decrement (update state :count dec)
                      state)))]
      ((:emit entity) (ca/create-event :increment {}))
      ((:emit entity) (ca/create-event :increment {}))
      ((:emit entity) (ca/create-event :decrement {}))
      (Thread/sleep 100)
      (is (= 1 (:count ((:get-state entity)))))
      (is (= 3 (count ((:get-events entity)))))
      ((:close entity)))))

;; =============================================================================
;; 並行ダウンローダーのテスト
;; =============================================================================

(deftest concurrent-downloader-tests
  (testing "concurrent downloader downloads all URLs"
    (let [urls ["http://a.com" "http://b.com" "http://c.com"]
          result-ch (ca/concurrent-downloader urls 2)
          results (ca/collect-n result-ch 3)]
      (is (= 3 (count results)))
      (is (every? #(= :completed (:status %)) results)))))

;; =============================================================================
;; メッセージブローカーのテスト
;; =============================================================================

(deftest message-broker-tests
  (testing "message broker sends and receives"
    (let [broker (ca/create-message-broker)]
      ((:create-queue broker) "test-queue" 10)
      ((:send broker) "test-queue" "hello")
      ((:send broker) "test-queue" "world")
      (is (= "hello" ((:receive broker) "test-queue")))
      (is (= "world" ((:receive broker) "test-queue")))
      ((:close broker)))))

;; =============================================================================
;; ヘルパー関数のテスト
;; =============================================================================

(deftest seq-channel-conversion-tests
  (testing "seq->channel and channel->seq"
    (let [original [1 2 3 4 5]
          ch (ca/seq->channel original)
          result (ca/collect-all ch)]
      (is (= original result)))))

(deftest collect-n-tests
  (testing "collect-n collects n items"
    (let [ch (chan 10)]
      (dotimes [i 10]
        (put! ch i))
      (let [result (ca/collect-n ch 5)]
        (is (= 5 (count result)))
        (is (= [0 1 2 3 4] result)))
      (close! ch))))

