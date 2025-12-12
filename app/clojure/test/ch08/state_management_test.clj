(ns ch08.state-management-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch08.state-management :as sm]))

;; =============================================================================
;; delay のテスト
;; =============================================================================

(deftest cast-the-die-tests
  (testing "cast-the-die returns a delay"
    (let [d (sm/cast-the-die)]
      (is (delay? d))))

  (testing "force evaluates the delay"
    (let [d (sm/cast-the-die)
          result (force d)]
      (is (>= result 1))
      (is (<= result 6)))))

(deftest cast-die-twice-tests
  (testing "cast-die-twice returns sum of two dice"
    (let [result (sm/cast-die-twice)]
      (is (>= result 2))
      (is (<= result 12)))))

;; =============================================================================
;; atom のテスト
;; =============================================================================

(deftest counter-tests
  (testing "counter operations"
    (sm/reset-counter!)
    (is (= 0 (sm/get-counter)))

    (sm/increment-counter!)
    (is (= 1 (sm/get-counter)))

    (sm/increment-counter!)
    (sm/increment-counter!)
    (is (= 3 (sm/get-counter)))

    (sm/reset-counter!)
    (is (= 0 (sm/get-counter)))))

(deftest swap-with-old-tests
  (testing "swap-with-old! returns old value"
    (let [a (atom 5)
          old-val (sm/swap-with-old! a inc)]
      (is (= 5 old-val))
      (is (= 6 @a)))))

(deftest compare-and-swap-tests
  (testing "compare-and-swap! - success"
    (let [a (atom 5)]
      (is (true? (sm/compare-and-swap! a 5 10)))
      (is (= 10 @a))))

  (testing "compare-and-swap! - failure"
    (let [a (atom 5)]
      (is (false? (sm/compare-and-swap! a 3 10)))
      (is (= 5 @a)))))

;; =============================================================================
;; ユーザー・セッション管理のテスト
;; =============================================================================

(deftest user-management-tests
  (testing "add and get user"
    (reset! @#'sm/users {})
    (sm/add-user! 1 "Alice" "alice@example.com")
    (let [user (sm/get-user 1)]
      (is (= "Alice" (:name user)))
      (is (= "alice@example.com" (:email user)))))

  (testing "get non-existent user"
    (is (nil? (sm/get-user 999)))))

(deftest session-management-tests
  (testing "create and get session"
    (reset! @#'sm/sessions {})
    (reset! @#'sm/users {})
    (sm/add-user! 1 "Alice" "alice@example.com")
    (let [session-id (sm/create-session! 1)
          session (sm/get-session session-id)]
      (is (some? session-id))
      (is (= 1 (:user-id session)))
      (is (some? (:created-at session))))))

;; =============================================================================
;; サイコロゲームのテスト
;; =============================================================================

(deftest game-state-tests
  (testing "roll-die! updates state"
    (sm/reset-game!)
    (let [initial-state (sm/get-game-state)]
      (is (= [] (:rolls initial-state)))
      (is (= 0 (:total initial-state))))

    (let [roll1 (sm/roll-die!)
          state1 (sm/get-game-state)]
      (is (>= roll1 1))
      (is (<= roll1 6))
      (is (= 1 (count (:rolls state1))))
      (is (= roll1 (:total state1))))

    (let [roll2 (sm/roll-die!)
          state2 (sm/get-game-state)]
      (is (= 2 (count (:rolls state2))))))

  (testing "play-rounds!"
    (let [state (sm/play-rounds! 5)]
      (is (= 5 (count (:rolls state))))
      (is (= (reduce + (:rolls state)) (:total state))))))

;; =============================================================================
;; future のテスト
;; =============================================================================

(deftest async-api-call-tests
  (testing "async-api-call returns a future"
    (let [f (sm/async-api-call "test" 10)]
      (is (future? f))
      (let [result @f]
        (is (= "test" (:name result)))
        (is (= "Result for test" (:result result)))))))

(deftest parallel-api-calls-tests
  (testing "parallel-api-calls combines results"
    (let [results (sm/parallel-api-calls ["a" "b" "c"])]
      (is (= 3 (count results)))
      (is (= "a" (:name (first results))))
      (is (= "c" (:name (last results)))))))

;; =============================================================================
;; promise のテスト
;; =============================================================================

(deftest promise-tests
  (testing "promise creation and delivery"
    (let [p (sm/create-promise)]
      (sm/deliver-value! p 42)
      (is (= 42 (sm/get-promise-value p)))))

  (testing "promise with timeout - delivered"
    (let [p (sm/create-promise)]
      (sm/deliver-value! p 42)
      (is (= 42 (sm/get-promise-value-with-timeout p 100 :default)))))

  (testing "promise with timeout - not delivered"
    (let [p (sm/create-promise)]
      (is (= :default (sm/get-promise-value-with-timeout p 10 :default))))))

;; =============================================================================
;; ミーティングスケジューリングのテスト
;; =============================================================================

(deftest meeting-time-tests
  (testing "meeting-time"
    (let [m (sm/meeting-time 9 10)]
      (is (= 9 (:start-hour m)))
      (is (= 10 (:end-hour m))))))

(deftest meetings-overlap-tests
  (testing "meetings-overlap? - overlapping"
    (let [m1 (sm/meeting-time 9 11)
          m2 (sm/meeting-time 10 12)]
      (is (true? (sm/meetings-overlap? m1 m2)))))

  (testing "meetings-overlap? - not overlapping"
    (let [m1 (sm/meeting-time 9 10)
          m2 (sm/meeting-time 10 11)]
      (is (false? (sm/meetings-overlap? m1 m2)))))

  (testing "meetings-overlap? - contained"
    (let [m1 (sm/meeting-time 9 12)
          m2 (sm/meeting-time 10 11)]
      (is (true? (sm/meetings-overlap? m1 m2))))))

(deftest possible-meetings-tests
  (testing "possible-meetings finds available slots"
    (let [existing [(sm/meeting-time 9 10) (sm/meeting-time 14 15)]
          slots (sm/possible-meetings existing 8 17 1)]
      (is (some #(= 8 (:start-hour %)) slots))
      (is (some #(= 10 (:start-hour %)) slots))
      (is (not (some #(= 9 (:start-hour %)) slots)))
      (is (not (some #(= 14 (:start-hour %)) slots))))))

;; =============================================================================
;; リトライ戦略のテスト
;; =============================================================================

(deftest retry-tests
  (testing "retry - success on first try"
    (let [result (sm/retry #(+ 1 2) 3)]
      (is (= 3 (:ok result)))))

  (testing "retry - failure"
    (let [call-count (atom 0)
          result (sm/retry #(do (swap! call-count inc)
                                 (throw (Exception. "fail")))
                           3)]
      (is (some? (:error result)))
      (is (= 4 @call-count)))))  ; 初回 + 3回リトライ

(deftest retry-with-default-tests
  (testing "retry-with-default - success"
    (let [result (sm/retry-with-default #(+ 1 2) 3 0)]
      (is (= 3 result))))

  (testing "retry-with-default - failure returns default"
    (let [result (sm/retry-with-default #(throw (Exception. "fail")) 2 :default)]
      (is (= :default result)))))

;; =============================================================================
;; 副作用の分離パターンのテスト
;; =============================================================================

(deftest calculate-discount-tests
  (testing "calculate-discount"
    (is (= 90.0 (sm/calculate-discount 100 0.1)))
    (is (= 80.0 (sm/calculate-discount 100 0.2)))
    (is (= 100 (sm/calculate-discount 100 0)))))

(deftest calculate-total-tests
  (testing "calculate-total"
    (is (= 300 (sm/calculate-total [{:price 100} {:price 200}])))
    (is (= 0 (sm/calculate-total [])))))

(deftest process-order-tests
  (testing "process-order!"
    (reset! @#'sm/order-db {})
    (let [items [{:price 100} {:price 200}]
          order-id (sm/process-order! "order-1" items 0.1)
          order (sm/get-order "order-1")]
      (is (= "order-1" order-id))
      (is (= items (:items order)))
      (is (= 300 (:subtotal order)))
      (is (= 0.1 (:discount-rate order)))
      (is (= 270.0 (:total order))))))

