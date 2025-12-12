(ns ch06.error-handling-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch06.error-handling :as eh]))

;; =============================================================================
;; nil チェックのテスト
;; =============================================================================

(deftest some-value-tests
  (testing "some-value?"
    (is (true? (eh/some-value? 1)))
    (is (true? (eh/some-value? "hello")))
    (is (true? (eh/some-value? false)))
    (is (false? (eh/some-value? nil)))))

(deftest nil-value-tests
  (testing "nil-value?"
    (is (true? (eh/nil-value? nil)))
    (is (false? (eh/nil-value? 1)))
    (is (false? (eh/nil-value? false)))))

;; =============================================================================
;; 安全な計算のテスト
;; =============================================================================

(deftest safe-divide-tests
  (testing "safe-divide"
    (is (= 5 (eh/safe-divide 10 2)))
    (is (nil? (eh/safe-divide 10 0)))
    (is (= 3 (eh/safe-divide 7 2)))))

(deftest safe-sqrt-tests
  (testing "safe-sqrt"
    (is (= 2.0 (eh/safe-sqrt 4)))
    (is (= 0.0 (eh/safe-sqrt 0)))
    (is (nil? (eh/safe-sqrt -1)))))

;; =============================================================================
;; ネスト値取得のテスト
;; =============================================================================

(deftest get-nested-value-tests
  (testing "get-nested-value"
    (is (= "Alice" (eh/get-nested-value {:user {:profile {:name "Alice"}}})))
    (is (nil? (eh/get-nested-value {:user {:profile {}}})))
    (is (nil? (eh/get-nested-value {:user {}})))))

;; =============================================================================
;; パース関数のテスト
;; =============================================================================

(deftest parse-int-tests
  (testing "parse-int"
    (is (= 123 (eh/parse-int "123")))
    (is (= 123 (eh/parse-int "  123  ")))
    (is (nil? (eh/parse-int "abc")))
    (is (nil? (eh/parse-int "")))))

(deftest index-of-tests
  (testing "index-of"
    (is (= 5 (eh/index-of "Hello (World)" \()))
    (is (= 0 (eh/index-of "(Test)" \()))
    (is (nil? (eh/index-of "Hello" \()))))

;; =============================================================================
;; TV番組名前抽出のテスト
;; =============================================================================

(deftest extract-name-tests
  (testing "extract-name"
    (is (= "Breaking Bad" (eh/extract-name "Breaking Bad (2008-2013)")))
    (is (= "The Wire" (eh/extract-name "The Wire (2002-2008)")))
    (is (= "Chernobyl" (eh/extract-name "Chernobyl (2019)")))
    (is (nil? (eh/extract-name "No Brackets")))
    (is (nil? (eh/extract-name "(2020)")))))

;; =============================================================================
;; TV番組年抽出のテスト
;; =============================================================================

(deftest extract-year-start-tests
  (testing "extract-year-start"
    (is (= 2008 (eh/extract-year-start "Breaking Bad (2008-2013)")))
    (is (= 2002 (eh/extract-year-start "The Wire (2002-2008)")))
    (is (nil? (eh/extract-year-start "Chernobyl (2019)")))
    (is (nil? (eh/extract-year-start "Invalid")))))

(deftest extract-year-end-tests
  (testing "extract-year-end"
    (is (= 2013 (eh/extract-year-end "Breaking Bad (2008-2013)")))
    (is (= 2008 (eh/extract-year-end "The Wire (2002-2008)")))
    (is (nil? (eh/extract-year-end "Chernobyl (2019)")))
    (is (nil? (eh/extract-year-end "Invalid")))))

(deftest extract-single-year-tests
  (testing "extract-single-year"
    (is (= 2019 (eh/extract-single-year "Chernobyl (2019)")))
    (is (nil? (eh/extract-single-year "Breaking Bad (2008-2013)")))
    (is (nil? (eh/extract-single-year "Invalid")))))

;; =============================================================================
;; TV番組パースのテスト
;; =============================================================================

(deftest parse-show-tests
  (testing "parse-show - 範囲年"
    (let [result (eh/parse-show "Breaking Bad (2008-2013)")]
      (is (some? result))
      (is (= "Breaking Bad" (:title result)))
      (is (= 2008 (:start result)))
      (is (= 2013 (:end result)))))

  (testing "parse-show - 単年"
    (let [result (eh/parse-show "Chernobyl (2019)")]
      (is (some? result))
      (is (= "Chernobyl" (:title result)))
      (is (= 2019 (:start result)))
      (is (= 2019 (:end result)))))

  (testing "parse-show - 無効な形式"
    (is (nil? (eh/parse-show "Invalid")))
    (is (nil? (eh/parse-show "No Brackets Here")))
    (is (nil? (eh/parse-show "(2020)")))))

;; =============================================================================
;; エラーハンドリング戦略のテスト
;; =============================================================================

(deftest parse-shows-best-effort-tests
  (testing "parse-shows-best-effort"
    (let [raw-shows ["Breaking Bad (2008-2013)"
                     "Invalid Show"
                     "Chernobyl (2019)"]
          result (eh/parse-shows-best-effort raw-shows)]
      (is (= 2 (count result)))
      (is (= "Breaking Bad" (:title (first result))))
      (is (= "Chernobyl" (:title (second result)))))))

(deftest parse-shows-all-or-nothing-tests
  (testing "parse-shows-all-or-nothing - 全成功"
    (let [raw-shows ["Breaking Bad (2008-2013)"
                     "Chernobyl (2019)"]
          result (eh/parse-shows-all-or-nothing raw-shows)]
      (is (some? result))
      (is (= 2 (count result)))))

  (testing "parse-shows-all-or-nothing - 一つでも失敗"
    (let [raw-shows ["Breaking Bad (2008-2013)"
                     "Invalid Show"]
          result (eh/parse-shows-all-or-nothing raw-shows)]
      (is (nil? result)))))

;; =============================================================================
;; フォールバックのテスト
;; =============================================================================

(deftest get-with-default-tests
  (testing "get-with-default"
    (is (= 5 (eh/get-with-default 5 10)))
    (is (= 10 (eh/get-with-default nil 10)))
    (is (= false (eh/get-with-default false true)))))

(deftest find-first-valid-tests
  (testing "find-first-valid"
    (is (= 1 (eh/find-first-valid 1 2 3)))
    (is (= 2 (eh/find-first-valid nil 2 3)))
    (is (= 3 (eh/find-first-valid nil nil 3)))
    (is (nil? (eh/find-first-valid nil nil nil)))))

;; =============================================================================
;; if-let/when-let のテスト
;; =============================================================================

(deftest greet-user-tests
  (testing "greet-user"
    (is (= "Hello, Alice!" (eh/greet-user {:name "Alice"})))
    (is (= "Hello, Guest!" (eh/greet-user {})))
    (is (= "Hello, Guest!" (eh/greet-user {:name nil})))))

(deftest process-order-tests
  (testing "process-order"
    (let [order {:id 1 :items [{:price 100} {:price 200}]}
          result (eh/process-order order)]
      (is (some? result))
      (is (= 1 (:order-id result)))
      (is (= 300 (:total result))))

    (is (nil? (eh/process-order {:id 1 :items []})))
    (is (nil? (eh/process-order {:id 1})))))

;; =============================================================================
;; fnil のテスト
;; =============================================================================

(deftest safe-inc-tests
  (testing "safe-inc"
    (is (= 6 (eh/safe-inc 5)))
    (is (= 1 (eh/safe-inc nil)))))

(deftest safe-conj-tests
  (testing "safe-conj"
    (is (= [1 2] (eh/safe-conj [1] 2)))
    (is (= [1] (eh/safe-conj nil 1)))))

(deftest count-occurrences-tests
  (testing "count-occurrences"
    (is (= {:a 2 :b 3 :c 1}
           (eh/count-occurrences [:a :b :b :c :a :b])))))

;; =============================================================================
;; ユーザープロファイルのテスト
;; =============================================================================

(deftest get-user-email-tests
  (testing "get-user-email"
    (is (= "alice@example.com" (eh/get-user-email {:email "alice@example.com"})))
    (is (= "no-email@example.com" (eh/get-user-email {:email nil})))
    (is (= "no-email@example.com" (eh/get-user-email {})))))

(deftest get-adult-users-tests
  (testing "get-adult-users"
    (let [adults (eh/get-adult-users eh/users)]
      (is (= 2 (count adults)))
      (is (every? #(>= (:age %) 18) adults)))))

(deftest user-summary-tests
  (testing "user-summary"
    (is (= "Alice (alice@example.com) - Age: 30"
           (eh/user-summary (first eh/users))))
    (is (= "Bob (N/A) - Age: 25"
           (eh/user-summary (second eh/users))))))

;; =============================================================================
;; 設定のテスト
;; =============================================================================

(deftest load-config-tests
  (testing "load-config"
    (let [config (eh/load-config {:port 3000})]
      (is (= "localhost" (:host config)))
      (is (= 3000 (:port config)))
      (is (= 30000 (:timeout config))))))

(deftest validate-config-tests
  (testing "validate-config"
    (is (true? (eh/validate-config eh/default-config)))
    (is (false? (eh/validate-config {:host nil :port 8080 :timeout 1000})))
    (is (false? (eh/validate-config {:host "localhost" :port 0 :timeout 1000})))))

;; =============================================================================
;; process-items のテスト
;; =============================================================================

(deftest process-items-tests
  (testing "process-items"
    (let [result (eh/process-items [1 2 3])]
      (is (= 3 (:count result)))
      (is (= 1 (:first result)))
      (is (= 3 (:last result))))

    (is (nil? (eh/process-items [])))
    (is (nil? (eh/process-items nil)))))
