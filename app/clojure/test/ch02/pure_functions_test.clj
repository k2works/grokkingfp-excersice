(ns ch02.pure-functions-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch02.pure-functions :as pf]))

;; =============================================================================
;; 純粋関数の基本
;; =============================================================================

(deftest basic-pure-functions-tests
  (testing "add 関数"
    (is (= 5 (pf/add 2 3)))
    (is (= 0 (pf/add 0 0)))
    (is (= -1 (pf/add 2 -3))))

  (testing "multiply 関数"
    (is (= 6 (pf/multiply 2 3)))
    (is (= 0 (pf/multiply 5 0))))

  (testing "square 関数"
    (is (= 4 (pf/square 2)))
    (is (= 9 (pf/square 3)))
    (is (= 0 (pf/square 0)))))

;; =============================================================================
;; 参照透過性
;; =============================================================================

(deftest referential-transparency-tests
  (testing "参照透過性デモ"
    (let [result (pf/referential-transparency-demo)]
      (is (= (:using-names result) (:using-values result))))))

;; =============================================================================
;; 純粋 vs 不純
;; =============================================================================

(deftest pure-vs-impure-tests
  (testing "純粋な税計算は同じ引数で同じ結果"
    (is (= (pf/pure-calculate-tax 1000 0.1)
           (pf/pure-calculate-tax 1000 0.1)))
    (is (= 100.0 (pf/pure-calculate-tax 1000 0.1)))
    (is (= 200.0 (pf/pure-calculate-tax 1000 0.2))))

  (testing "純粋な挨拶"
    (is (= "Good morning, Alice!" (pf/pure-greeting "Alice" 9)))
    (is (= "Good afternoon, Bob!" (pf/pure-greeting "Bob" 14)))
    (is (= "Good evening, Carol!" (pf/pure-greeting "Carol" 20)))))

;; =============================================================================
;; 高階関数
;; =============================================================================

(deftest higher-order-functions-tests
  (testing "transform-all"
    (is (= [2 4 6] (pf/transform-all #(* 2 %) [1 2 3])))
    (is (= [] (pf/transform-all inc []))))

  (testing "filter-by"
    (is (= [2 4] (pf/filter-by even? [1 2 3 4 5])))
    (is (= [] (pf/filter-by even? [1 3 5]))))

  (testing "reduce-with"
    (is (= 15 (pf/reduce-with + 0 [1 2 3 4 5])))
    (is (= 0 (pf/reduce-with + 0 [])))))

;; =============================================================================
;; 給与計算
;; =============================================================================

(deftest payroll-tests
  (testing "総支給額計算"
    (is (= 300000 (pf/calculate-gross-salary 300000 0 0)))
    (is (= 310000 (pf/calculate-gross-salary 300000 10 1000))))

  (testing "控除計算"
    (let [deductions (pf/calculate-deductions 300000 pf/tax-rates)]
      (is (= 60000.0 (:income-tax deductions)))
      (is (= 30000.0 (:social deductions)))
      (is (= 15000.0 (:health deductions)))
      (is (= 105000.0 (:total deductions)))))

  (testing "手取り計算"
    (is (= 195000.0 (pf/calculate-net-salary 300000 {:total 105000.0}))))

  (testing "給与計算全体"
    (let [employee {:id 1 :base-salary 300000 :overtime-hours 10 :hourly-rate 1000}
          result (pf/process-payroll employee pf/tax-rates)]
      (is (= 1 (:employee-id result)))
      (is (= 310000 (:gross-salary result)))
      (is (= 108500.0 (get-in result [:deductions :total])))
      (is (= 201500.0 (:net-salary result))))))

;; =============================================================================
;; 文字列処理
;; =============================================================================

(deftest string-processing-tests
  (testing "normalize-string"
    (is (= "hello world" (pf/normalize-string "  Hello World  ")))
    (is (= "test" (pf/normalize-string "TEST"))))

  (testing "count-words"
    (is (= 3 (pf/count-words "hello world test")))
    (is (= 0 (pf/count-words "")))
    (is (= 0 (pf/count-words "   ")))
    (is (= 1 (pf/count-words "  hello  "))))

  (testing "extract-words"
    (is (= ["hello" "world"] (pf/extract-words "Hello World")))
    (is (= [] (pf/extract-words ""))))

  (testing "word-frequencies"
    (is (= {"hello" 2 "world" 1}
           (pf/word-frequencies "hello world hello")))))

;; =============================================================================
;; バリデーション
;; =============================================================================

(deftest validation-tests
  (testing "validate-not-empty"
    (is (:valid (pf/validate-not-empty "Name" "Alice")))
    (is (not (:valid (pf/validate-not-empty "Name" ""))))
    (is (not (:valid (pf/validate-not-empty "Name" "   ")))))

  (testing "validate-min-length"
    (is (:valid (pf/validate-min-length "Password" 8 "12345678")))
    (is (not (:valid (pf/validate-min-length "Password" 8 "1234")))))

  (testing "validate-email-format"
    (is (:valid (pf/validate-email-format "test@example.com")))
    (is (not (:valid (pf/validate-email-format "invalid")))))

  (testing "validate-age"
    (is (:valid (pf/validate-age 30)))
    (is (not (:valid (pf/validate-age -1))))
    (is (not (:valid (pf/validate-age 200))))
    (is (not (:valid (pf/validate-age "thirty")))))

  (testing "validate-user"
    (let [valid-user {:name "Alice" :email "alice@example.com" :age 30}
          invalid-user {:name "" :email "invalid" :age -1}]
      (is (:valid (pf/validate-user valid-user)))
      (is (not (:valid (pf/validate-user invalid-user))))
      (is (= 3 (count (:errors (pf/validate-user invalid-user))))))))

;; =============================================================================
;; リスト処理
;; =============================================================================

(deftest list-processing-tests
  (testing "double-all"
    (is (= [2 4 6] (pf/double-all [1 2 3])))
    (is (= [] (pf/double-all []))))

  (testing "keep-even"
    (is (= [2 4] (pf/keep-even [1 2 3 4 5])))
    (is (= [] (pf/keep-even [1 3 5]))))

  (testing "sum-all"
    (is (= 15 (pf/sum-all [1 2 3 4 5])))
    (is (= 0 (pf/sum-all []))))

  (testing "average"
    (is (= 3 (pf/average [1 2 3 4 5])))
    (is (= 0 (pf/average []))))

  (testing "statistics"
    (let [stats (pf/statistics [1 2 3 4 5])]
      (is (= 5 (:count stats)))
      (is (= 15 (:sum stats)))
      (is (= 3 (:average stats)))
      (is (= 1 (:min stats)))
      (is (= 5 (:max stats))))
    (let [empty-stats (pf/statistics [])]
      (is (= 0 (:count empty-stats)))
      (is (nil? (:min empty-stats))))))

;; =============================================================================
;; コンポジション
;; =============================================================================

(deftest composition-tests
  (testing "process-numbers"
    (is (= 14 (pf/process-numbers [1 -2 3])))  ; 1^2 + 3^2 = 1 + 9 = 10... wait
    ; 正の数のみ: [1, 3]、square: [1, 9]、sum: 10
    (is (= 10 (pf/process-numbers [1 -2 3]))))

  (testing "process-string"
    (is (= "OLLEH" (pf/process-string "  hello  "))))

  (testing "partial application"
    (is (= 15 (pf/add-ten 5)))
    (is (= 10 (pf/multiply-by-two 5)))))
