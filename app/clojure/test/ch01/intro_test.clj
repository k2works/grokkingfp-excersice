(ns ch01.intro-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch01.intro :as intro]))

;; =============================================================================
;; 命令型 vs 関数型
;; =============================================================================

(deftest sum-tests
  (testing "命令型と関数型の合計は同じ結果"
    (is (= 15 (intro/imperative-sum [1 2 3 4 5])))
    (is (= 15 (intro/functional-sum [1 2 3 4 5]))))

  (testing "空リストの合計"
    (is (= 0 (intro/imperative-sum [])))
    (is (= 0 (intro/functional-sum []))))

  (testing "単一要素"
    (is (= 42 (intro/functional-sum [42])))))

;; =============================================================================
;; 基本関数
;; =============================================================================

(deftest greet-tests
  (testing "greet 関数"
    (is (= "Hello, World!" (intro/greet "World")))
    (is (= "Hello, Clojure!" (intro/greet "Clojure")))))

(deftest add-tests
  (testing "add 関数"
    (is (= 5 (intro/add 2 3)))
    (is (= 0 (intro/add 0 0)))
    (is (= -1 (intro/add 2 -3)))))

(deftest greet-multi-tests
  (testing "greet-multi 複数アリティ"
    (is (= "Hello, World!" (intro/greet-multi)))
    (is (= "Hello, Alice!" (intro/greet-multi "Alice")))
    (is (= "Hello, Dr. Smith!" (intro/greet-multi "Dr." "Smith")))))

;; =============================================================================
;; ローカル束縛
;; =============================================================================

(deftest calculate-area-tests
  (testing "円の面積計算"
    (is (< (Math/abs (- (intro/calculate-area 1) 3.14159)) 0.001))
    (is (< (Math/abs (- (intro/calculate-area 2) 12.56636)) 0.001))))

(deftest format-name-tests
  (testing "名前フォーマット"
    (let [result (intro/format-name "John" "Doe")]
      (is (= "John Doe" (:full result)))
      (is (= "JOHN DOE" (:upper result)))
      (is (= "J.D." (:initials result))))))

;; =============================================================================
;; データアクセス
;; =============================================================================

(deftest get-person-info-tests
  (testing "人物情報取得"
    (is (= "Alice is 30 years old"
           (intro/get-person-info {:name "Alice" :age 30})))))

(deftest get-employee-names-tests
  (testing "従業員名取得"
    (is (= '("Alice" "Bob" "Carol")
           (intro/get-employee-names intro/company)))))

;; =============================================================================
;; 条件分岐
;; =============================================================================

(deftest check-age-tests
  (testing "年齢チェック"
    (are [expected age] (= expected (intro/check-age age))
      "Invalid age" -1
      "Child" 5
      "Child" 12
      "Teenager" 13
      "Teenager" 19
      "Adult" 20
      "Adult" 64
      "Senior" 65
      "Senior" 100)))

(deftest describe-number-tests
  (testing "数値説明"
    (is (= "zero" (intro/describe-number 0)))
    (is (= "positive" (intro/describe-number 5)))
    (is (= "negative" (intro/describe-number -5)))))

(deftest greet-optional-tests
  (testing "オプショナル挨拶"
    (is (= "Hello, Alice!" (intro/greet-optional {:name "Alice"})))
    (is (= "Hello, stranger!" (intro/greet-optional {})))
    (is (= "Hello, stranger!" (intro/greet-optional {:name nil})))))

;; =============================================================================
;; ループと再帰
;; =============================================================================

(deftest factorial-tests
  (testing "階乗計算"
    (are [expected n] (= expected (intro/factorial n))
      1 0
      1 1
      2 2
      6 3
      24 4
      120 5
      3628800 10)))

(deftest sum-of-squares-tests
  (testing "平方和"
    (is (= 0 (intro/sum-of-squares [])))
    (is (= 1 (intro/sum-of-squares [1])))
    (is (= 14 (intro/sum-of-squares [1 2 3])))))

;; =============================================================================
;; ショッピングカート
;; =============================================================================

(deftest item-total-tests
  (testing "アイテム小計"
    (is (= 300 (intro/item-total {:price 100 :quantity 3})))
    (is (= 0 (intro/item-total {:price 100 :quantity 0})))))

(deftest cart-total-tests
  (testing "カート合計"
    (is (= 940 (intro/cart-total intro/sample-cart)))))

(deftest apply-discount-tests
  (testing "割引適用"
    (is (= 900.0 (intro/apply-discount 1000 0.1)))
    (is (= 800.0 (intro/apply-discount 1000 0.2)))
    (is (= 1000.0 (intro/apply-discount 1000 0)))))

(deftest checkout-tests
  (testing "チェックアウト"
    (let [result (intro/checkout intro/sample-cart 0.1)]
      (is (= 940 (:subtotal result)))
      (is (= 0.1 (:discount-rate result)))
      (is (= 94.0 (:discount result)))
      (is (= 846.0 (:total result))))))
