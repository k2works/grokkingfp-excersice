(ns ch04.functions-as-values-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch04.functions-as-values :as fav]))

;; =============================================================================
;; map のテスト
;; =============================================================================

(deftest len-tests
  (testing "len"
    (is (= 5 (fav/len "scala")))
    (is (= 0 (fav/len "")))))

(deftest double-value-tests
  (testing "double-value"
    (is (= 10 (fav/double-value 5)))
    (is (= 0 (fav/double-value 0)))
    (is (= -4 (fav/double-value -2)))))

(deftest demonstrate-map-tests
  (testing "demonstrate-map"
    (let [result (fav/demonstrate-map)]
      (is (= '(5 4 3) (:lengths result)))
      (is (= '(10 2 4 8 0) (:doubled result)))
      (is (= '(1 4 9 16 25) (:squared result))))))

;; =============================================================================
;; filter のテスト
;; =============================================================================

(deftest odd-number-tests
  (testing "odd-number?"
    (is (true? (fav/odd-number? 3)))
    (is (false? (fav/odd-number? 4)))))

(deftest larger-than-4-tests
  (testing "larger-than-4?"
    (is (true? (fav/larger-than-4? 5)))
    (is (false? (fav/larger-than-4? 4)))
    (is (false? (fav/larger-than-4? 3)))))

(deftest demonstrate-filter-tests
  (testing "demonstrate-filter"
    (let [result (fav/demonstrate-filter)]
      (is (= '(5 1) (:odds result)))
      (is (= '(5) (:large result)))
      (is (= '(2 4 6) (:even result))))))

;; =============================================================================
;; reduce のテスト
;; =============================================================================

(deftest sum-all-tests
  (testing "sum-all"
    (is (= 112 (fav/sum-all [5 1 2 4 100])))
    (is (= 0 (fav/sum-all [])))
    (is (= 5 (fav/sum-all [5])))))

(deftest find-max-tests
  (testing "find-max"
    (is (= 15 (fav/find-max [5 1 2 4 15])))
    (is (= 5 (fav/find-max [5])))))

(deftest find-min-tests
  (testing "find-min"
    (is (= 1 (fav/find-min [5 1 2 4 15])))
    (is (= 5 (fav/find-min [5])))))

(deftest product-tests
  (testing "product"
    (is (= 120 (fav/product [1 2 3 4 5])))
    (is (= 1 (fav/product [])))
    (is (= 6 (fav/product [2 3])))))

;; =============================================================================
;; sort-by のテスト
;; =============================================================================

(deftest score-tests
  (testing "score"
    (is (= 2 (fav/score "java")))   ; j, v
    (is (= 4 (fav/score "rust")))   ; r, u, s, t
    (is (= 0 (fav/score "aaa")))))

(deftest sort-by-score-tests
  (testing "sort-by-score"
    (is (= ["java" "rust"]
           (fav/sort-by-score ["rust" "java"])))))

(deftest sort-by-length-tests
  (testing "sort-by-length"
    (is (= ["a" "bb" "ccc"]
           (fav/sort-by-length ["ccc" "a" "bb"])))))

;; =============================================================================
;; 関数を返す関数のテスト
;; =============================================================================

(deftest larger-than-tests
  (testing "larger-than"
    (is (true? ((fav/larger-than 4) 5)))
    (is (false? ((fav/larger-than 4) 4)))
    (is (false? ((fav/larger-than 4) 3)))))

(deftest smaller-than-tests
  (testing "smaller-than"
    (is (true? ((fav/smaller-than 4) 3)))
    (is (false? ((fav/smaller-than 4) 4)))))

(deftest divisible-by-tests
  (testing "divisible-by"
    (is (true? ((fav/divisible-by 3) 9)))
    (is (false? ((fav/divisible-by 3) 10)))))

(deftest between-tests
  (testing "between"
    (is (true? ((fav/between 2 5) 3)))
    (is (true? ((fav/between 2 5) 2)))
    (is (true? ((fav/between 2 5) 5)))
    (is (false? ((fav/between 2 5) 1)))
    (is (false? ((fav/between 2 5) 6)))))

(deftest demonstrate-function-returning-tests
  (testing "demonstrate-function-returning"
    (let [result (fav/demonstrate-function-returning)]
      (is (= '(5) (:larger-than-4 result)))
      (is (= '(5 2 4) (:larger-than-1 result)))
      (is (= '(2 4 6) (:divisible-by-2 result)))
      (is (= '(2 3 4 5) (:between-2-and-5 result))))))

;; =============================================================================
;; partial のテスト
;; =============================================================================

(deftest partial-tests
  (testing "partial application"
    (is (= 15 (fav/add-10 5)))
    (is (= 10 (fav/multiply-by-2 5)))
    (is (= "Hello, World" (fav/prepend-hello "World")))))

(deftest demonstrate-partial-tests
  (testing "demonstrate-partial"
    (let [result (fav/demonstrate-partial)]
      (is (= 15 (:add-10 result)))
      (is (= 10 (:multiply-by-2 result)))
      (is (= "Hello, World" (:greeting result)))
      (is (= '(101 102 103) (:mapped result))))))

;; =============================================================================
;; comp のテスト
;; =============================================================================

(deftest demonstrate-comp-tests
  (testing "demonstrate-comp"
    (let [result (fav/demonstrate-comp)]
      ;; (* 2 5) = 10, (+ 10 10) = 20
      (is (= 20 (:processed result)))
      (is (= "John doe" (:normalized result))))))

;; =============================================================================
;; スレッディングマクロのテスト
;; =============================================================================

(deftest process-with-threading-tests
  (testing "process-with-threading"
    ;; [1 2 3 4 5] -> [1 3 5] -> [1 9 25] -> 35
    (is (= 35 (fav/process-with-threading [1 2 3 4 5])))))

(deftest string-processing-tests
  (testing "string-processing"
    (is (= "hello-world" (fav/string-processing "  Hello World  ")))))

;; =============================================================================
;; ワードスコアリングのテスト
;; =============================================================================

(deftest bonus-tests
  (testing "bonus"
    (is (= 5 (fav/bonus "scala")))
    (is (= 0 (fav/bonus "java")))))

(deftest penalty-tests
  (testing "penalty"
    (is (= 7 (fav/penalty "rust")))
    (is (= 7 (fav/penalty "haskell")))
    (is (= 0 (fav/penalty "java")))))

(deftest word-score-tests
  (testing "word-score"
    ;; scala: score=4, bonus=5, penalty=7 -> 2
    (is (= 2 (fav/word-score "scala")))
    ;; java: score=2, bonus=0, penalty=0 -> 2
    (is (= 2 (fav/word-score "java")))))

;; =============================================================================
;; プログラミング言語のテスト
;; =============================================================================

(deftest languages-after-tests
  (testing "languages-after"
    (let [after-2000 (fav/languages-after 2000)]
      (is (= 3 (count after-2000)))
      (is (every? #(> (:year %) 2000) after-2000)))))

(deftest language-names-tests
  (testing "language-names"
    (is (= ["Java" "Scala" "Clojure" "Haskell" "Rust"]
           (fav/language-names fav/languages)))))

(deftest functional-languages-tests
  (testing "functional-languages"
    (let [functional (fav/functional-languages)]
      (is (= 2 (count functional)))
      (is (every? #(= (:paradigm %) :functional) functional)))))

(deftest average-year-tests
  (testing "average-year"
    (is (= 0 (fav/average-year [])))
    (is (= 2000 (fav/average-year [{:year 2000}])))
    (is (= 2001 (fav/average-year fav/languages)))))

;; =============================================================================
;; juxt のテスト
;; =============================================================================

(deftest demonstrate-juxt-tests
  (testing "demonstrate-juxt"
    (let [result (fav/demonstrate-juxt)]
      (is (= [1 9 30] (:stats result)))
      (is (= ["Alice" 30] (:person-info result))))))

;; =============================================================================
;; 述語関数のテスト
;; =============================================================================

(deftest demonstrate-predicates-tests
  (testing "demonstrate-predicates"
    (let [result (fav/demonstrate-predicates)]
      (is (true? (:all-positive? result)))
      (is (= -3 (:any-negative? result)))  ; some は最初にマッチした値を返す
      (is (true? (:no-zeros? result)))
      (is (true? (:not-all-even? result))))))

;; =============================================================================
;; group-by のテスト
;; =============================================================================

(deftest group-by-paradigm-tests
  (testing "group-by-paradigm"
    (let [grouped (fav/group-by-paradigm)]
      (is (= 2 (count (:functional grouped))))
      (is (= 1 (count (:oop grouped))))
      (is (= 2 (count (:multi grouped)))))))

(deftest group-numbers-by-parity-tests
  (testing "group-numbers-by-parity"
    (let [grouped (fav/group-numbers-by-parity [1 2 3 4 5 6])]
      (is (= [2 4 6] (:even grouped)))
      (is (= [1 3 5] (:odd grouped))))))
