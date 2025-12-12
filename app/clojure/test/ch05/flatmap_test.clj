(ns ch05.flatmap-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch05.flatmap :as fm]))

;; =============================================================================
;; flatten のテスト
;; =============================================================================

(deftest demonstrate-flatten-tests
  (testing "demonstrate-flatten"
    (let [result (fm/demonstrate-flatten)]
      (is (= '(1 2 3 4 5) (:basic result)))
      (is (= '(1 2 3 4 5) (:nested result)))
      (is (= '(1 2 3 4) (:mixed result))))))

(deftest flatten-one-level-tests
  (testing "flatten-one-level"
    (is (= '(1 2 3 4) (fm/flatten-one-level [[1 2] [3 4]])))
    (is (= '([1 2] [3]) (fm/flatten-one-level [[[1 2] [3]]])))))

;; =============================================================================
;; mapcat のテスト
;; =============================================================================

(deftest demonstrate-mapcat-tests
  (testing "demonstrate-mapcat"
    (let [result (fm/demonstrate-mapcat)]
      (is (= '(["Chiusano" "Bjarnason"] ["Tolkien"]) (:with-map result)))
      (is (= '("Chiusano" "Bjarnason" "Tolkien") (:with-mapcat result))))))

;; =============================================================================
;; サイズ変化のテスト
;; =============================================================================

(deftest duplicate-each-tests
  (testing "duplicate-each"
    (is (= '(1 1 2 2 3 3) (fm/duplicate-each [1 2 3])))
    (is (= '() (fm/duplicate-each [])))))

(deftest wrap-each-tests
  (testing "wrap-each"
    (is (= '(2 4 6) (fm/wrap-each [1 2 3])))))

(deftest keep-even-tests
  (testing "keep-even"
    (is (= '(2 4) (fm/keep-even [1 2 3 4 5])))
    (is (= '() (fm/keep-even [1 3 5])))))

(deftest demonstrate-size-change-tests
  (testing "demonstrate-size-change"
    (let [result (fm/demonstrate-size-change)]
      (is (= '(1 1 2 2 3 3) (:increased result)))
      (is (= '(2 4 6) (:same result)))
      (is (= '(2 4) (:decreased result))))))

;; =============================================================================
;; book-adaptations のテスト
;; =============================================================================

(deftest book-adaptations-tests
  (testing "book-adaptations"
    (is (= 2 (count (fm/book-adaptations "Tolkien"))))
    (is (= [] (fm/book-adaptations "Unknown")))))

;; =============================================================================
;; レコメンデーションのテスト
;; =============================================================================

(deftest get-recommendations-nested-tests
  (testing "get-recommendations-nested"
    (let [recs (fm/get-recommendations-nested)]
      (is (= 2 (count recs)))
      (is (every? #(clojure.string/includes? % "Tolkien") recs)))))

(deftest get-recommendations-for-tests
  (testing "get-recommendations-for"
    (let [recs (fm/get-recommendations-for)]
      (is (= 2 (count recs)))
      (is (= (fm/get-recommendations-nested) recs)))))

;; =============================================================================
;; for 内包表記のテスト
;; =============================================================================

(deftest even-products-tests
  (testing "even-products"
    (let [result (fm/even-products)]
      (is (every? #(even? (:product %)) result))
      ;; 1*2=2, 1*3=3(odd), 2*1=2, 2*2=4, 2*3=6, 3*1=3(odd), 3*2=6, 3*3=9(odd)
      ;; Even products: 2, 2, 4, 6, 6 = 5 items
      (is (= 5 (count result))))))

;; =============================================================================
;; 円内の点のテスト
;; =============================================================================

(deftest inside-tests
  (testing "inside?"
    (is (true? (fm/inside? {:x 1 :y 1} 2)))
    (is (false? (fm/inside? {:x 5 :y 2} 2)))
    (is (true? (fm/inside? {:x 0 :y 0} 1)))))

(deftest points-inside-circles-tests
  (testing "points-inside-circles"
    (let [result (fm/points-inside-circles)]
      (is (= 1 (count result)))
      (is (= {:x 1 :y 1} (:point (first result))))
      (is (= 2 (:radius (first result)))))))

(deftest all-point-radius-combinations-tests
  (testing "all-point-radius-combinations"
    (let [result (fm/all-point-radius-combinations)]
      (is (= 4 (count result)))
      (is (= 1 (count (filter :inside? result)))))))

;; =============================================================================
;; for の型のテスト
;; =============================================================================

(deftest demonstrate-for-types-tests
  (testing "demonstrate-for-types"
    (let [result (fm/demonstrate-for-types)]
      ;; すべて遅延シーケンス
      (is (seq? (:from-vector result)))
      (is (seq? (:from-set result)))
      (is (seq? (:from-list result))))))

(deftest for-with-into-tests
  (testing "for-with-into"
    (let [result (fm/for-with-into)]
      (is (vector? (:as-vector result)))
      (is (set? (:as-set result)))
      ;; set は重複を除く
      (is (= 4 (count (:as-set result)))))))

;; =============================================================================
;; 商品検索のテスト
;; =============================================================================

(deftest search-products-tests
  (testing "search-products"
    (let [result (fm/search-products 500 1000 [:electronics])]
      (is (= 2 (count result)))
      (is (every? #(= :electronics (:category %)) result)))))

(deftest products-with-discounts-tests
  (testing "products-with-discounts"
    (let [result (fm/products-with-discounts)]
      (is (= 4 (count result)))
      (is (every? :discounted-price result))
      ;; Laptop: 1000 * 0.9 = 900
      (is (= 900.0 (:discounted-price (first result)))))))

(deftest product-tag-pairs-tests
  (testing "product-tag-pairs"
    (let [result (fm/product-tag-pairs)]
      ;; 3 + 3 + 2 + 2 = 10 pairs
      (is (= 10 (count result)))
      (is (every? :product-name result))
      (is (every? :tag result)))))

(deftest find-products-by-tag-tests
  (testing "find-products-by-tag"
    (let [result (fm/find-products-by-tag "work")]
      (is (= 2 (count result)))
      (is (some #(= "Laptop" (:name %)) result))
      (is (some #(= "Desk" (:name %)) result)))))

;; =============================================================================
;; 組み合わせ生成のテスト
;; =============================================================================

(deftest combinations-tests
  (testing "combinations"
    (is (= [[]] (fm/combinations [1 2 3] 0)))
    (is (= [[1] [2] [3]] (fm/combinations [1 2 3] 1)))
    (is (= [[1 2] [1 3] [2 3]] (fm/combinations [1 2 3] 2)))
    (is (= [[1 2 3]] (fm/combinations [1 2 3] 3)))))

(deftest permutations-tests
  (testing "permutations"
    (is (= [[]] (fm/permutations [])))
    (is (= [[1]] (fm/permutations [1])))
    (is (= 6 (count (fm/permutations [1 2 3]))))))

(deftest cartesian-product-tests
  (testing "cartesian-product"
    (is (= [[1 3] [1 4] [2 3] [2 4]]
           (fm/cartesian-product [1 2] [3 4])))
    (is (= [[1 3 5] [1 3 6] [1 4 5] [1 4 6]
            [2 3 5] [2 3 6] [2 4 5] [2 4 6]]
           (fm/cartesian-product [1 2] [3 4] [5 6])))))

;; =============================================================================
;; ファイルシステム走査のテスト
;; =============================================================================

(deftest all-files-tests
  (testing "all-files"
    (let [files (fm/all-files fm/file-system)]
      (is (= 4 (count files)))
      (is (every? #(= :file (:type %)) files)))))

(deftest files-with-path-tests
  (testing "files-with-path"
    (let [files (fm/files-with-path fm/file-system "")]
      (is (= 4 (count files)))
      (is (some #(= "/root/docs/readme.md" (:path %)) files))
      (is (some #(= "/root/src/utils/helpers.clj" (:path %)) files)))))

(deftest total-size-tests
  (testing "total-size"
    ;; 100 + 200 + 500 + 150 + 50 = 1000
    (is (= 1000 (fm/total-size fm/file-system)))))
