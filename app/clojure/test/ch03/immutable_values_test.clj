(ns ch03.immutable-values-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch03.immutable-values :as iv]))

;; =============================================================================
;; 基本操作のテスト
;; =============================================================================

(deftest appended-tests
  (testing "appended"
    (is (= [1 2 3 4] (iv/appended [1 2 3] 4)))
    (is (= [1] (iv/appended [] 1)))))

(deftest appended-all-tests
  (testing "appended-all"
    (is (= [1 2 3 4 5] (iv/appended-all [1 2] [3 4 5])))
    (is (= [1 2 3] (iv/appended-all [] [1 2 3])))))

(deftest slice-tests
  (testing "slice"
    (is (= [2 3] (iv/slice [1 2 3 4] 1 3)))
    (is (= [1 2] (iv/slice [1 2 3] 0 2)))
    (is (= [] (iv/slice [1 2 3] 1 1)))))

(deftest first-n-tests
  (testing "first-n"
    (is (= [1 2] (iv/first-n [1 2 3 4 5] 2)))
    (is (= [] (iv/first-n [1 2 3] 0)))
    (is (= [1 2 3] (iv/first-n [1 2 3] 5)))))

(deftest last-n-tests
  (testing "last-n"
    (is (= [4 5] (iv/last-n [1 2 3 4 5] 2)))
    (is (= [] (iv/last-n [1 2 3] 0)))))

;; =============================================================================
;; リスト変換パターンのテスト
;; =============================================================================

(deftest move-first-two-to-end-tests
  (testing "move-first-two-to-end"
    (is (= ["c" "a" "b"] (iv/move-first-two-to-end ["a" "b" "c"])))
    (is (= ["c" "d" "a" "b"] (iv/move-first-two-to-end ["a" "b" "c" "d"])))))

(deftest insert-before-last-tests
  (testing "insert-before-last"
    (is (= ["a" "c" "b"] (iv/insert-before-last ["a" "b"] "c")))
    (is (= [1 2 99 3] (iv/insert-before-last [1 2 3] 99)))))

(deftest insert-at-tests
  (testing "insert-at"
    (is (= ["a" "x" "b" "c"] (iv/insert-at ["a" "b" "c"] 1 "x")))
    (is (= ["x" "a" "b"] (iv/insert-at ["a" "b"] 0 "x")))
    (is (= ["a" "b" "x"] (iv/insert-at ["a" "b"] 2 "x")))))

(deftest insert-at-middle-tests
  (testing "insert-at-middle"
    (is (= ["a" "b" "X" "c" "d"] (iv/insert-at-middle ["a" "b" "c" "d"] "X")))
    (is (= ["a" "X" "b"] (iv/insert-at-middle ["a" "b"] "X")))))

;; =============================================================================
;; 旅程の再計画のテスト
;; =============================================================================

(deftest replan-tests
  (testing "replan"
    (is (= ["Paris" "Berlin" "Vienna" "Kraków"]
           (iv/replan ["Paris" "Berlin" "Kraków"] "Vienna" "Kraków")))
    (is (= ["Tokyo" "Osaka" "Kyoto" "Nara"]
           (iv/replan ["Tokyo" "Osaka" "Nara"] "Kyoto" "Nara")))))

;; =============================================================================
;; 文字列操作のテスト
;; =============================================================================

(deftest abbreviate-tests
  (testing "abbreviate"
    (is (= "A. Church" (iv/abbreviate "Alonzo Church")))
    (is (= "J. Doe" (iv/abbreviate "John Doe")))
    (is (= "Alice" (iv/abbreviate "Alice")))))

(deftest first-word-tests
  (testing "first-word"
    (is (= "Hello" (iv/first-word "Hello World")))
    (is (= "Single" (iv/first-word "Single")))))

(deftest last-word-tests
  (testing "last-word"
    (is (= "World" (iv/last-word "Hello World")))
    (is (= "Single" (iv/last-word "Single")))))

;; =============================================================================
;; assoc / update のテスト
;; =============================================================================

(deftest update-person-age-tests
  (testing "update-person-age"
    (let [person {:name "Alice" :age 30}]
      (is (= {:name "Alice" :age 31}
             (iv/update-person-age person 31))))))

(deftest increment-age-tests
  (testing "increment-age"
    (let [person {:name "Bob" :age 25}]
      (is (= {:name "Bob" :age 26}
             (iv/increment-age person))))))

(deftest remove-field-tests
  (testing "remove-field"
    (let [person {:name "Carol" :age 35 :email "carol@example.com"}]
      (is (= {:name "Carol" :age 35}
             (iv/remove-field person :email))))))

;; =============================================================================
;; カート操作のテスト
;; =============================================================================

(deftest add-item-tests
  (testing "add-item"
    (let [cart [{:name "Apple" :price 100 :quantity 1}]
          new-cart (iv/add-item cart {:name "Banana" :price 80 :quantity 2})]
      (is (= 2 (count new-cart)))
      (is (= "Banana" (:name (second new-cart)))))))

(deftest update-quantity-tests
  (testing "update-quantity"
    (let [cart [{:name "Apple" :price 100 :quantity 1}
                {:name "Banana" :price 80 :quantity 2}]
          updated (iv/update-quantity cart "Apple" 5)]
      (is (= 5 (:quantity (first updated))))
      (is (= 2 (:quantity (second updated)))))))

(deftest remove-item-tests
  (testing "remove-item"
    (let [cart [{:name "Apple" :price 100 :quantity 1}
                {:name "Banana" :price 80 :quantity 2}]
          removed (iv/remove-item cart "Apple")]
      (is (= 1 (count removed)))
      (is (= "Banana" (:name (first removed)))))))

;; =============================================================================
;; ゲーム状態管理のテスト
;; =============================================================================

(deftest move-player-tests
  (testing "move-player"
    (let [state iv/initial-game-state
          moved (iv/move-player state 5 3)]
      (is (= 5 (get-in moved [:player :position :x])))
      (is (= 3 (get-in moved [:player :position :y]))))))

(deftest damage-player-tests
  (testing "damage-player"
    (let [state iv/initial-game-state
          damaged (iv/damage-player state 30)]
      (is (= 70 (get-in damaged [:player :hp]))))))

(deftest heal-player-tests
  (testing "heal-player"
    (let [state (iv/damage-player iv/initial-game-state 50)
          healed (iv/heal-player state 30)]
      (is (= 80 (get-in healed [:player :hp]))))))

(deftest add-item-to-inventory-tests
  (testing "add-item-to-inventory"
    (let [state iv/initial-game-state
          with-item (iv/add-item-to-inventory state {:name "Potion" :effect :heal})]
      (is (= 1 (count (:items with-item))))
      (is (= "Potion" (:name (first (:items with-item))))))))

(deftest remove-enemy-tests
  (testing "remove-enemy"
    (let [state iv/initial-game-state
          without-goblin (iv/remove-enemy state "Goblin")]
      (is (= 1 (count (:enemies without-goblin))))
      (is (= "Orc" (:name (first (:enemies without-goblin))))))))
