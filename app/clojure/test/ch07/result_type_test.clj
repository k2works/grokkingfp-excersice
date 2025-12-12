(ns ch07.result-type-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch07.result-type :as rt]))

;; =============================================================================
;; Result 基本操作のテスト
;; =============================================================================

(deftest ok-tests
  (testing "ok"
    (let [result (rt/ok 42)]
      (is (= :ok (:status result)))
      (is (= 42 (:value result))))))

(deftest error-tests
  (testing "error"
    (let [result (rt/error "Something went wrong")]
      (is (= :error (:status result)))
      (is (= "Something went wrong" (:error result))))))

(deftest ok?-tests
  (testing "ok?"
    (is (true? (rt/ok? (rt/ok 42))))
    (is (false? (rt/ok? (rt/error "error"))))))

(deftest error?-tests
  (testing "error?"
    (is (true? (rt/error? (rt/error "error"))))
    (is (false? (rt/error? (rt/ok 42))))))

;; =============================================================================
;; Result 操作のテスト
;; =============================================================================

(deftest result-map-tests
  (testing "result-map - 成功時"
    (let [result (rt/result-map #(* % 2) (rt/ok 5))]
      (is (rt/ok? result))
      (is (= 10 (:value result)))))

  (testing "result-map - エラー時"
    (let [result (rt/result-map #(* % 2) (rt/error "error"))]
      (is (rt/error? result))
      (is (= "error" (:error result))))))

(deftest result-flatmap-tests
  (testing "result-flatmap - 成功→成功"
    (let [result (rt/result-flatmap #(rt/ok (* % 2)) (rt/ok 5))]
      (is (rt/ok? result))
      (is (= 10 (:value result)))))

  (testing "result-flatmap - 成功→エラー"
    (let [result (rt/result-flatmap #(rt/error "failed") (rt/ok 5))]
      (is (rt/error? result))))

  (testing "result-flatmap - エラー→関数未実行"
    (let [result (rt/result-flatmap #(rt/ok (* % 2)) (rt/error "error"))]
      (is (rt/error? result)))))

(deftest result-or-else-tests
  (testing "result-or-else - 成功時"
    (let [result (rt/result-or-else (rt/ok 5) (rt/ok 10))]
      (is (= 5 (:value result)))))

  (testing "result-or-else - エラー時"
    (let [result (rt/result-or-else (rt/error "error") (rt/ok 10))]
      (is (= 10 (:value result))))))

(deftest result-get-or-else-tests
  (testing "result-get-or-else"
    (is (= 5 (rt/result-get-or-else (rt/ok 5) 0)))
    (is (= 0 (rt/result-get-or-else (rt/error "error") 0)))))

(deftest result-to-option-tests
  (testing "result-to-option"
    (is (= 5 (rt/result-to-option (rt/ok 5))))
    (is (nil? (rt/result-to-option (rt/error "error"))))))

;; =============================================================================
;; TV番組パース（Result版）のテスト
;; =============================================================================

(deftest parse-int-result-tests
  (testing "parse-int-result"
    (let [result (rt/parse-int-result "123")]
      (is (rt/ok? result))
      (is (= 123 (:value result))))

    (let [result (rt/parse-int-result "abc")]
      (is (rt/error? result)))))

(deftest extract-name-result-tests
  (testing "extract-name-result"
    (let [result (rt/extract-name-result "Breaking Bad (2008-2013)")]
      (is (rt/ok? result))
      (is (= "Breaking Bad" (:value result))))

    (let [result (rt/extract-name-result "Invalid")]
      (is (rt/error? result)))))

(deftest parse-show-result-tests
  (testing "parse-show-result - 範囲年"
    (let [result (rt/parse-show-result "Breaking Bad (2008-2013)")]
      (is (rt/ok? result))
      (is (= "Breaking Bad" (get-in result [:value :title])))
      (is (= 2008 (get-in result [:value :start])))
      (is (= 2013 (get-in result [:value :end])))))

  (testing "parse-show-result - 単年"
    (let [result (rt/parse-show-result "Chernobyl (2019)")]
      (is (rt/ok? result))
      (is (= "Chernobyl" (get-in result [:value :title])))
      (is (= 2019 (get-in result [:value :start])))))

  (testing "parse-show-result - エラー"
    (let [result (rt/parse-show-result "Invalid")]
      (is (rt/error? result)))))

;; =============================================================================
;; ADT のテスト
;; =============================================================================

(deftest still-active-tests
  (testing "still-active"
    (let [years (rt/still-active 1981)]
      (is (= :still-active (:type years)))
      (is (= 1981 (:since years))))))

(deftest active-between-tests
  (testing "active-between"
    (let [years (rt/active-between 1968 1980)]
      (is (= :active-between (:type years)))
      (is (= 1968 (:start years)))
      (is (= 1980 (:end years))))))

(deftest artist-tests
  (testing "artist"
    (let [a (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))]
      (is (= "Metallica" (:name a)))
      (is (= :heavy-metal (:genre a)))
      (is (= "U.S." (:origin a))))))

;; =============================================================================
;; パターンマッチングのテスト
;; =============================================================================

(deftest was-artist-active-tests
  (testing "was-artist-active? - still-active"
    (let [metallica (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))]
      (is (true? (rt/was-artist-active? metallica 1990 2000)))
      (is (true? (rt/was-artist-active? metallica 1980 1985)))
      (is (false? (rt/was-artist-active? metallica 1970 1980)))))

  (testing "was-artist-active? - active-between"
    (let [led-zeppelin (rt/artist "Led Zeppelin" :hard-rock "England" (rt/active-between 1968 1980))]
      (is (true? (rt/was-artist-active? led-zeppelin 1970 1975)))
      (is (true? (rt/was-artist-active? led-zeppelin 1975 1985)))
      (is (false? (rt/was-artist-active? led-zeppelin 1981 1990))))))

(deftest active-length-tests
  (testing "active-length - still-active"
    (let [metallica (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))]
      (is (= 43 (rt/active-length metallica 2024)))))

  (testing "active-length - active-between"
    (let [led-zeppelin (rt/artist "Led Zeppelin" :hard-rock "England" (rt/active-between 1968 1980))]
      (is (= 12 (rt/active-length led-zeppelin 2024))))))

;; =============================================================================
;; 検索条件のテスト
;; =============================================================================

(deftest search-by-genre-tests
  (testing "search-by-genre"
    (let [condition (rt/search-by-genre [:heavy-metal :hard-rock])]
      (is (= :by-genre (:type condition)))
      (is (contains? (:genres condition) :heavy-metal)))))

(deftest matches-condition-tests
  (testing "matches-condition? - by-genre"
    (let [metallica (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))
          condition (rt/search-by-genre [:heavy-metal])]
      (is (true? (rt/matches-condition? metallica condition)))))

  (testing "matches-condition? - by-origin"
    (let [metallica (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))
          condition (rt/search-by-origin ["U.S." "Canada"])]
      (is (true? (rt/matches-condition? metallica condition)))))

  (testing "matches-condition? - by-active-years"
    (let [metallica (rt/artist "Metallica" :heavy-metal "U.S." (rt/still-active 1981))
          condition (rt/search-by-active-years 1990 2000)]
      (is (true? (rt/matches-condition? metallica condition))))))

(deftest search-artists-tests
  (testing "search-artists"
    (let [conditions [(rt/search-by-origin ["England"])]
          results (rt/search-artists rt/sample-artists conditions)]
      (is (= 2 (count results)))
      (is (every? #(= "England" (:origin %)) results)))

    (let [conditions [(rt/search-by-genre [:heavy-metal])
                      (rt/search-by-origin ["U.S."])]
          results (rt/search-artists rt/sample-artists conditions)]
      (is (= 1 (count results)))
      (is (= "Metallica" (:name (first results)))))))

;; =============================================================================
;; every?/some? のテスト
;; =============================================================================

(deftest all-active-tests
  (testing "all-active?"
    (is (false? (rt/all-active? rt/sample-artists)))
    (is (true? (rt/all-active? [(first rt/sample-artists)])))))

(deftest any-from-origin-tests
  (testing "any-from-origin?"
    (is (some? (rt/any-from-origin? rt/sample-artists "England")))
    (is (nil? (rt/any-from-origin? rt/sample-artists "Japan")))))

;; =============================================================================
;; バリデーションのテスト
;; =============================================================================

(deftest validate-not-empty-tests
  (testing "validate-not-empty"
    (is (rt/ok? (rt/validate-not-empty "Name" "Alice")))
    (is (rt/error? (rt/validate-not-empty "Name" "")))
    (is (rt/error? (rt/validate-not-empty "Name" "   ")))
    (is (rt/error? (rt/validate-not-empty "Name" nil)))))

(deftest validate-email-tests
  (testing "validate-email"
    (is (rt/ok? (rt/validate-email "test@example.com")))
    (is (rt/error? (rt/validate-email "invalid")))
    (is (rt/error? (rt/validate-email "no-at-sign.com")))))

(deftest validate-age-tests
  (testing "validate-age"
    (is (rt/ok? (rt/validate-age 30)))
    (is (rt/error? (rt/validate-age -1)))
    (is (rt/error? (rt/validate-age 200)))
    (is (rt/error? (rt/validate-age "thirty")))))

(deftest validate-user-tests
  (testing "validate-user - 成功"
    (let [user {:name "Alice" :email "alice@example.com" :age 30}
          result (rt/validate-user user)]
      (is (rt/ok? result))))

  (testing "validate-user - 複数エラー"
    (let [user {:name "" :email "invalid" :age -1}
          result (rt/validate-user user)]
      (is (rt/error? result))
      (is (= 3 (count (get-in result [:error :errors])))))))

;; =============================================================================
;; collect-errors のテスト
;; =============================================================================

(deftest collect-errors-tests
  (testing "collect-errors - すべて成功"
    (let [result (rt/collect-errors [(rt/ok 1) (rt/ok 2) (rt/ok 3)])]
      (is (rt/ok? result))
      (is (= '(1 2 3) (:value result)))))

  (testing "collect-errors - エラーあり"
    (let [result (rt/collect-errors [(rt/ok 1) (rt/error "e1") (rt/error "e2")])]
      (is (rt/error? result))
      (is (= 2 (count (get-in result [:error :errors])))))))

;; =============================================================================
;; 例外処理のテスト
;; =============================================================================

(deftest safe-divide-with-info-tests
  (testing "safe-divide-with-info"
    (let [result (rt/safe-divide-with-info 10 2)]
      (is (rt/ok? result))
      (is (= 5 (:value result))))

    (let [result (rt/safe-divide-with-info 10 0)]
      (is (rt/error? result))
      (is (= {:a 10 :b 0} (get-in result [:error :data]))))))

(deftest try-result-tests
  (testing "try-result - 成功"
    (let [result (rt/try-result #(+ 1 2))]
      (is (rt/ok? result))
      (is (= 3 (:value result)))))

  (testing "try-result - 例外"
    (let [result (rt/try-result #(throw (ex-info "Oops" {:code 500})))]
      (is (rt/error? result))
      (is (= "Oops" (get-in result [:error :message]))))))
