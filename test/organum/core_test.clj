(ns organum.core-test
  (:use clojure.test
        clojure.walk
        organum.core)
  (:require [clojure.java.io :as io]))

(deftest test-headline
  (testing "Parsing headline"
    (is (= (parse-headline "** TODO Some text :atag:btag:")
           (section 2 "Some text" ["atag" "btag"] "TODO")))))

(deftest test-block
  (testing "Parsing block header"
    (is (= (parse-block "  #+BEGIN_WIBBLE minor")
           (block "WIBBLE" "minor")))))

(deftest test-src-block
  (testing "Parsing block header"
    (is (= (parse-block "  #+BEGIN_SRC minor :tangle blah")
           (block "SRC" "minor")))))

(deftest test-testfile
  (testing "Parsing test.org"
    (let [sections (parse-file (io/resource "test.org"))]
      (is (vector? sections))
      (testing "Is only :section after leading :root"
        (is (every? #(= (:type %) :section) (rest sections))))
      (testing "Has top level headings"
        (is (some #(= (:level %) 1) sections)))
      (testing "Has second level headings"
        (is (some #(= (:level %) 2) sections)))
      (testing "Has headings with keywords"
        (is (some :kw sections)))
      (testing "Contains a property drawer"
        (is (some #(= (:type %) :drawer) (mapcat :content sections))))
      (testing "Contains table lines"
        (is (some #(= (:line-type %) :table-row) (mapcat :content sections))))
      (testing "Contains unordered list lines"
        (is (some #(= (:line-type %) :unordered-list) (mapcat :content sections)))))))
