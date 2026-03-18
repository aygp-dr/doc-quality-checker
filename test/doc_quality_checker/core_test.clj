(ns doc_quality_checker.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [doc_quality_checker.core :as core]
            [babashka.fs :as fs]
            [clojure.string :as str]))

;; --- Helpers ---

(defn with-temp-dir
  "Create a temp dir, call f with its path, clean up after."
  [f]
  (let [dir (fs/create-temp-dir {:prefix "dqc-test-"})]
    (try
      (f (str dir))
      (finally
        (fs/delete-tree dir)))))

(defn write-temp-file! [dir filename content]
  (let [path (str (fs/path dir filename))]
    (spit path content)
    path))

;; --- Unit tests for individual checks ---

(deftest test-extract-md-links
  (testing "extracts relative links from markdown"
    (let [links (core/extract-md-links "[foo](bar.md) [baz](https://example.com) [qux](#anchor)")]
      (is (= ["bar.md"] links))))
  (testing "no links returns empty"
    (is (empty? (core/extract-md-links "plain text")))))

(deftest test-extract-org-links
  (testing "extracts relative org links"
    (let [links (core/extract-org-links "[[spec.org]] [[https://example.com][link]] [[#anchor]]")]
      (is (= ["spec.org"] links))))
  (testing "handles file: prefix"
    (let [links (core/extract-org-links "[[file:spec.org][spec]]")]
      (is (= ["spec.org"] links)))))

(deftest test-check-broken-links
  (with-temp-dir
    (fn [dir]
      (let [path (write-temp-file! dir "test.md" "[exists](test.md) [broken](nope.md)")]
        (testing "detects broken links"
          (let [result (core/check-broken-links path (slurp path))]
            (is (= :broken-links (:check result)))
            (is (= 1 (count (:issues result))))
            (is (= "nope.md" (-> result :issues first :link)))))))))

(deftest test-extract-dates
  (testing "extracts YYYY-MM-DD dates"
    (let [dates (core/extract-dates "Created 2024-01-15, updated 2025-06-30")]
      (is (= 2 (count dates)))
      (is (= "2024-01-15" (:text (first dates)))))))

(deftest test-stale-date?
  (testing "date over 1 year old is stale"
    (is (core/stale-date? {:year 2020 :month 1 :day 1})))
  (testing "recent date is not stale"
    (is (not (core/stale-date? {:year 2026 :month 1 :day 1}))))
  (testing "invalid date returns false"
    (is (not (core/stale-date? {:year 2024 :month 13 :day 99})))))

(deftest test-check-stale-dates
  (testing "flags old dates"
    (let [result (core/check-stale-dates "f.md" "Written on 2020-01-01")]
      (is (pos? (:penalty result)))
      (is (= 1 (count (:issues result))))))
  (testing "no penalty for recent dates"
    (let [result (core/check-stale-dates "f.md" "Updated 2026-03-01")]
      (is (zero? (:penalty result))))))

(deftest test-check-missing-sections
  (testing "flags missing sections in README"
    (let [result (core/check-missing-sections "README.md" "# My Project\nSome description")]
      (is (pos? (:penalty result)))
      (is (= 3 (count (:issues result))))))
  (testing "no penalty for non-README"
    (let [result (core/check-missing-sections "guide.md" "# Guide\nSome text")]
      (is (zero? (:penalty result)))))
  (testing "no penalty when sections present"
    (let [content "# Project\n## Install\ndo this\n## Usage\nuse it\n## API\nendpoints"]
      (let [result (core/check-missing-sections "README.md" content)]
        (is (zero? (:penalty result)))))))

(deftest test-check-short-description
  (testing "flags short descriptions"
    (let [result (core/check-short-description "f.md" "# Title\n\nShort.")]
      (is (pos? (:penalty result)))))
  (testing "no penalty for adequate description"
    (let [result (core/check-short-description "f.md"
                   "# Title\n\nThis is a sufficiently long description for the document.")]
      (is (zero? (:penalty result))))))

(deftest test-check-no-code-examples
  (testing "flags missing code blocks in markdown"
    (let [result (core/check-no-code-examples "f.md" "# Title\nJust text")]
      (is (pos? (:penalty result)))))
  (testing "no penalty when code blocks present"
    (let [result (core/check-no-code-examples "f.md" "# Title\n```bash\necho hi\n```")]
      (is (zero? (:penalty result)))))
  (testing "detects org src blocks"
    (let [result (core/check-no-code-examples "f.org" "* Title\n#+begin_src clojure\n(+ 1 2)\n#+end_src")]
      (is (zero? (:penalty result))))))

(deftest test-check-inconsistent-headings
  (testing "flags skipped heading levels"
    (let [result (core/check-inconsistent-headings "f.md" "# Title\n### Skipped h2")]
      (is (pos? (:penalty result)))))
  (testing "no penalty for sequential levels"
    (let [result (core/check-inconsistent-headings "f.md" "# Title\n## Section\n### Sub")]
      (is (zero? (:penalty result)))))
  (testing "works for org files"
    (let [result (core/check-inconsistent-headings "f.org" "* Title\n*** Skipped")]
      (is (pos? (:penalty result))))))

(deftest test-check-todo-items
  (testing "flags TODO items"
    (let [result (core/check-todo-items "f.md" "# Doc\nTODO: finish this\nFIXME: broken")]
      (is (= 2 (count (:issues result))))
      (is (pos? (:penalty result)))))
  (testing "no penalty when clean"
    (let [result (core/check-todo-items "f.md" "# Clean doc\nAll done.")]
      (is (zero? (:penalty result))))))

(deftest test-has-skipped-levels?
  (is (true? (core/has-skipped-levels? [1 3])))
  (is (not (core/has-skipped-levels? [1 2 3])))
  (is (not (core/has-skipped-levels? [])))
  (is (not (core/has-skipped-levels? [2]))))

;; --- Integration tests ---

(deftest test-check-file
  (with-temp-dir
    (fn [dir]
      (testing "scores a well-formed markdown file"
        (let [path (write-temp-file! dir "good.md"
                     "# Good Document\n\nThis is a well-written document with enough description to pass.\n\n## Details\n\nSome content here.\n\n```bash\necho hello\n```\n")
              result (core/check-file path)]
          (is (> (:score result) 50))
          (is (string? (:file result)))))
      (testing "scores a poor markdown file low"
        (let [path (write-temp-file! dir "README.md"
                     "# X\n\n### Skipped heading\n\nTODO: write this\nFIXME: broken\nHACK: workaround\nXXX: bad\n2020-01-01\n2019-05-10\n")
              result (core/check-file path)]
          (is (< (:score result) 50)))))))

(deftest test-find-doc-files
  (with-temp-dir
    (fn [dir]
      (write-temp-file! dir "readme.md" "# Test")
      (write-temp-file! dir "spec.org" "* Test")
      (write-temp-file! dir "code.clj" "(ns foo)")
      (let [files (core/find-doc-files dir)]
        (is (= 2 (count files)))
        (is (every? #(or (str/ends-with? % ".md") (str/ends-with? % ".org")) files))))))

(deftest test-run-with-threshold
  (with-temp-dir
    (fn [dir]
      (write-temp-file! dir "doc.md"
        "# Good Document\n\nThis is a reasonably long description for testing purposes.\n\n## Section\n\nContent here.\n\n```clojure\n(+ 1 2)\n```\n")
      (testing "passes with low threshold"
        (let [result (core/run {:dir dir :format "text" :threshold 30})]
          (is (= 0 (:exit-code result)))))
      (testing "json output is valid"
        (let [result (core/run {:dir dir :format "json" :threshold 50})]
          (is (str/starts-with? (:output result) "{"))))
      (testing "edn output is valid"
        (let [result (core/run {:dir dir :format "edn" :threshold 50})]
          (is (str/starts-with? (:output result) "{")))))))

(deftest test-run-no-files
  (with-temp-dir
    (fn [dir]
      (let [result (core/run {:dir dir})]
        (is (= 2 (:exit-code result)))))))

;; --- Runner ---

(defn -main [& _args]
  (let [result (run-tests 'doc_quality_checker.core-test)]
    (when (or (pos? (:fail result)) (pos? (:error result)))
      (System/exit 1))))
