(ns doc_quality_checker.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [babashka.fs :as fs]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [doc_quality_checker.core :as core]
            [doc_quality_checker.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
;; find-doc-files, check-broken-links and check-file read the filesystem;
;; run reads it too, and -main prints and exits.
;; (stale-date? and check-stale-dates read the clock but have no side effects.)
(def ^:private side-effecting
  #{`core/find-doc-files `core/check-broken-links `core/check-file `core/run `core/-main})

(defn- checkable []
  (remove side-effecting (stest/enumerate-namespace 'doc_quality_checker.core)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (seq results) "expected at least one fdef'd fn to check")
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/path-like ::specs/content ::specs/ymd ::specs/issue
             ::specs/check-result ::specs/file-result ::specs/run-opts]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(def ^:private good-doc
  "# Good Document\n\nThis is a well-written document with enough description to pass.\n\n## Details\n\nSome content here.\n\n```bash\necho hello\n```\n")

(deftest real-values-conform
  (testing "configuration"
    (is (s/valid? (s/coll-of ::specs/section :kind set?) core/required-readme-sections)))
  (testing "every check on a README fixture"
    (is (s/valid? (s/coll-of ::specs/check-result)
                  (map #(% "README.md" good-doc) core/all-checks))))
  (testing "check-file and run on a real directory"
    (let [dir (fs/create-temp-dir {:prefix "dqc-spec-"})
          path (str (fs/path dir "good.md"))]
      (try
        (spit path good-doc)
        (is (s/valid? ::specs/file-result (core/check-file path)))
        (is (s/valid? ::specs/run-result (core/run {:dir (str dir) :format "json" :threshold 50})))
        (finally (fs/delete-tree dir))))))
