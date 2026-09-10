(ns doc_quality_checker.specs
  "Data specs for doc-quality-checker (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in doc_quality_checker.core."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]))

;; --- Inputs: a documentation file path and its content ---

(def ^:private gen-path-string
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen/elements ["docs" "src" "a b" "notes"]) 0 3)
                       (gen/elements ["README" "readme" "guide" "spec" "CHANGELOG" "x"])
                       (gen/elements [nil "md" "org" "MD" "Org" "txt" "clj"]))))

(s/def ::path-string (s/with-gen (s/and string? seq) (constantly gen-path-string)))
(s/def ::path-like
  (s/with-gen (s/or :string ::path-string
                    :path #(instance? java.nio.file.Path %))
    (constantly gen-path-string)))

(def ^:private doc-fragments
  ["# Title" "## Install" "## Usage" "### API" "#### Deep" "* Org title" "** Install"
   "*** Skipped" "#+TITLE: doc" "This is a sufficiently long description for the document."
   "Short." "```bash" "echo hi" "```" "#+begin_src clojure" "(+ 1 2)" "#+end_src"
   "[exists](test.md)" "[broken](nope.md)" "[web](https://example.com)" "[anchor](#top)"
   "[[spec.org]]" "[[file:notes.org][notes]]" "[[https://example.com][site]]"
   "Written on 2020-01-01" "Updated 2026-03-01" "bad date 2024-13-99"
   "TODO: finish this" "FIXME: broken" "hack around" ""])

;; Markdown or org text, as read by slurp.
(s/def ::content
  (s/with-gen string?
    #(gen/fmap (fn [parts] (str/join "\n" parts))
               (gen/vector (gen/one-of [(gen/elements doc-fragments) (gen/string-alphanumeric)])
                           0 15))))

(s/def ::link string?)
(s/def ::links (s/coll-of ::link))

;; A YYYY-MM-DD match, as parsed by extract-dates (not necessarily a valid date).
(s/def ::text string?)
(s/def ::year (s/int-in 0 10000))
(s/def ::month (s/int-in 0 100))
(s/def ::day (s/int-in 0 100))
(s/def ::ymd (s/keys :req-un [::year ::month ::day]))
(s/def ::date-match (s/keys :req-un [::text ::year ::month ::day]))

(s/def ::level pos-int?)
(s/def ::levels (s/coll-of ::level :kind vector?))

;; --- Check results ---

(s/def ::check #{:broken-links :stale-dates :missing-sections :short-description
                 :no-code-examples :inconsistent-headings :todo-items})
(s/def ::type #{:broken-link :stale-date :missing-section :short-description
                :no-code-examples :inconsistent-headings :todo-item})
(s/def ::date string?)
(s/def ::section #{"install" "usage" "api"})
(s/def ::length nat-int?)
(s/def ::issue (s/keys :req-un [::type] :opt-un [::link ::date ::section ::length ::text ::levels]))
(s/def ::issues (s/coll-of ::issue :kind vector? :gen-max 5))
(s/def ::penalty (s/int-in 0 21))
(s/def ::check-result (s/keys :req-un [::check ::issues ::penalty]))

(defn consistent-check
  "s/fdef :fn for the check-* fns: the result names the right check, and it
  carries a penalty exactly when it reports issues."
  [check]
  (fn [{ret :ret}]
    (and (= check (:check ret))
         (= (pos? (:penalty ret)) (boolean (seq (:issues ret)))))))

;; --- File results and output ---

(s/def ::file string?)
(s/def ::score (s/int-in 0 101))
(s/def ::checks (s/coll-of ::check-result :kind vector? :gen-max 7))
(s/def ::file-result (s/keys :req-un [::file ::score ::checks]))
(s/def ::results (s/coll-of ::file-result :kind vector? :gen-max 5))

(s/def ::threshold int?)
;; --format is passed through; anything but json/edn renders text.
(s/def ::format (s/with-gen string? #(gen/elements ["text" "json" "edn" "xml"])))
(s/def ::dir ::path-string)
(s/def ::help boolean?)
(s/def ::run-opts (s/keys :opt-un [::dir ::format ::threshold ::help]))

(s/def ::output string?)
(s/def ::exit-code #{0 1 2})
(s/def ::run-result (s/keys :req-un [::output ::exit-code ::results]))

(defn output-round-trips?
  "s/fdef :fn for format-output: json/edn output reads back with the same
  threshold; text output ends with the summary for every result."
  [{{:keys [results fmt threshold]} :args ret :ret}]
  (case fmt
    "json" (= threshold (get (json/parse-string ret) "threshold"))
    "edn" (= threshold (:threshold (edn/read-string ret)))
    (str/includes? ret (format "Summary: %d files checked" (count results)))))
