(ns doc_quality_checker.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [cheshire.core :as json]
            [doc_quality_checker.specs :as specs])
  (:import [java.time LocalDate]))

;; --- Configuration ---

(def cli-spec
  {:dir       {:desc "Directory to scan" :default "." :alias :d}
   :format    {:desc "Output format: text, json, edn" :default "text" :alias :f}
   :threshold {:desc "Minimum passing score (0-100)" :default 70 :alias :t :coerce :long}
   :help      {:desc "Show help" :alias :h :coerce :boolean}})

(def required-readme-sections
  #{"install" "usage" "api"})

;; --- File detection ---

(defn doc-file? [path]
  (let [ext (str/lower-case (or (fs/extension path) ""))]
    (contains? #{"md" "org"} ext)))

(declare org-file?)

(s/fdef doc-file?
  :args (s/cat :path ::specs/path-like)
  :ret boolean?
  :fn (fn [{{[_ path] :path} :args ret :ret}]
        (or ret (not (org-file? path)))))

(defn find-doc-files [dir]
  (->> (fs/glob dir "**.{md,org}")
       (map str)
       sort
       vec))

(s/fdef find-doc-files
  :args (s/cat :dir ::specs/path-like)
  :ret (s/coll-of string? :kind vector?))

(defn org-file? [file-path]
  (= "org" (str/lower-case (or (fs/extension file-path) ""))))

(s/fdef org-file?
  :args (s/cat :file-path ::specs/path-like)
  :ret boolean?)

(defn readme-file? [file-path]
  (re-find #"(?i)readme" (str (fs/file-name file-path))))

(s/fdef readme-file?
  :args (s/cat :file-path ::specs/path-like)
  :ret (s/nilable string?))

;; --- Check: broken-links ---

(defn extract-md-links
  "Extract relative file references from markdown [text](path) links."
  [content]
  (->> (re-seq #"\[([^\]]*)\]\(([^)]+)\)" content)
       (map #(nth % 2))
       (remove #(or (str/starts-with? % "http")
                    (str/starts-with? % "mailto:")
                    (str/starts-with? % "#")))))

(defn- relative-link? [link]
  (not (or (str/starts-with? link "http")
           (str/starts-with? link "mailto:")
           (str/starts-with? link "#"))))

(s/fdef extract-md-links
  :args (s/cat :content ::specs/content)
  :ret ::specs/links
  :fn (fn [{ret :ret}] (every? relative-link? ret)))

(defn extract-org-links
  "Extract relative file references from org [[path]] or [[path][desc]] links."
  [content]
  (->> (re-seq #"\[\[([^\]]+?)(?:\]\[[^\]]*?)?\]\]" content)
       (map second)
       (map #(if (str/starts-with? % "file:")
               (subs % 5)
               %))
       (remove #(or (str/starts-with? % "http")
                    (str/starts-with? % "mailto:")
                    (str/starts-with? % "#")))))

(s/fdef extract-org-links
  :args (s/cat :content ::specs/content)
  :ret ::specs/links
  :fn (fn [{ret :ret}] (every? relative-link? ret)))

(defn check-broken-links [file-path content]
  (let [dir   (or (fs/parent file-path) ".")
        links (if (org-file? file-path)
                (extract-org-links content)
                (extract-md-links content))
        broken (filterv #(not (fs/exists? (fs/path dir %))) links)]
    {:check   :broken-links
     :issues  (mapv (fn [link] {:type :broken-link :link link}) broken)
     :penalty (min 15 (* 5 (count broken)))}))

(s/fdef check-broken-links
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :broken-links))

;; --- Check: stale-dates ---

(defn extract-dates
  "Extract dates in YYYY-MM-DD format."
  [content]
  (->> (re-seq #"\b(\d{4})-(\d{2})-(\d{2})\b" content)
       (map (fn [[match year month day]]
              {:text  match
               :year  (parse-long year)
               :month (parse-long month)
               :day   (parse-long day)}))))

(s/fdef extract-dates
  :args (s/cat :content ::specs/content)
  :ret (s/coll-of ::specs/date-match)
  :fn (fn [{ret :ret}]
        (every? (fn [{:keys [text year month day]}]
                  (= text (format "%04d-%02d-%02d" year month day)))
                ret)))

(defn stale-date?
  "Returns true if date is more than 1 year before today."
  [{:keys [year month day]}]
  (try
    (let [date   (LocalDate/of (int year) (int month) (int day))
          cutoff (.minusYears (LocalDate/now) 1)]
      (.isBefore date cutoff))
    (catch Exception _ false)))

(s/fdef stale-date?
  :args (s/cat :date ::specs/ymd)
  :ret boolean?)

(defn check-stale-dates [_file-path content]
  (let [dates (extract-dates content)
        stale (filterv stale-date? dates)]
    {:check   :stale-dates
     :issues  (mapv (fn [d] {:type :stale-date :date (:text d)}) stale)
     :penalty (min 10 (* 3 (count stale)))}))

(s/fdef check-stale-dates
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :stale-dates))

;; --- Check: missing-sections ---

(defn extract-headings [file-path content]
  (let [pattern (if (org-file? file-path)
                  #"(?m)^\*+\s+(.+)"
                  #"(?m)^#{1,6}\s+(.+)")]
    (->> (re-seq pattern content)
         (mapv (fn [[_ title]] (str/lower-case (str/trim title)))))))

(s/fdef extract-headings
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret (s/coll-of string? :kind vector?)
  :fn (fn [{ret :ret}] (every? #(= % (str/lower-case (str/trim %))) ret)))

(defn check-missing-sections [file-path content]
  (if (readme-file? file-path)
    (let [headings     (extract-headings file-path content)
          heading-text (str/join " " headings)
          missing      (filterv #(not (str/includes? heading-text %))
                                required-readme-sections)]
      {:check   :missing-sections
       :issues  (mapv (fn [s] {:type :missing-section :section s}) missing)
       :penalty (min 20 (* 7 (count missing)))})
    {:check :missing-sections :issues [] :penalty 0}))

(s/fdef check-missing-sections
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :missing-sections))

;; --- Check: short-description ---

(defn extract-description
  "Get first non-heading, non-empty paragraph after the title."
  [file-path content]
  (let [lines      (str/split-lines content)
        skip?      (if (org-file? file-path)
                     #(or (str/blank? %)
                          (str/starts-with? % "*")
                          (str/starts-with? % "#+"))
                     #(or (str/blank? %)
                          (str/starts-with? % "#")))
        after-head (drop-while skip? lines)
        desc-lines (take-while #(not (str/blank? %)) after-head)]
    (str/trim (str/join " " desc-lines))))

(s/fdef extract-description
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret string?
  :fn (fn [{ret :ret}] (and (= ret (str/trim ret)) (not (str/includes? ret "\n")))))

(defn check-short-description [file-path content]
  (let [desc   (extract-description file-path content)
        short? (< (count desc) 20)]
    {:check   :short-description
     :issues  (if short?
                [{:type :short-description :length (count desc) :text desc}]
                [])
     :penalty (if short? 15 0)}))

(s/fdef check-short-description
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :short-description))

;; --- Check: no-code-examples ---

(defn check-no-code-examples [file-path content]
  (let [has-code? (if (org-file? file-path)
                    (re-find #"(?i)#\+begin_src" content)
                    (re-find #"```" content))]
    {:check   :no-code-examples
     :issues  (if has-code? [] [{:type :no-code-examples}])
     :penalty (if has-code? 0 15)}))

(s/fdef check-no-code-examples
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :no-code-examples))

;; --- Check: inconsistent-headings ---

(defn extract-heading-levels [file-path content]
  (let [pattern (if (org-file? file-path)
                  #"(?m)^(\*+)\s+"
                  #"(?m)^(#{1,6})\s+")]
    (->> (re-seq pattern content)
         (mapv (fn [[_ marker]] (count marker))))))

(s/fdef extract-heading-levels
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/levels
  :fn (fn [{{:keys [file-path]} :args ret :ret}]
        (or (org-file? (second file-path)) (every? #(<= % 6) ret))))

(defn has-skipped-levels?
  "Returns true if heading levels skip (e.g., h1 -> h3 with no h2)."
  [levels]
  (when (seq levels)
    (let [sorted (sort (distinct levels))]
      (boolean
       (some (fn [[a b]] (> (- b a) 1))
             (partition 2 1 sorted))))))

(s/fdef has-skipped-levels?
  :args (s/cat :levels (s/coll-of ::specs/level))
  :ret (s/nilable boolean?)
  :fn (fn [{{:keys [levels]} :args ret :ret}]
        (= (nil? ret) (empty? levels))))

(defn check-inconsistent-headings [file-path content]
  (let [levels        (extract-heading-levels file-path content)
        inconsistent? (has-skipped-levels? levels)]
    {:check   :inconsistent-headings
     :issues  (if inconsistent?
                [{:type :inconsistent-headings :levels (vec (sort (distinct levels)))}]
                [])
     :penalty (if inconsistent? 10 0)}))

(s/fdef check-inconsistent-headings
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :inconsistent-headings))

;; --- Check: TODO items ---

(defn check-todo-items [_file-path content]
  (let [matches (re-seq #"(?im)\b(TODO|FIXME|HACK|XXX)\b.*" content)]
    {:check   :todo-items
     :issues  (mapv (fn [[match _]]
                      {:type :todo-item
                       :text (str/trim (subs match 0 (min 80 (count match))))})
                    matches)
     :penalty (min 15 (* 3 (count matches)))}))

(s/fdef check-todo-items
  :args (s/cat :file-path ::specs/path-like :content ::specs/content)
  :ret ::specs/check-result
  :fn (specs/consistent-check :todo-items))

;; --- Scoring ---

(def all-checks
  [check-broken-links
   check-stale-dates
   check-missing-sections
   check-short-description
   check-no-code-examples
   check-inconsistent-headings
   check-todo-items])

(defn check-file [file-path]
  (let [content       (slurp file-path)
        checks        (mapv #(% file-path content) all-checks)
        total-penalty (reduce + (map :penalty checks))
        score         (max 0 (- 100 total-penalty))]
    {:file   (str file-path)
     :score  score
     :checks checks}))

(s/fdef check-file
  :args (s/cat :file-path ::specs/path-like)
  :ret ::specs/file-result)

;; --- Output formatting ---

(defn format-text [results threshold]
  (let [sb (StringBuilder.)]
    (doseq [{:keys [file score checks]} results]
      (.append sb (format "\n=== %s (score: %d/100) %s ===\n"
                          file score (if (>= score threshold) "PASS" "FAIL")))
      (doseq [{:keys [check issues penalty]} checks
              :when (pos? penalty)]
        (.append sb (format "  [-%d] %s\n" penalty (name check)))
        (doseq [issue issues]
          (.append sb (format "        %s\n" (pr-str issue))))))
    (let [total   (count results)
          passing (count (filter #(>= (:score %) threshold) results))
          failing (- total passing)]
      (.append sb (format "\nSummary: %d files checked, %d passed, %d failed (threshold: %d)\n"
                          total passing failing threshold)))
    (str sb)))

(s/fdef format-text
  :args (s/cat :results ::specs/results :threshold ::specs/threshold)
  :ret string?
  :fn (fn [{{:keys [results]} :args ret :ret}]
        (str/includes? ret (format "Summary: %d files checked" (count results)))))

(defn format-output [results fmt threshold]
  (case fmt
    "json" (json/generate-string {:results results :threshold threshold} {:pretty true})
    "edn"  (pr-str {:results results :threshold threshold})
    (format-text results threshold)))

(s/fdef format-output
  :args (s/cat :results ::specs/results :fmt ::specs/format :threshold ::specs/threshold)
  :ret string?
  :fn specs/output-round-trips?)

;; --- Main ---

(defn run
  "Run the checker, returning {:results [...] :exit-code n}."
  [opts]
  (let [dir       (:dir opts ".")
        fmt       (:format opts "text")
        threshold (:threshold opts 70)
        files     (find-doc-files dir)]
    (if (empty? files)
      {:output    (str "No documentation files found in " dir)
       :exit-code 2
       :results   []}
      (let [results   (mapv check-file files)
            output    (format-output results fmt threshold)
            failing?  (some #(< (:score %) threshold) results)]
        {:output    output
         :exit-code (if failing? 1 0)
         :results   results}))))

(s/fdef run
  :args (s/cat :opts ::specs/run-opts)
  :ret ::specs/run-result)

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})]
    (when (:help opts)
      (println "doc-quality-checker — Lint documentation for completeness and accuracy")
      (println)
      (println (cli/format-opts {:spec cli-spec}))
      (System/exit 0))
    (let [{:keys [output exit-code]} (run opts)]
      (println output)
      (System/exit exit-code))))

(s/fdef -main
  :args (s/* string?))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
