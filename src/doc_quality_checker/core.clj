(ns doc_quality_checker.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [cheshire.core :as json])
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

(defn find-doc-files [dir]
  (->> (fs/glob dir "**.{md,org}")
       (map str)
       sort
       vec))

(defn org-file? [file-path]
  (= "org" (str/lower-case (or (fs/extension file-path) ""))))

(defn readme-file? [file-path]
  (re-find #"(?i)readme" (str (fs/file-name file-path))))

;; --- Check: broken-links ---

(defn extract-md-links
  "Extract relative file references from markdown [text](path) links."
  [content]
  (->> (re-seq #"\[([^\]]*)\]\(([^)]+)\)" content)
       (map #(nth % 2))
       (remove #(or (str/starts-with? % "http")
                    (str/starts-with? % "mailto:")
                    (str/starts-with? % "#")))))

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

(defn check-broken-links [file-path content]
  (let [dir   (or (fs/parent file-path) ".")
        links (if (org-file? file-path)
                (extract-org-links content)
                (extract-md-links content))
        broken (filterv #(not (fs/exists? (fs/path dir %))) links)]
    {:check   :broken-links
     :issues  (mapv (fn [link] {:type :broken-link :link link}) broken)
     :penalty (min 15 (* 5 (count broken)))}))

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

(defn stale-date?
  "Returns true if date is more than 1 year before today."
  [{:keys [year month day]}]
  (try
    (let [date   (LocalDate/of (int year) (int month) (int day))
          cutoff (.minusYears (LocalDate/now) 1)]
      (.isBefore date cutoff))
    (catch Exception _ false)))

(defn check-stale-dates [_file-path content]
  (let [dates (extract-dates content)
        stale (filterv stale-date? dates)]
    {:check   :stale-dates
     :issues  (mapv (fn [d] {:type :stale-date :date (:text d)}) stale)
     :penalty (min 10 (* 3 (count stale)))}))

;; --- Check: missing-sections ---

(defn extract-headings [file-path content]
  (let [pattern (if (org-file? file-path)
                  #"(?m)^\*+\s+(.+)"
                  #"(?m)^#{1,6}\s+(.+)")]
    (->> (re-seq pattern content)
         (mapv (fn [[_ title]] (str/lower-case (str/trim title)))))))

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

(defn check-short-description [file-path content]
  (let [desc   (extract-description file-path content)
        short? (< (count desc) 20)]
    {:check   :short-description
     :issues  (if short?
                [{:type :short-description :length (count desc) :text desc}]
                [])
     :penalty (if short? 15 0)}))

;; --- Check: no-code-examples ---

(defn check-no-code-examples [file-path content]
  (let [has-code? (if (org-file? file-path)
                    (re-find #"(?i)#\+begin_src" content)
                    (re-find #"```" content))]
    {:check   :no-code-examples
     :issues  (if has-code? [] [{:type :no-code-examples}])
     :penalty (if has-code? 0 15)}))

;; --- Check: inconsistent-headings ---

(defn extract-heading-levels [file-path content]
  (let [pattern (if (org-file? file-path)
                  #"(?m)^(\*+)\s+"
                  #"(?m)^(#{1,6})\s+")]
    (->> (re-seq pattern content)
         (mapv (fn [[_ marker]] (count marker))))))

(defn has-skipped-levels?
  "Returns true if heading levels skip (e.g., h1 -> h3 with no h2)."
  [levels]
  (when (seq levels)
    (let [sorted (sort (distinct levels))]
      (boolean
       (some (fn [[a b]] (> (- b a) 1))
             (partition 2 1 sorted))))))

(defn check-inconsistent-headings [file-path content]
  (let [levels        (extract-heading-levels file-path content)
        inconsistent? (has-skipped-levels? levels)]
    {:check   :inconsistent-headings
     :issues  (if inconsistent?
                [{:type :inconsistent-headings :levels (vec (sort (distinct levels)))}]
                [])
     :penalty (if inconsistent? 10 0)}))

;; --- Check: TODO items ---

(defn check-todo-items [_file-path content]
  (let [matches (re-seq #"(?im)\b(TODO|FIXME|HACK|XXX)\b.*" content)]
    {:check   :todo-items
     :issues  (mapv (fn [[match _]]
                      {:type :todo-item
                       :text (str/trim (subs match 0 (min 80 (count match))))})
                    matches)
     :penalty (min 15 (* 3 (count matches)))}))

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

(defn format-output [results fmt threshold]
  (case fmt
    "json" (json/generate-string {:results results :threshold threshold} {:pretty true})
    "edn"  (pr-str {:results results :threshold threshold})
    (format-text results threshold)))

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

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
