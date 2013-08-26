(defn run-flowgraph-transformations [file]
  (System/gc)
  (println "Running Transformation on" file)
  (print "Load Time: ")
  (let [jamopp-model (time (load-model file))
        outfile (str/replace (str/replace file "models/" "results/")
                             ".java.xmi" ".xmi")
        outvizfile (str/replace outfile ".xmi" ".pdf")
        fg-trg (new-model)]
    (println "Execution Times:")
    (print "  - JaMoPP to StructureGraph (with Vars): ")
    (time (java2flowgraph jamopp-model fg-trg))
    (print "  - Control Flow Analysis:                ")
    (time (synthesize-cf-edges fg-trg))
    (print "  - Data Flow Analysis:                   ")
    (time (synthesize-df-edges fg-trg))
    (save-model fg-trg outfile)
    (when (< (count (eallobjects fg-trg)) 80)
      (print-model fg-trg outvizfile))
    fg-trg))

(defmacro make-test [n file expected-cfs expected-dfs]
  `(deftest ~n
     (println "========================================================================")
     (let [fg-trg# (run-flowgraph-transformations ~file)
           exp-cfs# ~expected-cfs
           exp-dfs# ~expected-dfs
           cfs# (set (map (fn [[s# t#]] [(eget s# :txt) (eget t# :txt)])
                          (ecrosspairs fg-trg# :cfPrev :cfNext)))
           dfs# (set (map (fn [[s# t#]] [(eget s# :txt) (eget t# :txt)])
                          (ecrosspairs fg-trg# nil :dfNext)))]
       (cond
        (set? exp-cfs#) (let [cf-d1# (clojure.set/difference exp-cfs# cfs#)
                              cf-d2# (clojure.set/difference cfs# exp-cfs#)]
                          (is (empty? cf-d1#) "Missing cf-edges")
                          (is (empty? cf-d2#) "Too many cf-edges"))
        (number? exp-cfs#) (do
                             (println "Only checking number of cfNext links.")
                             (is (= exp-cfs# (count cfs#))))
        :else (println "No expected cfNext links given."))
       (cond
        (set? exp-dfs#) (let [df-d1# (clojure.set/difference exp-dfs# dfs#)
                              df-d2# (clojure.set/difference dfs# exp-dfs#)]
                          (is (empty? df-d1#) "Missing df-edges")
                          (is (empty? df-d2#) "Too many df-edges"))
        (number? exp-dfs#) (do
                             (println "Only checking number of dfNext links.")
                             (is (= exp-dfs# (count dfs#))))
        :else (println "No expected dfNext links given.")))))
