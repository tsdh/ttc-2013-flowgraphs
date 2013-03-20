(ns ttc2013.flowgraphs.flowgraph-transformation-test
  (:use funnyqt.emf)
  (:use funnyqt.query)
  (:use funnyqt.query.emf)
  (:use funnyqt.protocols)
  (:use clojure.test)
  (:require clojure.set)
  (:require [clojure.string :as str])
  (:use ttc2013.flowgraphs.flowgraph-transformation))

(def fg-mm    (load-metamodel "metamodel/FlowGraph.ecore"))
(def java-mm  (load-metamodel "metamodel/java.ecore"))

(do
  (println "Warming up the JVM... ")
  (let [m (load-model "models/Test6.java.xmi")
        nm (new-model)]
    (java2flowgraph m nm)
    (synthesize-cf-edges nm)
    (synthesize-df-edges nm))
  (println "Warmup done!")
  (println))

(defn run-transformations [file]
  (System/gc)
  (println "Running Transformation on" file)
  (print "Load Time: ")
  (let [jamopp-model (time (load-model file))
        outfile (str/replace (str/replace file "models/" "results/")
                             ".java.xmi" ".xmi")
        outvizfile (str/replace outfile ".xmi" ".pdf")
        fg-trg (new-model)]
    (time (do
            (println "Execution Times:")
            (print "  - JaMoPP to StructureGraph (with Vars): ")
            (time (java2flowgraph jamopp-model fg-trg))
            (print "  - Control Flow Analysis:                ")
            (time (synthesize-cf-edges fg-trg))
            (print "  - Data Flow Analysis:                   ")
            (time (synthesize-df-edges fg-trg))
            (print "=> Overall Time: ")))
    (save-model fg-trg outfile)
    (when (< (count (eallobjects fg-trg)) 80)
      (print-model fg-trg outvizfile))
    fg-trg))

;;* Task 4

(defmacro make-test [n file expected-cfs expected-dfs]
  `(deftest ~n
     (System/gc)
     (println "========================================================================")
     (let [fg-trg# (run-transformations ~file)
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

(make-test test-fg-transform-test0 "models/Test0.java.xmi"
           #{["testMethod()"   "int a = 1;"]
             ["int a = 1;"     "int b = 2;"]
             ["int b = 2;"     "int c = a + b;"]
             ["int c = a + b;" "a = c;"]
             ["a = c;"         "b = a;"]
             ["b = a;"         "c = a / b;"]
             ["c = a / b;"     "b = a - b;"]
             ["b = a - b;"     "return b * c;"]
             ["return b * c;"  "Exit"]}
           #{["int a = 1;"     "int c = a + b;"]
             ["int b = 2;"     "int c = a + b;"]
             ["int c = a + b;" "a = c;"]
             ["a = c;"         "b = a;"]
             ["a = c;"         "c = a / b;"]
             ["a = c;"         "b = a - b;"]
             ["b = a;"         "c = a / b;"]
             ["b = a;"         "b = a - b;"]
             ["c = a / b;"     "return b * c;"]
             ["b = a - b;"     "return b * c;"]})

(make-test test-fg-transform-test1 "models/Test1.java.xmi"
           #{["testMethod()"   "int i = a * 2;"]
             ["int i = a * 2;" "i = i + 19;"]
             ["i = i + 19;"    "i > a"]
             ["i > a"          "Exit"]
             ["i > a"          "a < 1"]
             ["a < 1"          "return;"]
             ["a < 1"          "a == 1"]
             ["a == 1"         "break;"]
             ["a == 1"         "i--;"]
             ["i--;"           "i > a"]
             ["break;"         "Exit"]
             ["return;"        "Exit"]}
           #{["testMethod()"   "int i = a * 2;"]
             ["testMethod()"   "i > a"]
             ["testMethod()"   "a < 1"]
             ["testMethod()"   "a == 1"]
             ["int i = a * 2;" "i = i + 19;"]
             ["i = i + 19;"    "i > a"]
             ["i = i + 19;"    "i--;"]
             ["i--;"           "i > a"]
             ["i--;"           "i--;"]})

(make-test test-fg-transform-test2 "models/Test2.java.xmi"
           #{["testMethod()"   "int i = a * 2;"]
             ["int i = a * 2;" "i > a"]
             ["i > a"          "Exit"]
             ["i > a"          "a < 1"]
             ["a < 1"          "return;"]
             ["a < 1"          "a == 0"]
             ["a == 0"         "continue;"]
             ["a == 0"         "i++;"]
             ["i++;"           "i > a"]
             ["continue;"      "i > a"]
             ["return;"        "Exit"]}
           #{["testMethod()"   "int i = a * 2;"]
             ["testMethod()"   "i > a"]
             ["testMethod()"   "a < 1"]
             ["testMethod()"   "a == 0"]
             ["int i = a * 2;" "i > a"]
             ["int i = a * 2;" "i++;"]
             ["i++;"           "i > a"]
             ["i++;"           "i++;"]})

(make-test test-fg-transform-test3 "models/Test3.java.xmi"
           #{["testMethod()"   "int i = a * 2;"]
             ["int i = a * 2;" "i > a"]
             ["i > a"          "Exit"]
             ["i > a"          "a < 1"]
             ["a < 1"          "return;"]
             ["a < 1"          "a == 1"]
             ["a == 1"         "break;"]
             ["a == 1"         "a--;"]
             ["a--;"           "i--;"]
             ["i--;"           "i > a"]
             ["break;"         "Exit"]
             ["return;"        "Exit"]}
           #{["testMethod()"   "int i = a * 2;"]
             ["testMethod()"   "i > a"]
             ["testMethod()"   "a < 1"]
             ["testMethod()"   "a == 1"]
             ["testMethod()"   "a--;"]
             ["int i = a * 2;" "i > a"]
             ["int i = a * 2;" "i--;"]
             ["a--;"           "i > a"]
             ["a--;"           "a < 1"]
             ["a--;"           "a == 1"]
             ["a--;"           "a--;"]
             ["i--;"           "i--;"]
             ["i--;"           "i > a"]})

(make-test test-fg-transform-test4 "models/Test4.java.xmi"
           #{["testMethod()" "int i = 100;"]
             ["int i = 100;" "i > 0"]
             ["i > 0"        "Exit"]
             ["i > 0"        "i > 50"]
             ["i > 50"       "i--;"]
             ["i > 50"       "i = i - 10;"]
             ["i = i - 10;"  "i == 50"]
             ["i == 50"      "break;"]
             ["i == 50"      "i > 50"]
             ["break;"       "i--;"]
             ["i--;"         "i > 0"]}
           #{["int i = 100;" "i > 0"]
             ["int i = 100;" "i > 50"]
             ["int i = 100;" "i = i - 10;"]
             ["int i = 100;" "i--;"]
             ["i = i - 10;"  "i == 50"]
             ["i = i - 10;"  "i > 50"]
             ["i = i - 10;"  "i = i - 10;"]
             ["i = i - 10;"  "i--;"]
             ["i--;"         "i > 0"]
             ["i--;"         "i > 50"]
             ["i--;"         "i = i - 10;"]
             ["i--;"         "i--;"]})

(make-test test-fg-transform-test5 "models/Test5.java.xmi"
           #{["testMethod()" "int i = 100;"]
             ["int i = 100;" "i > 0"]
             ["i > 0"        "i > 50"]
             ["i > 0"        "return i;"]
             ["i > 50"       "i--;"]
             ["i > 50"       "i = i - 10;"]
             ["i = i - 10;"  "i == 50"]
             ["i == 50"      "break outer;"]
             ["i == 50"      "i > 50"]
             ["break outer;" "return i;"]
             ["i--;"         "i > 0"]
             ["return i;"    "Exit"]}
           #{["int i = 100;" "i > 0"]
             ["int i = 100;" "i > 50"]
             ["int i = 100;" "i = i - 10;"]
             ["int i = 100;" "i--;"]
             ["int i = 100;" "return i;"]
             ["i = i - 10;"  "i == 50"]
             ["i = i - 10;"  "i > 50"]
             ["i = i - 10;"  "i--;"]
             ["i = i - 10;"  "i = i - 10;"]
             ["i = i - 10;"  "return i;"]
             ["i--;"         "i > 0"]
             ["i--;"         "i > 50"]
             ["i--;"         "i = i - 10;"]
             ["i--;"         "i--;"]
             ["i--;"         "return i;"]})

(make-test test-fg-transform-test6 "models/Test6.java.xmi"
           #{["testMethod()"    "int i = a;"]
             ["int i = a;"      "i > 0"]
             ["i > 0"           "i > 50"]
             ["i > 0"           "return i;"]
             ["i > 50"          "i < 60"]
             ["i > 50"          "i--;"]
             ["return i;"       "Exit"]
             ["i < 60"          "i = i - 5;"]
             ["i < 60"          "i == 46"]
             ["i--;"            "i > 0"]
             ["i = i - 5;"      "continue inner;"]
             ["i == 46"         "break outer;"]
             ["i == 46"         "i = i - 10;"]
             ["continue inner;" "i > 50"]
             ["break outer;"    "return i;"]
             ["i = i - 10;"     "i == 50"]
             ["i == 50"         "continue outer;"]
             ["i == 50"         "i > 50"]
             ["continue outer;" "i > 0"]}
           #{["testMethod()"    "int i = a;"]
             ["int i = a;"      "i > 0"]
             ["int i = a;"      "i > 50"]
             ["int i = a;"      "i < 60"]
             ["int i = a;"      "i = i - 5;"]
             ["int i = a;"      "i == 46"]
             ["int i = a;"      "i = i - 10;"]
             ["int i = a;"      "i--;"]
             ["int i = a;"      "return i;"]
             ["i = i - 5;"      "i > 50"]
             ["i = i - 5;"      "i < 60"]
             ["i = i - 5;"      "i = i - 5;"]
             ["i = i - 5;"      "i == 46"]
             ["i = i - 5;"      "i = i - 10;"]
             ["i = i - 5;"      "i--;"]
             ["i = i - 5;"      "return i;"]
             ["i = i - 10;"     "i == 50"]
             ["i = i - 10;"     "i > 0"]
             ["i = i - 10;"     "i > 50"]
             ["i = i - 10;"     "i < 60"]
             ["i = i - 10;"     "i = i - 5;"]
             ["i = i - 10;"     "i == 46"]
             ["i = i - 10;"     "i = i - 10;"]
             ["i = i - 10;"     "i--;"]
             ["i = i - 10;"     "return i;"]
             ["i--;"            "i > 0"]
             ["i--;"            "i > 50"]
             ["i--;"            "i < 60"]
             ["i--;"            "i = i - 5;"]
             ["i--;"            "i == 46"]
             ["i--;"            "i = i - 10;"]
             ["i--;"            "i--;"]
             ["i--;"            "return i;"]})

(make-test test-fg-transform-test7 "models/Test7.java.xmi"
           512 961)

(make-test test-fg-transform-test8 "models/Test8.java.xmi"
           1702 3202)

(make-test test-fg-transform-test9 "models/Test9.java.xmi"
           14452 27202)

(make-test test-fg-transform-test10 "models/Test10.java.xmi"
           #{["testMethod()" "a < 0"]
             ["a < 0"        "a < -10"]
             ["a < -10"      "a < -100"]
             ["a < -100"     "return -100;"]
             ["return -100;" "Exit"]
             ["a < -100"     "return -50;"]
             ["return -50;"  "Exit"]
             ["a < -10"      "return -5;"]
             ["return -5;"   "Exit"]
             ["a < 0"        "a++;"]
             ["a++;"         "a > 10"]
             ["a > 10"       "return 10;"]
             ["return 10;"   "Exit"]
             ["a > 10"       "return 5;"]
             ["return 5;"    "Exit"]}
           #{["testMethod()" "a < 0"]
             ["testMethod()" "a < -10"]
             ["testMethod()" "a < -100"]
             ["testMethod()" "a++;"]
             ["a++;"         "a > 10"]})

(make-test test-fg-transform-test11 "models/Test11.java.xmi"
           #{["testMethod()"  "a < 0"]
             ["a < 0"         "a < -10"]
             ["a < -10"       "a < -100"]
             ["a < -100"      "a < -1000"]
             ["a < -1000"     "return -1001;"]
             ["a < 0"         "return 1;"]
             ["a < -10"       "return 1;"]
             ["a < -100"      "return 1;"]
             ["a < -1000"     "return 1;"]
             ["return -1001;" "Exit"]
             ["return 1;"     "Exit"]}
           #{["testMethod()"  "a < 0"]
             ["testMethod()"  "a < -10"]
             ["testMethod()"  "a < -100"]
             ["testMethod()"  "a < -1000"]})
