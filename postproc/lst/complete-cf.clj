(defn cf-peek [el]
  (if (has-type? el 'flowgraph.FlowInstr)
    el
    (recur (first (econtents el)))))

(defn cf-synth [v exit loop-expr loop-succ label-succ-map]
  (when (seq v)
    (let [[el & [n & _ :as tail]] v]
      (type-case el
        'flowgraph.Method
                   (let [stmts (econtents el)]
                     (eadd! el :cfNext (cf-peek (first stmts)))
                     (recur stmts exit nil nil nil))
        'flowgraph.SimpleStmt
                   (do (when n (eadd! el :cfNext (cf-peek n)))
                       (recur tail exit loop-expr loop-succ label-succ-map))
        'flowgraph.Block
                   (recur (concat (econtents el) tail)
                          exit loop-expr loop-succ label-succ-map)
        'flowgraph.Expr
                   (do (when n (eadd! el :cfNext (cf-peek n)))
                       (recur tail exit loop-expr loop-succ label-succ-map))
        'flowgraph.Label
                   (recur (cons (eget el :stmt) tail) exit loop-expr loop-succ
                          (assoc label-succ-map el n))
        'flowgraph.Return
                   (do (eadd! el :cfNext exit)
                       (recur tail exit loop-expr loop-succ label-succ-map))
        'flowgraph.Break
                   (do (if-let [l (eget el :label)]
                         (eadd! el :cfNext (cf-peek (label-succ-map l)))
                         (eadd! el :cfNext (cf-peek loop-succ)))
                       (recur tail exit loop-expr loop-succ label-succ-map))
        'flowgraph.Continue
                   (do (if-let [l (eget el :label)]
                         (eadd! el :cfNext (cf-peek l))
                         (eadd! el :cfNext loop-expr))
                       (recur tail exit loop-expr loop-succ label-succ-map))
        'flowgraph.Loop
                   (let [[expr body] (econtents el)]
                     (recur (cons expr (cons body (cons expr tail)))
                            exit expr n label-succ-map))
        'flowgraph.If
                   (let [[expr then else] (econtents el)]
                     (cf-synth [expr then (cf-peek n)]
                               exit loop-expr loop-succ label-succ-map)
                     (if else
                       (recur (cons expr (cons else tail))
                              exit loop-expr loop-succ label-succ-map)
                       (recur (cons expr tail)
                              exit loop-expr loop-succ label-succ-map)))
        'flowgraph.Exit (assert (nil? n))))))

(defn synthesize-cf-edges [model]
  (doseq [m (eallobjects model 'flowgraph.Method)
          :let [exit (the (eallobjects model 'flowgraph.Exit))]]
    (cf-synth [m] exit nil nil nil)))
