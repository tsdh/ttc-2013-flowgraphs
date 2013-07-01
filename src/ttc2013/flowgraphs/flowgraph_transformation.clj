(ns ttc2013.flowgraphs.flowgraph-transformation
  (:use funnyqt.emf)
  (:use funnyqt.protocols)
  (:use funnyqt.query)
  (:use funnyqt.query.emf)
  (:use funnyqt.declarative)
  (:use [funnyqt.utils :only [pr-identity errorf]])
  (:use ttc2013.flowgraphs.stmt2str))

;;* Task 1

(defn used-vars [s]
  (reachables s [p-seq [p-* <>--]
                 [p-restr 'references.IdentifierReference]
                 :target]))

(deftransformation java2flowgraph [[in :emf] [out :emf]]
  (^:top method2method [m]
      :from 'members.ClassMethod
      :to [fgm 'flowgraph.Method, ex 'flowgraph.Exit]
      (eset! fgm :txt (stmt2str m))
      (eset! ex :txt "Exit")
      (eset! fgm :stmts (map stmt2item (seq (eget m :statements))))
      (eset! fgm :exit ex)
      (eset! fgm :def (map param2param (eget m :parameters))))
  (stmt2item [stmt]
      :generalizes [local-var-stmt2simple-stmt condition2if block2block
                    return2return while-loop2loop break2break continue2continue
                    label2label stmt2simple-stmt])
  (var-creating-rule [v]
      :generalizes [param2param local-var2var])
  (param2param [p]
      :from 'parameters.Parameter
      :to [fgp 'flowgraph.Param]
      (eset! fgp :txt (stmt2str p)))
  (local-var2var [lv]
      :from 'variables.LocalVariable
      :to [fgv 'flowgraph.Var]
      (eset! fgv :txt (stmt2str lv)))
  (local-var-stmt2simple-stmt [lv]
      :from 'statements.LocalVariableStatement
      :to [fgss 'flowgraph.SimpleStmt]
      (let [v (local-var2var (adj lv :variable))]
        (eset! fgss :txt (stmt2str lv))
        (eadd! fgss :def v)
        (eset! fgss :use (map var-creating-rule
                              (used-vars (adj lv :variable :initialValue))))))
  (stmt2simple-stmt [s]
      :from 'statements.Statement
      :to [fgss 'flowgraph.SimpleStmt]
      (eset! fgss :txt (stmt2str s))
      (doseq [aex  (reachables s [p-seq [p-* <>--]
                                  [p-restr 'expressions.AssignmentExpression]])]
        (eadd! fgss :def (var-creating-rule (the (used-vars (adj aex :child)))))
        (eaddall! fgss :use (map var-creating-rule (used-vars (adj aex :value)))))
      (doseq [umex (reachables s [p-seq [p-* <>--]
                                  [p-restr 'expressions.UnaryModificationExpression]])]
        (let [var (var-creating-rule (the (used-vars (adj umex :child))))]
          (eadd! fgss :def var)
          (eadd! fgss :use var))))
  (label2label [l]
      :from 'statements.JumpLabel
      :to [fgl 'flowgraph.Label]
      (eset! fgl :txt (stmt2str l))
      (eset! fgl :stmt (stmt2item (eget l :statement))))
  (expression2expr [ex]
      :from 'expressions.Expression
      :to [fgex 'flowgraph.Expr]
      (eset! fgex :txt (stmt2str ex))
      (eset! fgex :use (map var-creating-rule (used-vars ex))))
  (condition2if [c]
      :from 'statements.Condition
      :to [fgif 'flowgraph.If]
      (eset! fgif :txt (stmt2str c))
      (eset! fgif :expr (expression2expr (eget c :condition)))
      (eset! fgif :then (stmt2item (eget c :statement)))
      (when-let [else (eget c :elseStatement)]
        (eset! fgif :else (stmt2item else))))
  (block2block [b]
      :from 'statements.Block
      :to [fgb 'flowgraph.Block]
      (eset! fgb :txt (stmt2str b))
      (eset! fgb :stmts (map stmt2item (eget b :statements))))
  (return2return [r]
      :from 'statements.Return
      :to [fgr 'flowgraph.Return]
      (eset! fgr :txt (stmt2str r))
      (eset! fgr :use (map var-creating-rule (used-vars r))))
  (break2break [b]
      :from 'statements.Break
      :to [fgb 'flowgraph.Break]
      (eset! fgb :txt (stmt2str b))
      (eset! fgb :label (label2label (eget b :target))))
  (continue2continue [c]
      :from 'statements.Continue
      :to [fgc 'flowgraph.Continue]
      (eset! fgc :txt (stmt2str c))
      (eset! fgc :label (label2label (eget c :target))))
  (while-loop2loop [wl]
      :from 'statements.WhileLoop
      :to   [fgl 'flowgraph.Loop]
      (eset! fgl :txt (stmt2str wl))
      (eset! fgl :expr (expression2expr (eget wl :condition)))
      (eset! fgl :body (stmt2item (eget wl :statement)))))

;;* Task 2

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

;;* Task 3

(defn find-nearest-definers [fi uv]
  (loop [preds (mapcat #(adjs % :cfPrev) (if (coll? fi) fi [fi]))
         r []
         known #{}]
    (if (seq preds)
      (let [definers (filter #(member? uv (eget % :def)) preds)
            others   (remove #(member? uv (eget % :def)) preds)]
        (recur (remove #(member? % known) (mapcat #(adjs % :cfPrev) others))
               (into r definers)
               (into known preds)))
      r)))

(defn synthesize-df-edges [model]
  (doseq [fi (eallobjects model 'flowgraph.FlowInstr)
          used-var (eget fi :use)
          nearest-definer (find-nearest-definers fi used-var)]
    (eadd! nearest-definer :dfNext fi))
  (doseq [v (vec (eallobjects model 'Var))]
    (edelete! v)))
