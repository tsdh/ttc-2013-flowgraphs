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
