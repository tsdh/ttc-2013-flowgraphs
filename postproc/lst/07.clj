        'flowgraph.Method (let [stmts (econtents el)]
                            (eadd! el :cfNext (cf-peek (first stmts)))
                            (recur stmts exit nil nil nil))
