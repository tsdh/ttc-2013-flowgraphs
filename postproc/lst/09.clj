        'flowgraph.Break (do (if-let [l (eget el :label)]
                               (eadd! el :cfNext (cf-peek (label-succ-map l)))
                               (eadd! el :cfNext (cf-peek loop-succ)))
                           (recur tail exit loop-expr loop-succ label-succ-map))
