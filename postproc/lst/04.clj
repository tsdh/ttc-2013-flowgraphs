  (^:top method2method [m]
         :from 'members.ClassMethod
         :to [fgm 'flowgraph.Method, fgex 'flowgraph.Exit]
         (eset! fgm :txt (stmt2str m)) ;; Invoke the model-to-text transformation
         (eset! fgex :txt "Exit")
         (eset! fgm :exit fgex)
         (eset! fgm :stmts (map stmt2item (eget m :statements)))  ;; transform the statements
         (eset! fgm :def (map param2param (eget m :parameters)))) ;; transform the parameters
