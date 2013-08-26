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
