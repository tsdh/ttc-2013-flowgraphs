(defn find-nearest-definers [fi uv]
  (loop [preds (mapcat #(adjs % :cfPrev) (if (coll? fi) fi [fi])),
         r [], known #{}]
    (if (seq preds)
      (let [definers (filter #(member? uv (eget % :def)) preds)
            others   (remove #(member? uv (eget % :def)) preds)]
        (recur (remove #(member? % known) (mapcat #(adjs % :cfPrev) others))
               (into r definers) (into known preds)))
      r)))
