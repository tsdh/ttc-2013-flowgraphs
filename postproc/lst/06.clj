(defn cf-synth [v exit loop-expr loop-succ label-succ-map]
  (when (seq v)
    (let [[el & [n & _ :as tail]] v]
      (type-case el
