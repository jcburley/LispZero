(defn- gen-list
  "Generate just one list"
  [n]
  (println (str "(cons 'foo" n " '(x y))\n")))

(defn gen-big-list
  "Generate the 'big' list"
  [n]
  (run! #(gen-list %) (range n)))
