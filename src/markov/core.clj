(ns markov.core)


(defn rand-weighted
  "Returns a weighted element from a sequence of [item prob] tuples"
  ([coll] (rand-weighted coll (rand)))
  ([[[item p] & more] choice]
     (if (< choice p)
       item
       (recur more (- choice p)))))


(defn- make-prop [m]
  "From a frequency map m, return a sequence of normalized [item prop] tuples"
  (let [max-count (apply + (map second m))]
    (for [[i c] m]
      [i (/ c max-count)])))

(defn make-chain [n coll]
  "Return a Markov-chain with previous length n from coll"
  (let [chain (partition (inc n) 1 coll )
        prev-future (map (juxt butlast last) chain)
        grouped (group-by first prev-future)
        chain-fn (comp make-prop frequencies (partial map second))]
    (reduce #(update-in %1 [%2] chain-fn) grouped (keys grouped))))


(defn next-element [previous chain]
  "Return next element after previous sequence from chain"
  (-> (get chain previous) rand-weighted))


(declare random-walk)


(defn- random-walk-helper [previous chain & [error-fn]]
  (let [link (try
               (next-element previous chain)
               (catch Exception e
                 (if error-fn
                   (error-fn previous chain)
                   (throw e))))
        next-seq (-> previous rest vec (conj link))]
     (lazy-seq (cons link (random-walk next-seq chain error-fn)))))


(defn prepare-string [text]
  "Cleanup and tokenize string for use with Markow-functions"
  (let [splitted (clojure.string/split text #"\n|\s|\r")]
    (remove #{""} splitted)))


(defn random-walk [previous chain & [error-fn]]
  "Return an infinite random-walk of the provided chain.
   Uses previous for initial seeding.
   May provide an error-fn for error-continuation.
   Error-fn should be of this signature (fn [previous chain])"
  (if (string? previous)
    (random-walk-helper (prepare-string previous) chain error-fn)
    (random-walk-helper previous chain error-fn)))
