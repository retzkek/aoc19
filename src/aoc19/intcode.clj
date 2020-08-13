(ns aoc19.intcode)

(def mem (atom {}))
(def rel-base (atom 0))

(defn read-program []
  "read program from *in* and parse into intcode program"
  (map #(Integer/parseInt %) (clojure.string/split (read-line) #",")))

(defn new-program! [program]
  (reset! mem (zipmap (range (count program)) program))
  (reset! rel-base 0))

(defn peek-mem [addr]
  "return value of memory location addr"
  (if (contains? @mem addr) (@mem addr) 0))

(defn poke-mem! [addr val]
  "set value of memory location addr"
  (swap! mem assoc addr val))

(defn pval [mode value]
  "return value given by access mode: 0=direct, 1=value, 2=relative"
  (case mode
    0 (peek-mem value)
    1 value
    2 (peek-mem (+ @rel-base value))))

(defn paddr [mode value]
  "return address given by access mode: 0=direct, 2=relative"
  (case mode
    0 value
    2 (+ @rel-base value)))

(defn op-binary [op addr modes]
  (poke-mem! (paddr (nth modes 2) (peek-mem (+ 3 addr)))
             (op (pval (first modes) (peek-mem (+ 1 addr)))
                 (pval (second modes) (peek-mem (+ 2 addr))))))

(defn op-input [addr modes]
  (print "> ")
  (flush)
  (poke-mem! (paddr (first modes) (peek-mem (+ 1 addr)))
             (Integer/parseInt (read-line))))

(defn op-output [addr modes]
  (let [v (pval (first modes) (peek-mem (+ 1 addr)))]
    (println v)))

(defn op-jump [f addr modes]
  (if (f (pval (first modes) (peek-mem (+ 1 addr))))
    (pval (second modes) (peek-mem (+ 2 addr)))
    (+ addr 3)))

(defn op-cmp [f addr modes]
  (poke-mem! (paddr (nth modes 2) (peek-mem (+ 3 addr)))
             (if (f (pval (first modes) (peek-mem (+ 1 addr)))
                    (pval (second modes) (peek-mem (+ 2 addr))))
               1
               0)))

(defn op-rel-base [addr modes]
  (swap! rel-base #(+ % (pval (first modes) (peek-mem (+ 1 addr))))))

(defn int->list [n len]
  (map #(int (rem (quot n (Math/pow 10 %)) 10)) (range len)))

(defn exe [addr]
  (let [opmodes (peek-mem addr)
        opcode (rem opmodes 100)
        modecode (quot opmodes 100)
        modes (int->list modecode 3)]
    (do
      #_(println addr opmodes)
      (case opcode
        99 (print (sort-by first (seq @mem)))
        1 (do (op-binary + addr modes) (exe (+ addr 4)))
        2 (do (op-binary * addr modes) (exe (+ addr 4)))
        3 (do (op-input addr modes) (exe (+ addr 2)))
        4 (do (op-output addr modes) (exe (+ addr 2)))
        5 (do (exe (op-jump #(not (= % 0)) addr modes)))
        6 (do (exe (op-jump #(= % 0) addr modes)))
        7 (do (op-cmp < addr modes) (exe (+ addr 4)))
        8 (do (op-cmp = addr modes) (exe (+ addr 4)))
        9 (do (op-rel-base addr modes) (exe (+ addr 2)))))))
