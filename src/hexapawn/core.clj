(ns hexapawn.core)

(defn start-board []
  [[:black :black :black]
   [:empty :empty :empty]
   [:white :white :white]])

(defn valid-pos [[row col :as pos]]
  (and (>= row 0) (< row 3) (>= col 0) (< col 3) pos))

(def actions #{:advance :capture-left :capture-right})

(def opposite-player
  {:black :white
   :white :black})

(defn new-game [start-player]
  {:player start-player
   :board (start-board)})

(defn end-pos [player [row col] move]
  (let [i (case player
            :black 1
            :white -1)]
    (valid-pos (case move
                 :advance      [(+ row i) col]
                 :capture-left  [(+ row i) (+ col i)]
                 :capture-right [(+ row i) (- col i)]))))

(defn valid-move? [game player [row col :as pos] action]
  (and
   (= (:player game) player)
   (= (get-in game [:board row col]) player)
   (when-let [e-pos (end-pos player pos action)]
     (let [[end-row end-col] e-pos 
           end-v (get-in game [:board end-row end-col])]
       (case action
         :advance (= end-v :empty)
         :capture-left (= end-v (opposite-player player))
         :capture-right (= end-v (opposite-player player)))))))

(defn map-indexed2 [f coll]
  (->> coll
       (map-indexed 
        (fn [i is] 
          (map-indexed 
           (fn [j v] (apply f [[i j] v])) is)))
       (reduce concat)))

(defn get-positions [game player]
  (->> (:board game) 
       (map-indexed2 vector)
       (filter #(= player (second %)))))

(defn next-moves [game]
  (let [player (:player game)] 
    (->>
     (for [[pos _] (get-positions game (:player game))]
       (for [a actions]
         [player pos a]))
     (reduce concat)
     (filter (fn [[pl ps a]] (valid-move? game pl ps a))))))

