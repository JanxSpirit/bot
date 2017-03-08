(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])

  (defn up-square [dungeon] (get (get dungeon 2) 3))

  (let [command-options [(repeat 10 {:action :move
                                     :metadata {}})
                         (repeat 2 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 4 {:action :shoot
                                      :metadata {}})
                         (repeat 1 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    (let [arena (dissoc state :saved-state :up-square)]
      {:command (rand-nth (flatten command-options))
       :state (assoc arena :up-square (up-square arena))})))
