(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])

  (defn immediate-squares [dungeon] {:up (get (get dungeon 2) 3)
                                     :left (get (get dungeon 3) 2)
                                     :right (get (get dungeon 3) 4)
                                     :down (get (get dungeon 4) 3)})

  (let [command-options [(repeat 10 {:action :move
                                     :metadata {}})
                         (repeat 2 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 4 {:action :shoot
                                      :metadata {}})
                         (repeat 1 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]]

    (let [cleanstate (dissoc state :saved-state :up-square)]
      {:command (rand-nth (flatten command-options))
       :state (assoc cleanstate :immediate-squares (immediate-squares (cleanstate :arena)))})))
