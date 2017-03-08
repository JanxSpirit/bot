(fn [state time-left]
  (def turn-directions [:right :left :about-face])
  (def smoke-directions [:forward :backward :left :right :drop])

  (defn immediate-squares [dungeon] {:north (get (get dungeon 2) 3)
                                     :west (get (get dungeon 3) 2)
                                     :east (get (get dungeon 3) 4)
                                     :south (get (get dungeon 4) 3)})

  (defn my-orientation [arena] (get-in (find #(= :wombat (get-in % [:contents :type])) (flatten arena)) [:contents :orientation]))

  (defn food-in-front-of-me [immed-sqs] immed-sqs)

  (let [command-options [(repeat 10 {:action :move
                                     :metadata {}})
                         (repeat 2 {:action :turn
                                    :metadata {:direction (rand-nth turn-directions)}})
                         (repeat 4 {:action :shoot
                                      :metadata {}})
                         (repeat 1 {:action :smoke
                                    :metadata {:direction (rand-nth smoke-directions)}})]
        immed-sqs (immediate-squares (state :arena))]


    (let [cleanstate (dissoc state :saved-state :up-square :my-orientation)]
      {:command (rand-nth (flatten command-options))
       :state (assoc cleanstate :immediate-squares (immediate-squares (cleanstate :arena)) :my-orientation (my-orientation (cleanstate :arena)))})))
