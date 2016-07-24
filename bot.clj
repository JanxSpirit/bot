(defn run
  [{:keys  [arena saved-state bot-id energy spawn-bot? initiative-order wombat-count] :as frame-details}]
  (println bot-id ": my initiative order is " initiative-order " of " wombat-count " wombats")
  (let [command-options [[{:cmd  "SHOOT"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7]) :energy 20}}
                          {:cmd  "SET_STATE"
                           :metadata  {:foo  "bar"}}]
                         [{:cmd  "MOVE"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]
                         {:cmd "SMOKESCREEN"}]]
    {:commands (rand-nth command-options)}))
