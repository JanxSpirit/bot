(defn run
  [{:keys  [arena state bot_id energy spawn-bot?] :as step-details}]
  (let [command-options [;;[{:cmd "SMOKESCREEN"}]
                         [{:cmd  "SHOOT"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]
                         [{:cmd  "MOVE"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]
                         [{:cmd  "MOVE"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]
                         [{:cmd  "MOVE"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]
                         [{:cmd  "MOVE"
                           :metadata  {:direction (rand-nth  [0 1 2 3 4 5 6 7])}}]]]
    {:commands (rand-nth command-options)}))
