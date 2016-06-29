(defn run
  [{:keys [arena state bot_id energy spawn-bot?] :as step-details}]
  (println "arena -- " (count arena))
  {:commands [{:cmd "SHOOT"
               :metadata {:direction (rand-nth [0 1 2 3 4]) :energy 1}}
              {:cmd "SET_STATE"
               :metadata {:foo "bar"}}]})
