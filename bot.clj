(fn [state time-left]
  ;; TODO Wrap code below into this fn

  (def ^:private
    orientations
    [:n :s :e :w])

  (defn- get-arena-dimensions
    "returns the dimensions of a given arena (NOTE: NOT 0 indexed)"
    {:added "1.0"
     :defined-in "wombats.arena.utils"}
    [arena]
    (let [x ((comp count first) arena)
          y (count arena)]
      [x y]))

  (defn- get-arena-dimensions-zero-based
    "returns the dimensions of a given arena"
    {:added "1.0"}
    [arena]
    (map dec (get-arena-dimensions arena)))

  (defn- get-in-arena
    "pulls the cell contents out of an arena at given coords"
    {:added "1.0"}
    [[x y] arena]
    (get-in arena [y x]))

  (defn- modify-orientation
    "Return a new orientation based off a provided orientation and the direction
  you want to turn"
    {:added "1.0"
     :defined "wombats.game.utils"}
    [current-orientation modifier]
    (let [current-idx (.indexOf orientations current-orientation)]
      (if (not= current-idx -1)
        (condp = modifier
          :right (get orientations (mod (inc current-idx) 4))
          :left (get orientations (mod (dec current-idx) 4))
          :about-face (get orientations (mod (+ 2 current-idx) 4))
          current-orientation)
        current-orientation)))

  (defn- get-move-coords
    "Gets the updated coords for moving.

  :Note wrapping not assumed."
    {:added "1.0"}
    [[x y] orientation]
    (case orientation
      :n [x (dec y)]
      :e [(inc x) y]
      :s [x (inc y)]
      :w [(dec x) y]))

  (defn- get-move-frontier
    "Returns the coords from the move command"
    {:added "1.0"}
    ([coords orientation dimensions]
     (get-move-frontier coords orientation dimensions false))
    ([[x y] orientation [max-x max-y] wrap?]
     (let [new-coords (get-move-coords [x y] orientation)
           [new-x new-y] new-coords]
       (if wrap?
         (case orientation
           (:n :s) [new-x (mod new-y max-y)]
           (:e :w) [(mod new-x max-x) new-y])
         (case orientation
           :n (if (< new-y 0) nil new-coords)
           :w (if (< new-x 0) nil new-coords)
           :e (if (> new-x max-x) nil new-coords)
           :s (if (> new-y max-y) nil new-coords))))))

  (defn- calculate-frontier
    "Caclulates the new frontier set based off of the provided frontier."
    {:added "1.0"}
    ([frontier arena-dimensions]
     (calculate-frontier frontier arena-dimensions false))
    ([{:keys [orientation coords weight cmd-sequence]} arena-dimensions wrap?]
     (let [frontier-orientations
           (map (fn [next-direction]
                  {:orientation (modify-orientation orientation next-direction)
                   :coords coords
                   :weight (inc weight)
                   :cmd-sequence (conj cmd-sequence {:action :turn
                                                     :metadata {:direction next-direction}})})
                (if (= weight 0)
                  [:right :left :about-face]
                  [:right :left]))

           frontier-move
           {:orientation orientation
            :coords (get-move-frontier coords orientation arena-dimensions wrap?)
            :weight (inc weight)
            :cmd-sequence (conj cmd-sequence {:action :move})}]
       (conj frontier-orientations frontier-move))))

  (defn- can-safely-occupy-space?
    "Predicate used to determine what cells can pass as frontiers"
    {:added "1.0"}
    [cell]
    (not (contains? #{"wood-barrier" "steel-barrier" "fog"}
                    (get-in cell [:contents :type]))))

  (defn- filter-frontier
    "Filters all the possible frontiers, returning only explore-able frontiers"
    {:added "1.0"}
    [frontier arena explored]
    (filter (fn [{coords :coords}]
              (if (nil? coords)
                false
                (let [cell (get-in-arena coords arena)
                      uuid (get-in cell [:contents :uuid])]
                  (and (nil? (get explored uuid))
                       (can-safely-occupy-space? cell))))) frontier))

  (defn- add-to-sorted-arena
    "Adds a frontier node to the sorted arena"
    {:added "1.0"}
    [sorted-arena
     {{type :type
       uuid :uuid} :contents}
     {weight :weight
      coords :coords
      cmd-sequence :cmd-sequence}]
    (let [formatted-frontier {:weight weight
                              :uuid uuid
                              :coords coords
                              :cmd-sequence cmd-sequence}]
      (update-in sorted-arena
                 [weight (keyword type)]
                 (fn [coll]
                   (if (not (nil? coll))
                     (conj coll formatted-frontier)
                     [formatted-frontier])))))

  (defn- to-global-coords
    "Converts local coordinates passed by the partially occluded arena
  to their corresponding global coordinates"
    {:added "1.0"}
    [{[origin-x origin-y] :local-coords
      [global-x global-y] :global-coords
      [dim-x dim-y] :global-dimensions}]
    (fn [[target-x target-y]]
      (let [delta-x (- target-x origin-x)
            delta-y (- target-y origin-y)
            new-x (mod (+ global-x delta-x) dim-y)
            new-y (mod (+ global-y delta-y) dim-x)]
        [new-x new-y])))

  (defn sort-arena-by-distance-then-type
    "sorts an arena by distance then type"
    {:added "1.0"}
    [{:keys [arena local-coords] :as enriched-state}]
    (let [arena-dimensions (get-arena-dimensions-zero-based arena)
          update-global-coords-fn (to-global-coords enriched-state)
          {{orientation-str :orientation
            uuid :uuid} :contents} (get-in-arena local-coords arena)
          orientation (keyword orientation-str)]
      (loop [frontier [{:coords local-coords
                        :orientation orientation
                        :uuid uuid
                        :weight 0
                        :cmd-sequence []}]
             explored {}
             sorted-arena []]
        (if (empty? frontier)
          (assoc enriched-state :sorted-arena sorted-arena)

          (let [frontier-node (first frontier)
                cell (get-in-arena (:coords frontier-node) arena)
                next-frontier (calculate-frontier frontier-node arena-dimensions)
                filtered-frontier (filter-frontier next-frontier arena explored)]
            (recur (vec (concat (rest frontier) filtered-frontier))
                   (merge explored {(get-in cell [:contents :uuid]) true})
                   (add-to-sorted-arena sorted-arena
                                        cell
                                        (update frontier-node
                                                :coords
                                                update-global-coords-fn))))))))

  (defn- remove-self
    [uuid]
    (fn [{:keys [wombat] :as weight-map}]
      (if wombat
        (let [filtered-list (vec (filter #(not= uuid (:uuid %)) wombat))]
          (if (empty? filtered-list)
            (dissoc weight-map :wombat)
            (assoc weight-map :wombat filtered-list)))
        weight-map)))

  (defn- remove-self-from-sorted-arena
    "removes current user from the sorted arena"
    {:added "1.0"}
    [{:keys [local-coords arena my-uuid] :as enriched-state}]
    (update-in
     enriched-state
     [:sorted-arena]
     (fn [sorted-arena]
       (-> sorted-arena
           (update 0 (remove-self my-uuid))
           (update 1 (remove-self my-uuid))))))

  (defn- track-able-cell?
    [{{type :type} :contents}]
    (not (contains? #{"fog"} type)))

  (defn- update-global-view
    "updates what your bot has seen historically"
    {:added "1.0"}
    [{:keys [global-arena arena my-uuid] :as enriched-state}]
    (let [update-global-coords-fn (to-global-coords enriched-state)
          current-global-arena (if global-arena
                                 global-arena
                                 (vec (repeat 10 (vec (repeat 10 nil)))))]

      (assoc enriched-state :global-arena
             (:y-global-arena
              (reduce
               (fn [{:keys [y-idx y-global-arena] :as acc} row]
                 {:y-idx (inc y-idx)
                  :y-global-arena
                  (:x-global-arena
                   (reduce
                    (fn [{:keys [x-idx x-global-arena]} cell]
                      {:x-idx (inc x-idx)
                       :x-global-arena (if (track-able-cell? cell)
                                         (assoc-in x-global-arena
                                                   (update-global-coords-fn
                                                    (reverse [x-idx y-idx]))
                                                   cell)
                                         x-global-arena)})
                    {:x-idx 0
                     :x-global-arena y-global-arena} row))})
               {:y-idx 0
                :y-global-arena current-global-arena} arena)))))

  (defn- add-me
    [{:keys [local-coords arena] :as enriched-state}]
    (let [self (get-in-arena local-coords arena)]
      (assoc enriched-state :me self)))

  (defn- add-closest-food
    [{:keys [sorted-arena] :as enriched-state}]
    (assoc enriched-state
           :closest-food
           (reduce (fn [food weight-map]
                     (if food
                       food
                       (if (:food weight-map)
                         (:cmd-sequence (first (:food weight-map)))
                         nil)))
                   nil
                   sorted-arena)))

  (defn- choose-action
    [{:keys [closest-food action-taken] :as enriched-state}]
    (let [action-seq (or closest-food [{}])
          action (first action-seq)
          remaining-action-seq (vec (rest action-seq))]
      (merge {:action action
              :remaining-action-seq remaining-action-seq})))

  (defn- format-response
    [{:keys [closest-food] :as enriched-state}]
    {:action (if closest-food
               (first closest-food)
               {})
     :state enriched-state}

    ;; TODO
    enriched-state)

  (defn enrich-state
    "Adds additional information to the given state used to improve
     the decision-making process"
    {:added "1.0"}
    [{:keys [arena local-coords] :as state}]
    (-> state
        (add-me)
        (sort-arena-by-distance-then-type)
        (remove-self-from-sorted-arena)
        (update-global-view)))

  (defn main-fn
    [state time-left]
    (let [enriched-state (-> (enrich-state state)
               (add-closest-food)
               #_(choose-action)
               #_(format-response))
          closest-food (:closest-food enriched-state)]
      (if closest-food 
        {:command (first closest-food) 
         :state (rest closest-food)}
        {:command {:action :move :metadata {}}})
      ))

  (main-fn state time-left))
