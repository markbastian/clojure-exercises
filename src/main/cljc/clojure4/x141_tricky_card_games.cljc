(ns clojure4.x141-tricky-card-games)

;http://www.4clojure.com/problem/141
;Tricky card games

;Use this on www.4clojure.com since cond-> seems broken there
(def __old
  (fn [trump]
    (fn [[{leader :suit} & _ :as trick]]
      (apply max-key
             (fn [{:keys [suit rank]}]
               (+ (if (and trump (= suit trump))
                    13
                    (if (= suit leader) 0 -13)) rank)) trick))))

(def __
  (fn [trump]
    (let [m (cond-> (zipmap [:heart :club :spade :diamond] (repeat -13))
                    trump (assoc trump 13))]
      (fn [[{leader :suit} & _ :as trick]]
        (let [n (cond-> m leader (assoc leader 0))]
          (apply max-key (fn [{:keys [suit rank]}] (+ (n suit) rank)) trick))))))

(assert (let [notrump (__ nil)]
          (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                                   {:suit :club :rank 9}]))
               (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                   {:suit :club :rank 10}])))))

(assert (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                               {:suit :club :rank 10}])))

(assert (= {:suit :heart :rank 8}
           ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                         {:suit :diamond :rank 10} {:suit :heart :rank 4}])))