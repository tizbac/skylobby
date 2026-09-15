(ns skylobby.fx.matchmaking-test
  (:require
    [cljfx.api :as fx]
    [clojure.test :refer [deftest is]]
    [skylobby.fx.matchmaking :as fx.matchmaking]))


(set! *warn-on-reflection* true)


(deftest matchmaking-view
  (is (map?
        (fx.matchmaking/matchmaking-view
          {:fx/context (fx/create-context nil)}))))


(deftest matchmaking-view-with-ready-check
  (let [context
        (fx/create-context
          {:by-server
           {"server1"
            {:client-data {:username "mm6"}
             :matchmaking-join-queue-id "1"
             :matchmaking-join-min 2
             :matchmaking-join-max 2
             :matchmaking-queues
             {"1" {:queue-name "1v1"
                   :team-size 1
                   :am-in true
                   :status :ready-check
                   :ready-check true
                   :ready-deadline (+ (System/currentTimeMillis) 5000)
                   :countdown 5
                   :players ["mm6" "mm7"]
                   :me-ready true
                   :ready-players #{"mm6"}
                   :banner "Ready check!"}}}}})]
    (is (map?
          (fx.matchmaking/matchmaking-view
            {:fx/context context
             :server-key "server1"})))))


(deftest matchmaking-view-with-queues-not-joined
  (let [context
        (fx/create-context
          {:by-server
           {"server1"
            {:client-data {:username "mm6"}
             :matchmaking-join-queue-id "2"
             :matchmaking-join-min 2
             :matchmaking-join-max 4
             :matchmaking-queues
             {"1" {:queue-name "1v1"
                   :team-size 1
                   :am-in false
                   :status :searching}
              "2" {:queue-name "2v2"
                   :team-size 2
                   :am-in false
                   :status :searching}}}}})]
    (is (map?
          (fx.matchmaking/matchmaking-view
            {:fx/context context
             :server-key "server1"})))))


(deftest matchmaking-window
  (is (map?
        (fx.matchmaking/matchmaking-window-impl
          {:fx/context (fx/create-context nil)}))))
