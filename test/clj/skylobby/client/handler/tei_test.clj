(ns skylobby.client.handler.tei-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [skylobby.client.handler :as handler]
    skylobby.client.tei))


(set! *warn-on-reflection* true)


(deftest handle-queue-list
  (testing "update all matchmaking queues and prune removed ones"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {:bleh {:blah :blah}
                           "0" {:queue-name "1v1"
                                :team-size 1
                                :current-search-time 0
                                :current-size 1
                                :am-in true}}}}})]
      (handler/handle state-atom :server1 "MMQUEUES 1:2v2:2\t2:3v3:3")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"1" {:queue-name "2v2"
                      :team-size 2
                      :current-search-time 0
                      :current-size 0
                      :am-in false}
                 "2" {:queue-name "3v3"
                      :team-size 3
                      :current-search-time 0
                      :current-size 0
                      :am-in false}}}}}
             @state-atom)))))

(deftest handle-queue-list-empty
  (testing "empty queue list clears queues"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"0" {:queue-name "1v1"}}}}})]
      (handler/handle state-atom :server1 "MMQUEUES")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues {}}}}
             @state-atom)))))

(deftest handle-my-queue-list
  (testing "update my matchmaking queues, others marked not joined"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"1" {:queue-name "2v2"
                                :team-size 2
                                :am-in true}
                           "2" {:queue-name "3v3"
                                :team-size 3
                                :am-in false}}}}})]
      (handler/handle state-atom :server1 "MMMYQUEUES 2:3v3:3")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"1" {:queue-name "2v2"
                      :team-size 2
                      :am-in false}
                 "2" {:queue-name "3v3"
                      :team-size 3
                      :am-in true
                      :current-search-time 0
                      :current-size 0}}}}}
             @state-atom)))))

(deftest handle-queue-info
  (testing "update matchmaking queue info"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"0" {:queue-name "1v1"
                                :am-in false}
                           "1" {:queue-name "2v2"
                                :am-in true}}}}})]
      (handler/handle state-atom :server1 "MMINFO 1\t2v2\t2\t12345\t987")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"0" {:queue-name "1v1"
                      :am-in false}
                 "1" {:queue-name "2v2"
                      :team-size 2
                      :am-in true
                      :current-search-time 12345
                      :current-size 987}}}}}
             @state-atom)))))

(deftest handle-ready-check
  (testing "update matchmaking queue ready check"
    (let [ready-millis (System/currentTimeMillis)]
      (with-redefs [skylobby.util/curr-millis (constantly ready-millis)]
        (let [state-atom (atom
                           {:by-server
                            {:server1
                             {:matchmaking-queues
                              {"0" {:queue-name "1v1"
                                    :am-in false}
                               "1" {:queue-name "2v2"
                                    :team-size 2
                                    :am-in true}}}}})]
          (handler/handle state-atom :server1 "MMREADYCHECK 1\t2v2\t60\talice\tbob")
          (is (= {:by-server
                  {:server1
                   {:matchmaking-queues
                    {"0" {:queue-name "1v1"
                          :am-in false}
                     "1" {:queue-name "2v2"
                          :team-size 2
                          :am-in true
                          :status :ready-check
                          :ready-check true
                          :players ["alice" "bob"]
                          :ready-deadline (+ ready-millis 60000)}}}}}
                 @state-atom)))))))

(deftest handle-cancelled
  (testing "matchmaking ready check cancelled"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"1" {:queue-name "2v2"
                                :team-size 2
                                :am-in true
                                :ready-check true
                                :ready-deadline 12345
                                :countdown 60
                                :players ["alice" "bob"]}}}}})]
      (handler/handle state-atom :server1 "MMCANCELLED 1\t2v2\tReady check timed out")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"1" {:queue-name "2v2"
                      :team-size 2
                      :am-in true
                      :status :searching
                      :ready-check false
                      :ready-deadline nil
                      :countdown nil
                      :players ["alice" "bob"]
                      :banner "Match cancelled: Ready check timed out"}}}}}
             @state-atom)))))

(deftest handle-started
  (testing "matchmaking match started"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"1" {:queue-name "2v2"
                                :team-size 2
                                :am-in true
                                :ready-check true
                                :players ["alice" "bob"]
                                :ready-deadline 12345
                                :countdown 5}}}}})]
      (handler/handle state-atom :server1 "MMSTARTED 1\t2v2\talice\tbob")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"1" {:queue-name "2v2"
                      :team-size 2
                      :status :in-game
                      :ready-check false
                      :am-in false
:ready-deadline nil
                       :countdown nil
                       :players ["alice" "bob"]
                       :banner "Match starting! alice vs bob"}}}}}
             @state-atom)))))

(deftest handle-kicked
  (testing "matchmaking player kicked"
    (let [state-atom (atom
                       {:by-server
                        {:server1
                         {:matchmaking-queues
                          {"1" {:queue-name "2v2"
                                :team-size 2
                                :am-in true
                                :ready-check true}}}}})]
      (handler/handle state-atom :server1 "MMKICKED 1\t2v2\tFailed ready check 2 times in a row")
      (is (= {:by-server
              {:server1
               {:matchmaking-queues
                {"1" {:queue-name "2v2"
                      :team-size 2
                      :status :searching
                      :ready-check false
                      :am-in false
                      :ready-deadline nil
                      :countdown nil
                      :banner "Kicked: Failed ready check 2 times in a row"}}}}}
             @state-atom)))))