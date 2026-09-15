(ns skylobby.client.tei
  (:require
    [clojure.string :as string]
    [skylobby.client.handler :as handler]
    [skylobby.client.message :as message]
    [skylobby.util :as u]
    [taoensso.timbre :as log]))


(set! *warn-on-reflection* true)


; matchmaking

(defn parse-queue-triple [queue-triple]
  (when-let [[_all id queue-name team-size] (re-find #"([^:]+):([^:]+):([^:]+)" queue-triple)]
    [id {:queue-name queue-name
         :team-size (u/to-number team-size)
         :current-search-time 0
         :current-size 0
         :am-in false}]))

(defn queue-entries-from [queues-str]
  (->> (string/split (or queues-str "") #"\t")
       (map parse-queue-triple)
       (filter some?)
       (into {})))

(defmethod handler/handle "MMQUEUES" [state-atom server-key m]
  (let [[_all queues-str] (re-find #"[^\s]+ (.*)" m)
        queue-entries (queue-entries-from queues-str)
        queue-ids (set (keys queue-entries))]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues]
      (fn [matchmaking-queues]
        (u/deep-merge
          (->> matchmaking-queues
               (filter (comp queue-ids first))
               (into {})
               (map (fn [[k v]] [k (assoc v :current-search-time 0 :current-size 0)]))
               (into {}))
          queue-entries)))))

(defmethod handler/handle "MMMYQUEUES" [state-atom server-key m]
  (let [[_all queues-str] (re-find #"[^\s]+ (.*)" m)
        queue-entries (queue-entries-from queues-str)
        queue-ids (set (keys queue-entries))]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues]
      (fn [matchmaking-queues]
        (u/deep-merge
          (->> matchmaking-queues
               (map (fn [[k v]] [k (assoc v :am-in false)]))
               (into {}))
          (->> (filter (comp queue-ids first) queue-entries)
               (map (fn [[k v]] [k (assoc v :am-in true)]))
               (into {})))))))

(defmethod handler/handle "MMINFO" [state-atom server-key m]
  (let [[_all queue-info] (re-find #"[^\s]+ (.*)" m)
        [queue-id queue-name team-size search-time size] (string/split queue-info #"\t")]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues queue-id]
      (fn [queue-data]
        (-> (or queue-data {})
            (assoc
              :queue-name queue-name
              :team-size (u/to-number team-size)
              :current-search-time (u/to-number search-time)
              :current-size (u/to-number size)))))))

(defmethod handler/handle "MMREADYCHECK" [state-atom server-key m]
  (let [[_all payload] (re-find #"[^\s]+ (.*)" m)
        [queue-id queue-name secs & players] (string/split payload #"\t")]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues queue-id]
      (fn [queue-data]
        (let [queue-data (or queue-data {})
              team-size (:team-size queue-data (max 1 (quot (count players) 2)))]
          (assoc queue-data
                 :queue-name queue-name
                 :team-size team-size
                 :status :ready-check
                 :ready-check true
                 :players (vec players)
                 :am-in true
                 :ready-deadline (+ (u/curr-millis) (* 1000 (u/to-number secs)))))))))

(defmethod handler/handle "MMCANCELLED" [state-atom server-key m]
  (let [[_all payload] (re-find #"[^\s]+ (.*)" m)
        [queue-id queue-name reason] (string/split payload #"\t")]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues queue-id]
      (fn [queue-data]
        (-> (or queue-data {})
            (assoc
              :status :searching
              :ready-check false
              :ready-deadline nil
              :countdown nil
              :banner (str "Match cancelled: " reason)))))))

(defmethod handler/handle "MMSTARTED" [state-atom server-key m]
  (let [[_all payload] (re-find #"[^\s]+ (.*)" m)
        [queue-id queue-name & players] (string/split payload #"\t")
        players (vec players)
        banner (str "Match starting! " (when (seq players) (string/join " vs " players)))]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues queue-id]
      (fn [queue-data]
        (-> (or queue-data {})
            (assoc
              :status :in-game
              :ready-check false
              :am-in false
              :ready-deadline nil
              :countdown nil
              :banner banner))))))

(defmethod handler/handle "MMKICKED" [state-atom server-key m]
  (let [[_all payload] (re-find #"[^\s]+ (.*)" m)
        [queue-id queue-name reason] (string/split payload #"\t")]
    (swap! state-atom update-in [:by-server server-key :matchmaking-queues queue-id]
      (fn [queue-data]
        (-> (or queue-data {})
            (assoc
              :status :searching
              :ready-check false
              :am-in false
              :ready-deadline nil
              :countdown nil
              :banner (str "Kicked: " reason)))))))


; token


(defmethod handler/handle "s.user.user_token" [state-atom server-key m]
  (if-let [[_all _username auth-token] (re-find #"[^\s]+ ([^\t]+)\t([^\t]+)" m)]
    (let [state (swap! state-atom assoc-in [:by-server server-key :auth-token] auth-token)
          client-data (-> state :by-server (get server-key) :client-data)
          user-agent (u/user-agent (:user-agent-override state))
          client-id (u/client-id state-atom state)
          flags "skylobby=true"
          suffix (str "\t" user-agent "\t" client-id "\t" flags)
          message (str "c.user.login " auth-token suffix)]
      (message/send state-atom client-data message))
    (log/error "Error parsing user token message")))


; battles

(defmethod handler/handle "s.battle.update_lobby_title" [state-atom server-key m]
  (if-let [[_all battle-id battle-title] (re-find #"[^\s]+ ([^\s]+)\s([^\t]+)" m)]
    (swap! state-atom assoc-in [:by-server server-key :battles battle-id :battle-title] battle-title)
    (log/error "Error parsing battle rename message")))


(defmethod handler/handle "s.system.disconnect" [state-atom server-key m]
  (let [[_all reason] (re-find #"[^\s]+ ([^\s]+)\s([^\t]+)" m)]
    (case reason
      "Flood protection"
      (do
        (log/info "Removing rejoin battle after flood protection")
        (swap! state-atom update :last-battle dissoc server-key))
      (log/warn "No specific handler for disconnect reason" reason))))
