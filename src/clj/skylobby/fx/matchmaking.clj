(ns skylobby.fx.matchmaking
  (:require
    [cljfx.api :as fx]
    [clojure.java.io :as io]
    [clojure.string :as string]
    skylobby.fx
    [skylobby.util :as u]
    [taoensso.tufte :as tufte]))


(set! *warn-on-reflection* true)


(def matchmaking-window-width 800)
(def matchmaking-window-height 700)
(def cyan "#69d8ff")
(def white "#f3f5f4")
(def muted "#9ba5a6")
(def panel-bg "rgba(7,12,15,0.62)")
(def panel-line "rgba(255,255,255,0.14)")
(def accent "rgba(65,184,222,0.72)")
(def amber "#e4b02e")

(def button-primary
  {:-fx-background-color accent
   :-fx-text-fill "#142024"
   :-fx-font-weight :bold
   :-fx-padding "8 18"
   :-fx-background-radius 2})

(def button-ghost
  {:-fx-background-color "transparent"
   :-fx-border-color panel-line
   :-fx-border-width 1
   :-fx-text-fill white
   :-fx-padding "6 14"
   :-fx-background-radius 2})

(def button-danger
  {:-fx-background-color "rgba(190,60,60,0.55)"
   :-fx-text-fill white
   :-fx-padding "6 14"
   :-fx-background-radius 2})


(defn panel
  [& children]
  {:fx/type :v-box
   :style {:-fx-background-color panel-bg
           :-fx-border-color panel-line
           :-fx-border-width 0.5
           :-fx-padding 10}
   :children children})


(defn label
  [text & [style]]
  {:fx/type :label
   :text (str text)
   :style (merge {:-fx-text-fill white :-fx-font-family "Arial"} style)})


(defn panel-title
  [text]
  (label text {:-fx-text-fill "#aeb8ba" :-fx-font-size 10}))


(defn nav-button
  [text selected? on-action]
  {:fx/type :button
   :text (str text)
   :on-action on-action
   :style (merge
            {:-fx-background-color "transparent"
             :-fx-background-insets 0
             :-fx-border-radius 0
             :-fx-padding "6 12"
             :-fx-text-fill (if selected? white muted)
             :-fx-font-size 12}
            (when selected?
              {:-fx-border-color (str "transparent transparent " cyan " transparent")
               :-fx-border-width "0 0 2 0"}))})


(defn roster-row
  [{:keys [index username my-username ready?]}]
  (let [you? (= username my-username)]
    {:fx/type :h-box
     :alignment :center-left
     :style {:-fx-background-color (if (and you? ready?)
                                     "rgba(31,83,99,0.56)"
                                     "rgba(5,10,13,0.46)")
             :-fx-border-color "transparent transparent rgba(255,255,255,0.05) transparent"
             :-fx-border-width "0 0 0.5 0"
             :-fx-padding "5 8"}
     :children
     [
      {:fx/type :label
       :text (str (inc index))
       :style {:-fx-text-fill "#91a1a4" :-fx-font-size 11 :-fx-min-width 22}}
      (label username {:-fx-text-fill "#cbd0d0" :-fx-font-size 12})
      {:fx/type :region
       :h-box/hgrow :always}
      (when ready?
        (label (str "◉ " username " ready") {:-fx-text-fill cyan :-fx-font-size 9}))
      (when you?
        (label "YOU" {:-fx-text-fill white
                      :-fx-background-color amber
                      :-fx-padding "2 4"
                      :-fx-font-size 9}))]}))


(defn team-card
  [{:keys [label-text team-name sub-text]}]
  {:fx/type :v-box
   :style {:-fx-background-color "rgba(8,15,19,0.48)"
           :-fx-border-color "transparent transparent transparent rgba(255,255,255,0.25)"
           :-fx-border-width "0 0 0 2"
           :-fx-padding "10 13 12"
           :-fx-pref-width 190}
   :children
   [
    (panel-title label-text)
    (label team-name {:-fx-font-size 17 :-fx-font-weight :bold})
    (label sub-text {:-fx-text-fill "#aab4b5" :-fx-font-size 9})]})


(defn mode-card
  [{:keys [mode-name title countdown searching-size max-size]}]
  {:fx/type :v-box
   :style {:-fx-background-color "linear-gradient(to right, rgba(8,13,16,0.82), rgba(8,13,16,0.38))"
           :-fx-padding "14 16"
           :-fx-pref-width 290}
   :children
   [
    (label mode-name {:-fx-text-fill "#b8c1c2" :-fx-font-size 10})
    (label title {:-fx-font-size 20 :-fx-font-weight :bold})
    (cond
      countdown
      (label (str "Match beginning in: " countdown) {:-fx-text-fill "#9ea7a8" :-fx-font-size 10})

      searching-size
      (label (str "Searching for players... " searching-size " / " max-size) {:-fx-text-fill "#9ea7a8" :-fx-font-size 10})

      :else
      (label "Select a mode to queue up" {:-fx-text-fill "#9ea7a8" :-fx-font-size 10}))]})


(defn banner-panel
  [client-data text]
  {:fx/type :v-box
   :style {:-fx-background-color "rgba(4,9,12,0.58)"
           :-fx-padding "7 10"
           :-fx-pref-width 280}
   :children
   [
    (label (str "[matchmaking] " (or text "Waiting for matchmaking events..."))
           {:-fx-text-fill "#bfc7c8" :-fx-font-size 9})
    {:fx/type :h-box
     :alignment :center-right
     :spacing 8
     :children
     [
      {:fx/type :button
       :text "Refresh Queues"
       :style button-ghost
       :on-action {:event/type :spring-lobby/matchmaking-refresh
                   :client-data client-data}}
      {:fx/type :button
       :text "Leave All Queues"
       :style button-ghost
       :on-action {:event/type :spring-lobby/matchmaking-leave-all
                   :client-data client-data}}]}]})


(defn controls
  [{:keys [server-key client-data selected selected-queue-id join-min join-max max-total my-in-selected ready-check countdown]}]
  {:fx/type :h-box
   :alignment :center
   :spacing 12
   :children
   (cond
     ready-check
     [
      (label (str "Ready check " (or countdown 0) "s") {:-fx-font-size 14})
      (label (str "Click Ready to confirm your spot in " (:queue-name selected))
             {:-fx-text-fill muted :-fx-font-size 11})
      {:fx/type :button
       :text "Decline"
       :style button-danger
       :on-action {:event/type :spring-lobby/matchmaking-decline
                   :client-data client-data
                   :queue-id selected-queue-id}}
      {:fx/type :button
       :text "Ready"
       :style button-primary
       :on-action {:event/type :spring-lobby/matchmaking-ready
                   :client-data client-data
                   :queue-id selected-queue-id}}]

     my-in-selected
     [
      (label (str "In queue: " (:queue-name selected)) {:-fx-font-size 13})
      {:fx/type :button
       :text "Leave Queue"
       :style button-ghost
       :on-action {:event/type :spring-lobby/matchmaking-leave
                   :client-data client-data
                   :queue-id selected-queue-id}}
      {:fx/type :button
       :text "Leave All"
       :style button-ghost
       :on-action {:event/type :spring-lobby/matchmaking-leave-all
                   :client-data client-data}}]

     :else
     [
      (label "Players per game:" {:-fx-text-fill muted :-fx-font-size 10})
      (label "min" {:-fx-text-fill muted :-fx-font-size 9})
      {:fx/type :combo-box
       :value (min (int join-min) (int max-total))
       :items (mapv int (range 2 (inc (int max-total))))
       :on-value-changed {:event/type :spring-lobby/assoc-in
                          :path [:by-server server-key :matchmaking-join-min]}}
      (label "max" {:-fx-text-fill muted :-fx-font-size 9})
      {:fx/type :combo-box
       :value (min (int join-max) (int max-total))
       :items (mapv int (range 2 (inc (int max-total))))
       :on-value-changed {:event/type :spring-lobby/assoc-in
                          :path [:by-server server-key :matchmaking-join-max]}}
      {:fx/type :button
       :text "Join"
       :style button-primary
       :disable (nil? selected)
       :on-action {:event/type :spring-lobby/matchmaking-join
                   :client-data client-data
                   :queue-id selected-queue-id
                   :min-players join-min
                   :max-players join-max}}])})


(defn find-ready-check
  [queues]
  (->> queues
       (some (fn [[queue-id queue-data]] (when (:ready-check queue-data) [queue-id queue-data])))))


(defn matchmaking-view
  [{:fx/keys [context]
    :keys [server-key]}]
  (let [server-data (fx/sub-val context get-in [:by-server server-key])
        client-data (:client-data server-data)
        my-username (:username client-data)
        queues (or (:matchmaking-queues server-data) {})
        queues-sorted (sort-by first queues)
        selected-queue-id (:matchmaking-join-queue-id server-data (first (keys queues)))
        selected (get queues selected-queue-id)
        join-min (or (:matchmaking-join-min server-data) 2)
        join-max (or (:matchmaking-join-max server-data)
                     (if selected (* 2 (:team-size selected)) 2))
        max-total (if selected (max 2 (* 2 (:team-size selected))) 4)
        ready-check-entry (find-ready-check queues)
        [active-queue-id active-queue] (if ready-check-entry
                                         ready-check-entry
                                         [selected-queue-id selected])
        ready-check (boolean (:ready-check active-queue))
        active-in-queue (boolean (:am-in active-queue))
        countdown (:countdown active-queue)
        players (or (:players active-queue) [])
        banner (cond
                 (empty? queues)
                 "No matchmaking queues available yet - click Refresh Queues"
                 :else
                 (:banner active-queue))
        ready-players (or (:ready-players active-queue) #{})
        roster-title (if ready-check
                       (str "Players " (count players) " / " (count players) "  ~  ready " (count ready-players))
                       (str "Players " (count players) " / " max-total "  Max"))
        ranked-players (map-indexed
                         (fn [idx username]
                           {:index idx
                            :username username
                            :my-username my-username
                            :ready? (boolean (contains? ready-players username))})
                         players)
        team-size (or (:team-size active-queue)
                      (max 1 (quot (count players) 2)))
        team-a-names (take team-size (seq players))
        team-b-names (drop team-size (seq players))
        roster-children
        (into
          [(panel-title roster-title)
           {:fx/type :region :min-height 4}]
          (if (seq ranked-players)
            (map roster-row ranked-players)
            [(label (if ready-check
                      "A ready check is starting..."
                      "Waiting for players...")
                    {:-fx-text-fill muted :-fx-font-size 10})]))]
    {:fx/type :anchor-pane
     :background {:fills [{:fill "rgba(17,24,39,0.7)"}]
                  :images [{:image (str (io/resource "skylobby/background.jpg"))
                            :size {:width 0 :height 0 :width-as-percentage false :height-as-percentage false :contain false :cover true}}]}
     :style {:-fx-font-family "Arial"
             :-fx-text-fill white}
     :children
     [
      ; brand (top-left)
      {:fx/type :v-box
       :anchor-pane/top 16
       :anchor-pane/left 28
       :children
       [
        (label "SKYLOBBY" {:-fx-font-size 26 :-fx-font-weight :bold :-fx-opacity 0.85})
        (label "S P R I N G   L O B B Y" {:-fx-text-fill muted :-fx-font-size 9})]}

      ; nav (queue tabs, top-center)
      {:fx/type :h-box
       :anchor-pane/top 8
       :anchor-pane/left 250
       :anchor-pane/right 270
       :alignment :center-left
       :style {:-fx-border-color "transparent transparent rgba(255,255,255,0.08) transparent"
               :-fx-border-width "0 0 1 0"
               :-fx-padding "0 0 4 0"}
       :children
       (concat
         [(label "MODES" {:-fx-text-fill "#aeb7b8" :-fx-font-size 9})]
         (map
           (fn [[queue-id queue-data]]
             (nav-button
               (str (:queue-name queue-data)
                    (when (:am-in queue-data) "  ◉")
                    (when (and (:ready-check queue-data) (not= queue-id active-queue-id)) "  ⌛"))
               (= queue-id selected-queue-id)
               {:event/type :spring-lobby/matchmaking-select-queue
                :client-data client-data
                :queue-id queue-id}))
           queues-sorted))}

      ; profile (top-right)
      {:fx/type :v-box
       :anchor-pane/top 16
       :anchor-pane/right 28
       :alignment :top-right
       :children
       [
        (label (str (or my-username "?")) {:-fx-font-size 13 :-fx-font-weight :bold})
        (label "LOBBY" {:-fx-text-fill muted :-fx-font-size 9})]}

      ; roster (left)
      (assoc
        (apply panel roster-children)
        :anchor-pane/top 140
        :anchor-pane/left 48
        :anchor-pane/bottom 200
        :anchor-pane/right Double/POSITIVE_INFINITY)

      ; team card (right, top)
      (assoc
        (team-card
          {:label-text "Your Team"
           :team-name (if (seq team-a-names) (string/join ", " team-a-names) "—")
           :sub-text (str "vs " (if (seq team-b-names)
                                  (string/join ", " team-b-names)
                                  "waiting for opponents")
                          "  ·  " (:queue-name active-queue)
                          "  (" team-size "v" team-size ")")})
        :anchor-pane/top 140
        :anchor-pane/right 48
        :anchor-pane/left Double/POSITIVE_INFINITY)

      ; mode card (top-left, under the brand)
      (assoc
        (mode-card
          {:mode-name (str "◈  " (:queue-name active-queue "Matchmaking"))
           :title (string/upper-case (or (:queue-name active-queue) "NO QUEUES"))
           :countdown (when ready-check (or countdown 0))
           :searching-size (when (and active-in-queue (not ready-check))
                             (or (:current-size active-queue) (count players)))
           :max-size max-total})
        :anchor-pane/top 60
        :anchor-pane/left 48)

      ; controls (bottom, full width)
      (assoc
        (controls
          {:server-key server-key
           :client-data client-data
           :selected selected
           :selected-queue-id selected-queue-id
           :join-min join-min
           :join-max join-max
           :max-total max-total
           :my-in-selected (boolean (:am-in selected))
           :ready-check ready-check
           :countdown countdown})
        :anchor-pane/left 48
        :anchor-pane/right 48
        :anchor-pane/bottom 150
        :alignment :center)

      ; banner / chat (bottom-right)
      (assoc
        (banner-panel client-data banner)
        :anchor-pane/right 48
        :anchor-pane/bottom 48)]}))


(defn matchmaking-window-impl
  [{:fx/keys [context]
    :keys [screen-bounds server-key]}]
  (let [show-matchmaking-window (fx/sub-val context :show-matchmaking-window)]
    {:fx/type :stage
     :showing (boolean show-matchmaking-window)
     :title (str u/app-name " Matchmaking")
     :icons skylobby.fx/icons
     :on-close-request {:event/type :spring-lobby/dissoc
                        :key :show-matchmaking-window}
     :width ((fnil min matchmaking-window-width) (:width screen-bounds) matchmaking-window-width)
     :height ((fnil min matchmaking-window-height) (:height screen-bounds) matchmaking-window-height)
     :scene
     {:fx/type :scene
      :stylesheets (fx/sub-ctx context skylobby.fx/stylesheet-urls-sub)
      :root
      (if show-matchmaking-window
        {:fx/type matchmaking-view
         :server-key server-key}
        {:fx/type :pane
         :pref-width matchmaking-window-width
         :pref-height matchmaking-window-height})}}))

(defn matchmaking-window [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
    (tufte/p :matchmaking-window
      (matchmaking-window-impl state))))