(ns skylobby.fx.channel
  (:require
   [cljfx.api :as fx]
   [clojure.string :as string]
   java-time
   [skylobby.chat :as chat]
   [skylobby.fx :refer [monospace-font-family]]
   [skylobby.fx.ext :refer [ext-focused-by-default
                            ext-recreate-on-key-changed
                            ext-scroll-on-create
                            ext-with-auto-complete-word
                            ext-with-context-menu
                            with-scroll-text-flow-prop]]
   [skylobby.fx.font-icon :as font-icon]
   [skylobby.fx.rich-text :as fx.rich-text]
   [skylobby.fx.sub :as sub]
   [skylobby.fx.tooltip-nofocus :as tooltip-nofocus]
   [skylobby.fx.user :as fx.user]
   [skylobby.fx.virtualized-scroll-pane :as fx.virtualized-scroll-pane]
   [skylobby.util :as u]
   [taoensso.timbre :as log]
   [taoensso.tufte :as tufte])
  (:import
   (java.time LocalDateTime)
   (java.util TimeZone)
   (javafx.scene.control TextField)
   (javafx.scene.input Clipboard ClipboardContent)
   (org.fxmisc.richtext.model ReadOnlyStyledDocumentBuilder SegmentOps StyledSegment)
   (org.nibor.autolink LinkExtractor LinkSpan LinkType)))


(set! *warn-on-reflection* true)


(def max-history 1000)


(def known-spads-commands
  ["!map"
   "!notify"
   "!pick"
   "!ring"
   "!set"
   "!status"
   "!vote"
   "!wakeup"])


(def default-font-size 16)
(def font-icon-size 20)

(defn font-size-or-default [font-size]
  (int (or (when (number? font-size) font-size)
           default-font-size)))

(def irc-colors ; same are on fx.clj , here just for checking
  #{"00"
    "01"
    "02"
    "03"
    "04"
    "05"
    "06"
    "07"
    "08"
    "09"
    "10"
    "11"
    "12"
    "13"
    "14"
    "15"})

(def
  ^LinkExtractor
  link-extractor
  (-> (LinkExtractor/builder)
      (.linkTypes #{LinkType/URL LinkType/WWW})
      (.build)))

(defn text-style [font-size]
  {:-fx-font-family monospace-font-family
   :-fx-font-size (font-size-or-default font-size)})


(defn segment
  [text style]
  (StyledSegment. text style))

(defn channel-document
  ([messages]
   (channel-document messages nil))
  ([all-messages {:keys [color-my-username highlight my-username]}]
   (let [messages (take-last max-history all-messages)
         highlight (->> highlight
                        (filter some?)
                        (map string/trim)
                        (remove string/blank?))
         builder (ReadOnlyStyledDocumentBuilder. (SegmentOps/styledTextOps) "")]
     (when-not (= (count all-messages)
                  (count messages))
       (.addParagraph builder
                      ^java.util.List
                      (vec [(segment (str "< " (- (count all-messages) (count messages)) " previous messages >") ["text" "skylobby-chat-message"])])
                      ^java.util.List
                      []))
     (doseq [message (filter :timestamp messages)]
       (let [{:keys [message-type text timestamp username]} message]
         (.addParagraph builder
                        ^java.util.List
                        (vec
                         (concat
                          [(segment
                            (str "[" (u/format-hours timestamp) "] ")
                            ["text" "skylobby-chat-time"])
                           (segment
                            (str
                             (case message-type
                               :ex (str "* " username " ")
                               :join (str username " has joined")
                               :leave (str username " has left")
                               :info (str "* " text)
                      ; else
                               (str username ": ")))
                            ["text" (if (= :info message-type)
                                      "skylobby-chat-info"
                                      (str "skylobby-chat-username"
                                           (if message-type
                                             (str "-" (name message-type))
                                             (when (and color-my-username (= username my-username))
                                               "-me"))))])]
                          (when (or (not message-type)
                                    (= :ex message-type))
                            (let [links (seq (.extractLinks link-extractor text))
                                  segments (if links
                                             (:segs
                                              (reduce
                                               (fn [{:keys [i segs]} ^LinkSpan link]
                                                 (let [begin (if link (.getBeginIndex link) (count text))
                                                       end (when link (.getEndIndex link))]
                                                   {:i end
                                                    :segs
                                                    (concat
                                                     segs
                                                     (when (and i begin (not= i begin))
                                                       [{:text-segment (subs text i begin)
                                                         :is-url false}])
                                                     (when link
                                                       [{:text-segment (subs text begin end)
                                                         :is-url true}]))}))
                                               {:i 0
                                                :segs []}
                                               (concat links [nil])))
                                             [{:text-segment text
                                               :is-url false}])]
                              (->> segments
                                   (mapv
                                    (fn [{:keys [is-url text-segment]}]
                                      (segment
                                       (str text-segment)
                                       ["text"
                                        (if is-url
                                          "skylobby-chat-message-url"
                                          (if (and (seq highlight)
                                                   (some (fn [substr]
                                                           (and text-segment substr
                                                                (string/includes? (string/lower-case text-segment)
                                                                                  (string/lower-case substr))))
                                                         highlight))
                                            "skylobby-chat-message-highlight"
                                            (str "skylobby-chat-message"
                                                 (when message-type
                                                   (str "-" (name message-type))))))])))))

                            (map
                             (fn [[_all _ _irc-color-code text-segment]]
                               (if (contains? irc-colors _irc-color-code)
                                 (segment
                                  (str text-segment)
                                  ["text", (str "skylobby-chat-message-irc-" _irc-color-code)])
                                 (segment (str text-segment)
                                          ["text", (str "skylobby-chat-message"
                                                        (when message-type
                                                          (str "-" (name message-type))))])))
                             (re-seq #"([\u0003](\d\d))?([^\u0003]*)" text)))))
                        ^java.util.List
                        [])))
     (when-not (seq messages)
       (.addParagraph builder
                      ^java.util.List
                      (vec [(segment "< no messages >" ["text" "skylobby-chat-message"])])
                      ^java.util.List
                      []))
     (.build builder))))

(defn- get-text-area
  ^org.fxmisc.richtext.StyleClassedTextArea
  [^javafx.event.Event event area-id-css]
  (let [^javafx.scene.control.MenuItem menu-item (.getTarget event)
        ^javafx.scene.control.ContextMenu context-menu (.getParentPopup menu-item)
        ^javafx.scene.Node node (.getOwnerNode context-menu)
        ^javafx.scene.Scene scene (.getScene node)
        ^javafx.scene.Parent root (.getRoot scene)]
    (first (.lookupAll root area-id-css))))

(defn visible-messages-sub [context server-key channel-name]
  (let [hide-barmanager-messages (fx/sub-val context :hide-barmanager-messages)
        hide-joinas-spec (fx/sub-val context :hide-joinas-spec)
        hide-spads-set (fx/sub-ctx context sub/hide-spads-set)
        hide-vote-messages (fx/sub-val context :hide-vote-messages)
        ignore-users-set (fx/sub-ctx context sub/ignore-users-set server-key)
        filter-fn (partial chat/visible-message?
                           {:hide-barmanager-messages hide-barmanager-messages
                            :hide-joinas-spec hide-joinas-spec
                            :hide-spads-set hide-spads-set
                            :hide-vote-messages hide-vote-messages
                            :ignore-users-set ignore-users-set})
         messages (fx/sub-val context get-in [:by-server server-key :channels channel-name :messages])]
     (->> messages
          (filter filter-fn)
          doall)))


(def chat-background "#313338")
(def chat-text "#dbdee1")
(def chat-muted "#949ba4")
(def chat-name "#f0a02b")
(def chat-mention "#f08422")
(def chat-green "#35d27a")
(def chat-divider "#ed4245")
(def chat-link "#00a8fc")
(def chat-ex "#00c8c8")

(def avatar-colors
  ["#f0a02b" "#5865f2" "#23a55a" "#ed4245" "#eb459e" "#13c2c8" "#9b59b6" "#faa61a" "#00a8fc"])

(def irc-fills
  {"00" "#ffffff"
   "01" "#ffffff"
   "02" "#00007f"
   "03" "#009300"
   "04" "#ff0000"
   "05" "#7f0000"
   "06" "#9c009c"
   "07" "#fc7f00"
   "08" "#ffff00"
   "09" "#00fc00"
   "10" "#009393"
   "11" "#00ffff"
   "12" "#0000fc"
   "13" "#ff00ff"
   "14" "#7f7f7f"
   "15" "#d2d2d2"})


(defn- message-local-date-time [timestamp]
  (LocalDateTime/ofInstant
    (java-time/instant timestamp)
    (.toZoneId (TimeZone/getDefault))))

(defn- message-time-text [timestamp]
  (java-time/format "MM/dd/yy, h:mm a" (message-local-date-time timestamp)))

(defn- divider-day-text [timestamp]
  (java-time/format "MMMM d, yyyy" (message-local-date-time timestamp)))

(defn- divider-day-key [timestamp]
  (java-time/format "yyyyMMdd" (message-local-date-time timestamp)))

(defn- avatar-color [username]
  (let [username (string/lower-case (or username "?"))]
    (nth avatar-colors (mod (hash username) (count avatar-colors)))))

(defn- avatar-label [username]
  (let [initial (string/upper-case (subs (or username "?") 0 1))]
    {:fx/type :label
     :text initial
     :min-width 40
     :min-height 40
     :max-width 40
     :max-height 40
     :alignment :center
     :style {:-fx-background-color (avatar-color username)
             :-fx-background-radius "20"
             :-fx-text-fill "white"
             :-fx-font-weight :bold
             :-fx-font-size 16}}))

(defn- raise-highlight? [text highlight]
  (boolean
    (and (seq highlight)
         (some (fn [substr]
                 (and (seq text)
                      (seq substr)
                      (string/includes? (string/lower-case text)
                                        (string/lower-case substr))))
               highlight))))

(defn- plain-text-node
  ([text] (plain-text-node text chat-text))
  ([text fill]
   {:fx/type :text
    :text text
    :style {:-fx-fill fill}}))

(defn- mention-pill [text {:keys [font-size]}]
  {:fx/type :label
   :text text
   :style {:-fx-font-size (or font-size default-font-size)
           :-fx-font-weight 500
           :-fx-padding "0 3"
           :-fx-background-radius 3
           :-fx-background-color "rgba(240,132,34,0.14)"
           :-fx-text-fill chat-mention}})

(defn- irc-paragraphs [text]
  (->> (re-seq #"([\u0003](\d\d))?([^\u0003]*)" (or text ""))
        (filter (complement (comp string/blank? #(nth % 3))))
       (mapv (fn [[_all _code digits text]]
               {:fill (get irc-fills digits chat-text)
                :text text}))))

(defn- url-paragraphs [text]
  (let [links (seq (.extractLinks link-extractor text))]
    (if-not links
      [{:url? false :text text}]
      (:segs
        (reduce
          (fn [{:keys [i segs]} ^LinkSpan link]
            (let [begin (if link (.getBeginIndex link) (count text))
                  end (when link (.getEndIndex link))]
              {:i end
               :segs
               (concat
                 segs
                 (when (and i begin (not= i begin))
                   [{:url? false :text (subs text i begin)}])
                 (when link
                   [{:url? true :text (subs text begin end)}]))}))
          {:i 0 :segs []}
          (concat links [nil]))))))

(defn- mention-split [text]
  (let [matcher (re-matcher #"@\S+" text)]
    (loop [i 0 out []]
      (if (.find matcher)
        (let [s (.start matcher)
              e (.end matcher)]
          (recur e (cond-> out
                     (<= i s) (conj {:mention? false :text (subs text i s)})
                     true (conj {:mention? true :text (subs text s e)}))))
        (conj out {:mention? false :text (subs text i)})))))

(defn- url-clickable? [text]
  (boolean (re-find #"^https?://" (string/lower-case text))))

(defn- message-content-nodes [{:keys [text]} highlight font-size]
  (let [font-size (int (or (when (number? font-size) font-size) default-font-size))]
    (->> (irc-paragraphs text)
         (mapcat
           (fn [{:keys [fill text]}]
             (mapcat
               (fn [{:keys [url? text]}]
                 (if url?
                   (let [clickable (url-clickable? text)]
                     [{:fx/type :text
                       :text text
                       :cursor (when clickable :hand)
                       :style {:-fx-fill chat-link
                               :-fx-underline true}
                       :on-mouse-clicked (when clickable
                                           {:event/type :spring-lobby/desktop-browse-url
                                            :url text})}])
                   (map
                     (fn [{:keys [mention? text]}]
                       (cond
                         (and (string/trim text)
                              mention?)
                         (mention-pill text {:font-size font-size})
                         (raise-highlight? text highlight)
                         (plain-text-node text "#ff0000")
                         :else
                         (plain-text-node text fill)))
                     (mention-split text))))
               (url-paragraphs text))))
         (remove (comp string/blank? :text))
         vec)))

(defn- message-content [message highlight font-size]
  (let [font-size (int (or (when (number? font-size) font-size) default-font-size))]
    {:fx/type :text-flow
     :max-width ##Inf
     :style {:-fx-font-size font-size}
     :children
     (if (= :ex (:message-type message))
       [{:fx/type :text
         :text (or (:text message) "")
         :style {:-fx-fill chat-ex
                 :-fx-font-style :italic}}]
       (message-content-nodes message highlight font-size))}))

(defn- name-label [{:keys [username message-type] :as message}
                   {:keys [color-my-username my-username]}]
  (let [is-me (and color-my-username (= username my-username))]
    {:fx/type :label
     :text (case message-type
             :ex (str "* " username)
             username)
     :style-class
     [(case message-type
        :ex "skylobby-chat-name-ex"
        (if is-me "skylobby-chat-name-me" "skylobby-chat-name"))]
     :style
     {:-fx-text-fill (case message-type
                       :ex chat-ex
                       (if is-me chat-mention chat-name))}}))

(defn- timestamp-label [timestamp]
  (when timestamp
    {:fx/type :label
     :text (message-time-text timestamp)
:style-class ["skylobby-chat-timestamp"]
      :style {:-fx-text-fill chat-muted
              :-fx-font-size 11
              :-fx-padding "0 0 0 8"}}))

(defn- system-row [text]
  {:fx/type :label
   :text text
   :alignment :center
   :text-alignment :center
   :max-width ##Inf
   :style {:-fx-padding "4 8"
           :-fx-text-fill chat-muted
           :-fx-font-size 14
           :-fx-font-style :italic}})

(defn- copy-message! [text]
  (let [clipboard (Clipboard/getSystemClipboard)
        content (ClipboardContent.)]
    (.putString content (str text))
    (.setContent clipboard content)))

(defn- message-row [{:keys [message-type text timestamp username] :as message} opts]
  {:fx/type ext-with-context-menu
   :props {:context-menu {:fx/type :context-menu
                          :items
                          [{:fx/type :menu-item
                            :text "Copy"
                            :on-action
                            (fn [_]
                              (copy-message! text))}]}}
   :desc
   (case message-type
     (:join :leave)
     (system-row (str username " has " (if (= :join message-type) "joined" "left")))
     (:info)
     (system-row (str "* " text))
     {:fx/type :h-box
      :alignment :top-left
      :max-width ##Inf
      :style-class ["skylobby-chat-message-row"]
      :style {:-fx-padding "4 8 4 6"}
      :children
      [(avatar-label username)
       {:fx/type :v-box
        :h-box/hgrow :always
        :max-width ##Inf
        :style {:-fx-padding "0 0 0 9"}
        :children
        [{:fx/type :h-box
          :alignment :center-left
          :style {:-fx-min-height 22}
          :children (keep identity [(name-label message opts) (timestamp-label timestamp)])}
         (message-content message (or (:highlight opts) []) (:font-size opts))]}]})})

(defn- date-divider [timestamp]
  {:fx/type :stack-pane
   :style {:-fx-padding "6 0"}
   :children
   [{:fx/type :h-box
     :alignment :center-left
     :children
     [{:fx/type :region
       :h-box/hgrow :always
       :style {:-fx-background-color chat-divider
               :-fx-min-height 1
               :-fx-max-height 1}}
      {:fx/type :region
       :min-width 0
       :max-width 0}
      {:fx/type :region
       :h-box/hgrow :always
       :style {:-fx-background-color chat-divider
               :-fx-min-height 1
               :-fx-max-height 1}}]}
    {:fx/type :label
     :text (divider-day-text timestamp)
     :style {:-fx-background-color chat-background
             :-fx-text-fill chat-divider
             :-fx-font-size 12
             :-fx-font-weight :bold
             :-fx-padding "0 8"}}]})

(defn- previous-messages-row [message-count]
  (system-row (str "< " message-count " previous messages >")))

(defn- no-messages-row []
  (system-row "< no messages >"))

(def merge-window-ms (* 5 60 1000))

(defn- mergeable? [prev curr]
  (and (not (case (:message-type curr)
              (:join :leave :info)
              true
              false))
       (= (:username prev) (:username curr))
       (= (:message-type prev) (:message-type curr))
       (some? (:timestamp prev))
       (some? (:timestamp curr))
       (<= (- (:timestamp curr) (:timestamp prev))
           merge-window-ms)))

(defn- cluster-messages [messages]
  (let [[rest-clusters last-cluster]
        (reduce
          (fn [[rest-clusters last-cluster] message]
            (if (and (seq last-cluster)
                     (mergeable? (peek last-cluster) message))
              [rest-clusters (conj last-cluster message)]
              [(if (seq last-cluster)
                 (conj rest-clusters last-cluster)
                 rest-clusters)
               [message]]))
          [[] []]
          messages)]
    (if (seq last-cluster)
      (conj rest-clusters last-cluster)
      rest-clusters)))

(defn- cluster-message [cluster]
  (let [message (peek cluster)]
    (if (next cluster)
      (assoc message :text (string/join "\n" (map :text cluster)))
      message)))

(defn- chat-rows [messages opts]
  (reduce
    (fn [[prev-day rows] cluster]
      (let [message (cluster-message cluster)
            timestamp (:timestamp message)
            day (when timestamp (divider-day-key timestamp))
            rows (if (and day (not= day prev-day))
                  (conj rows (date-divider timestamp))
                  rows)]
        [day (conj rows (message-row message opts))]))
    [nil []]
    (cluster-messages messages)))

(defn channel-view-history-impl
  [{:fx/keys [context]
    :keys [channel-name server-key]}]
  (let [username (fx/sub-val context get-in [:by-server server-key :username])
        chat-auto-scroll (fx/sub-val context :chat-auto-scroll)
        chat-font-size (fx/sub-val context :chat-font-size)
        chat-color-username (fx/sub-val context :chat-color-username)
        chat-highlight-username (fx/sub-val context :chat-highlight-username)
        chat-highlight-words (fx/sub-val context :chat-highlight-words)
        hide-barmanager-messages (fx/sub-val context :hide-barmanager-messages)
        hide-joinas-spec (fx/sub-val context :hide-joinas-spec)
        hide-spads-set (fx/sub-ctx context sub/hide-spads-set)
        hide-vote-messages (fx/sub-val context :hide-vote-messages)
        ignore-users-set (fx/sub-ctx context sub/ignore-users-set server-key)
        messages (fx/sub-ctx context visible-messages-sub server-key channel-name)
        shown (take-last max-history messages)
        other-message-count (- (count messages) (count shown))
        highlight (concat
                    (when chat-highlight-words
                      (string/split chat-highlight-words #"[\s,]+"))
                    (when chat-highlight-username
                      [username]))
        opts {:color-my-username chat-color-username
              :font-size chat-font-size
              :highlight highlight
              :my-username username}
        rows
        (if (seq shown)
          (let [[_prev-day rows] (chat-rows shown opts)]
            (if (pos? other-message-count)
              (into [(previous-messages-row other-message-count)] rows)
              rows))
          [(no-messages-row)])]
    {:fx/type ext-recreate-on-key-changed
     :key {:ignore ignore-users-set
           :joinas-spec hide-joinas-spec
           :server-key server-key
           :spads hide-spads-set
           :vote hide-vote-messages
           :hide-barmanager hide-barmanager-messages}
     :desc
     {:fx/type with-scroll-text-flow-prop
      :props {:auto-scroll [messages chat-auto-scroll]}
      :desc
      {:fx/type :scroll-pane
       :fit-to-width true
       :event-filter {:event/type :spring-lobby/filter-channel-scroll}
       :style-class ["skylobby-chat-history"]
       :style {:-fx-background-color chat-background
               :-fx-border-color "transparent"
               :-fx-fit-to-width true}
       :content
       {:fx/type :v-box
        :style {:-fx-background-color chat-background}
        :children rows}}}}))

(defn channel-view-history
  [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
                 (tufte/p :channel-view-history
                          (channel-view-history-impl state))))


(defn channel-view-text [{:fx/keys [context] :keys [channel-name disable server-key]}]
  (let [message-draft (fx/sub-val context get-in [:message-drafts server-key channel-name])
        history-index (fx/sub-val context get-in [:by-server server-key :channels channel-name :history-index])]
    {:fx/type ext-recreate-on-key-changed
     :key (str history-index)
     :desc
     {:fx/type ext-focused-by-default
      :desc
      {:fx/type fx/ext-on-instance-lifecycle
       :on-created (fn [^TextField text-field]
                     (.positionCaret text-field (count message-draft)))
       :desc
       {:fx/type :text-field
        :disable (boolean disable)
        :id "channel-text-field"
        :text (str message-draft)
        :style {:-fx-control-inner-background "transparent"
                :-fx-text-fill chat-text
                :-fx-prompt-text-fill chat-muted}
        :on-text-changed {:event/type :spring-lobby/assoc-in
                          :path [:message-drafts server-key channel-name]}
        :on-action {:event/type :skylobby.fx.event.chat/send
                    :channel-name channel-name
                    :message message-draft
                    :server-key server-key}
        :on-key-pressed {:event/type :spring-lobby/on-channel-key-pressed
                         :channel-name channel-name
                         :server-key server-key}}}}}))

(defn channel-send-button
  [{:fx/keys [context]
    :keys [channel-name disable server-key]}]
  (let [message-draft (fx/sub-val context get-in [:message-drafts server-key channel-name])]
    {:fx/type :button
     :text "Send"
     :disable (boolean (or disable (string/blank? message-draft)))
     :on-action {:event/type :skylobby.fx.event.chat/send
                 :channel-name channel-name
                 :message message-draft
                 :server-key server-key}}))

(defn channel-view-input
  [{:fx/keys [context]
    :keys [channel-name disable server-key usernames]}]
  (let [chat-auto-complete (fx/sub-val context :chat-auto-complete)
        is-battle-channel (u/battle-channel-name? channel-name)
        mute-path [:mute server-key (if is-battle-channel :battle channel-name)]
        mute (fx/sub-val context get-in mute-path)
        mute-ring (fx/sub-val context get-in [:mute-ring server-key])]
    {:fx/type :h-box
     :style-class ["skylobby-chat-input"]
     :style {:-fx-background-color "#383a40"
             :-fx-background-radius 8
             :-fx-padding 8
             :-fx-spacing 8}
     :children
     (concat
      [{:fx/type channel-send-button
        :channel-name channel-name
        :disable disable
        :server-key server-key}
       {:fx/type ext-recreate-on-key-changed
        :key chat-auto-complete
        :h-box/hgrow :always
        :desc
        (if chat-auto-complete
          {:fx/type ext-with-auto-complete-word
           :props {:auto-complete (concat known-spads-commands (map u/sanitize-filter usernames))}
           :desc {:fx/type channel-view-text
                  :channel-name channel-name
                  :disable disable
                  :server-key server-key}}
          {:fx/type channel-view-text
           :channel-name channel-name
           :disable disable
           :server-key server-key})}]
      (when is-battle-channel
        [{:fx/type :button
          :text ""
          :tooltip
          {:fx/type tooltip-nofocus/lifecycle
           :show-delay skylobby.fx/tooltip-show-delay
           :text
           (str
            (if mute-ring "Enable" "Disable")
            " mute sound for this server")}
          :on-action {:event/type (if mute-ring :spring-lobby/dissoc-in :spring-lobby/assoc-in)
                      :path [:mute-ring server-key]}
          :graphic
          {:fx/type font-icon/lifecycle
           :icon-literal (if mute-ring
                           (str "mdi-volume-off:" font-icon-size ":red")
                           (str "mdi-volume-high:" font-icon-size ":white"))}}])
      [{:fx/type :button
        :text ""
        :tooltip
        {:fx/type tooltip-nofocus/lifecycle
         :show-delay skylobby.fx/tooltip-show-delay
         :text
         (str
          (if mute "Enable" "Disable")
          " tab highlighting on new messages")}
        :on-action {:event/type (if mute :spring-lobby/dissoc-in :spring-lobby/assoc-in)
                    :path mute-path}
        :graphic
        {:fx/type font-icon/lifecycle
         :icon-literal (if mute
                         (str "mdi-message-bulleted-off:" font-icon-size ":red")
                         (str "mdi-message:" font-icon-size ":white"))}}])}))

(defn channel-view-users
  [{:fx/keys [context]
    :keys [channel-name server-key]}]
  (let [channel-users (fx/sub-val context get-in [:by-server server-key :channels channel-name :users])
        server-users (fx/sub-val context get-in [:by-server server-key :users])
        users (->> channel-users
                   keys
                   (sort String/CASE_INSENSITIVE_ORDER)
                   (map (fn [username]
                          (or (get server-users username)
                              {:username username})))
                   vec)]
    {:fx/type :v-box
     :style-class ["skylobby-chat"]
     :min-width 340
     :style {:-fx-background-color "#2b2d31"}
     :children
     [{:fx/type :label
       :text (str (count users) " users in " channel-name)
       :style {:-fx-text-fill "#949ba4"
               :-fx-font-size 11
               :-fx-padding "6 8"}}
      {:fx/type :scroll-pane
       :v-box/vgrow :always
       :fit-to-width true
       :content
       {:fx/type :v-box
        :children
        (mapv
          (fn [user]
            {:fx/type ext-with-context-menu
             :props {:context-menu
                     {:fx/type :context-menu
                      :items
                      [{:fx/type :menu-item
                        :text "Message"
                        :on-action {:event/type :spring-lobby/join-direct-message
                                    :server-key server-key
                                    :username (:username user)}}]}}
             :desc
             (fx.user/user-row user)})
          users)}}]}))


(defn channel-view-impl
  [{:fx/keys [context] :keys [channel-name disable hide-users server-key usernames]}]
  (let [selected-server-tab (fx/sub-val context :selected-server-tab)
        parsed-selected-server-tab (fx/sub-ctx context sub/parsed-selected-server-tab)]
    {:fx/type :h-box
     :children
     (if (or (= server-key selected-server-tab)
             (= server-key parsed-selected-server-tab))
       (concat
[{:fx/type :v-box
           :h-box/hgrow :always
           :style-class ["skylobby-chat"]
           :style {:-fx-background-color "#2b2d31"}
           :children
          [{:fx/type channel-view-history
            :v-box/vgrow :always
            :channel-name channel-name
            :server-key server-key}
           {:fx/type channel-view-input
            :channel-name channel-name
            :disable disable
            :server-key server-key
            :usernames usernames}]}]
        (when (and (not hide-users)
                   channel-name
                   (not (string/starts-with? channel-name "@")))
          [{:fx/type channel-view-users
            :channel-name channel-name
            :server-key server-key}]))
       [])}))


(defn channel-view
  [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
                 (tufte/p :channel-view
                          (channel-view-impl state))))
