(ns skylobby.fx.battles-window
  (:require
    [clojure.string :as string]
    skylobby.fx
    [skylobby.fx.battles-table :as fx.battles-table]
    [spring-lobby.fx.font-icon :as font-icon]
    [spring-lobby.util :as u]))


(set! *warn-on-reflection* true)


(def window-key :battles)

(def battles-window-width 1600)
(def battles-window-height 1000)


(def battles-window-keys
  (concat
    [:css :filter-battles]
    fx.battles-table/battles-table-keys))


(defn battles-window-view
  [{:keys [battles css filter-battles screen-bounds server-key show-battles-window window-states] :as state}]
  (let [_ screen-bounds]
    {:fx/type :stage
     :showing show-battles-window
     :title (str u/app-name " Chat " server-key)
     :icons skylobby.fx/icons
     :on-close-request {:event/type :spring-lobby/dissoc
                        :key :show-battles-window}
     :x (skylobby.fx/fitx screen-bounds (get-in window-states [window-key :x]))
     :y (skylobby.fx/fity screen-bounds (get-in window-states [window-key :y]))
     :width (skylobby.fx/fitwidth screen-bounds (get-in window-states [window-key :width]) battles-window-width)
     :height (skylobby.fx/fitheight screen-bounds (get-in window-states [window-key :height]) battles-window-height)
     :on-width-changed (partial skylobby.fx/window-changed window-key :width)
     :on-height-changed (partial skylobby.fx/window-changed window-key :height)
     :on-x-changed (partial skylobby.fx/window-changed window-key :x)
     :on-y-changed (partial skylobby.fx/window-changed window-key :y)
     :scene
     {:fx/type :scene
      :stylesheets (skylobby.fx/stylesheet-urls css)
      :root
      {:fx/type :v-box
       :children
       [{:fx/type :h-box
         :alignment :center-left
         :children
         (concat
           [
            {:fx/type :label
             :text (str "Battles (" (count battles) ")")}
            {:fx/type :pane
             :h-box/hgrow :always}
            {:fx/type :label
             :text (str " Filter: ")}
            {:fx/type :text-field
             :text (str filter-battles)
             :on-text-changed {:event/type :spring-lobby/assoc
                               :key :filter-battles}}]
           (when-not (string/blank? filter-battles)
             [{:fx/type :button
               :on-action {:event/type :spring-lobby/dissoc
                           :key :filter-battles}
               :graphic
               {:fx/type font-icon/lifecycle
                :icon-literal "mdi-close:16:white"}}]))}
        (merge
          {:fx/type fx.battles-table/battles-table
           :v-box/vgrow :always}
          (select-keys state fx.battles-table/battles-table-keys))]}}}))
