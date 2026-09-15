(ns skylobby.fx.channel-test
  (:require
    [cljfx.api :as fx]
    [cljfx.defaults :as fx.defaults]
    [cljfx.lifecycle :as fx.lifecycle]
    [clojure.test :refer [deftest is]]
    [skylobby.fx.channel :as fx.channel]))


(set! *warn-on-reflection* true)


(deftest channel-document
  (is (some?
        (fx.channel/channel-document
          [{:message-type :ex
            :text "message"
            :timestamp 0
            :username "me"}])))
  (is (some?
        (fx.channel/channel-document
          [{:message-type nil
            :text "message 2"
            :timestamp 1
            :username "me"}]))))


(deftest channel-view-history-impl
  (is (map?
        (fx.channel/channel-view-history-impl
          {:fx/context (fx/create-context nil)}))))


(deftest channel-view-history-impl-with-messages
  (let [now (System/currentTimeMillis)
        context
        (fx/create-context
          {:chat-auto-scroll true
           :chat-font-size 14
           :by-server
           {"server1"
            {:username "me"
             :channels
             {"#main"
              {:messages
               (let [date (- now (* 26 60 60 1000))]
                 [{:message-type nil
                   :username "alice"
                   :text "hello @bob try https://example.com \u000304red\u00030 ..."
                   :timestamp date}
                  {:message-type :info
                   :text "info message"
                   :timestamp (- now 60000)}
                  {:message-type :join
                   :username "bob"
                   :timestamp (- now 30000)}
{:message-type :ex
                    :username "server"
                    :text "* alice locked the game"
                    :timestamp (- now 10000)}
                   {:message-type nil
                    :username "dave"
                    :text "no timestamp"}])}}}}})]
    (is (map?
          (fx.channel/channel-view-history-impl
            {:fx/context context
             :server-key "server1"
             :channel-name "#main"})))))


(deftest channel-view-history-impl-instantiates
  (let [now (System/currentTimeMillis)
        context
        (fx/create-context
          {:by-server
           {"server1"
            {:username "me"
             :channels
             {"#main"
              {:messages
               [{:message-type nil
                 :username "alice"
                 :text "hello @bob https://example.com"
                 :timestamp now}
                {:message-type :join
                 :username "bob"
                 :text ""
                 :timestamp now}
                {:message-type :info
                 :text "info"
                 :timestamp now}
                {:message-type :leave
                 :username "carol"
                 :timestamp now}
                {:message-type :ex
                 :username "carol"
                 :text "* locked"
                 :timestamp now}]}}}}})]
    (is (map?
          (fx.lifecycle/create
            fx.lifecycle/dynamic
            (fx.channel/channel-view-history-impl
              {:fx/context context
               :server-key "server1"
               :channel-name "#main"})
            (fx.defaults/fill-opts nil))))))


(deftest channel-view-input
  (is (map?
        (fx.channel/channel-view-input
          {:fx/context (fx/create-context nil)}))))


(deftest channel-view-users
  (is (map?
        (fx.channel/channel-view-users
          {:fx/context (fx/create-context nil)}))))


(deftest channel-view
  (is (map?
        (fx.channel/channel-view
          {:fx/context (fx/create-context nil)}))))


(deftest irc-paragraphs-plain-text
  (let [paragraphs (@#'fx.channel/irc-paragraphs "hello @bob how are you?")]
    (is (= [{:fill fx.channel/chat-text :text "hello @bob how are you?"}]
           paragraphs))))


(deftest irc-paragraphs-with-color
  (is (= [{:fill fx.channel/chat-text :text "plain "}
          {:fill "#009300" :text "green"}]
         (@#'fx.channel/irc-paragraphs "plain \u000303green\u0003"))))


(deftest cluster-messages-merges-rapid-same-sender
  (let [now (System/currentTimeMillis)
        messages
        [{:message-type nil :username "alice" :text "one" :timestamp now}
         {:message-type nil :username "alice" :text "two" :timestamp (+ now 1000)}
         {:message-type nil :username "bob" :text "hi" :timestamp (+ now 2000)}]]
    (is (= 2
           (count (@#'fx.channel/cluster-messages messages))))
    (is (= "one\ntwo"
           (:text (@#'fx.channel/cluster-message
                    (first (@#'fx.channel/cluster-messages messages))))))
    (is (= (+ now 1000)
           (:timestamp (@#'fx.channel/cluster-message
                         (first (@#'fx.channel/cluster-messages messages))))))))


(deftest cluster-messages-separates-distinct-senders
  (let [now (System/currentTimeMillis)
        messages
        [{:message-type nil :username "alice" :text "one" :timestamp now}
         {:message-type nil :username "bob" :text "two" :timestamp (+ now 1000)}
         {:message-type nil :username "alice" :text "three" :timestamp (+ now 2000)}]]
    (is (= 3
           (count (@#'fx.channel/cluster-messages messages))))))


(deftest cluster-messages-separates-past-merge-window
  (let [now (System/currentTimeMillis)
        messages
        [{:message-type nil :username "alice" :text "one" :timestamp now}
         {:message-type nil :username "alice" :text "two"
          :timestamp (+ now (* 6 60 1000))}]]
    (is (= 2
           (count (@#'fx.channel/cluster-messages messages))))))


(deftest cluster-messages-does-not-merge-system-types
  (let [now (System/currentTimeMillis)
        messages
        [{:message-type :info :username nil :text "one" :timestamp now}
         {:message-type :info :username nil :text "two"
          :timestamp (+ now 1000)}]]
    (is (= 2
           (count (@#'fx.channel/cluster-messages messages))))))
