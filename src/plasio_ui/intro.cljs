(ns plasio-ui.intro
  (:require [figwheel.client :as fw]
            [cljs.core.async :as async :refer [put!]]
            [om.core :as om :include-macros true]
            [clojure.string :refer [blank?]]
            [sablono.core :as html :refer-macros [html]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn intro-page [{:keys [action-chan]} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:block-create? true})
    
    om/IDidMount
    (did-mount [_]
      (let [name-node (om/get-node owner "name")]
        (.focus name-node)))
    
    om/IRenderState
    (render-state [_ {:keys [block-create?]}]
      (let [params #(vector (.-value (om/get-node owner "name"))
                            (clojure.string/lower-case (.-value (om/get-node owner "session-name")))
                            (.-value (om/get-node owner "host-addr")))
            key-down (fn [e]
                       (om/set-state! owner :block-create? (some blank? (take 2 (params)))))
            action (fn [type]
                     (fn []
                       (println type (params))
                       (put! action-chan [type (params)])))]

        (html [:div.intro-page
               [:h1 "plasio.js"]
               [:h2 "Hobu Technology Preview"]
               [:div.form
                [:input {:type "text"
                         :ref  "name"
                         :on-key-up key-down
                         :placeholder "Your Name"}]
                [:input {:type "text"
                         :ref  "session-name"
                         :on-key-up key-down
                         :placeholder "Session Name"}]

                [:input {:type "text"
                         :style {:margin-top "2.5em"}
                         :ref  "host-addr"
                         :on-key-up key-down
                         :placeholder "Data host address (leave blank for default)"}]

                [:div.controls
                 [:button {:type "button"
                           :disabled block-create?
                           :on-click (action :join)} "Join"]]]])))))
