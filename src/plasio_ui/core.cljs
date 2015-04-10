(ns plasio-ui.core
  (:require [plasio-ui.intro :as intro]
            [figwheel.client :as fw]
            [cljs.core.async :as async :refer [<! put!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defonce app-data (atom {:snapshots (vec (repeat 100 {}))
                         :backtrack 0.5 }))

(def ^:private greyhound-server "http://52.4.221.249:8080")
(def ^:private greyhound-pipeline  "mini-shred")

(def ^:private loaders
  (js-obj "point" (js/PlasioLib.Loaders.GreyhoundPipelineLoader. greyhound-server greyhound-pipeline)
          "overlay" (js/PlasioLib.Loaders.MapboxLoader.)
          "transform" (js/PlasioLib.Loaders.TransformLoader.)))

(def ^:private greyhound-bbox
  (js/Array -10755071.77716193 4894622.884751885 -9979898.42756837 5405017.135473464))

;; define some data API end-points to handle stuff

(def ^:private ^:dynamic *theme*
  {:tick-color          "rgba(204,204,204,0.3)"
   :round-radius        "3px"
   :background-color    "RGBA(204, 204, 204, 0.1)"
   :backtrack-base-color "RGBA(255, 102, 0, 0.5)"
   :backtrack-base-color-disabled "RGBA(120,120,120,0.5)"
   :backtrack-marker-color "rgba(255,102,0,1.0)"})

(def ^:private marker-defaults {:height 50
                                :width  40
                                :color  "rgba(0,255,0,1.0)"})

(defn- make-marker [{:keys [style marker-height marker-width]} f]
  (let [height (or marker-height (:height marker-defaults))
        width  (or marker-width  (:width  marker-defaults))
        color  (:marker-color *theme*)]
    [:div.marker {:style (merge {:position :absolute
                                 :height (str height "px")
                                 :width  (str width  "px")
                                 :cursor "pointer"
                                 :background-color color}
                                style)
                  :on-click f}]))

(defn- offset [index total]
  (/ index total))

(defn- make-tick [tick index total]
  [:div.tick {:style {:position "absolute"
                      :width  "1px"
                      :height "5%"
                      :top    "65%"
                      :background-color (:tick-color *theme*)
                      :left   (str (+ 3 (* 94 (offset index total))) "%")}} ""])

(defn- target-offsets [target]
  (js/console.log target)
  (let [lst (.getClientRects target)
        t   (aget lst 0)]
    [(.-left t) (.-top t) (.-width t) (.-height t)]))

(defn- click-percent [[ox oy w h] evt]
  (/ (- (.-clientX evt) ox) w))

(defn- to-parent [node parent-id]
  (loop [n node]
    (js/console.log n)
    (println (.-id n))
    (if (nil? n)
      nil
      (if (= (.-id n) parent-id)
        n
        (recur (.-parentNode n))))))

(defn- clamp [n x v]
  (max (min x v) n))

(defn- make-backtrack-point [{:keys [percent-chan snapshots]} owner]
  (letfn [(update-percent [e]
            (let [tb (om/get-state owner :trackbase)
                  p  (clamp 0 1 (click-percent tb e))]
              (put! percent-chan p)
              (om/set-state! owner :backtrack p)))

          (mouse-move [e]
            (when (om/get-state owner :tracking)
              (update-percent e)))

          (mouse-up [e]
            (detach-handlers)
            (om/set-state! owner :tracking false)
            (update-percent e))

          (mouse-down [e]
            (om/set-state! owner :tracking true)
            (om/set-state! owner :trackbase (target-offsets (to-parent (.-target e) "backtrack")))
            (attach-handlers)
            (update-percent e))

          (detach-handlers []
            (.removeEventListener js/document "mousemove" mouse-move)
            (.removeEventListener js/document "mouseup" mouse-up))

          (attach-handlers []
            (.addEventListener js/document "mousemove" mouse-move)
            (.addEventListener js/document "mouseup" mouse-up))]
    (reify
      om/IInitState
      (init-state [_]
        {:tracking false
         :backtrack 1})

      om/IRenderState
      (render-state [_ {:keys [backtrack]}]
        (let [ss (seq snapshots)
              bg-key (if ss :backtrack-base-color :backtrack-base-color-disabled)]
          (html
           [:div#backtrack
            {:style {:position "absolute"
                     :left "3%"
                     :right "3%"
                     :cursor (if ss  "ew-resize" "default")
                     :top "20%"
                     :height "35%"}
             :on-mouse-down mouse-down}
            [:div.backtrack-base
             {:style {:position "absolute"
                      :width "100%"
                      :background-color (get *theme* bg-key)
                      :height "20%"
                      :border-radius (:round-radius *theme*)
                      :top "40%"
                      }}]
            (when ss
              [:div.backtrack-marker
               {:style {:position "absolute"
                        :width "10px"
                        :margin-left "-5px"
                        :left (str (* 100 backtrack) "%")
                        :border-radius (:round-radius *theme*)
                        :height "100%"
                        :background-color (:backtrack-marker-color *theme*)}}])]))))))

(defn time-travel-widget [{:keys [height snapshots] :as app} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:percent-chan (async/chan)})

    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :percent-chan)]
        ;; start the loop to get stuff
        ;;
        (go (loop [m (<! c)]
              (let [c (count snapshots)]
                (when (pos? c)
                  (let [index (js/Math.floor (* c m))])))
              (recur (<! c))))))

    om/IRenderState
    (render-state [_ {:keys [percent-chan current-index] :as state}]
      (html [:div.timeline {:style {:position "relative"
                                    :height (str (or height 100) "px")
                                    :border-radius (:round-radius *theme*)
                                    :background-color (:background-color *theme*)}}
             ;; place all time ticks
             (when-let [ticks (seq snapshots)]
               (let [c (count ticks)]
                 (vec (cons :div.ticks (mapv make-tick ticks (range) (repeat c))))))


             ;; finally draw the overlay for how far we are on the timeline
             (om/build make-backtrack-point {:percent-chan percent-chan
                                             :snapshots    snapshots})]))))

(defn- init-plasio-renderer [elem host]
  (let [r (js/renderer.core.createRenderer elem)
        resize-handler  (fn [] (.setRenderViewSize r (.-offsetWidth elem) (.-offsetHeight elem)))]
    (println (.. loaders -point -constructor -provides))
    (println (.. loaders -overlay -constructor -provides))
    (println (.. loaders -transform -constructor -provides))
    (.addLoader r (.. loaders -point -constructor))               
    (.addLoader r (.. loaders -overlay -constructor))               
    (.addLoader r (.. loaders -transform -constructor))               
    (aset js/window "onresize" resize-handler)
    (resize-handler)
    r))



(defn make-peer [[id name] owner transfer]
  (let [self?     (= owner :self)
        owner?    (= id owner)
        name      (if owner? (str name " (Presenter)") name)]
   [:div.peer-item {:class (when owner? "owner")}
    (if self?
      [:a {:href "javascript:"
           :onClick (partial transfer id)} name]
      name)]

   ))

(defn make-owner
  ([renderer node]
     (make-owner renderer node {} nil nil))
  ([renderer node props peer statef]
     (let [props (or props (js-obj))
           orbital (.-orbital props)
           camera  (js/PlasioLib.Cameras.Orbital. node renderer
                                                  (fn [eye target]
                                                    (.setEyePosition renderer eye)
                                                    (.setTargetPosition renderer target))
                                                  orbital)
           prop-listener (.addPropertyListener renderer (js/Array)
                                               (fn [state]
                                                 (when (ifn? statef) (statef state))
                                                 (when peer (.broadcastState peer state))))
           policy (doto (js/PlasioLib.QuadTreeNodePolicy. loaders renderer greyhound-bbox)
                    (.on "bbox"
                         (fn [bbox]
                           (let [range (mapv - (.-maxs bbox) (.-mins bbox))
                                 range (to-array range)
                                 off (mapv #(+ %2 (/ %1 2)) range (.-mins bbox))
                                 off (to-array off)
                                 x (aget range 0)
                                 y (aget range 1)
                                 far-plane (js/Math.sqrt (+ (* x x) (* y y)))]
                             (println "FAR PLANE:" far-plane)
                             (.updateCamera renderer 0 (js-obj "far" far-plane))
                             (when-not orbital
                               (.setHint camera range))))))

           key-handler (fn [e]
                         (let [code (or (.. e -which) (.. e -keyCode))
                               point-size (- code 48)]
                           (.setRenderOptions renderer (js-obj "pointSize" point-size))))]

       (.addEventListener js/document "keypress" key-handler)

       (.start policy)
       {:camera camera
        :prop-listener prop-listener
        :policy policy
        :key-handler key-handler})))

(defn unmake-owner [renderer state]
  (let [{:keys [camera prop-listener policy key-handler]} state]
    (when camera
      (.stop camera))
    (when prop-listener
      (.removePropertyListener renderer prop-listener))
    (when policy
      (.stop policy))
    (when key-handler
      (.removeEventListener js/document "keypress" key-handler)))) 

(defn plasio-app [{:keys [done-chan name session host]} owner]
  (let [state-props (atom {})]
    (reify
      om/IInitState
      (init-state [_]
        {:notify-chan (async/chan)})

      om/IDidMount
      (did-mount [_]
        (let [node (om/get-node owner "plasio-app")
              renderer (init-plasio-renderer node host)]
          (swap! state-props assoc :renderer renderer :node node)
          (when-let [ps (:pending-state @state-props)]
            (println "PENDING:" ps)
            (.applyState renderer ps)
            (swap! state-props dissoc :pending-state)))) 

      om/IWillMount
      (will-mount [_]
        (let [peer (js/PlasioLib.P2PNode. name)]
          (doto peer
            (.on "ready"
                 (fn []
                   (om/set-state! owner :peer peer)
                   (om/set-state! owner :status :joining)
                   (.join peer session)))

            (.on "error"
                 (fn []
                   ;; the peer errored, so switch to single user mode
                   (om/set-state! owner :owner :self)

                   (let [{:keys [renderer node]} @state-props]
                     (when (and renderer node)
                       (let [p (make-owner renderer node)]
                         (swap! state-props assoc :owner-props p))))))

            (.on "joined"
                 (fn []
                   (om/set-state! owner :status :joined)
                   (om/set-state! owner :peers  {})))

            (.on "left"
                 (fn []
                   (om/set-state! owner :status :left)
                   (put! done-chan true)))

            (.on "peer-joined"
                 (fn [_ id]
                   (om/update-state! owner :peers #(assoc %1 id id))
                   (when-let [last-state (:last-state @state-props)]
                     (println "BROADCAST!!!")
                     (.broadcastState peer last-state))))

            (.on "peer-left"
                 (fn [_ id]
                   (om/update-state! owner :peers  #(dissoc %1 id))))

            (.on "peer-name"
                 (fn [_ id name]
                   (om/update-state! owner :peers #(assoc %1 id name))))

            (.on "state"
                 (fn [_ state]
                   (println "STATE!")
                   (if-let [r (:renderer @state-props)]
                     (.applyState r state)
                     (swap! state :pending-state state))))

            (.on "owner"
                 (fn [_ own]
                   (om/set-state! owner :owner own)
                   (unmake-owner (:renderer @state-props)
                                 (:owner-props @state-props))
                   (swap! state-props dissoc :owner-props)))

            (.on "self-owner"
                 (fn [_ props]
                   (om/set-state! owner :owner :self)
                   (let [{:keys [renderer node peer]} @state-props]
                     (when (and renderer node peer)
                       (let [p (make-owner renderer node props peer #(swap! state-props assoc :last-state %1))]
                         (swap! state-props assoc :owner-props p)))))))

          (swap! state-props assoc :peer peer)))

      om/IRenderState
      (render-state [_ {:keys [peer status peers owner]}]
        (let [transfer-func (fn [id]
                              (println @state-props)
                              (let [props (when-let [op (:owner-props @state-props)]
                                            (js-obj "orbital" (.serialize (:camera op))))]
                               
                                (js/console.log "Transfer:" props)
                                (.transferOwnership peer id props)))]
          (html [:div
                 [:div.plasio-app {:ref "plasio-app"
                                   :key "render-area"
                                   :style {:position "absolute"
                                           :overflow "hidden"
                                           :left "0px"
                                           :right "0px"
                                           :top "0px"
                                           :bottom "0px"}}]
                 (when (= status :joining)
                   [:div.status {} "Joining... Please wait"])

                 (when (= status :joined)
                   [:div.peers
                    [:div.session-name session]
                    [:div (if (= owner :self)
                            (str "You are the Presenter")
                            (str "Viewing " (get peers owner)))
                     [:hr]
                     (if (zero? (count peers))
                       [:div.no-peers "No active peers"]
                       (vec (concat [:div.peer-list]
                                    (map make-peer peers (repeat owner) (repeat transfer-func)))))]])]))))))   

(defn- default-host []
  (.. js/window -location -hostname))

(defn main-app [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:action-chan (async/chan)
       :done-chan (async/chan)})

    om/IWillMount
    (will-mount [_]
      (let [c (om/get-state owner :action-chan)]
        (go (loop [[action [name session host]] (<! c)]
              (om/set-state! owner :app {:name name
                                         :session session
                                         :host (if (clojure.string/blank? host) (default-host) host)})
              (recur (<! c)))))

      (let [c (om/get-state owner :done-chan)]
        (go (loop [m (<! c)]
              (om/set-state! owner :app nil)
              (recur (<! c))))))

    om/IRenderState
    (render-state [_ {:keys [action-chan done-chan app]}]
      (if app
        (om/build plasio-app (assoc app :done-chan done-chan))
        (om/build intro/intro-page {:action-chan action-chan})))))

;; define your app data so that it doesn't get over-written on reload
;; (defonce app-data (atom {}))

;;(om/root time-travel-widget app-data {:target (.getElementById js/document "slider-area")})
(comment (let [name (.. js/window -location -search)
       owner-mode? (clojure.string/blank? name)]
   (om/root plasio-app {:action (if owner-mode? :create :join)
                        :params [(if owner-mode? "uday" (subs name 1)) "hello-world"]} {:target (.getElementById js/document "app")})))

(om/root main-app {} {:target (.getElementById js/document "app")})

(fw/start {
  :on-jsload (fn []
               ;; (stop-and-start-my app)
               )})
