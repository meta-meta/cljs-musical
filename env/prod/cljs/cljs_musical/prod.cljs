(ns cljs-musical.prod
  (:require [cljs-musical.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
