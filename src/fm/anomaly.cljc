(ns fm.anomaly
  (:require
   [clojure.alpha.spec :as s]
   [fm.lib :as lib]))

(s/def ::args
  (s/and
   map?
   (fn [{::keys [spec]}]
     (= spec ::args))))

(s/def ::ret
  (s/and
   map?
   (fn [{::keys [spec]}]
     (= spec ::ret))))

(s/def ::rel
  (s/and
   map?
   (fn [{::keys [spec]}]
     (= spec ::rel))))

(s/def ::throw
  (s/and
   map?
   (fn [{::keys [spec data]}]
     (and
      (= spec ::throw)
      (instance? Throwable data)))))

(defn contains-anomaly?*
  [recur? xs]
  (lib/reduce*
   recur?
   (fn [acc x]
     (if (or
          (true? acc)
          (s/valid?
           (s/or
            ::args  ::args
            ::ret   ::ret
            ::rel   ::rel
            ::throw ::throw)
           x))
       (reduced true)
       false))
   false
   xs))

(defn recd-recur?
  [_ x]
  (vector? x))

(def recd-anomaly?*
  (partial
   contains-anomaly?*
   recd-recur?))

(s/def ::received
  (s/and
   vector?
   not-empty
   recd-anomaly?*))

(s/def :fm/anomaly
  (s/or
   ::args     ::args
   ::ret      ::ret
   ::rel      ::rel
   ::throw    ::throw
   ::received ::received))
