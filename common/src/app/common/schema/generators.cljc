;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.schema.generators
  (:refer-clojure :exclude [set subseq uuid for filter map let boolean])
  #?(:cljs (:require-macros [app.common.schema.generators]))
  (:require
   [app.common.pprint :as pp]
   [app.common.schema.registry :as sr]
   [app.common.uri :as u]
   [app.common.uuid :as uuid]
   [clojure.test :as ct]
   [clojure.core :as c]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as tg]
   [clojure.test.check.properties :as tp]
   [clojure.test.check.clojure-test :as ctg]
   [cuerdas.core :as str]
   [malli.generator :as mg]))

(defn- get-testing-var
  []
  (c/let [testing-vars #?(:clj ct/*testing-vars*
                          :cljs (:testing-vars ct/*current-env*))]
    (first testing-vars)))

(defn- get-testing-sym
  [var]
  (c/let [tmeta (meta var)]
    (:name tmeta)))

(defn default-reporter-fn
  "Default function passed as the :reporter-fn to clojure.test.check/quick-check.
  Delegates to clojure.test/report."
  [{:keys [type] :as args}]
  (case type
    :complete
    (ct/report {:type ::complete ::params args})

    :failure
    (ct/report {:type ::fail ::params args})

    :shrunk
    (ct/report {:type ::thrunk ::params args})

    nil))

(defn inc-report-counter
  "Increments the named counter in *report-counters*, a ref to a map.
  Does nothing if *report-counters* is nil."
  ([name]
   (when ct/*report-counters*
     (dosync (commute ct/*report-counters* update-in [name] (fnil inc 0)))))

  ([name num]
   (when ct/*report-counters*
     (dosync (commute ct/*report-counters* update-in [name] (fnil + 0) num)))))


(defmethod ct/report ::complete
  [{:keys [::params] :as m}]
  (inc-report-counter :pass)
  (c/let [tvar (get-testing-var)
          tsym (get-testing-sym tvar)]
    (println "Generative test:" (str "'" tsym "'")
             (str "(pass=TRUE, tests=" (:num-tests params)  ", seed=" (:seed params)  ")"))))

(defmethod ct/report ::thrunk
  [{:keys [::params] :as m}]
  (c/let [smallest (-> params :shrunk :smallest vec)]
    (println)
    (println "Failed with params:")
    (pp/pprint smallest)))

(defmethod ct/report ::fail
  [{:keys [::params] :as m}]
  (println "==============")
  (pp/pprint params)
  (println "==============")
  (inc-report-counter :fail)
  (c/let [tvar (get-testing-var)
          tsym (get-testing-sym tvar)]

    (println)
    (println "Generative test:" (str "'" tsym "'")
             (str "(pass=FALSE, tests=" (:num-tests params)  ", seed=" (:seed params)  ")"))))

(defmacro for
  [bindings & body]
  `(tp/for-all ~bindings ~@body))

(defmacro let
  [& params]
  `(tg/let ~@params))

(defn check!
  [p & {:keys [num] :or {num 20} :as options}]
  (c/let [result (tc/quick-check num p (assoc options :reporter-fn default-reporter-fn :max-size 50))
          total-failed (:failing-size result)]

    (ct/is (= num (:num-tests result)))
    (ct/is (true? (:pass? result)))))

(defn sample
  ([g]
   (mg/sample g {:registry sr/default-registry}))
  ([g opts]
   (mg/sample g (assoc opts :registry sr/default-registry))))

(defn generate
  ([g]
   (mg/generate g {:registry sr/default-registry}))
  ([g opts]
   (mg/generate g (assoc opts :registry sr/default-registry))))

(defn generator
  ([s]
   (mg/generator s {:registry sr/default-registry}))
  ([s opts]
   (mg/generator s (assoc opts :registry sr/default-registry))))

(defn filter
  [pred gen]
  (tg/such-that pred gen 100))

(defn small-double
  [& {:keys [min max] :or {min -100 max 100}}]
  (tg/double* {:min min, :max max, :infinite? false, :NaN? false}))

(defn small-int
  [& {:keys [min max] :or {min -100 max 100}}]
  (tg/large-integer* {:min min, :max max}))

(defn word-string
  []
  (as-> tg/string-ascii $$
    (tg/resize 10 $$)
    (tg/fmap (fn [v] (apply str (re-seq #"[A-Za-z]+" v))) $$)
    (tg/such-that (fn [v] (>= (count v) 4)) $$ 100)
    (tg/fmap str/lower $$)))

(defn word-keyword
  []
  (->> (word-string)
       (tg/fmap keyword)))

(defn email
  []
  (->> (word-string)
       (tg/such-that (fn [v] (>= (count v) 4)))
       (tg/fmap str/lower)
       (tg/fmap (fn [v]
                  (str v "@example.net")))))


(defn uri
  []
  (tg/let [scheme (tg/elements ["http" "https"])
           domain (as-> (word-string) $
                    (tg/such-that (fn [x] (> (count x) 5)) $ 100)
                    (tg/fmap str/lower $))
           ext    (tg/elements ["net" "com" "org" "app" "io"])]
    (u/uri (str scheme "://" domain "." ext))))

(defn uuid
  []
  (->> tg/small-integer
       (tg/fmap (fn [_] (uuid/next)))))

(defn subseq
  "Given a collection, generates \"subsequences\" which are sequences
  of (not necessarily contiguous) elements from the original
  collection, in the same order. For collections of distinct elements
  this is effectively a subset generator, with an ordering guarantee."
  ([elements]
   (subseq [] elements))
  ([dest elements]
   (->> (apply tg/tuple (repeat (count elements) tg/boolean))
        (tg/fmap (fn [bools]
                   (into dest
                         (comp
                          (c/filter first)
                          (c/map second))
                         (c/map list bools elements)))))))

(def any tg/any)
(def boolean tg/boolean)

(defn set
  [g]
  (tg/set g))

(defn elements
  [s]
  (tg/elements s))

(defn one-of
  [& gens]
  (tg/one-of (into [] gens)))

(defn fmap
  [f g]
  (tg/fmap f g))

(defn mcat
  [f g]
  (tg/bind g f))

(defn tuple
  [& opts]
  (apply tg/tuple opts))
