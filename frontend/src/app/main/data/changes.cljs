;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.changes
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.exceptions :as ex]
   [app.common.files.changes :as cpc]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cph]
   [app.common.logging :as log]
   [app.common.schema :as sm]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.main.features :as features]
   [app.main.store :as st]
   [app.main.worker :as uw]
   [app.util.time :as dt]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

;; Change this to :info :debug or :trace to debug this module
(log/set-level! :debug)

(def page-change?
  #{:add-page :mod-page :del-page :mov-page})
(def update-layout-attr?
  #{:hidden})

(def commit?
  (ptk/type? ::commit))

(defn update-indexes
  "Given a commit, send the changes to the worker for updating the
  indexes."
  [{:keys [changes] :as commit}]
  (ptk/reify ::update-indexes
    ptk/WatchEvent
    (watch [_ _ _]
      (let [changes (->> changes
                         (map (fn [{:keys [id type page] :as change}]
                                (cond-> change
                                  (and (page-change? type) (nil? (:page-id change)))
                                  (assoc :page-id (or id (:id page))))))
                         (filter :page-id)
                         (group-by :page-id))]

        (->> (rx/from changes)
             (rx/merge-map (fn [[page-id changes]]
                             (log/debug :hint "update-indexes" :page-id page-id :changes (count changes))
                             (uw/ask! {:cmd :update-page-index
                                       :page-id page-id
                                       :changes changes})))
             (rx/ignore))))))

(defn commit
  "Create a commit event instance"
  [{:keys [commit-id redo-changes undo-changes origin save-undo? #_affected-frames
           file-id file-revn page-id undo-group tags stack-undo? source]}]

  (dm/assert!
   "expect valid vector of changes"
   (and (cpc/check-changes! redo-changes)
        (cpc/check-changes! undo-changes)))

  (let [commit-id (or commit-id (uuid/next))
        commit    {:id commit-id
                   :created-at (dt/now)
                   :source (d/nilv source :local)
                   :origin (ptk/type origin)
                   :file-id file-id
                   :file-revn file-revn
                   :changes redo-changes
                   :redo-changes redo-changes
                   :undo-changes undo-changes
                   ;; :frames affected-frames
                   :save-undo? save-undo?
                   :undo-group undo-group
                   :tags tags
                   :stack-undo? stack-undo?}]

    (ptk/reify ::commit
      cljs.core/IDeref
      (-deref [_] commit)

      ptk/UpdateEvent
      (update [_ state]
        (let [current-file-id (get state :current-file-id)
              path            (if (= file-id current-file-id)
                                [:workspace-data]
                                [:workspace-libraries file-id :data])]

          (d/update-in-when state path (fn [file]
                                         (let [file (cpc/process-changes file redo-changes false)
                                               pids (into #{} (map :page-id) redo-changes)]
                                           (reduce #(ctst/update-object-indices %1 %2) file pids)))))))))

(defn- resolve-file-revn
  [state file-id]
  (let [file (:workspace-file state)]
    (if (= (:id file) file-id)
      (:revn file)
      (dm/get-in state [:workspace-libraries file-id :revn]))))

(defn commit-changes
  "Schedules a list of changes to execute now, and add the corresponding undo changes to
   the undo stack.

   Options:
   - save-undo?: if set to false, do not add undo changes.
   - undo-group: if some consecutive changes (or even transactions) share the same
                 undo-group, they will be undone or redone in a single step
   "
  [{:keys [redo-changes undo-changes origin save-undo? undo-group tags stack-undo? file-id]
    :or {save-undo? true
         stack-undo? false
         undo-group (uuid/next)
         tags #{}}
    :as params}]
  (ptk/reify ::commit-changes
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id  (or file-id (:current-file-id state))
            uchg     (vec undo-changes)
            rchg     (vec redo-changes)
            features (features/get-team-enabled-features state)]

        (rx/of (-> params
                   (assoc :undo-group undo-group)
                   (assoc :features features)
                   (assoc :tags tags)
                   (assoc :stack-undo? stack-undo?)
                   (assoc :save-undo? save-undo?)
                   (assoc :file-id file-id)
                   ;; FIXME: maybe move this to handle 100% on the persistence layer ??
                   (assoc :file-revn (resolve-file-revn state file-id))
                   (assoc :undo-changes uchg)
                   (assoc :redo-changes rchg)
                   (commit)))))))
