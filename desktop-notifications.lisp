(in-package :desktop-notifications)
(access:enable-dot-syntax)

;; FIXME: https://github.com/stumpwm/stumpwm/issues/546
(defun command-menu (screen items command-list &key (prompt "Select:")
                                                 (initial-selection 0)
                                                 extra-keymap)
  (let ((results
          (select-from-batch-menu screen items
                                  :prompt prompt
                                  :allowed-markers (mapcan (lambda (x)
                                                             (if (first x)
                                                                 (list (first x))))
                                                           command-list)
                                  :initial-selection initial-selection
                                  :extra-keymap extra-keymap)))
    (dolist (command-entry command-list)
      (let ((selections (assoc (first command-entry) results))
            (func (second (second command-entry)))
            (options (caddr command-entry)))
        (when selections
          (cond
            ((eql :all options)
             (funcall func (mapcar '(lambda (l) #Dl.data) (cdr selections))))
            ((or (eql options :all) (eql options nil))
             (dolist (l (cdr selections))
               (funcall func #Dl.data)))
            (t (error (format nil "keyword ~s not a valid command option for selection-menu."
                              options)))))))))

(defvar *notifications-mode-line-formatter* nil)
(when (null *notifications-mode-line-formatter*)
  (setf *notifications-mode-line-formatter* #\N))

(defvar *known-app-icons* nil)
(when (null *known-app-icons*)
  (setf *known-app-icons*
        (hash
         ("an2linux"
          "11bd24f2a87232d676b907a44da8d45cb735333ee54b08ebc8902b18e5122c3e")
         ("Signal"
          "5c873e44e334649293e78f8d4b997273ad7c1b57cadf9184dec56289023bfbea")
         ("WhatsApp"
          "ed3c15121d92822d3a1a79605b6f341bbf53825cc7c599777b01b7830491f9aa")
         ("Telegram"
          "200881a43e2742609375e2f8d974161155f1ac2201e4aeed03e0f348d39a712c")
         ("Charging"
          "9ae2195c884e9b2818f260f458cc9d7616e40d6584a0ce973979eaecfbe4fdcd")
         ("G.any"
          "d1c5fcebcd64931f806e08b6862b2598c18ac4de190770ddd5c071c112902ca9")
         ("G.Maps"
          "222e844de959d6a963b9b0467ebb91bc787eb63845918517ee119a883eeb0f1d")
         ("Revolut"
          "e6b1e823cfe1148b714c4a7f32560ca71dce371a7c0073db4d6517e7d2462923")
         ("N26"
          "a832af1edca060d050060062165bb387573c25fa29927865c586c9c61b16b475")
         ("Yandex.Mail"
          "6f5ccbe8ba68eea8eadaabf0aea32d3b55a6b4f38952f3b9a3935d6d7834d56a")
         ("Firefox"
          "d01d10d82a4c0fae771a2b20e877b41b0098289289403e94695d1d75f13b4f5c")
         ("G.playstore"
          "cce653154bef5edbc5636c780b653e3382318cfee3e2add7211462d23ea3c20e")
         ("Samsung updates"
          "d56a6c1d132a0485bf980b67c186bf25d018e2041aa439d25f244bd0eb798d07")
         ("Updates"
          "650d001aa653b252b7b9e33b7c8ae9c881f357d4a8bd03c446267cbd97285a07"))))

(defvar *db-file* nil)
(when (null *db-file*)
  (setf *db-file* (merge-pathnames
                   (format nil
                           "~a/.cache/notifications.sqlite3"
                           (getenv "HOME")))))

(defparameter *notifications-mode-line* nil)

(defclass app ()
  ((name    :col-type (or (:varchar 128) :null)
            :initarg   :name
            :accessor  name)
   (sha256  :col-type (:varchar 64)
            :initarg   :sha256
            :accessor  sha256))
  (:metaclass dao-table-class))

(defclass msg ()
  ((app     :col-type app
            :initarg :app)
   (summary :col-type (or (:varchar 128) :null))
   (body    :col-type :text)
   (new     :col-type :boolean))
  (:metaclass dao-table-class))

(defun update-mode-line-string ()
  (let ((apps (select-dao 'app))
        (notifications-area nil))
    (dolist (l apps)
      (let ((msg-count (length (select-dao 'msg
                                 (includes 'app)
                                 (mito.dao::where (:and
                                                   :new
                                                   (:= :app #Dl)))))))
        (if (> msg-count 0)
            (push (format nil "~a: ~a" #Dl.name msg-count)
                  notifications-area))))
    (setf *notifications-mode-line* (if (> (length notifications-area) 0)
                                        (format nil "~{ ~a~}" (nreverse notifications-area))
                                        (format nil "Msgs: 0")))))

(defun sha256sum (fn)
  (format nil "~{~(~2,'0x~)~}"
          (map 'list
               #'identity
               (if (probe-file fn)
                   (ironclad:digest-file     :sha256 fn)
                   (ironclad:digest-sequence :sha256 (babel:string-to-octets fn))))))

(defun process-unknown-app (icon sha256)
  (let* ((f-basename (concat sha256 ".png"))
         (f-path (uiop:subpathname* (uiop:pathname-directory-pathname icon) f-basename)))
    (unless (uiop:file-exists-p f-path) (uiop:rename-file-overwriting-target icon f-path)))
  (format nil "~a" (ppcre:scan-to-strings "^(.{8})" sha256)))

(defun find-or-create-app (appname icon)
  (let* ((an2linux-p (ppcre:scan "^AN2Linux$" appname))
         (sha256  (if an2linux-p
                      (sha256sum icon)
                      (sha256sum appname)))
         (name    (if an2linux-p
                      (process-unknown-app icon sha256)
                      appname))
         (found-p (find-dao 'app :sha256 sha256)))
    (cond ((null found-p) (create-dao 'app :name name :sha256 sha256))
          (found-p found-p))))

(defun notify-to-db (appname icon summary body)
  "Notify to sql"
  (let ((new-msg (create-dao 'msg
                             :app (find-or-create-app appname icon)
                             :summary summary
                             :body body
                             :new t)))
    (update-mode-line-string)
    (uiop:delete-file-if-exists icon)
    (message "^b^n~A /^b^6~A^n/~%^b^n~A"
             (str:trim (remove #\Newline #Dnew-msg.summary))
             #Dnew-msg.app.name
             #Dnew-msg.body)))

(defun known-app-icons ()
  (maphash (lambda (k v)
             (let ((rec (find-dao 'app :name k)))
               (unless rec (create-dao 'app :name k :sha256 v))))
           *known-app-icons*))

(defun notifications-mode-line (&rest r)
  (declare (ignore r))
  (if (null *notifications-mode-line*)
      (update-mode-line-string))
  (format nil "~a" *notifications-mode-line*))

(defun initialize-database ()
  (connect-toplevel :sqlite3 :database-name *db-file*)
  (ensure-table-exists 'app)
  (ensure-table-exists 'msg)
  (known-app-icons))

(defun off ()
  (notify:notify-server-off)
  (disconnect-toplevel))

(defun on ()
  (initialize-database)
  (add-hook *quit-hook* 'off)
  (setf notify:*notification-received-hook* (list 'notify-to-db))
  (add-screen-mode-line-formatter *notifications-mode-line-formatter* 'notifications-mode-line)
  (notify:notify-server-on))

(defun reset ()
  (dolist (dao (select-dao 'msg (includes 'app)))
    (delete-dao dao))
  (dolist (dao (select-dao 'app))
    (delete-dao dao))
  (known-app-icons)
  (update-mode-line-string))

(defun crud-msg-all ()
  (select-dao 'msg (includes 'app)))

(defun crud-msg-for-app (appname)
  (select-dao 'msg (includes 'app)
              (mito.dao::where
               (:= :app (find-dao 'app :name appname)))))

(defun crud-msg-delete (rec)
  (delete-dao rec))

(defun crud-msg-acknowledge (rec)
  (setf #Drec.new nil)
  (update-dao rec))

(defun crud-app-all ()
  (select-dao 'app))

(defun crud-app-delete (rec)
  (dolist (l (crud-msg-for-app #Drec.name))
    (crud-msg-delete l)))

(defun crud-app-acknowledge (rec)
  (dolist (l (crud-msg-for-app #Drec.name))
    (crud-msg-acknowledge l)))

(defparameter *commands-msg*
  '((#\d 'crud-msg-delete)
    (#\a 'crud-msg-acknowledge)))

(defparameter *commands-app*
  '((#\d 'crud-app-delete)
    (#\a 'crud-app-acknowledge)))

(defun command-menu-msg-format (items)
  (entries-from-nested-list
   (mapcar (lambda (i) (list
                        (format nil "~15a ~a: ~a"
                                (str:concat "[" #Di.app.name " ]")
                                #Di.summary
                                (str:substring 0 64 #Di.body))
                        #Di))
           items)))

(defun command-menu-app-format (items)
  (entries-from-nested-list
   (mapcar (lambda (i) (list
                        (format nil "~15a: ~3a (~a)"
                                (str:concat "[ " #Di.name " ]")
                                (length (crud-msg-for-app #Di.name))
                                (str:substring 0 8 #Di.sha256))
                        #Di))
           items)))

(defvar *menu-prompt-str*
  "[d] - DEL, [a] - ACK, [u] - UNMARK, [RET] - apply")

(defun menu-show-all-msgs ()
  (command-menu (current-screen)
                (command-menu-msg-format (crud-msg-all))
                *commands-msg*
                :prompt *menu-prompt-str*)
  (update-mode-line-string))

(defun menu-show-msgs-for-app (appname)
  (command-menu (current-screen)
                (command-menu-msg-format (crud-msg-for-app appname))
                *commands-msg*
                :prompt *menu-prompt-str*)
  (update-mode-line-string))

(defun menu-show-apps-as-categories ()
  (command-menu (current-screen)
                (command-menu-app-format (crud-app-all))
                *commands-app*
                :prompt *menu-prompt-str*)
  (update-mode-line-string))

;; TODO: app add/edit interface
;; (let ((rec (first (select-dao 'app (mito.dao::where (:like :sha256 "%e6b1%"))))))
;;   (print #Drec.sha256))
