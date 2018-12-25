;; TODO: handle all notifications, special handling for an2linux stuff
;; Currently an icon file is required, some apps don't supply it though.
;; notify-send provides no icon by default.
(in-package :desktop-notifications)
(access:enable-dot-syntax)

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
                                 (mito.dao::where (:= :app #Dl))))))
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
               (ironclad:digest-file :sha256 fn))))

(defun id-unknown-app (icon sha256)
  (describe icon)
  (describe sha256)
  (let* ((f-basename (concat sha256 ".png"))
         (f-path (uiop:subpathname* (uiop:pathname-directory-pathname icon) f-basename)))
    (if (uiop:file-exists-p f-path)
        (uiop:delete-file-if-exists icon)
        (uiop:rename-file-overwriting-target icon f-path)))
  (format nil "~a" (ppcre:scan-to-strings "^(.{8})" sha256)))

(defun find-or-create-app (icon)
  (let ((found-p (find-dao 'app :sha256 (sha256sum icon))))
    (cond ((null found-p) (progn
                            (let* ((sha256 (sha256sum icon))
                                   (name (id-unknown-app icon sha256)))
                              (create-dao 'app :name name
                                               :sha256 sha256))))
          (found-p found-p))))

(defun notify-to-db (appname icon summary body)
  "Notify to sql"
  (declare (ignore appname))
  (let ((new-msg (create-dao 'msg
                             :app (find-or-create-app icon)
                             :summary summary
                             :body body
                             :new t)))
    (update-mode-line-string)
    (uiop:delete-file-if-exists icon)
    (message "^b^n~A /^b^6~A^n/~%^b^n~A"
             (string-trim '(#\Space #\Tab #\Newline)
                          (remove #\Newline #Dnew-msg.summary))
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
  ;; (load-module "notify")
  (notify:notify-server-off)
  (disconnect-toplevel))

(defun on ()
  (initialize-database)
  (add-hook *quit-hook* 'off)
  (setf notify:*notification-received-hook* (list 'notify-to-db))
  (add-screen-mode-line-formatter #\N 'notifications-mode-line)
  (notify:notify-server-on))

(defun reset ()
  (dolist (dao (select-dao 'msg (includes 'app)))
    (delete-dao dao))
  (dolist (dao (select-dao 'app))
    (delete-dao dao))
  (known-app-icons)
  (update-mode-line-string))
