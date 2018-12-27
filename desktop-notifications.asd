(asdf:defsystem #:desktop-notifications
    :serial t
    :description "desktop-notifications for StumpWM"
    :author "Voob of Doom <vod@njk.li>"
    :license "GPLv3"
    :depends-on (:stumpwm
                 ;; notify is loaded from stumpwm-contrib
                 :notify
                 :str
                 :cl-ppcre
                 :cl-hash-util
                 :access
                 :mito
                 :ironclad
                 :babel)
    :components ((:file "package")
                 (:file "desktop-notifications")))
