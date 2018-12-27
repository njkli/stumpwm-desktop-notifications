(defpackage #:desktop-notifications
  (:use #:cl
        #:mito
        #:stumpwm
        #:cl-hash-util)
  (:export *known-app-icons*
           *db-file*
           on
           off
           reset
           menu-show-all-msgs)
  (:shadow :command-menu))
