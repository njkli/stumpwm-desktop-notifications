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
           menu-show-all-msgs
           menu-show-apps-as-categories
           menu-show-msgs-for-app)
  (:shadow :command-menu))
