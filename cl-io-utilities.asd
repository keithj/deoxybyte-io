
(defsystem cl-io-utilities
    :name "cl-io-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :depends-on (:cl-gp-utilities)
    :components
    ((:module :cl-io-utilities
              :pathname "src/"
              :components ((:file "package")
                           (:file "cl-io-utilities"
                                  :depends-on ("package"))))))
