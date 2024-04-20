(define-configuration buffer
  ((default-modes (append '(noscript-mode) %slot-default%))))

;; Emacs connection
(push
 (merge-pathnames #p".config/emacs/.local/straight/repos/sly/slynk/"
                  (user-homedir-pathname))
 asdf:*central-registry*)
(asdf:load-system :slynk)
(slynk:create-server :port 4008)
(load-after-system :slynk (nyxt-init-file "slynk.lisp"))
