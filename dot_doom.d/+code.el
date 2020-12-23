;;; +code.el -- ~/.doom.d/+code.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Code:

;; format-all-buffer for code formatting
;; Note that formatters should be loaded on PATH
(use-package! format-all
  :defer t
  :after elisp-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +code.el ends here
