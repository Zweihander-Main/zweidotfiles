;;; +ui.el -- ~/.doom.d/+ui.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains all general ui, theme, and view-layer configuration.
;;;
;;; Code:

(setq doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "Iosevka SS09 Extended" :size 15)
      doom-unicode-font (font-spec :family "Iosevka Term SS09 Extended" :size 15)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-serif-font doom-variable-pitch-font
      org-ellipsis "▼"
      display-line-numbers-type t
      line-spacing 0.1
      garbage-collection-messages nil
      split-height-threshold nil ;; Prefer vertical split
      split-width-threshold 0
      window-min-width 2
      window-min-height 1)

(doom-themes-org-config)
(doom-init-extra-fonts-h)

(use-package! org-variable-pitch
  :after org
  :config
  ;; Functions

  ;; Fix issues with jit-lock in org-capture buffer when variable-pitch fonts are used:
  (defun zwei/disable-org-variable-pitch-minor-mode ()
    "Disable variable pitch mode."
    (org-variable-pitch-minor-mode -1))

  ;; Hooks
  (add-hook! 'org-mode-hook #'(+org-pretty-mode org-variable-pitch-minor-mode))
  (add-hook! 'org-capture-mode-hook #'(zwei/disable-org-variable-pitch-minor-mode))

  ;; Vars
  (setq org-variable-pitch-fixed-font "Iosevka SS09 Extended"))


(add-hook! 'org-agenda-mode-hook #'(solaire-mode hl-line-mode))

(custom-set-faces!
  '(highlight :background "DarkOrange3")
  '(org-roam-link :inherit org-link
                  :foreground "DarkOrange3")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +ui.el ends here
