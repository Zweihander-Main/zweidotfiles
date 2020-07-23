;;; config.el --- Lives at $DOOMDIR/config.el
;;; Commentary:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Code:

;; ===========
;;   General
;; ===========

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zweihänder"
      user-mail-address "zweidev@zweihander.me")


;; ===========
;;   Theming
;; ===========

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace"i :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


;; ===============
;;   Directories
;; ===============

;; Load machine specific directories
(load! "~/.doom.d/machine_config.el")
;; Set all directories around org
(setq org-roam-directory org-directory
      default-directory org-directory
      deft-directory org-directory
      org-journal-dir (concat org-directory "/dailies"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; ===============
;;   Org-related
;; ===============

;; ispell configuration
(setq ispell-list-command "--list"
      ispell-extra-args '("--sug-mode=fast"))

;; General org configuration
(setq org-hide-emphasis-markers t)

;; Org-roam customization
(setq org-roam--extract-titles '(title alias)
      org-roam-tag-sources '(prop all-directories)
      org-roam-index-file "index.org"
      org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n- related :: \n\n* "
         :unnarrowed t)))
(custom-set-faces!
  `(org-roam-link :inherit org-link :foreground "dark orange"))
(after! org-roam
      (setq org-roam-capture-ref-templates
            '(("r" "ref" plain (function org-roam-capture--get-point)
               "%?"
               :file-name "websites/${slug}"
               :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}\n- related :: \n\n* "
               :unnarrowed t))))

;; Org-roam-server
(use-package! org-roam-server
  :after org-roam
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 38080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; Org-journal customization
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-enable-agenda-integration t)

;; Deft customization
(setq deft-use-filter-string-for-filename t
      deft-recursive t)

;; Enable emacs to open links in Windows
(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)))

;;; config.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org-roam-server))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-roam-link ((t (:inherit org-link :foreground "dark orange"))))
 '(outline-1 ((t (:weight normal))))
 '(outline-2 ((t (:weight normal))))
 '(outline-3 ((t (:weight normal))))
 '(outline-4 ((t (:weight normal))))
 '(outline-5 ((t (:weight normal))))
 '(outline-6 ((t (:weight normal)))))
