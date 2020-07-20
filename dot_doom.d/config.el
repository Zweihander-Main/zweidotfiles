;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zweihänder"
      user-mail-address "zweidev@zweihander.me")

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/mnt/c/Users/Zweihander/Documents/Org"
      org-roam-directory org-directory
      default-directory org-directory
      deft-directory org-directory
      org-journal-dir (concat org-directory "/dailies"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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

;; ispell configuration
(setq ispell-list-command "--list"
      ispell-extra-args '("--sug-mode=fast"))

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


;; Org-journal customization
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y"
      org-journal-enable-agenda-integration t)

;; Deft customization
(setq deft-use-filter-string-for-filename t
      deft-recursive t)
