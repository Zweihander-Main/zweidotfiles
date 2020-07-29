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

;;; Code:

;; ======================
;;   General + Theming
;; ======================

(load! "lisp/org-variable-pitch.el")

(setq user-full-name "Zweihänder"
      user-mail-address "zweidev@zweihander.me"
      doom-theme 'doom-tomorrow-night
      doom-font (font-spec :family "Iosevka SS09 Extended" :size 15)
      doom-unicode-font (font-spec :family "Iosevka Term SS09 Extended" :size 15)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-serif-font doom-variable-pitch-font
      org-variable-pitch-fixed-font "Iosevka SS09 Extended"
      org-ellipsis "▼"
      display-line-numbers-type t
      line-spacing 0.1)

(doom-themes-org-config)
(doom-init-extra-fonts-h)

(add-hook! 'org-mode-hook #'(+org-pretty-mode org-variable-pitch-minor-mode))
(add-hook! 'org-agenda-mode-hook #'(solaire-mode hl-line-mode))

(custom-set-faces!
  '(org-roam-link :inherit org-link
                  :foreground "DarkOrange3")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number))


;; ===============
;;   Directories
;; ===============

;; Load machine specific directories
(load! "~/.doom.d/machine_config.el")
;; Set all directories around org
(setq org-roam-directory org-directory
      default-directory org-directory
      deft-directory org-roam-directory
      org-journal-dir (concat org-directory "/dailies"))


;; ===============
;;   Org-related
;; ===============

;; ispell configuration
(setq ispell-list-command "--list"
      ispell-extra-args '("--sug-mode=fast"))

;; General org configuration
(setq org-hide-emphasis-markers t)

;; Org (journal, agenda, ect.)
(after! org
  ;; Agenda and capture
  (map! :g "<f1>" (lambda () (interactive) (org-agenda nil " ")))
  (require 'find-lisp)
  (setq zwei/org-agenda-directory (concat org-roam-directory "/gtd/")
        org-agenda-files (find-lisp-find-files zwei/org-agenda-directory "\.org$")
        +org-capture-todo-file (concat zwei/org-agenda-directory "inbox.org")
        org-agenda-start-with-log-mode t
        org-agenda-block-separator nil
        org-fast-tag-selection-single-key t
        org-capture-templates
        `(("i" "inbox"
           entry
           (file ,+org-capture-todo-file)
           "* TODO %?")
          ("c" "org-protocol-capture"
           entry
           (file ,+org-capture-todo-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)
          ("w" "Weekly Review"
           entry
           (file+olp+datetree ,(concat zwei/org-agenda-directory "reviews.org"))
           (file ,(concat zwei/org-agenda-directory "templates/weekly_review.org")))))


  (defun zwei/org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (jethro/bulk-process-entries))

  (defun zwei/org-inbox-capture ()
    "Shortcut to org-capture->inbox."
    (interactive)
    "Capture a an inbox task."
    (org-capture nil "i"))

  (map! :leader
        (:prefix-map ("n" . "notes")
         :desc "Inbox entry" "i" #'zwei/org-inbox-capture))

  (map! :after org-agenda
        :map org-agenda-mode-map
        :localleader
        "p" #'zwei/org-process-inbox)

  ;; Journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t)

  ;; Logging
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Tagging -- currently used for just where tasks take place, not status
  (setq org-tag-alist '((:startgroup . "place")
                        ("@work" . ?w)
                        ("@play" . ?p)
                        ("@down" . ?d)
                        ("@end" . ?e)
                        (:endgroup . "place")))

  ;; Filing
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 0)
                             ("work.org" :level . 0)
                             ("reading.org" :level . 0 )
                             ("projects.org" :maxlevel . 1))))

;; ====================================================================================Jethro test

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the EFFORT property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  "Bulk process entries in agenda."
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))


(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO."
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

(use-package! org-clock-convenience
  ;;  :bind (:map org-agenda-mode-map
  ;;         ("<S-up>" #'org-clock-convenience-timestamp-up)
  ;;         ("<S-down>" #'org-clock-convenience-timestamp-down)
  ;;         ("o" #'org-clock-convenience-fill-gap)
  ;;         ("e" #'org-clock-convenience-fill-gap-both))
  )

(after! org-agenda
  :config
  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "To Refile")
                                              (org-agenda-files '(,(concat zwei/org-agenda-directory "inbox.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(concat zwei/org-agenda-directory "emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat zwei/org-agenda-directory "someday.org")
                                                                  ,(concat zwei/org-agenda-directory "projects.org")
                                                                  ,(concat zwei/org-agenda-directory "next.org")))
                                              ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Projects")
                                              (org-agenda-files '(,(concat zwei/org-agenda-directory "projects.org")
                                                                  ,(concat zwei/org-agenda-directory "work.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat zwei/org-agenda-directory "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))


;; ===========================================================End
;; Org-roam customization
(after! org-roam
  (setq  org-roam--extract-titles '(title alias)
         org-roam-tag-sources '(prop all-directories)
         org-roam-index-file (concat org-roam-directory "/20200724000434-index.org")
         org-roam-capture-templates
         '(("d" "default"
            plain
            (function org-roam--capture-get-point)
            "%?"
            :file-name "%<%Y%m%d%H%M%S>-${slug}"
            :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n- related :: \n\n* "
            :unnarrowed t))
         org-roam-capture-ref-templates
         '(("r" "ref"
            plain
            (function org-roam-capture--get-point)
            "%?"
            :file-name "websites/${slug}"
            :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}\n- related :: \n\n* "
            :unnarrowed t))))

;; Org-roam-server
(use-package! org-roam-server
  :after org-roam
  :config (setq org-roam-server-host "127.0.0.1"
                org-roam-server-port 38080
                org-roam-server-export-inline-images t
                org-roam-server-authenticate nil
                org-roam-server-network-poll t
                org-roam-server-network-arrows nil
                org-roam-server-network-label-truncate t
                org-roam-server-network-label-truncate-length 60
                org-roam-server-network-label-wrap-length 20))

;; Anki-editor
(use-package! anki-editor
  :after org
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (defun filter-out-p (str _ _)
    "Filter out <p> tags from STR when exporting Anki notes."
    (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                              "" str))
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))

;; Deft customization
(after! deft
  (setq deft-use-filter-string-for-filename t
        deft-recursive t))

;; Enable emacs to open links in Windows
(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program cmd-exe
          browse-url-generic-args cmd-args
          browse-url-browser-function 'browse-url-generic)))


;; ================
;;   Code related
;; ================

;; format-all-buffer for code formatting
;; Note that formatters should be loaded on PATH
(use-package! format-all)

;;; config.el ends here
