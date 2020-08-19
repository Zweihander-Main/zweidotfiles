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
      line-spacing 0.1
      garbage-collection-messages nil)

(doom-themes-org-config)
(doom-init-extra-fonts-h)

(add-hook! 'org-mode-hook #'(+org-pretty-mode org-variable-pitch-minor-mode))

;; Fix issues with jit-lock in org-capture buffer when variable-pitch fonts are used:
(defun zwei/disable-org-variable-pitch-minor-mode ()
  "Disable variable pitch mode."
  (org-variable-pitch-minor-mode -1))
(add-hook! 'org-capture-mode-hook #'(zwei/disable-org-variable-pitch-minor-mode))

(add-hook! 'org-agenda-mode-hook #'(solaire-mode hl-line-mode))

(custom-set-faces!
  '(org-roam-link :inherit org-link
                  :foreground "DarkOrange3")
  '(line-number :family "Iosevka Term SS09")
  '(line-number-current-line :inherit line-number))


;; ===============
;;   Directories
;; ===============

;; Load machine specific directories which includes org-directory
(load! "~/.doom.d/machine_config.el")
;; Set all directories around org
(setq default-directory org-directory
      org-roam-directory (concat org-directory "/zettel")
      deft-directory org-roam-directory
      org-journal-dir (concat org-directory "/dailies"))

(defvar zwei/org-agenda-directory (concat org-directory "/gtd")
  "Directory for GTD/work/agenda sytem.")

(defvar zwei/org-agenda-todo-file (concat zwei/org-agenda-directory "/inbox.org")
  "Inbox file for quickly capturing ideas/tasks.")

(defvar zwei/org-agenda-reviews-file (concat zwei/org-agenda-directory "/reviews.org")
  "Reviews files for interval reviews.")

(defvar zwei/org-agenda-templates-directory (concat zwei/org-agenda-directory "/templates")
  "Directory to store templates for GTD system (weekly review template for example).")

(defvar zwei/org-agenda-weekly-review-template-file (concat zwei/org-agenda-templates-directory "/weekly_review.org")
  "Template file for weekly review.")

(defvar zwei/org-agenda-projects-file (concat zwei/org-agenda-directory "/projects.org")
  "File for all tasks that can be put into a given active project.")

(defvar zwei/org-agenda-tickler-file (concat zwei/org-agenda-directory "/tickler.org")
  "File for all tickler tasks. Can include projects but only non-active ones.")

(defvar zwei/org-agenda-next-file (concat zwei/org-agenda-directory "/next.org")
  "File for one-off tasks that should be done immediately or are currently being worked on.")

(setq +org-capture-todo-file zwei/org-agenda-todo-file)


;; ===============
;;   Org-related
;; ===============

;; ispell configuration
(after! ispell
  :config
  (setq ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=fast")))

;; Org
(after! org
  :config
  ;; General
  (setq org-hide-emphasis-markers t
        org-hierarchical-todo-statistics nil
        org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A checklist that needs doing
           "[-](N)"   ; Checklist is in progress
           "[?](W)"   ; Checklist is being held up or paused
           "|"
           "[X](D)")) ; Checklist was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("NEXT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)))

  ;; Capture
  (setq org-capture-templates
        `(("i" "inbox"
           entry
           (file ,zwei/org-agenda-todo-file)
           "* TODO %?")
          ("c" "org-protocol-capture"
           entry
           (file ,zwei/org-agenda-todo-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)
          ("w" "Weekly Review"
           entry
           (file+olp+datetree ,zwei/org-agenda-reviews-file)
           (file ,zwei/org-agenda-weekly-review-template-file))))

  ;; Logging
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Statistics Cookies related
  (defun zwei/org-toggle-statistics-cookies ()
    "Toggle between [/] and [%] type statistics cookies on line."
    (interactive)
    (let ((type (plist-get (zwei/org-find-statistics-cookies) :type)))
      (zwei/org-delete-statistics-cookies)
      (cond ((eq type '%) (zwei/org-insert-statistics-cookies '/))
            ((eq type '/) (zwei/org-insert-statistics-cookies '%)))))

  (defun zwei/org-delete-statistics-cookies ()
    "Delete statistics cookies on line."
    (let ((cookie (zwei/org-find-statistics-cookies)))
      (when cookie
        (delete-region (plist-get cookie :begin) (plist-get cookie :end))
        (save-excursion
          (end-of-line)
          (when (eq (char-before) ? )
            (delete-backward-char 1))))))

  (defun zwei/org-insert-statistics-cookies (&optional type)
    "Insert statistics cookie of optional TYPE % (default) or /."
    (save-excursion
      (setq cur-tags-string (org-get-tags-string))
      (if (not(eq cur-tags-string ""))
          (when (org-back-to-heading t)
           (re-search-forward org-tag-line-re)
           (goto-char (-(match-beginning 1) 1)))
          (end-of-line))
      (insert (concat " " (if (eq type '/) "[/]" "[%]")))
      (org-update-statistics-cookies nil)))

  (defun zwei/org-find-statistics-cookies ()
    "Find statistics cookies on line and return as plist."
    (save-excursion
      (beginning-of-line)
      (let ((end-point (save-excursion (end-of-line) (point)))
            (search-point (point))
            (cookie nil))
        (while (and (not cookie) search-point)
          (setq search-point (re-search-forward "\\[" end-point t))
          (when search-point
            (forward-char -1)
            (setq cookie (cadr (org-element-statistics-cookie-parser)))
            (forward-char 1)))
        (if cookie
            (plist-put cookie :type (if (eq (string-match-p "%" (plist-get cookie :value)) nil) '/ '%))
          cookie))))

  ;; Tagging -- currently used for place and goal
  (setq org-tag-persistent-alist '((:startgroup . "place")
                                   ("@work" . ?w)
                                   ("@play" . ?p)
                                   ("@down" . ?d)
                                   ("@end" . ?e)
                                   (:endgroup . "place")
                                   (:startgroup "goal")
                                   ("1#PHYSICAL" . ?1)
                                   ("2#MENTAL" . ?2)
                                   ("3#CODING" . ?3)
                                   ("4#AUTOMATION" . ?4)
                                   ("5#BUSINESS" . ?5)
                                   ("6#WANKER" . ?6)
                                   (:endgroup "goal"))
        org-fast-tag-selection-single-key nil
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("crypt" "@work" "@play" "@down" "@end")
        org-tag-faces
        '(("1#PHYSICAL"   . (:foreground "#CC2200" :weight bold))
          ("2#MENTAL"     . (:foreground "#00886D" :weight bold))
          ("3#CODING"     . (:foreground "#00441F" :weight bold))
          ("4#AUTOMATION" . (:foreground "#00FF33" :weight bold))
          ("5#BUSINESS"   . (:foreground "#886D00" :weight bold))
          ("6#WANKER"     . (:foreground "#6A3B9F" :weight bold))))


  ;; Filing
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 1)
                             (zwei/org-agenda-tickler-file :maxlevel . 1)
                             (zwei/org-agenda-next-file :level . 0 )))

  ;; Archive related
  (defun zwei/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/KILL" 'file))

  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix ("r" . "refile")
         :desc "Archive all done tasks" "a" #'zwei/org-archive-done-tasks)))

;; Org-journal
(after! org-journal
  :config
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))

;; Org-agenda
(after! org-agenda
  :config
  (require 'find-lisp)
  (setq org-agenda-files (find-lisp-find-files zwei/org-agenda-directory "\.org$")
        org-agenda-start-with-log-mode t
        org-agenda-start-day "-1d"
        org-agenda-span 3
        org-agenda-block-separator nil
        org-agenda-bulk-custom-functions `((?c zwei/org-agenda-process-inbox-item))
        org-agenda-prefix-format
        `((agenda . " %i %-12:c%?-12t% s|%e|")
          (todo . " %i %-12:c|%e|")
          (tags . " %i %-12:c|%e|")
          (search . " %i %-12:c|%e|"))
        org-columns-default-format
        "%40ITEM(Task) %Effort(E Est){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"
        org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day "+0d")
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '(,zwei/org-agenda-todo-file))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files '(,zwei/org-agenda-projects-file
                                       ,zwei/org-agenda-tickler-file
                                       ,zwei/org-agenda-next-file))
                   ))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '(,zwei/org-agenda-projects-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-offs")
                   (org-agenda-files '(,zwei/org-agenda-next-file))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))))

  (defun zwei/org-agenda-bulk-mark-regexp-category (regexp)
    "Mark entries whose category matches REGEXP for future agenda bulk action."
    (interactive "sMark entries with category matching regexp: ")
    (let ((entries-marked 0) txt-at-point)
      (save-excursion
        (goto-char (point-min))
        (goto-char (next-single-property-change (point) 'org-hd-marker))
        (while (and (re-search-forward regexp nil t)
                    (setq category-at-point
                          (get-text-property (match-beginning 0) 'org-category)))
          (if (get-char-property (point) 'invisible)
              (beginning-of-line 2)
            (when (string-match-p regexp category-at-point)
              (setq entries-marked (1+ entries-marked))
              (call-interactively 'org-agenda-bulk-mark)))))
      (unless entries-marked
        (message "No entry matching this regexp."))))

  (defun zwei/org-agenda-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (zwei/org-agenda-bulk-mark-regexp-category "inbox")
    (zwei/bulk-process-entries))

  (defun zwei/org-inbox-capture ()
    "Shortcut to org-capture->inbox."
    (interactive)
    "Capture a an inbox task."
    (org-capture nil "i"))

  (defvar zwei/org-current-effort "1:00"
    "Current effort for agenda items.")

  (defun zwei/org-agenda-set-effort (effort)
    "Set the EFFORT property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " zwei/org-current-effort) nil nil zwei/org-current-effort)))
    (setq zwei/org-current-effort effort)
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
          (funcall-interactively 'org-set-effort nil zwei/org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (defun zwei/org-agenda-process-inbox-item ()
    "Process a single item in the agenda."
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'zwei/org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defun zwei/bulk-process-entries ()
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
                (let (org-loop-over-headlines-in-active-region) (funcall 'zwei/org-agenda-process-inbox-item))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo t)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

  (defun zwei/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO."
    (org-todo "NEXT"))

  (defun zwei/org-agenda-redo-all-buffers ()
    "Refresh/redo all org-agenda buffers."
    (interactive)
    (dolist (buffer (doom-visible-buffers))
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-redo)))))

  (defun zwei/org-agenda-edit-headline ()
    "Perform org-edit-headline on current agenda item."
    (interactive)
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
          (call-interactively #'org-edit-headline)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker)
        (beginning-of-line 1))))

  (defun zwei/org-agenda-break-into-child (child)
    "Create CHILD heading under current heading with the same properties and custom effort."
    (interactive
     (list (read-string "Child task: " nil nil nil)))
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           cur-tags cur-line cur-priority cur-stats-cookies)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (setq cur-line (thing-at-point 'line t))
          (if (string-match org-priority-regexp cur-line)
              (setq cur-priority (match-string 2 cur-line)))
          (setq cur-tags (org-get-tags-string))
          (setq cur-stats-cookies (zwei/org-find-statistics-cookies))
          (if (eq cur-stats-cookies 'nil)
              (zwei/org-insert-statistics-cookies))
          (call-interactively #'+org/insert-item-below)
          (call-interactively #'org-demote-subtree)
          (funcall-interactively 'org-edit-headline child)
          (funcall-interactively 'org-set-tags-to cur-tags)
          (if cur-priority
              (funcall-interactively 'org-priority (string-to-char cur-priority)))
          (org-update-parent-todo-statistics)
          (end-of-line 1))
        (beginning-of-line 1)))
    (zwei/org-agenda-redo-all-buffers)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (and (search-forward child nil t)
           (setq txt-at-point
                 (get-text-property (match-beginning 0) 'txt)))
      (if (get-char-property (point) 'invisible)
          (beginning-of-line 2)
        (when (string-match-p child txt-at-point)
          (call-interactively 'zwei/org-agenda-set-effort)))))

  (add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)
  (add-hook! '(org-after-todo-state-change-hook org-capture-after-finalize-hook) :append #'zwei/org-agenda-redo-all-buffers)

  ;; Key mapping for org-agenda
  (map! :g "<f1>" (lambda () (interactive) (org-agenda nil " ")))

  (map! :leader
        (:prefix-map ("n" . "notes")
         :desc "Inbox entry" "i" #'zwei/org-inbox-capture))

  (map! :after org-agenda
        :map org-agenda-mode-map
        :localleader
        "p" #'zwei/org-agenda-process-inbox
        "e" #'zwei/org-agenda-edit-headline
        "b" #'zwei/org-agenda-break-into-child))

;; Org-clock-convenience
(use-package! org-clock-convenience
  :after org-agenda)

(map! :after org-clock-convenience
      :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)

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
  :defer t
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
  :after org-roam
  :defer t
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


(after! centaur-tabs
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)

       ;; Prevent org and doom related buffers
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*Messages" name)
       (string-prefix-p "*scratch" name)
       (string-prefix-p "*doom" name)
       (string-prefix-p "*tide-server" name)
       (string-prefix-p "*vls" name)
       (string-prefix-p "*Org Agenda" name)
       (string-prefix-p "*Apropos" name)

       ;; Stop org-roam string from showing up
       (string-match-p (concat "[0-9]\\{14\\}" ".*-.*\\.org") name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       ))))

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
(use-package! format-all
  :defer t)

;;; config.el ends here
