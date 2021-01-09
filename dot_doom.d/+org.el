;;; +org.el -- ~/.doom.d/+org.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains all org-related config (agenda, roam, ect.).
;;;
;;; Code:


;; ========
;;  ispell
;; ========
(after! ispell
  :config
  (setq ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=fast")))


;; =============
;;  org-capture
;; =============
(after! org-capture
  (setq org-capture-templates
        `(("i" "inbox"
           entry
           (file ,zwei/org-agenda-todo-file)
           "* TODO %?")
          ("n" "next"
           entry
           (file ,zwei/org-agenda-next-file)
           "* NEXT %? %^g:@work: %^{Effort}p ")
          ("c" "org-protocol-capture"
           entry
           (file ,zwei/org-agenda-todo-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)
          ("w" "Weekly Review"
           entry
           (file+olp+datetree ,zwei/org-agenda-reviews-file)
           (file ,zwei/org-agenda-weekly-review-template-file)))))


;; =====
;;  org
;; =====
(after! org

  ;; Functions

  ;;; Statistics Cookies related

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
            (delete-char -1))))))

  (defun zwei/org-insert-statistics-cookies (&optional type)
    "Insert statistics cookie of optional TYPE % (default) or /."
    (save-excursion
      (let (cur-tags-string (org-get-tags-string))
        (if (not(eq cur-tags-string ""))
            (when (org-back-to-heading t)
              (re-search-forward org-tag-line-re)
              (goto-char (-(match-beginning 1) 1)))
          (end-of-line))
        (insert (concat " " (if (eq type '/) "[/]" "[%]")))
        (org-update-statistics-cookies nil))))

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

  (defun zwei/find-gtd-file ()
    "Find a file in `zwei/org-agenda-directory'."
    (interactive)
    (doom-project-find-file zwei/org-agenda-directory))

  ;;; Archive related

  (defun zwei/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/KILL" 'file))

  ;; Mappings

  (map! :after org
        :map org-mode-map
        :localleader
        :prefix "r"
        :desc "Archive all done tasks" "a" #'zwei/org-archive-done-tasks)

  (map! :after org
        :leader
        :prefix "n"
        :desc "Find in gtd" "g" #'zwei/find-gtd-file)

  ;; General config

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

  (add-to-list 'org-modules 'org-habit-plus)


  ;; Logging
  (setq org-log-done 'time
        org-log-into-drawer t)


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
          ("2#MENTAL"     . (:foreground "#008F40" :weight bold))
          ("3#CODING"     . (:foreground "#42A5F5" :weight bold))
          ("4#AUTOMATION" . (:foreground "#00FF33" :weight bold))
          ("5#BUSINESS"   . (:foreground "#F5C400" :weight bold))
          ("6#WANKER"     . (:foreground "#6A3B9F" :weight bold))))


  ;; Filing
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 1)
                             (zwei/org-agenda-tickler-file :maxlevel . 1)
                             (zwei/org-agenda-next-file :level . 0 ))))


;; Disable fancy-priorities for now
(after! org-fancy-priorities
  :config
  (setq org-fancy-priorities-mode -1))


;; ===============
;;   Org-journal
;; ===============
(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))


;; =============
;;   Org-habit
;; =============
(after! org-habit-plus
  (defcustom org-habit-scheduled-past-days nil
    "Value to use instead of `org-scheduled-past-days', for habits only.

If nil `org-scheduled-past-days' is used.

Setting this to say 10000 is a way to make sure habits always show up
as a reminder, even if you set `org-scheduled-past-days' to a
small value because you regard scheduled items as a way of
\"turning on\" TODO items on a particular date, rather than as a
means of creating calendar-based reminders."
    :group `org-habit
    :type '(choice integer (const nil))
    :package-version '(Org . "9.3")
    :safe (lambda (v) (org (integerp v) (null n)))))


;; ===============
;;   Org-agenda
;; ===============
(use-package! org-agenda
  :defer t
  :after org
  :defer-incrementally mu4e org-roam) ;; get mu4e and roam loading when agenda opened

(after! org-agenda

  ;; Variables

  (defvar zwei/org-current-effort "1:00"
    "Current effort for agenda items.")

  ;; Functions

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
    (zwei/org-agenda-bulk-mark-regexp-category "")
    (zwei/org-agenda-bulk-process-entries))

  (defun zwei/org-inbox-capture ()
    "Shortcut to org-capture->inbox."
    (interactive)
    "Capture a an inbox task."
    (org-capture nil "i"))


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
    (interactive)
    (org-with-wide-buffer
     (let ((answer nil)
           (continue nil)
           (type "todo"))
       (while (not continue)
         (setq answer
               (read-answer "Item options: [v]iew/[e]dit/[t]odo/[d]one/[a]:note/[l]ink/[k]ill/[n]ext/[i]nfo/[RET]:Continue "
                            '(("view" ?v "View in minibuffer")
                              ("edit" ?e "Edit the headline of the item")
                              ("todo" ?t "Change TODO state of item")
                              ("done" ?d "Mark done and archive")
                              ("note" ?a "Add a note to the item")
                              ("link" ?l "Open link and mark done")
                              ("kill" ?k "Kill current line")
                              ("next" ?n "Put in next file")
                              ("info" ?i "Conver to list item and refile under item")
                              ("continue" ?\r "Continue processing"))))
         (cond ((string= answer "continue") (setq continue t))
               ((string= answer "view") (org-agenda-tree-to-indirect-buffer 1)  )
               ((string= answer "link") (setq type "link"
                                              continue t)
                )
               ((string= answer "next") (setq type "next"
                                              continue t))
               ((string= answer "done") (setq type "done"
                                              continue t))
               ((string= answer "info") (setq type "info"
                                              continue t))
               ((string= answer "kill") (setq type "kill"
                                              continue t))
               ((string= answer "edit") (call-interactively #'zwei/org-agenda-edit-headline))
               ((string= answer "todo") (org-agenda-todo))
               ((string= answer "note") (call-interactively #'org-agenda-add-note))))
       (cond ((string= type "todo")
              (progn
                (org-agenda-set-tags)
                (org-agenda-priority)
                (call-interactively 'zwei/org-agenda-set-effort)
                (org-agenda-refile nil nil t)))
             ((string= type "kill")
              (progn
                (org-agenda-todo "KILL")
                (org-agenda-archive)))
             ((string= type "done")
              (progn
                (org-agenda-todo "DONE")
                (org-agenda-archive)))
             ((string= type "next")
              (progn
                (org-agenda-todo "NEXT")
                (org-agenda-set-tags)
                (org-agenda-priority)
                (call-interactively 'zwei/org-agenda-set-effort)
                (org-agenda-refile nil
                                   (list (concat (car (last (split-string zwei/org-agenda-next-file "/"))) "/") ;; should be "next.org/"
                                         zwei/org-agenda-next-file nil nil) t)))
             ((string= type "link")
              (let ((ret-msg ""))
                (setq ret-msg (org-agenda-open-link))
                (unless (and (stringp ret-msg )(string= ret-msg "No link to open here"))
                  (org-agenda-todo "DONE")
                  (org-agenda-archive))))
             ((string= type "info")
              (let ((org-refile-target-verify-function))
                (org-agenda-refile nil nil t)
                (let* ((bookmark (plist-get org-bookmark-names-plist :last-refile))
                       (pos (bookmark-get-position bookmark))
                       (filename (bookmark-get-filename bookmark))
                       (buffer (get-file-buffer filename))
                       (inhibit-read-only t))
                  (org-with-remote-undo buffer
                    (with-current-buffer buffer
                      (widen)
                      (goto-char pos)
                      (debug)
                      (org-todo "")
                      (org-toggle-item t))))))))))

  (defun zwei/org-agenda-bulk-process-entries ()
    "Bulk process entries in agenda."
    (interactive)
    ;; Set temporary variable lookup -- set hl-line-face from hl-line to hl-line-active
    (when (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (hl-line-highlight)
              (highlight-lines-matching-regexp (string-trim (thing-at-point 'line t)) 'highlight)
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
           cur-tags cur-line cur-priority cur-stats-cookies txt-at-point)
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

  ;; Hooks

  (add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

  ;; Mappings

  (map! :g "<f1>" (lambda () (interactive) (org-agenda nil "1") (evil-goto-first-line)))
  (map! :g "<f2>" (lambda () (interactive) (org-agenda nil "2") (evil-goto-first-line)))

  (map! :leader
        :prefix "n"
        :desc "Inbox entry" "i" #'zwei/org-inbox-capture)

  (map! :map org-agenda-mode-map
        :localleader
        :desc "Process inbox items" "p" #'zwei/org-agenda-process-inbox
        :desc "Process marked items" "P" #'zwei/org-agenda-bulk-process-entries
        :desc "Process current item" "i" #'zwei/org-agenda-process-inbox-item
        :desc "Edit headline" "e" #'zwei/org-agenda-edit-headline
        :desc "Toggle entry text mode" "E" #'org-agenda-entry-text-mode
        :desc "Break into child tasks" "b" #'zwei/org-agenda-break-into-child)

  ;; Config

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
        `(("1" "Agenda"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day "+0d")
                     (org-deadline-warning-days 365)))
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
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))
          ("2" "Inbox"
           ((todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-prefix-format " |%e|")
                   (org-agenda-files '(,zwei/org-agenda-todo-file)))))))))

;; Org-clock-convenience for agenda
(use-package! org-clock-convenience
  :after org-clock)

(map! :after org-clock-convenience
      :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)


;; ==========
;;  org-roam
;; ==========
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
            :unnarrowed t)))
  (org-roam-db-build-cache)) ;; Seems to be neccesary

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

;; Deft -- in use for org-roam
(after! deft
  (setq deft-use-filter-string-for-filename t
        deft-recursive t))


;; =============
;;  Anki-editor
;; =============
(use-package! anki-editor
  :after org-roam
  :defer t
  :config
  (setq anki-editor-anki-connect-listening-port 38040)
  (defun filter-out-p (str backend comm)
    "Filter out <p> tags from STR when exporting Anki notes."
    (replace-regexp-in-string "\n<p>\\|</p>\n\\|<p>\\|</p>"
                              "" str))
  (setq anki-editor--ox-anki-html-backend
        (org-export-create-backend :parent 'html
                                   :filters '((:filter-paragraph . filter-out-p)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +org.el ends here
