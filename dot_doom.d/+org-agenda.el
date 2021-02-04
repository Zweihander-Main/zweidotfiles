;;; +org-agenda.el -- ~/.doom.d/+org-agenda.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains all org-related config (agenda, roam, ect.).
;;;
;;; Code:

;; =============
;;   Org-habit
;; =============
(after! org
  (add-to-list 'org-modules 'org-habit-plus))

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
               ((string= answer "link")
                (let ((ret-msg ""))
                  (setq ret-msg (org-agenda-open-link))
                  (unless (and (stringp ret-msg )(string= ret-msg "No link to open here"))
                    (setq type "link"
                          continue t))))
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
              (progn
                (org-agenda-todo "DONE")
                (org-agenda-archive)))
             ((string= type "info")
              (let ((org-refile-target-verify-function)
                    (org-refile-targets '((zwei/org-agenda-projects-file :maxlevel . 2)
                                          (zwei/org-agenda-tickler-file :maxlevel . 2)
                                          (zwei/org-agenda-next-file :level . 1 ))))
                ;; TODO: add in way to add to ideas, herf, english to add, ect. -- need roam refile
                ;; TODO: add in way to defer to bottom
                ;; TODO: Allow for schedule
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

  (defun zwei/org-agenda-current-is-todo-esque ()
    "Returns if current heading is a form of todo"
    (let ((state (org-get-todo-state)))
      (or
       (string= "TODO" state)
       (string= "NEXT" state))))

  (defun zwei/org-agenda-skip-all-siblings-but-first (&optional check-func)
    "Skip all but the first non-done entry.
If CHECK-FUNC is provided, will check using that too."
    (let ((should-skip-entry)
          (all-checks (lambda ()
                        (let ((pass t))
                          (when check-func
                            (save-excursion
                              (when (funcall check-func)
                                (setq pass nil))))
                          (and pass (zwei/org-agenda-current-is-todo-esque))))))
      (unless (funcall all-checks)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (funcall all-checks)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (if (funcall all-checks)
            (condition-case nil
                (progn (evil-org-top) (outline-forward-same-level 1) (point))
              (error (goto-char (point-max))))
          (or (outline-next-heading)
              (goto-char (point-max)))))))

  ;; Hooks

  (add-hook! 'org-clock-in-hook :append #'zwei/set-todo-state-next)

  ;; Mappings -- see custom commands for more

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

  (setq org-agenda-files (directory-files zwei/org-agenda-directory t "\.org$" t)
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
        org-agenda-custom-commands nil)

  ;; Custom commands and their mappings

  (add-to-list 'org-agenda-custom-commands
               `("1" "Agenda"
                 ((agenda ""
                          ((org-agenda-span 1)
                           (org-agenda-start-day "+0d")
                           (org-deadline-warning-days 365)))
                  (todo "NEXT"
                        ((org-agenda-overriding-header "In Progress")
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-tickler-file
                                             ,zwei/org-agenda-next-file))))
                  (todo "TODO"
                        ((org-agenda-overriding-header "One-offs")
                         (org-agenda-files '(,zwei/org-agenda-next-file))
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))))
                  (todo "TODO"
                        ((org-agenda-overriding-header "Projects")
                         (org-agenda-files '(,zwei/org-agenda-projects-file)))))))

  (map! :g "<f1>" (lambda () (interactive) (org-agenda nil "1") (evil-goto-first-line)))

  (add-to-list 'org-agenda-custom-commands
               `("2" "Inbox"
                 ((todo "TODO"
                        ((org-agenda-overriding-header "To Refile")
                         (org-agenda-prefix-format " |%e|")
                         (org-agenda-files '(,zwei/org-agenda-todo-file)))))))

  (map! :g "<f2>" (lambda () (interactive) (org-agenda nil "2") (evil-goto-first-line)))

  (add-to-list 'org-agenda-custom-commands
               `("3" "Work"
                 ((tags "+@work+TODO=\"TODO\"|+@work+TODO=\"NEXT\""
                        ((org-agenda-overriding-header "Work")
                         (org-agenda-skip-function
                          '(zwei/org-agenda-skip-all-siblings-but-first
                            #'(lambda()
                                (org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))))
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-next-file)))) ; no tickler
                  (tags "+@work+TODO=\"WAIT\""
                        ((org-agenda-overriding-header "\nWaiting")
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'deadline 'scheduled 'timestamp))
                         (org-agenda-files '(,zwei/org-agenda-projects-file
                                             ,zwei/org-agenda-next-file)))))))

  (map! :g "<f3>" (lambda () (interactive) (org-agenda nil "3") (evil-goto-first-line)))

  (add-to-list 'org-agenda-custom-commands
               `("x" . "utility searches"))

  (add-to-list 'org-agenda-custom-commands
               `("x1" "weekly recap"
                 (,@(mapcar #'(lambda (tag)
                                `(org-ql-block '(and (or (clocked 7)
                                                         (closed 7))
                                                     (tags ,tag))
                                               ((org-ql-block-header (concat ,tag)))))
                            (hash-table-keys zwei/org-tag-goal-table))
                  (org-ql-block `(and (or (closed 7)
                                          (clocked 7))
                                      (not (tags ,@(hash-table-keys zwei/org-tag-goal-table))))
                                ((org-ql-block-header "OTHER"))))
                 ((org-agenda-files
                   (directory-files zwei/org-agenda-directory t "\\(\.org\\)\\|\\(.org_archive\\)$" t)))))

  (add-to-list 'org-agenda-custom-commands
               `("x2" "daily review"
                 (,@(mapcar #'(lambda (tag)
                                `(org-ql-block '(and (or (clocked 1)
                                                         (closed 1))
                                                     (tags ,tag))
                                               ((org-ql-block-header (concat ,tag)))))
                            (hash-table-keys zwei/org-tag-goal-table))
                  (org-ql-block `(and (or (closed 1)
                                          (clocked 1))
                                      (not (tags ,@(hash-table-keys zwei/org-tag-goal-table))))
                                ((org-ql-block-header "OTHER"))))
                 ((org-agenda-files
                   (directory-files zwei/org-agenda-directory t "\\(\.org\\)\\|\\(.org_archive\\)$" t))))))

;; Org-clock-convenience for agenda
(use-package! org-clock-convenience
  :after org-clock)

(map! :after org-clock-convenience
      :map org-agenda-mode-map
      "<S-up>" #'org-clock-convenience-timestamp-up
      "<S-down>" #'org-clock-convenience-timestamp-down
      "o" #'org-clock-convenience-fill-gap
      "e" #'org-clock-convenience-fill-gap-both)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +org-agenda.el ends here
