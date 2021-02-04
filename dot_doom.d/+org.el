;;; +org.el -- ~/.doom.d/+org.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains all org-related config NOT related to knowledge management
;;; (org-roam.el) or specific agenda config (org-agenda.el).
;;;
;;; Code:


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
        org-extend-today-until 4 ;; add some buffer after midnight
        org-hierarchical-todo-statistics nil
        org-startup-folded 'overview
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

  ;; Logging
  (setq org-log-done 'time
        org-log-into-drawer t)


  ;; Tagging -- currently used for place and goal

  (defvar zwei/org-tag-goal-table (make-hash-table :test 'equal)
    "Hash table with GOALSTRING as key, plist '(numkey ?# colorstring STRING) as values.")
  (clrhash zwei/org-tag-goal-table)
  (puthash "1#PHYSICAL" '(numkey ?1 colorstring "#CC2200") zwei/org-tag-goal-table)
  (puthash "2#MENTAL" '(numkey ?2 colorstring "#008F40") zwei/org-tag-goal-table)
  (puthash "3#CODING" '(numkey ?3 colorstring "#42A5F5") zwei/org-tag-goal-table)
  (puthash "4#AUTOMATION" '(numkey ?4 colorstring "#00FF33") zwei/org-tag-goal-table)
  (puthash "5#BUSINESS" '(numkey ?5 colorstring "#F5C400") zwei/org-tag-goal-table)
  (puthash "6#WANKER" '(numkey ?6 colorstring "#6A3B9F") zwei/org-tag-goal-table)

  (setq org-tag-persistent-alist `((:startgroup . "place")
                                   ("@work" . ?w)
                                   ("@play" . ?p)
                                   ("@down" . ?d)
                                   ("@end" . ?e)
                                   (:endgroup . "place")
                                   (:startgroup "goal")
                                   ,@(reverse
                                      (let (result)
                                        (maphash
                                         (lambda (k v)
                                           (push (cons k (plist-get v 'numkey))
                                                 result))
                                         zwei/org-tag-goal-table)
                                        result))
                                   (:endgroup "goal"))
        org-fast-tag-selection-single-key nil
        org-use-tag-inheritance t
        org-tags-exclude-from-inheritance '("crypt" "@work" "@play" "@down" "@end")
        org-tag-faces
        (let (result)
          (maphash
           (lambda (k v)
             (push
              (cons k (list ':foreground (plist-get v 'colorstring) ':weight 'bold))
              result))
           zwei/org-tag-goal-table)
          result))

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
           (file ,zwei/org-agenda-weekly-review-template-file))
          ("d" "Daily Review"
           entry
           (function (lambda () (org-journal-new-entry nil)))
           (file ,zwei/org-agenda-daily-review-template-file)
           :jump-to-captured t))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +org.el ends here
