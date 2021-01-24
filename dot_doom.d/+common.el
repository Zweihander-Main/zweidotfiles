;;; +common.el -- ~/.doom.d/+common.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;;
;;; Contains common variables and functions used repeatedly throughout the config.
;;;
;;; Code:


;; ===============
;;   Directories
;; ===============

;; Load machine specific directories which includes org-directory
(load! "~/.doom.d/+machine_var") ;; At doom due to chezmoi config discrepencies

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

(defvar zwei/org-agenda-daily-review-template-file (concat zwei/org-agenda-templates-directory "/daily_review.org")
  "Template file for daily review.")

(defvar zwei/org-agenda-projects-file (concat zwei/org-agenda-directory "/projects.org")
  "File for all tasks that can be put into a given active project.")

(defvar zwei/org-agenda-tickler-file (concat zwei/org-agenda-directory "/tickler.org")
  "File for all tickler tasks. Can include projects but only non-active ones.")

(defvar zwei/org-agenda-next-file (concat zwei/org-agenda-directory "/next.org")
  "File for one-off tasks that should be done immediately or are currently being worked on.")

(setq +org-capture-todo-file zwei/org-agenda-todo-file)


;; ==================
;;  Common Functions
;; ==================

(defun zwei/which-linux-distro ()
  "Info from lsb_release. Will output strings such as 'Debian' or 'Arch'."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "echo -n $(lsb_release -is)")))

(defun zwei/save-and-restore-state (ref-name &optional action)
  "Save point, buffer, and mark into variable defined by REF-NAME.

ACTION values:
Write: Will write values.
Clear: Will set to nil.
Read: Will return values in (point,buffer,mark) form.
Restore: Will restore state and clear variable.

For read, can get data using:
\(setf (values point buffer mark) (zwei/save-and-restore-state nil 'read')).

Will clear data on non-write if old buffer doesn't exist anymore.

Intended for short term usage - not designed to survive restart."
  (let* ((constant "ZWEISAVEANDRESTORESTATE_")
         (var-string (concat constant ref-name))
         (stored-data (ignore-errors (symbol-value (intern var-string))))
         (old-point (gensym "old-point"))
         (old-buff) (gensym "old-buff")
         (old-mark) (gensym "old-mark"))
    (when stored-data
      (setq old-point (nth 0 stored-data))
      (setq old-buff (nth 1 stored-data))
      (setq old-mark (nth 2 stored-data)))
    (cond ((string= action "write")
           (let ((old-point (point))
                 (old-buff (current-buffer))
                 (old-mark (save-mark-and-excursion--save)))
             (set (intern var-string) (list old-point old-buff old-mark))))
          ((or (string= action "clear") (not (buffer-live-p old-buff)) (not stored-data))
           (set (intern var-string) nil))
          ((string= action "read") stored-data)
          (t (progn (unless (eq (current-buffer) old-buff)
                      (switch-to-buffer old-buff))
                    (goto-char old-point)
                    (save-mark-and-excursion--restore old-mark)
                    (set (intern var-string) nil))))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +common.el ends here
