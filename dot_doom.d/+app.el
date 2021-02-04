;;; +app.el -- ~/.doom.d/+app.el
;;;
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;; Configuration for the general usage of Emacs.
;;;
;;; Code:

(setq user-full-name "Zweihänder"
      user-mail-address "zweidev@zweihander.me")

;; Fix issues with emacs 27 warnings
(setq byte-compile-warnings '(cl-functions))

;; Load lisp files for compiled Emacs on Debian/WSL
(when (string= (zwei/which-linux-distro) "Debian")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))


;; ===============
;;   Backup/Save
;; ===============
(setq delete-old-versions -1
      backup-by-copying t ;; don't clobber symlinks
      kept-new-versions 10 ;;keep 10 versions
      delete-old-versions t ;; delte old versions silently
      version-control t ;; number backups
      vc-make-backup-files t ;;backup version controlled files
      auto-save-interval 3)


;; ================
;;  Message Buffer
;; ================

(defadvice message (after message-tail activate)
  "Goto point max after a new message."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (let ((windows (get-buffer-window-list (current-buffer) nil t)))
      (while windows
        (set-window-point (car windows) (point-max))
        (setq windows (cdr windows))))))


;; ==========
;;  Flycheck
;; =========
(after! flycheck
  (require 'org-lint)
  (defun flycheck-org-lint-start (checker callback)
    "Flycheck mode for org lint"
    (funcall
     callback 'finished
     (save-excursion
       (mapcar
        (lambda (err)
          (goto-char (car err))
          (flycheck-error-new-at
           (org-current-line) (1+ (current-column))
           'warning (cadr err) :checker checker))
        (org-lint-link-to-local-file (org-element-parse-buffer))))))

  (flycheck-define-generic-checker 'org-lint
    "Syntax checker for org-lint."
    :start 'flycheck-org-lint-start
    :modes '(org-mode))

  (add-to-list 'flycheck-checkers 'org-lint)
  (setq-default flycheck-disabled-checkers '(proselint)))


;; ========
;;  ispell
;; ========
(after! ispell
  :config
  (setq ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=fast")))


;; =====
;;  Ivy
;; =====

                                        ; Set search to ignore archives
(setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)\\|\\.org_archive")


;; ==============
;;  Centaur Tabs
;; ==============
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


;; =============
;;  OS Specific
;; =============

(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    ;; Enable emacs to open links in Windows
    (setq browse-url-generic-program cmd-exe
          browse-url-generic-args cmd-args
          browse-url-browser-function 'browse-url-generic))

  ;; Copy in WSL
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark))

  ;; Paste in WSL
  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
      (setq clipboard (substring clipboard 0 -1))
      (insert clipboard))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +app.el ends here
