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

;; Enable emacs to open links in Windows
(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program cmd-exe
          browse-url-generic-args cmd-args
          browse-url-browser-function 'browse-url-generic)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; +app.el ends here
