;;; pre-early-init.el --- before early init -*- no-byte-compile: t; lexical-binding: t; -*-

(setq debug-on-error t)

(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(when (featurep 'native-compile)
  (let ((eln-cache-dir (convert-standard-filename
                         (expand-file-name "eln-cache" user-emacs-directory))))
    (when (boundp 'native-comp-eln-load-path)
      (setcar native-comp-eln-load-path eln-cache-dir))
    (setq native-compile-target-directory eln-cache-dir)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache eln-cache-dir))))

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)
