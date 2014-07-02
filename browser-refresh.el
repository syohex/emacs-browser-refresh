;;; browser-refresh.el --- Broser refresh utility

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-browser-refresh
;; Version: 0.01
;; Package-Requires: ((eieio "1.3") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'eieio)

(declare-function do-applescript "nsfns.m")

(defgroup browser-refresh nil
  "Browser refresh utility for multi browsers and multi platform"
  :group 'external)

(defcustom browser-refresh-activate t
  "Activate browser after refresh"
  :type 'boolean
  :group 'browser-refresh)

(defcustom browser-refresh-default-browser 'chrome
  "Default browser"
  :type 'symbol
  :group 'browser-refresh)

(defcustom browser-refresh-save-buffer t
  "Non-nil means saving buffer before browser refresh"
  :type 'boolean
  :group 'browser-refresh)

;;
;; Base class
;;

(defclass browser-refresh-base ()
  ((activate :initarg :activate)))

;;
;; MacOSX
;;

(defclass browser-refresh-mac (browser-refresh-base)
  ())

(defsubst browser-refresh--activate-string (activate-p)
  (if activate-p "activate" ""))

(defun browser-refresh--chrome-applescript (app activate-p)
  (do-applescript
   (format
    "
  tell application \"%s\"
    %s
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" app (browser-refresh--activate-string activate-p))) )

(defmethod chrome ((refresher browser-refresh-mac))
  (browser-refresh--chrome-applescript "Google Chrome" (oref refresher :activate)))

(defmethod firefox ((refresher browser-refresh-mac))
  (do-applescript
   (format
    "
  tell application \"Firefox\"
    %s
    tell application \"System Events\" to keystroke \"r\" using command down
  end tell
" (browser-refresh--activate-string (oref refresher :activate)))))

(defmethod safari ((refresher browser-refresh-mac))
  (do-applescript
   (format
    "
  tell application \"Safari\"
    %s
    tell its first document
    set its URL to (get its URL)
    end tell
  end tell
" (browser-refresh--activate-string (oref refresher :activate)))))

;;
;; GNU/Linux
;;

(defclass browser-refresh-linux (browser-refresh-base)
  ())

(defconst browser-refresh--xdotool-base-option
  '("search" "--sync" "--onlyvisible"))

(defun browser-refresh--send-key-with-xdotool (window-ids key)
  (dolist (window-id window-ids)
    (unless (zerop (call-process "xdotool" nil nil nil "key" "--window" window-id key))
      (error "Failed: 'xdotool key --window %s %s'" window-id key))))

(defun browser-refresh--linux-search-window-id (class)
  (with-temp-buffer
    (unless (zerop (call-process "xdotool" nil t nil "search" "--class" class))
      (error "Failed: 'xdotool search --class %s'" class))
    (goto-char (point-min))
    (cl-loop with window-ids = nil
             until (eobp)
             do
             (progn
               (push (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     window-ids)
               (forward-line 1))
             finally return window-ids)))

(defmethod activate ((refresher browser-refresh-linux) window-id)
  (when (oref refresher :activate)
    (unless (zerop (call-process "xdotool" nil nil nil "windowactivate" window-id))
      (error "Failed: 'xdotool windowactivate %s'" window-id))))

(defmethod chrome ((refresher browser-refresh-linux))
  (let ((window-ids (browser-refresh--linux-search-window-id "Google-Chrome")))
    (browser-refresh--send-key-with-xdotool window-ids "F5")
    (activate refresher (car window-ids))))

(defmethod firefox ((refresher browser-refresh-linux))
  (let ((window-ids (browser-refresh--linux-search-window-id "Firefox")))
    (browser-refresh--send-key-with-xdotool window-ids "F5")
    (activate refresher (car window-ids))))

(defun browser-refresh--make-refresher ()
  (let ((class (cl-case system-type
                 (gnu/linux 'browser-refresh-linux)
                 (darwin 'browser-refresh-mac)
                 (otherwise (error "%s is not supported yet" system-type)))))
    (make-instance class :activate browser-refresh-activate)))

;;;###autoload
(defun browser-refresh ()
  (interactive)
  (when (and browser-refresh-save-buffer (buffer-modified-p))
    (save-buffer))
  (let ((refresher (browser-refresh--make-refresher)))
    (funcall browser-refresh-default-browser refresher)))

(provide 'browser-refresh)

;;; browser-refresh.el ends here
