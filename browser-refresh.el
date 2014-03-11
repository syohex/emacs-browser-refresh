;;; browser-refresh.el --- Broser refresh utility

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL:
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
  ""
  :group 'external)

(defcustom browser-refresh-activate nil
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
;; MacOSX
;;

(defclass browser-refresh-mac ()
  ())

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
")) app (if activate-p "activate" ""))

(defmethod chrome ((refresher browser-refresh-mac))
  (browser-refresh--chrome-applescript "Google Chrome"))

(defmethod firefox ((refresher browser-refresh-mac))
  (do-applescript
   (format
    "
  tell application \"Firefox\"
    %s
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
") ""))

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
")
) "")

;;
;; GNU/Linux
;;

(defclass browser-refresh-linux ()
  ())

(defconst browser-refresh--xdotool-base-option
  '("search" "--sync" "--onlyvisible"))

(defun browser-refresh--send-key-with-xdotool (window-ids key &optional activate)
  (dolist (window-id window-ids)
    (let ((cmd (concat "xdotool key --window " window-id " " key)))
      (when activate
        (add-to-list 'options "windowactivate" t))
      (unless (zerop (call-process-shell-command cmd))
        (error "Failed: %s" cmd)))))

(defun browser-refresh--linux-search-window-id (class)
  (let ((cmd (concat "xdotool search --onlyvisible --class " class)))
    (with-temp-buffer
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: %s" cmd))
      (goto-char (point-min))
      (cl-loop with window-ids = nil
               until (eobp)
               do
               (progn
                 (push (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       window-ids)
                 (forward-line 1))
               finally return window-ids))))

(defmethod chrome ((refresher browser-refresh-linux))
  (let ((window-ids (browser-refresh--linux-search-window-id "Google-Chrome")))
    (browser-refresh--send-key-with-xdotool window-ids "F5")))

(defmethod firefox ((refresher browser-refresh-linux))
  (let ((window-ids (browser-refresh--linux-search-window-id "Firefox")))
    (browser-refresh--send-key-with-xdotool window-ids "F5")))

(defun browser-refresh--make-refresher ()
  (cl-case system-type
    (gnu/linux (make-instance 'browser-refresh-linux))
    (otherwise (error "%s is not supported yet" system-type))))

;;;###autoload
(defun browser-refresh ()
  (interactive)
  (when browser-refresh-save-buffer
    (save-buffer))
  (let ((refresher (browser-refresh--make-refresher)))
    (chrome refresher)))

(provide 'browser-refresh)

;;; browser-refresh.el ends here
