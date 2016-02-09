;;; terminal-notifier.el --- OS X Notification Center messages.

;; Copyright (c) 2013 Kris Molendyke

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; Requires `https://github.com/alloy/terminal-notifier'.
;;
;; Example Usage:
;;
;; (tn-notify "Ahoy, message!" "Whoa there, title!" "Yo, subtitle!")

;;; Code:

(defmacro tn--append-argument (arguments name value &optional default-value)
  "Append to ARGUMENTS the given NAME and VALUE.

Optional argument DEFAULT-VALUE The value to fall back to if VALUE is nil.

NAME will be formatted as `-name'."
  (let ((actual-value `(or ,value ,default-value)))
    `(if ,actual-value
         (setq ,arguments
               (append ,arguments (list (format "-%s" ,name) ,actual-value))))))

(defun tn-notify (message &optional title subtitle group activate open command)
  "Display MESSAGE in the OS X Notification Center.

Optional argument TITLE The notification title.  Defaults to
`Emacs'.

Optional argument SUBTITLE The notification subtitle.

Optional argument GROUP A string which identifies the group the
notifications belong to.  Old notifications with the same ID will
be removed.

Optional argument ACTIVATE The bundle identifier of the
application to activate when the user clicks the notification.
Defaults to `org.gnu.Emacs'.

Optional argument OPEN The URL of a resource to open when the
user clicks the notification.

Optional argument COMMAND A shell command to perform when the
user clicks the notification.

The `terminal-notifier' binary is searched for on the `exec-path'
and then within the `terminal-notifier.app' bundles at either the
root or home `Applications' directories."
  (when (eq system-type 'darwin)
    (let* ((root-app-dir "/Applications/terminal-notifier.app/Contents/MacOS")
           (home-app-dir (expand-file-name (concat "~" root-app-dir)))
           (executable (or (executable-find "terminal-notifier")
                           (locate-file "terminal-notifier"
                                        (list root-app-dir home-app-dir)))))
      (if executable
          (let ((arguments (list "terminal-notifier" "terminal-notifier"
                                 executable "-message" message)))
            (tn--append-argument arguments "title" title "Emacs")
            (tn--append-argument arguments "subtitle" subtitle)
            (tn--append-argument arguments "group" group)
            (tn--append-argument arguments "activate" activate "org.gnu.Emacs")
            (tn--append-argument arguments "sender" activate "org.gnu.Emacs")
            (tn--append-argument arguments "open" open)
            (tn--append-argument arguments "command" command)
            (apply 'start-process arguments))
        (message "terminal-notifier not found.")))))

(provide 'terminal-notifier)

;;; terminal-notifier.el ends here
