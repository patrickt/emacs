;;; pith.el --- a Transient UI for Tidal and SuperCollider

;;; Commentary:
;;; patrick's interesting tunefulness helper

;;; Code:

(add-to-list 'load-path "/Users/patrickt/Library/Application Support/SuperCollider/downloaded-quarks/scel/el")

(require 'transient)
(require 'popper)

;; why isn't sclang requiring correctly
(autoload 'sclang-eval-string "sclang" "Evaluate SCLang string" nil)
(autoload 'tidal-send-string "tidal" "Evaluate Tidal string" nil)

(transient-define-prefix pith-dispatch ()
  "Dispatch menu for pith functionality."
  ["pith ⛑"
   ["Global"
    ("r" "toggle recording" pith-toggle-recording)
    ("n" "new tidal file" pt/new-tidal-file)
    ("q" "quit" transient-suspend)]
   ["Tidal"
    ("td" "hush" pt/hush)
    ("tv" "show post buffer" pt/tidal-see-output-no-select)
    ("tb" "browse samples folder" pith-dired-samples-folder)
    ("tr" "reload secrets" pith-reload-secrets)
    ("tc" "toggle clock" pith-toggle-clock)]
   ["SuperCollider"
    ("ss" "start" sclang-start)
    ("sd" "hush" sclang-main-stop)
    ("sv" "show post buffer" sclang-show-post-buffer)
    ("sw" "open workspace" sclang-switch-to-workspace)
    ("sy" "open documentation" sclang-open-help-gui)
    ("sb" "browse SynthDefs" pith-show-synthdesclib)
    ("sr" "reload samples" pith-reload-samples)
    ("sR" "refactor" pith-sclang-refactor)]])

(transient-define-prefix pith-sclang-refactor ()
  ["Refactor"
   ["SynthDefs"
    ("v" "variable to NamedControl" pt/sclang-var-to-namedcontrol)
    ("r" "region to NamedControl parameter" pt/sclang-region-to-namedcontrol)]])

(defun pith-toggle-recording ()
  "Turn recording on or off."
  (interactive)
  (popper-display-popup-at-bottom (get-buffer "*SCLang:PostBuffer*"))
  (sclang-eval-string "(if(Server.default.isRecording, { Server.default.stopRecording }, { Server.default.record}););"))

(defun pith-show-synthdesclib ()
  "Show the synth descriptions."
  (interactive)
  (sclang-eval-string "(SynthDescLib.global.browse)"))

(defun pith-dired-samples-folder ()
  "Jump into my samples folder."
  (interactive)
  (dired "~/beats/packs"))

(defun pith-reload-samples ()
  "Reload all samples."
  (interactive)
  (popper-display-popup-at-bottom (get-buffer "*SCLang:PostBuffer*"))
  (sclang-eval-string "~reload.value"))

(defun pith-reload-secrets ()
  (interactive)
  (tidal-send-string ":script ./Secrets.hs"))

(defun pith-toggle-clock ()
  "Toggle visibility of the clock."
  (interactive)
  (sclang-eval-string "(~clockWindow.visible = ~clockWindow.visible.not;)"))

(provide 'pith)
;;; pith.el ends here
