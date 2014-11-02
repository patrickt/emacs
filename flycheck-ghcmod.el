;;; flycheck-ghcmod --- use ghc-mode as a backend for flycheck
;;; Commentary:
;;; Originally written by Mark Schultz. The original gist is available here:
;;; https://gist.github.com/markschultz/8189199
(require 'flycheck)

;;; Code:
(flycheck-define-checker haskell-ghcmod
                         "A Haskell syntax and type checker using ghc-mod"
                         :command ("ghc-mod" "check" source-inplace)
                         :error-patterns
                         ((warning line-start (file-name) ":" line ":" column ":"
                                   (or " " "\n    ") "Warning:" (optional "\n")
                                   (one-or-more " ")
                                   (message (one-or-more not-newline)
                                            (zero-or-more "\n"
                                                          (one-or-more " ")
                                                          (one-or-more not-newline)))
                                   line-end)
                          (error line-start (file-name) ":" line ":" column ":"
                                 (or (message (one-or-more not-newline))
                                     (and "\n" (one-or-more " ")
                                          (message (one-or-more not-newline)
                                                   (zero-or-more "\n"
                                                                 (one-or-more " ")
                                                                 (one-or-more not-newline)))))
                                 line-end))
                         :modes haskell-mode
                         :next-checkers ((warnings-only . haskell-hlint)))

(add-to-list 'flycheck-checkers 'haskell-ghcmod)

(provide 'flycheck-ghcmod)
;;; flycheck-ghcmod ends here
