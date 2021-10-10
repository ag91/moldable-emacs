(use-modules (guix packages)
             (guix download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages emacs)
             (guix build-system emacs)
             (gnu packages emacs-xyz))

(define-public emacs-moldable-emacs
  (package
   (name "emacs-moldable-emacs")
   (version "0.0.0")
   (source (dirname (current-filename)))
   (build-system emacs-build-system)
   (arguments
    `(#:include (cons "^molds/" %default-include)))
   (propagated-inputs
    `(("emacs-dash" ,emacs-dash)
      ("emacs-s" ,emacs-s)
      ("emacs-async" ,emacs-async)))
   (home-page "https://github.com/ag91/moldable-emacs")
   (synopsis "Adapting Emacs for moldable development ")
   (description
    "This is an extension of Emacs aiming to enable Moldable
Development. Or better still, aiming to make you a better story teller
when you deal with code.")
   (license license:gpl3+)))

emacs-moldable-emacs
