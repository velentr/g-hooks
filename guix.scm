;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (gnu packages bash)
             (gnu packages guile)
             (gnu packages gnupg)
             (gnu packages package-management)
             (gnu packages version-control)
             (guix build-system guile)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages))

(package
  (name "g-hooks")
  (version "0")
  (source (local-file "." "g-hooks" #:recursive? #t))
  (build-system guile-build-system)
  (arguments
   (list
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'patch-tool-inputs
          (lambda _
            (substitute* "g-hooks/core.scm"
              (("define %git \"git\"")
               (string-append "define %git \"" (which "git") "\"")))))
        (add-after 'unpack 'remove-unused-files
          (lambda _
            (delete-file "guix.scm")))
        (add-after 'install 'wrap-program
          (lambda* (#:key inputs #:allow-other-keys)
            (let* ((bin (string-append #$output "/bin"))
                   (bytestructures (assoc-ref inputs "guile-bytestructures"))
                   (gcrypt (assoc-ref inputs "guile-gcrypt"))
                   (git (assoc-ref inputs "guile-git"))
                   (guix (assoc-ref inputs "guix"))
                   (version (target-guile-effective-version))
                   (scm (string-append "/share/guile/site/" version))
                   (go (string-append "/lib/guile/" version "/site-ccache")))
              (install-file "scripts/g-hooks" bin)
              (wrap-program (string-append bin "/g-hooks")
                '("GUILE_AUTO_COMPILE" prefix ("0"))
                ;; TODO: fix this nonsense
                `("GUILE_LOAD_PATH" prefix
                  (,(string-append #$output scm)
                   ,(string-append bytestructures scm)
                   ,(string-append gcrypt scm)
                   ,(string-append git scm)
                   ,(string-append guix scm)))
                `("GUILE_LOAD_COMPILED_PATH" prefix
                  (,(string-append #$output go)
                   ,(string-append bytestructures go)
                   ,(string-append gcrypt go)
                   ,(string-append git go)
                   ,(string-append guix go))))))))))
  (native-inputs (list guile-3.0))
  (inputs
   (list bash-minimal git guile-bytestructures guile-gcrypt guile-git guix))
  (synopsis "Manage git hooks using guix")
  (description "g-hooks allows you to describe git hooks using guix’s
g-expressions, then build and install them as a private profile under
.git/g-hooks.")
  (home-page "https://github.com/velentr/g-hooks")
  (license license:gpl3))
