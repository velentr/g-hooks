;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(use-modules (gnu packages bash)
             (gnu packages guile)
             (gnu packages gnupg)
             (gnu packages package-management)
             (gnu packages version-control)
             (guix build-system gnu)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages))

(package
  (name "g-hooks")
  (version "0")
  (source (local-file "." "g-hooks" #:recursive? #t))
  (build-system gnu-build-system)
  (arguments
   (list
    #:tests? #f
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'unpack 'patch-tool-inputs
          (lambda _
            (substitute* "g-hooks/core.scm"
              (("define %git \"git\"")
               (string-append "define %git \"" (which "git") "\"")))))
        (replace 'configure
          (lambda _
            (setenv "GUILE_AUTO_COMPILE" "0")
            (setenv "GUILE_FLAGS" "-L .")
            (setenv "DESTDIR" #$output))))))
  (native-inputs (list guile-3.0))
  (inputs (list git guix))
  (synopsis "Manage git hooks using guix")
  (description "g-hooks allows you to describe git hooks using guix’s
g-expressions, then build and install them as a private profile under
.git/g-hooks.")
  (home-page "https://github.com/velentr/g-hooks")
  (license license:gpl3))
