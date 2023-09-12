;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks utils)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix sets)
  #:use-module (guix utils)
  #:export (program
            program*
            python-script
            python-script*))

;;; Commentary:
;;;
;;; Utilities for simplifying g-hook definitions.
;;;
;;; There are a handful of common patterns when defining g-hooks (such as
;;; running a program from a guix package). This module exports macros and
;;; functions to simplify these common cases so most users don't have to write
;;; g-expressions manually.
;;;
;;; Code:

(define-syntax-rule (run args)
  "Return a gexp that runs ARGS using SYSTEM* and exit if it fails. Since this
is a macro, any of ARGS may be an ungexp expression to escape the resulting
gexp."
  #~(let ((rc (status:exit-val (apply system* args))))
      (unless (= rc 0)
        (exit rc))))

(define-syntax-rule (program package path args ...)
  "Run a program from PACKAGE, where the program is at PATH relative to the
package root, with the given ARGS. This hook will exit on failure but not on
success so it may be composed with other hooks."
  (run (list #$(file-append package path) args ...)))

(define-syntax-rule (program* package path args ...)
  "Run a program from PACKAGE, where the program is at PATH relative to the
package root, with the given ARGS, appending COMMAND-LINE to the args. This hook
will exit on failure but not on success so it may be composed with other hooks."
  (run (cons* #$(file-append package path)
              args ...
              (cdr (command-line)))))

(define (unique lst)
  "Return a list of all the unique elements of LST, compared using EQUAL?. The
ordering of the elements in the returned list may be different than in LST."
  (set->list (list->set lst)))

(define (propagated-inputs-closure packages)
  "Get the list of the transitive closure of all propagated inputs of the list of
PACKAGES."
  (unique
   (apply append
          (map
           (lambda (package)
             (cons
              package
              (map cadr
                   (package-transitive-propagated-inputs package))))
           packages))))

(define-syntax-rule (python-script-impl modules args)
  "Set GUIX_PYTHONPATH to point at the transitive closure of the python MODULES,
then run ARGS. This is the internal implementation of the python-script and
python-script* macros."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (set-path-environment-variable
         "GUIX_PYTHONPATH"
         (list #$(string-append "lib/python"
                                (version-major+minor (package-version python))
                                "/site-packages"))
         (quote #$(propagated-inputs-closure modules)))
        #$(run args))))

(define-syntax-rule (python-script path modules args ...)
  "Run the python script at PATH with the given python MODULES in the library
path and the given ARGS. Command-line arguments are not forwarded to the python
script when it is run."
  (python-script-impl modules
                      (list #$(file-append python "/bin/python3")
                            #$(local-file path)
                            args ...)))

(define-syntax-rule (python-script* path modules args ...)
  "Run the python script at PATH with the given python MODULES in the library
path and the given ARGS. Command-line arguments are appended to ARGS when the
python script is run."
  (python-script-impl modules
                      (cons* #$(file-append python "/bin/python3")
                             #$(local-file path)
                             args ...
                             (cdr (command-line)))))
