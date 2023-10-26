;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks utils)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix sets)
  #:use-module (guix utils)
  #:export (hooks
            bash-script
            bash-script*
            for-each-staged-file
            program
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

(define-syntax-rule (hooks hook ...)
  "Combine all HOOK ... into a single hook that runs each in sequence."
  #~(begin #$hook ...))

(define-syntax-rule (run args)
  "Return a gexp that runs ARGS using SYSTEM* and exit if it fails. Since this
is a macro, any of ARGS may be an ungexp expression to escape the resulting
gexp."
  #~(let ((rc (status:exit-val (apply system* args))))
      (unless (= rc 0)
        (exit rc))))

(define-syntax program-path
  (syntax-rules ()
    ((program-path (package output) path)
     ;; TODO: is it possible to set the output using file-append?
     #~(string-append (ungexp package output) path))
    ((program-path package path)
     (file-append package path))))

(define-syntax-rule (program package path args ...)
  "Run a program from PACKAGE, where the program is at PATH relative to the
package root, with the given ARGS. This hook will exit on failure but not on
success so it may be composed with other hooks."
  (run (list #$(program-path package path) args ...)))

(define-syntax-rule (program* package path args ...)
  "Run a program from PACKAGE, where the program is at PATH relative to the
package root, with the given ARGS, appending COMMAND-LINE to the args. This hook
will exit on failure but not on success so it may be composed with other hooks."
  (run (cons* #$(program-path package path)
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

(define-syntax-rule (bash-script-impl inputs args)
  "Set PATH to point at the transitive closure of the bin directories of INPUTS,
then run ARGS. This is the internal implementation of the bash-script and
bash-script* macros."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (set-path-environment-variable
         "PATH"
         '("bin")
         (quote #$(propagated-inputs-closure inputs)))
        #$(run args))))

(define-syntax-rule (bash-script path inputs args ...)
  "Run the bash script at PATH with the given INPUTS in the path and the given
ARGS. Command-line arguments are not forwarded to the bash script when it is
run."
  (bash-script-impl inputs
                    (list #$(file-append bash-minimal "/bin/bash")
                          #$(local-file path)
                          args ...)))

(define-syntax-rule (bash-script* path inputs args ...)
  "Run the bash script at PATH with the given INPUTS in the path and the given
ARGS. Command-line arguments are appended to ARGS when it is run."
  (bash-script-impl inputs
                    (cons* #$(file-append bash-minimal "/bin/bash")
                           #$(local-file path)
                          args ...
                          (cdr (command-line)))))

(define-syntax-rule (for-each-staged-file (filename) body ...)
  "For each staged file in the current repository, bind the file name to
FILENAME and execute BODY ... (which should be g-expressions)."
  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 rdelim))
      (call-with-port
          (open-pipe* OPEN_READ
                      #$(file-append git-minimal "/bin/git")
                      "diff" "--staged" "--name-only")
        (lambda (port)
          (do ((filename (read-line port) (read-line port)))
              ((eof-object? filename))
            #$body ...)))))
