;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-actions actions git)
  #:use-module (g-actions configuration)
  #:use-module (g-actions paths)
  #:use-module (g-hooks utils)
  #:use-module (ice-9 match)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:export (clone
            checkout*))

;;; Commentary:
;;;
;;; Actions for manipulating git repositories.
;;;
;;; Code:

(define-syntax-rule (git* args ...)
  "Run git with the given cli ARGS."
  (program git "/bin/git" args ...))

(define (clone repo)
  "Action to ensure a repository is clone."
  (let* ((repo-path (repository->path repo))
         (obj-path (path* repo-path "objects")))
    #~(unless (and (file-exists? #$obj-path) (file-is-directory? #$obj-path))
        #$(git* "clone" #$(repository->url repo) "--bare" #$repo-path))))

(define (refspec->ref refspec)
  "Convert an abstract REFSPEC description into a concrete git ref that may be
checked out."
  (match refspec
    (('latest branch)
     branch)
    (_
     refspec)))

(define (fetch-refspec repo refspec)
  (list
   (clone repo)
   (git*
    "-C"
    #$(repository->path repo)
    "fetch"
    "origin"
    #$(refspec->ref refspec))))

(define* (checkout* #:key repository ref path)
  "Checkout REPOSITORY to REF at the given PATH."
  (list
   (fetch-refspec repository ref)
   (git*
    "-C"
    #$(repository->path repository)
    "worktree"
    "add"
    "--detach"
    (string-join (list (getcwd) #$path) "/")
    #$(refspec->ref ref))))
