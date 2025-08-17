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
  "Fetch from the remote REPO to get a local copy of REFSPEC."
  (git*
   "-C"
   #$(repository->path repo)
   "fetch"
   "origin"
   #$(refspec->ref refspec)))

(define (maybe-fetch-refspec repo refspec)
  "Fetch from the remote REPO to get a local copy of REFSPEC if it does not
already exist or the latest ref was requested."
  (list
   (clone repo)
   (match refspec
     (('latest branch)
      (fetch-refspec repo refspec))
     (_
      #~(unless
            (zero? (status:exit-val
                    (system* #$(program-path git "/bin/git")
                             "-C" #$(repository->path repo)
                             "rev-parse"
                             "--verify"
                             "--quiet"
                             "--end-of-options"
                             #$refspec)))
          #$(fetch-refspec repo refspec))))))

(define* (checkout* #:key repository ref path)
  "Checkout REPOSITORY to REF at the given PATH."
  (list
   (maybe-fetch-refspec repository ref)
   (git*
    "-C"
    #$(repository->path repository)
    "worktree"
    "add"
    "--detach"
    (string-join (list (getcwd) #$path) "/")
    #$(refspec->ref ref))))
