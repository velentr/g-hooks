;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-actions configuration)
  #:use-module (g-actions paths)
  #:use-module (ice-9 sandbox)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hash)
  #:export (eval-configuration-in-sandbox
            repository*
            repository?
            repository->path
            repository->url))

;;; Commentary:
;;;
;;; Configuration data structures and methods.
;;;
;;; Code:

(define-immutable-record-type <repository>
  (make-repository url)
  repository?
  ;; Primary url of the repository
  (url repository->url))

(define* (repository* #:key url)
  "Create a remote git repository pointing at URL."
  (make-repository url))

(define (repository->key repo)
  "Get the unique key for REPO."
  (let ((digest (sha256 (string->utf8 (repository->url repo)))))
    (base64-encode
     digest
     0 (bytevector-length digest)
     #f #f
     base64url-alphabet)))

(define (repository->path repo)
  "Get the unique absolute path for REPO."
  (xdg-data-home "g-actions" "git-repos" (repository->key repo)))

(define (eval-configuration-in-sandbox config-exp)
  "Evaluate CONFIG-EXP in a sandbox including the constructors from this
module."
  (eval-in-sandbox
   config-exp
   #:bindings
   (cons*
    '((g-actions configuration)
      repository*)
    '((g-actions actions git)
      checkout*)
    all-pure-bindings)))
