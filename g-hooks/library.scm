;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks library)
  #:use-module (gnu packages license)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (g-hooks utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (g-hooks-service-type
            g-hooks-provenance-service-type

            ;; Core git hook services and extension macros
            applypatch-msg
            pre-applypatch
            post-applypatch
            pre-commit
            pre-merge-commit
            prepare-commit-msg
            commit-msg
            post-commit
            pre-rebase
            post-checkout
            post-merge
            pre-push
            pre-receive
            update
            proc-receive
            post-receive
            post-update
            reference-transaction
            push-to-checkout
            pre-auto-gc
            post-rewrite
            sendemail-validate
            fsmonitor-watchman
            p4-changelist
            p4-prepare-changelist
            p4-post-changelist
            p4-pre-submit
            post-index-change
            %base-g-hooks-services

            ;; Deprecated g-expressions
            black/pre-commit
            git-lfs/post-checkout
            git-lfs/post-commit
            git-lfs/post-merge
            git-lfs/pre-push
            gitlint/commit-msg
            reuse-lint/pre-commit
            rustfmt/pre-commit))


;;; Commentary:
;;;
;;; Library of useful g-hooks.
;;;
;;; A g-hook is defined as a guix service that extends one of the specific
;;; G-HOOK-<HOOK-NAME>-SERVICE-TYPE services with one or more g-expressions.
;;; Convenience macros (e.g. PRE-COMMIT) can be used to simplify the service
;;; extension without having to define a new service type.
;;;
;;; The root G-HOOKS-SERVICE-TYPE can also be extended with a list of
;;; filename/derivation pairs that will get populated in the g-hooks output
;;; directory (generally used for provenance information).
;;;
;;; There are also a handful of legacy g-expressions exported without service
;;; definitions; these are deprecated and will be removed before v1.0. These are
;;; suffixed with '/<git hook>' (e.g. /pre-commit) by convention, but this is
;;; only for documentation purposes; there's nothing that checks g-expressions
;;; are only used for specific git hooks.
;;;
;;; Code:

(define (g-hooks-derivation entries mextensions)
  "Compute the derivation for the g-hooks root service containing ENTRIES and
monadic MEXTENSIONS."
  (mlet %store-monad
      ((extensions (mapm/accumulate-builds identity mextensions)))
    (lower-object
     (file-union "g-hooks" (append entries (concatenate extensions))))))

(define g-hooks-service-type
  (service-type (name 'g-hooks)
                (extensions '())
                (compose identity)
                (extend g-hooks-derivation)
                (default-value '())
                (description
                 "Build the g-hooks top-level directory, which contains the
@file{hooks/} directory to symlink into @file{.git/hooks} as well as metadata
about the g-hooks derivation.")))

(define (g-hooks-provenance-service config)
  "Extend G-HOOKS-SERVICE-TYPE with provenance information about the g-hooks
profile."
  (define (channel->sexp channel)
    `(channel (name ,(channel-name channel))
              (url ,(channel-url channel))
              (branch ,(channel-branch channel))
              (commit ,(channel-commit channel))))
  (with-monad %store-monad
    (return
     `(("channels.scm"
        ,(plain-file "channels.scm"
                     (object->string
                      (map channel->sexp (current-channels)))))))))

(define g-hooks-provenance-service-type
  (service-type (name 'g-hooks-provenance)
                (extensions
                 (list (service-extension g-hooks-service-type
                                          g-hooks-provenance-service)))
                (default-value '())
                (description
                 "Add provenance information to the g-hooks profile in the
profile itself, including the channels used when building the profile.")))

;; Encapsulation of a git-hook combining the name of the hook with the set of
;; g-expressions that are run for the hook. This is the configuration type for
;; the various base g-hook services and is converted to a script in .git/hooks
;; during instantiation.
(define-record-type <git-hook-gexp>
  (git-hook-gexp name gexps)
  git-hook-gexp?
  ;; The name of the hook, which should be one of the hooks supported by git
  ;; (see githooks(5)).
  (name git-hook-gexp->name)
  ;; List of g-expressions that are run for this hook.
  (gexps git-hook-gexp->gexps))

(define (git-hook-gexp->g-hook hook)
  "Convert HOOK (a <GIT-HOOK-GEXP> record) into a script that extends
G-HOOK-SERVICE-TYPE."
  (with-monad %store-monad
    (return
     (let ((name (git-hook-gexp->name hook))
           (gexps (git-hook-gexp->gexps hook)))
       (if (nil? gexps)
           '()
           `((,(string-append "hooks/" name)
              ,(program-file name #~(begin #$@gexps)))))))))

(define (extend-git-hook hook gexps)
  "Extend HOOK with the list of GEXPS."
  (git-hook-gexp (git-hook-gexp->name hook)
                 (append (git-hook-gexp->gexps hook) gexps)))

(define (hook-syntax->service-type hook-name)
  "Given a syntax form of an identifier HOOK-NAME, generate the symbol
for its service type. This symbol is suitable for converting back into
a syntax form using DATUM->SYNTAX."
  (string->symbol
   (string-append "g-hooks-"
                  (symbol->string (syntax->datum hook-name))
                  "-service-type")))

(define (define-base-g-hook-service hook-name)
  "Define the syntactic s-expression for the base g-hook service HOOK-NAME. This
includes a public service type named g-hooks-HOOK-NAME-service-type and a syntax
macro named HOOK-NAME that expands into a simple service definition for the
service."
  (let* ((string-name (symbol->string hook-name))
         (service-name (string->symbol (string-append "g-hooks-" string-name)))
         (service-type-name (hook-syntax->service-type hook-name)))
    `(begin
       (define-public ,service-type-name
         (service-type (name (quote ,service-name))
                       (extensions (list (service-extension g-hooks-service-type
                                                            git-hook-gexp->g-hook)))
                       (compose concatenate)
                       (extend extend-git-hook)
                       (default-value (git-hook-gexp ,string-name (list)))
                       (description "")))
       (define-syntax-rule (,hook-name gexp)
         (simple-service (quote gexp) ,service-type-name (list gexp))))))

(define-syntax define-base-g-hooks-services
  (lambda (x)
    (syntax-case x ()
      ((_ name all-g-hooks)
       (let* ((all-g-hooks-datum (syntax->datum #'all-g-hooks))
              (all-service-types
               (datum->syntax
                x (map (lambda (service-type)
                         (list 'service
                               (hook-syntax->service-type service-type)))
                       all-g-hooks-datum)))
              (all-service-definitions
               (datum->syntax
                x (map define-base-g-hook-service all-g-hooks-datum))))
         #`(begin
             #,@all-service-definitions
             (define name
               (list (service g-hooks-service-type)
                     (service g-hooks-provenance-service-type)
                     #,@all-service-types))))))))

;; Base services required for defining g-hooks. This includes all core git hook
;; services (this is required in order to extend these dynamicallly), but they
;; will not produce hooks by default if they are not extended.
(define-base-g-hooks-services %base-g-hooks-services
  (applypatch-msg
   pre-applypatch
   post-applypatch
   pre-commit
   pre-merge-commit
   prepare-commit-msg
   commit-msg
   post-commit
   pre-rebase
   post-checkout
   post-merge
   pre-push
   pre-receive
   update
   proc-receive
   post-receive
   post-update
   reference-transaction
   push-to-checkout
   pre-auto-gc
   post-rewrite
   sendemail-validate
   fsmonitor-watchman
   p4-changelist
   p4-prepare-changelist
   p4-post-changelist
   p4-pre-submit
   post-index-change))

(define black/pre-commit
  (for-each-staged-file
   (file-name)
   #~(let ((ext-idx (string-rindex file-name #\.)))
       (if (and ext-idx
                (equal? (substring file-name ext-idx) ".py"))
           #$(program python-black
                      "/bin/black"
                      "--check"
                      "--color"
                      "--diff"
                      "--fast"
                      "--quiet"
                      file-name)))))

(define (make-git-lfs hook)
  (program* git-lfs "/bin/git-lfs" #$hook))

(define git-lfs/post-checkout
  (make-git-lfs "post-checkout"))

(define git-lfs/post-commit
  (make-git-lfs "post-commit"))

(define git-lfs/post-merge
  (make-git-lfs "post-merge"))

(define git-lfs/pre-push
  (make-git-lfs "pre-push"))

(define gitlint/commit-msg
  #~(with-input-from-file "/dev/tty"
      (lambda ()
        #$(program gitlint
                   "/bin/gitlint"
                   "--staged"
                   "--msg-filename"
                   (cadr (command-line))
                   "run-hook"))))

(define reuse-lint/pre-commit
  (program reuse "/bin/reuse" "lint"))

(define rustfmt/pre-commit
  (for-each-staged-file
   (file-name)
   #~(let ((ext-idx (string-rindex file-name #\.)))
       (if (and ext-idx
                (equal? (substring file-name ext-idx) ".rs"))
           #$(program (rust "tools") "/bin/rustfmt" "--check" file-name)))))
