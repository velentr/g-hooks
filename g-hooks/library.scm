;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks library)
  #:use-module (gnu packages license)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (g-hooks utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (g-hooks-service-type

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

(define-syntax-rule (define-git-hook-service
                      service-type-name
                      service-name
                      hook-name)
  "Define a core g-hook service named SERVICE-TYPE-NAME. SERVICE-NAME is a
display name for the service and HOOK-NAME is the string name of the git hook."
  (define-public service-type-name
    (service-type (name (quote service-name))
                  (extensions
                   (list (service-extension g-hooks-service-type
                                            git-hook-gexp->g-hook)))
                  (compose concatenate)
                  (extend extend-git-hook)
                  (default-value (git-hook-gexp hook-name '()))
                  (description ""))))

(define-syntax-rule (define-git-hook-syntax hook-name service-name)
  (define-syntax-rule (hook-name gexp)
    (simple-service (quote gexp) service-name (list gexp))))

(define-git-hook-service g-hooks-applypatch-msg-service-type
  g-hooks-applypatch-msg "applypatch-msg")

(define-git-hook-syntax applypatch-msg
  g-hooks-applypatch-msg-service-type)

(define-git-hook-service g-hooks-pre-applypatch-service-type
  g-hooks-pre-applypatch "pre-applypatch")

(define-git-hook-syntax pre-applypatch
  g-hooks-pre-applypatch-service-type)

(define-git-hook-service g-hooks-post-applypatch-service-type
  g-hooks-post-applypatch "post-applypatch")

(define-git-hook-syntax post-applypatch
  g-hooks-post-applypatch-service-type)

(define-git-hook-service g-hooks-pre-commit-service-type
  g-hooks-pre-commit "pre-commit")

(define-git-hook-syntax pre-commit
  g-hooks-pre-commit-service-type)

(define-git-hook-service g-hooks-pre-merge-commit-service-type
  g-hooks-pre-merge-commit "pre-merge-commit")

(define-git-hook-syntax pre-merge-commit
  g-hooks-pre-merge-commit-service-type)

(define-git-hook-service g-hooks-prepare-commit-msg-service-type
  g-hooks-prepare-commit-msg "prepare-commit-msg")

(define-git-hook-syntax prepare-commit-msg
  g-hooks-prepare-commit-msg-service-type)

(define-git-hook-service g-hooks-commit-msg-service-type
  g-hooks-commit-msg "commit-msg")

(define-git-hook-syntax commit-msg
  g-hooks-commit-msg-service-type)

(define-git-hook-service g-hooks-post-commit-service-type
  g-hooks-post-commit "post-commit")

(define-git-hook-syntax post-commit
  g-hooks-post-commit-service-type)

(define-git-hook-service g-hooks-pre-rebase-service-type
  g-hooks-pre-rebase "pre-rebase")

(define-git-hook-syntax pre-rebase
  g-hooks-pre-rebase-service-type)

(define-git-hook-service g-hooks-post-checkout-service-type
  g-hooks-post-checkout "post-checkout")

(define-git-hook-syntax post-checkout
  g-hooks-post-checkout-service-type)

(define-git-hook-service g-hooks-post-merge-service-type
  g-hooks-post-merge "post-merge")

(define-git-hook-syntax post-merge
  g-hooks-post-merge-service-type)

(define-git-hook-service g-hooks-pre-push-service-type
  g-hooks-pre-push "pre-push")

(define-git-hook-syntax pre-push
  g-hooks-pre-push-service-type)

(define-git-hook-service g-hooks-pre-receive-service-type
  g-hooks-pre-receive "pre-receive")

(define-git-hook-syntax pre-receive
  g-hooks-pre-receive-service-type)

(define-git-hook-service g-hooks-update-service-type
  g-hooks-update "update")

(define-git-hook-syntax update
  g-hooks-update-service-type)

(define-git-hook-service g-hooks-proc-receive-service-type
  g-hooks-proc-receive "proc-receive")

(define-git-hook-syntax proc-receive
  g-hooks-proc-receive-service-type)

(define-git-hook-service g-hooks-post-receive-service-type
  g-hooks-post-receive "post-receive")

(define-git-hook-syntax post-receive
  g-hooks-post-receive-service-type)

(define-git-hook-service g-hooks-post-update-service-type
  g-hooks-post-update "post-update")

(define-git-hook-syntax post-update
  g-hooks-post-update-service-type)

(define-git-hook-service g-hooks-reference-transaction-service-type
  g-hooks-reference-transaction "reference-transaction")

(define-git-hook-syntax reference-transaction
  g-hooks-reference-transaction-service-type)

(define-git-hook-service g-hooks-push-to-checkout-service-type
  g-hooks-push-to-checkout "push-to-checkout")

(define-git-hook-syntax push-to-checkout
  g-hooks-push-to-checkout-service-type)

(define-git-hook-service g-hooks-pre-auto-gc-service-type
  g-hooks-pre-auto-gc "pre-auto-gc")

(define-git-hook-syntax pre-auto-gc
  g-hooks-pre-auto-gc-service-type)

(define-git-hook-service g-hooks-post-rewrite-service-type
  g-hooks-post-rewrite "post-rewrite")

(define-git-hook-syntax post-rewrite
  g-hooks-post-rewrite-service-type)

(define-git-hook-service g-hooks-sendemail-validate-service-type
  g-hooks-sendemail-validate "sendemail-validate")

(define-git-hook-syntax sendemail-validate
  g-hooks-sendemail-validate-service-type)

(define-git-hook-service g-hooks-fsmonitor-watchman-service-type
  g-hooks-fsmonitor-watchman "fsmonitor-watchman")

(define-git-hook-syntax fsmonitor-watchman
  g-hooks-fsmonitor-watchman-service-type)

(define-git-hook-service g-hooks-p4-changelist-service-type
  g-hooks-p4-changelist "p4-changelist")

(define-git-hook-syntax p4-changelist
  g-hooks-p4-changelist-service-type)

(define-git-hook-service g-hooks-p4-prepare-changelist-service-type
  g-hooks-p4-prepare-changelist "p4-prepare-changelist")

(define-git-hook-syntax p4-prepare-changelist
  g-hooks-p4-prepare-changelist-service-type)

(define-git-hook-service g-hooks-p4-post-changelist-service-type
  g-hooks-p4-post-changelist "p4-post-changelist")

(define-git-hook-syntax p4-post-changelist
  g-hooks-p4-post-changelist-service-type)

(define-git-hook-service g-hooks-p4-pre-submit-service-type
  g-hooks-p4-pre-submit "p4-pre-submit")

(define-git-hook-syntax p4-pre-submit
  g-hooks-p4-pre-submit-service-type)

(define-git-hook-service g-hooks-post-index-change-service-type
  g-hooks-post-index-change "post-index-change")

(define-git-hook-syntax post-index-change
  g-hooks-post-index-change-service-type)

;; Base services required for defining g-hooks. This includes all core git hook
;; services (this is required in order to extend these dynamicallly), but they
;; will not produce hooks by default if they are not extended.
(define %base-g-hooks-services
  (list (service g-hooks-service-type)
        (service g-hooks-applypatch-msg-service-type)
        (service g-hooks-pre-applypatch-service-type)
        (service g-hooks-post-applypatch-service-type)
        (service g-hooks-pre-commit-service-type)
        (service g-hooks-pre-merge-commit-service-type)
        (service g-hooks-prepare-commit-msg-service-type)
        (service g-hooks-commit-msg-service-type)
        (service g-hooks-post-commit-service-type)
        (service g-hooks-pre-rebase-service-type)
        (service g-hooks-post-checkout-service-type)
        (service g-hooks-post-merge-service-type)
        (service g-hooks-pre-push-service-type)
        (service g-hooks-pre-receive-service-type)
        (service g-hooks-update-service-type)
        (service g-hooks-proc-receive-service-type)
        (service g-hooks-post-receive-service-type)
        (service g-hooks-post-update-service-type)
        (service g-hooks-reference-transaction-service-type)
        (service g-hooks-push-to-checkout-service-type)
        (service g-hooks-pre-auto-gc-service-type)
        (service g-hooks-post-rewrite-service-type)
        (service g-hooks-sendemail-validate-service-type)
        (service g-hooks-fsmonitor-watchman-service-type)
        (service g-hooks-p4-changelist-service-type)
        (service g-hooks-p4-prepare-changelist-service-type)
        (service g-hooks-p4-post-changelist-service-type)
        (service g-hooks-p4-pre-submit-service-type)
        (service g-hooks-post-index-change-service-type)))

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
