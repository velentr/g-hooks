;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks core)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix build lisp-utils)
  #:use-module (guix build utils)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix describe)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix scripts package)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (g-hooks
            main))

;;; Commentary:
;;;
;;; Manage git hooks using Guix.
;;;
;;; G-hooks are git hooks (e.g. pre-commit, commit-msg, etc.) defined using
;;; g-expressions leveraging guix's profiles for multi-generation support with
;;; rollback and garbage collection.
;;;
;;; The profile for a set of g-hooks is stored in .git/g-hooks/
;;; (i.e. %G-HOOKS-ROOT). Inside each profile generation is (1) channels.scm
;;; describing the channels used to build the hooks and (2) a hooks/ directory
;;; containing scripts for each hook. The .git/hooks/ directory is symlinked
;;; to .git/g-hooks/g-hooks/hooks/, which is the currently active generation
;;; of the g-hooks. Other g-hooks-*-link symlinks in .git/g-hooks/ are other
;;; generations for the g-hooks profile.
;;;
;;; In addition to the profile in .git/g-hooks, there are also a set of
;;; symlinks in /var/guix/profiles/per-user/*/g-hooks/ (i.e. %G-HOOKS-GCROOT)
;;; that act as garbage collection roots and point at the generations of the
;;; profile in .git/g-hooks/. The names of these links are determined by
;;; PATH-IDENTIFIER, which will hash the canonical path name so that they will
;;; be globally unique. The gc roots point at the generation links in
;;; .git/g-hooks/ instead of directly at the store paths so that if the user
;;; deletes the git repository, the store object for the g-hooks will get
;;; garbage collected automatically with no additional action from the
;;; user. If this happens, the %G-HOOKS-GCROOT links will be broken but will
;;; not be automatically cleaned up by 'guix gc'. As a result, we scan through
;;; and delete these broken links in DELETE-GENERATIONS.
;;;
;;; Code:

(define (path-identifier path)
  "Return a deterministic, globally unique representation of PATH that may be
used as a valid filename."
  (let ((canonical (canonicalize-path path)))
    (string-append
     (bytevector->base32-string (sha256 (string->utf8 canonical)))
     "-"
     (normalize-string canonical))))

(define-syntax-rule (g-hooks (name gexp) ...)
  "Create a set of g-hooks with NAME bound to GEXP."
  (list
   (cons (quote name) gexp)
   ...))

(define (call arg0 . argv)
  "Call the program ARG0 with arguments ARGV, capturing its output. If the
program exits successfully, the result is the subprocess's stdout broken up as a
list of lines. If the program exits abnormally, the result is #f."
  (let ((port (apply open-pipe* (cons OPEN_READ (cons arg0 argv)))))
    (define (read-output so-far)
      (let ((line (read-line port)))
        (if (eof-object? line)
            so-far
            (read-output (cons line so-far)))))
    (let ((output-rev (read-output '()))
          (status (close-pipe port)))
      (if (and (status:exit-val status)
               (= 0 (status:exit-val status)))
          (reverse output-rev)
          #f))))

(define %git "git")

(define %git-common-dir
  (delay
    (car (or (call %git "rev-parse" "--path-format=absolute" "--git-common-dir")
             (error "not a git directory")))))

(define (load-config)
  "Load the g-hooks configuration for this repository. The configuration file is
named either g-hooks.scm or .g-hooks in the top-level directory of the git
repository."
  (let ((top-level
         (car
          (or
           (call %git "rev-parse" "--path-format=absolute" "--show-toplevel")
           (error "not a git directory")))))
    (define (maybe-load-config config-file)
      (let ((config-path (string-append top-level "/" config-file)))
        (if (file-exists? config-path)
            (primitive-load config-path)
            #f)))
    (or (maybe-load-config "g-hooks.scm")
        (maybe-load-config ".g-hooks")
        (error "configuration file not found"))))

(define %global-id
  (delay (path-identifier (force %git-common-dir))))

(define %g-hooks-gcroot
  (delay (string-append %profile-directory "/g-hooks")))

(define %g-hooks-gcprofile
  (delay (string-append (force %g-hooks-gcroot) "/" (force %global-id))))

(define %g-hooks-root
  (delay (string-append (force %git-common-dir) "/g-hooks")))

(define %g-hooks-profile
  (delay (string-append (force %g-hooks-root) "/g-hooks")))

;; All git hooks as of git 2.41.0, taken from githooks(5).
(define %recognized-hooks
  '(applypatch-msg
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

(define (g-hooks->git-hook-derivation g-hooks)
  "Convert a set of G-HOOKS into a derivation that can be used as a .git/hooks
directory."
  (define (channel->sexp channel)
    `(channel (name ,(channel-name channel))
              (url ,(channel-url channel))
              (branch ,(channel-branch channel))
              (commit ,(channel-commit channel))))
  (let ((names
         (map (lambda (hook)
                (let ((name (car hook)))
                  (if (member name %recognized-hooks)
                      (string-append "hooks/" (symbol->string name))
                      (error "unrecognized hook:" name))))
              g-hooks))
        (scripts
         (map (lambda (hook)
                (program-file (symbol->string (car hook)) (cdr hook)))
              g-hooks))
        (channels (plain-file "channels.scm"
                              (object->string
                               (map channel->sexp (current-channels))))))
    (gexp->derivation
     "g-hooks"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (mkdir-p (string-append #$output "/hooks"))
           (for-each
            (lambda (name script)
              (symlink script (string-append #$output "/" name)))
            (quote ("channels.scm" #$@names))
            (quote (#$channels #$@scripts))))))))

(define (reconfigure args)
  "Reconfigure .git/hooks/ based on the repository's g-hooks."
  (unless (null? (cdr args))
    (error "unrecognized arguments:" (string-join args)))
  (let ((g-hooks (load-config)))
    (run-with-store (open-connection)
      (mlet* %store-monad
          ((g-hooks-drv (g-hooks->git-hook-derivation g-hooks))
           (_ (built-derivations (list g-hooks-drv)))
           (g-hooks-out-path -> (derivation->output-path g-hooks-drv)))
        (let* ((number (+ 1 (generation-number (force %g-hooks-profile))))
               (generation (generation-file-name
                            (force %g-hooks-profile) number))
               (gc-generation (generation-file-name
                               (force %g-hooks-gcprofile) number))
               (hooks-dir (string-append (force %git-common-dir) "/hooks")))
          (mkdir-p (force %g-hooks-root))
          (mkdir-p (force %g-hooks-gcroot))
          ;; make the generation pointing at the new g-hooks
          (switch-symlinks generation g-hooks-out-path)
          ;; point the profile at the latest generation
          (switch-symlinks (force %g-hooks-profile) generation)
          ;; remove the existing hooks/ directory (if it exists); this
          ;; shouldn't remove the symlink
          (delete-file-recursively hooks-dir)
          ;; finally point the .git/hooks/ directory at the profile
          (switch-symlinks hooks-dir (string-append (force %g-hooks-profile)
                                                    "/hooks"))
          ;; make a garbage collection root pointing at the new generation to
          ;; avoid removing the hooks during gc passes; note that we point at
          ;; the generation instead of the store path directly so if the git
          ;; repo is removed, the hooks can be garbage collected
          ;;
          ;; it's also important that this symlink is created last so it is
          ;; never broken to avoid race conditions with our delete-generations
          ;; code
          (switch-symlinks gc-generation generation))
        (return g-hooks-out-path)))))

(define (print-generation number)
  "Pretty-print generation NUMBER in a human-readable format."
  (define (display-channel channel)
    (format #t "    ~a:~%" (channel-name channel))
    (format #t "      repository url: ~a~%" (channel-url channel))
    (when (channel-branch channel)
      (format #t "      branch: ~a~%" (channel-branch channel)))
    (format #t "      commit: ~a~%" (channel-commit channel)))
  (define (sexp->channel sexp)
    (match sexp
           (('channel ('name name)
                      ('url url)
                      ('branch branch)
                      ('commit commit)
                      rest ...)
            (channel (name name)
                     (url url)
                     (branch branch)
                     (commit commit)))))
  (unless (zero? number)
    (let* ((generation (generation-file-name (force %g-hooks-profile) number))
           (channels
            (map
             sexp->channel
             (call-with-input-file (string-append generation "/channels.scm")
               read))))
      (display-generation (force %g-hooks-profile) number)
      (format #t "  file name: ~a~%" generation)
      (format #t "  canonical file name: ~a~%" (canonicalize-path generation))
      (unless (null? channels)
        (format #t "  channels:~%")
        (for-each display-channel channels)))))

(define (broken-symlinks directory)
  "Return a list of all broken symlinks in the given DIRECTORY."
  (map
   (lambda (entry)
     (string-append directory "/" entry))
   (scandir directory
            (lambda (entry)
              (not (file-exists? (string-append directory "/" entry)))))))

(define (delete-generations args)
  "Delete all generations other than the current generation."
  (unless (null? (cdr args))
    (error "unrecognized arguments:" (string-join args)))
  (with-store store
    (delete-matching-generations store (force %g-hooks-profile) #f))
  ;; clean up any broken symlinks; these could be from either deleting the
  ;; generations in the previous line or from the user deleting a different
  ;; git repository (the store will already be gc'd in the latter case, but we
  ;; manage some additional symlinks under the gcroot that guix will not clean
  ;; up by default)
  (for-each delete-file (broken-symlinks (force %g-hooks-gcroot))))

(define (list-generations args)
  "Display all g-hooks generations in a human-readable format."
  (unless (null? (cdr args))
    (error "unrecognized arguments:" (string-join args)))
  (for-each print-generation (profile-generations (force %g-hooks-profile))))

(define (switch-generation args)
  "Switch the g-hooks profile to the generation specified in ARGS."
  (unless (= (length args) 2)
    (error "switch-generation takes exactly one argument"))
  (let* ((generation-spec (cadr args))
         (number (relative-generation-spec->number
                  (force %g-hooks-profile)
                  generation-spec)))
    (if number
        (switch-to-generation* (force %g-hooks-profile) number)
        (error "cannot switch to generation:" generation-spec))))

(define %main-options
  '((version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))

(define %main-usage
  "\
usage: g-hooks [-h | --help] [-v | --version] <command> [<args>]

run COMMAND with ARGS, if given

  -h, --help          display this usage message and exit
  -v, --version       display version information and exit

the valid values for COMMAND are listed below:

  reconfigure         switch to a new g-hooks configuration
  list-generations    list all available g-hooks generations
  switch-generation   switch to an existing g-hooks generation
  delete-generations  delete old g-hooks generations
")

(define %main-commands
  `(("reconfigure" . ,reconfigure)
    ("list-generations" . ,list-generations)
    ("delete-generations" . ,delete-generations)
    ("switch-generation" . ,switch-generation)))

(define (main)
  (let ((options (getopt-long
                  (command-line)
                  %main-options
                  #:stop-at-first-non-option #t)))
    (cond
     ((option-ref options 'help #f)
      (display %main-usage))
     ((option-ref options 'version #f)
      (display "g-hooks version 0.3.0")
      (newline))
     ((not (null? (option-ref options '() '())))
      (let* ((arguments (option-ref options '() '()))
             (command (car arguments))
             (command-processor (assoc-ref %main-commands command)))
        (if command-processor
            (command-processor arguments)
            (error "unrecognized command:" command)))))))
