;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks core)
  #:use-module (g-hooks library)
  #:use-module (gcrypt hash)
  #:use-module (gnu services)
  #:use-module (guix build utils)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix scripts package)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
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
;;; In addition to the profile in .git/g-hooks, there are also a set of symlinks
;;; in /var/guix/gcroots/auto (i.e. guix's indirect gcroot directory) that act
;;; as garbage collection roots and point at the generations of the profile in
;;; .git/g-hooks/. The gc roots point at the generation links in .git/g-hooks/
;;; instead of directly at the store paths so that if the user deletes the git
;;; repository, the store object for the g-hooks will get garbage collected
;;; automatically with no additional action from the user. If this happens, the
;;; indirect gcroot links will be broken but will not be automatically cleaned
;;; up by 'guix gc'.
;;;
;;; Code:

(define-syntax-rule (g-hooks services ...)
  "Create a set of g-hooks using SERVICES."
  (cons*
   services
   ...
   %base-g-hooks-services))

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

(define %g-hooks-root
  (delay (string-append (force %git-common-dir) "/g-hooks")))

(define %g-hooks-profile
  (delay (string-append (force %g-hooks-root) "/g-hooks")))

(define (g-hooks->git-hook-derivation g-hooks)
  "Convert a set of G-HOOKS into a derivation that can be used as a .git/hooks
directory."
  (service-value
   (fold-services g-hooks #:target-type g-hooks-service-type)))

(define (reconfigure args)
  "Reconfigure .git/hooks/ based on the repository's g-hooks."
  (unless (null? (cdr args))
    (error "unrecognized arguments:" (string-join args)))
  (let ((g-hooks (load-config)))
    (run-with-store (open-connection)
      (mlet* %store-monad
          ((g-hooks-drv (g-hooks->git-hook-derivation g-hooks))
           (_ (built-derivations (list g-hooks-drv)))
           (g-hooks-out-path -> (derivation->output-path g-hooks-drv))
           (number -> (+ 1 (generation-number (force %g-hooks-profile))))
           (generation -> (generation-file-name
                           (force %g-hooks-profile) number))
           (hooks-dir -> (string-append (force %git-common-dir) "/hooks")))
        (mkdir-p (force %g-hooks-root))
        ;; make the generation pointing at the new g-hooks
        (switch-symlinks generation g-hooks-out-path)
        ;; point the profile at the latest generation
        (switch-symlinks (force %g-hooks-profile) generation)
        ;; remove the existing hooks/ directory (if it exists); this shouldn't
        ;; remove the symlink
        (delete-file-recursively hooks-dir)
        ;; finally point the .git/hooks/ directory at the profile
        (switch-symlinks hooks-dir (string-append (force %g-hooks-profile)
                                                  "/hooks"))
        ;; make an indirect garbage collection root pointing at the new
        ;; generation to avoid removing the hooks during gc passes; note that we
        ;; point at the generation instead of the store path directly so if the
        ;; git repo is removed, the hooks can be garbage collected
        (mbegin %store-monad
          ((store-lift add-indirect-root) generation)
          (return g-hooks-out-path))))))

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
    (delete-matching-generations store (force %g-hooks-profile) #f)))

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
      (display "g-hooks version 0.3.2")
      (newline))
     ((not (null? (option-ref options '() '())))
      (let* ((arguments (option-ref options '() '()))
             (command (car arguments))
             (command-processor (assoc-ref %main-commands command)))
        (if command-processor
            (command-processor arguments)
            (error "unrecognized command:" command)))))))
