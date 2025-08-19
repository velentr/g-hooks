;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-actions ui)
  #:use-module (g-actions configuration)
  #:use-module (g-actions paths)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix status)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:export (g-actions-main))

(define (load-binding-from-configuration binding)
  "Load the configuration file from ~/.config/g-actions/config.scm and evaluate
BINDING in a sandbox."
  (let ((config-sexp
         (call-with-input-file (xdg-config-home "g-actions" "config.scm")
           (lambda (conf)
             (define (accumulate-sexps so-far)
               (let ((maybe-sexp (read conf)))
                 (if (eof-object? maybe-sexp)
                     (reverse so-far)
                     (accumulate-sexps (cons maybe-sexp so-far)))))
             (accumulate-sexps '())))))
    (eval
     (cons
      'begin
      (append
       config-sexp
       (list binding)))
     (interaction-environment))))

(define (variable-from-configuration args)
  "Load the variable defined in the configuration file and named as the command
argument."
  (let ((variable-name (assoc-ref args 'argument)))
    (unless variable-name
      (error "variable name is a required argument"))
    (load-binding-from-configuration (string->symbol variable-name))))

(define (run-action-with-store store workflow-name workflow)
  (cond
   ((nil? workflow)
    #t)
   ((pair? workflow)
    (and
     (run-action-with-store store workflow-name (car workflow))
     (run-action-with-store store workflow-name (cdr workflow))))
   (else
    (run-with-store store
      (mlet* %store-monad
          ((workflow-drv (gexp->script workflow-name workflow))
           (_ (built-derivations (list workflow-drv)))
           (workflow-out-path -> (derivation->output-path workflow-drv)))
        (return
         (zero? (status:exit-val (system* workflow-out-path)))))))))

(define (run args)
  "Execute a g-action."
  (let* ((workflow-name (assoc-ref args 'argument))
         (workflow (variable-from-configuration args))
         (olddir (getcwd))
         (workdir (mkdtemp
                   (string-append "/tmp/g-actions-" workflow-name "-XXXXXX"))))
    (chdir workdir)
    (let ((success
           (parameterize ((%graft? (assoc-ref args 'graft?)))
             (with-status-verbosity (assoc-ref args 'verbosity)
               (with-store store
                 (set-build-options-from-command-line store args)
                 (with-build-handler (build-notifier
                                      #:use-substitutes?
                                      (assoc-ref args 'substitutes?)
                                      #:verbosity
                                      (assoc-ref args 'verbosity)
                                      #:dry-run?
                                      (assoc-ref args 'dry-run?))
                   (run-action-with-store store workflow-name workflow)))))))
      (chdir olddir)
      (if (assoc-ref args 'skip-cleanup?)
          (begin
            (display workdir)
            (newline))
          (delete-file-recursively workdir))
      (unless success
        (error "workflow failed")))))

(define (whereis args)
  "Print the path to the given repository defined in the g-actions
configuration."
  (let ((repository (variable-from-configuration args)))
    (unless (repository? repository)
      (error "variable is not a repository:" (assoc-ref args 'argument)))
    (display (repository->path repository))
    (newline)))

(define %main-usage
  "\
usage: guix g-actions [-h | --help] <command> [<args>]

run COMMAND with ARGS, if given

  -h, --help             display this usage message and exit

the valid values for COMMAND are listed below:

  run                    execute a named workflow from the config file
  whereis                determine the persistent path to the given repository

the valid ARGS are listed below:

  -S, --skip-cleanup     skip cleaning up the working directory after running")

(define %main-options
  (cons*
   (option '(#\h "help") #f #f
           (lambda _
             (display %main-usage)
             (show-build-options-help)
             (newline)
             (exit 0)))
   (option '(#\S "skip-cleanup") #f #f
           (lambda (opt name arg result . rest)
             (apply values
                    (alist-cons 'skip-cleanup? #t
                                (alist-delete 'skip-cleanup? result eq?))
                    rest)))
   %standard-build-options))

(define %default-options
  ;; note that this is modified dynamically and cannot be a compile-time
  ;; constant
  `((graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (debug . 0)
    (verbosity . 1)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)))

(define %main-commands
  `(("run" . ,run)
    ("whereis" . ,whereis)))

(define (parse-sub-command arg result)
  "Parse ARG as a sub-command, adding it to RESULT."
  (if (assoc-ref result 'action)
      ;; we've already got an action, this is an argument
      (alist-cons 'argument arg result)
      (let ((command-processor (assoc-ref %main-commands arg)))
        (if command-processor
            (alist-cons 'action command-processor result)
            (error "unrecognized command:" arg)))))

(define (g-actions-main args)
  (let* ((opts (parse-command-line args %main-options (list %default-options)
                                   #:argument-handler parse-sub-command))
         (action (assoc-ref opts 'action)))
    (when action
      (action opts))))
