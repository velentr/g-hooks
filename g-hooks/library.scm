;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks library)
  #:use-module (gnu packages license)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (g-hooks utils)
  #:export (gitlint/commit-msg
            reuse-lint/pre-commit))

;;; Commentary:
;;;
;;; Library of useful g-hooks.
;;;
;;; G-hooks are suffixed with '/<git hook>' (e.g. /pre-commit) by convention,
;;; but this is only for documentation purposes; there's nothing that checks
;;; g-hooks are only used for specific git hooks.
;;;
;;; Code:

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
