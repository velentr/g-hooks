;;; SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-hooks utils)
  #:use-module (guix gexp)
  #:export (program))

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

(define-syntax-rule (program package path args ...)
  "Run a program from PACKAGE, where the program is at PATH relative to the
package root, with the given ARGS. This hook will exit on failure but not on
success so it may be composed with other hooks."
  #~(let ((rc (status:exit-val
               (system* #$(file-append package path) args ...))))
      (unless (= rc 0)
        (exit rc))))
