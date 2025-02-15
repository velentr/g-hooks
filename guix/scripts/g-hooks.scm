;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (guix scripts g-hooks)
  #:use-module (g-hooks core)
  #:use-module (guix scripts)
  #:export (guix-g-hooks))

(define-command (guix-g-hooks . args)
  (category development)
  (synopsis "build and deploy git hooks")

  (g-hooks-main args))
