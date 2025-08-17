;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (guix extensions g-actions)
  #:use-module (g-actions ui)
  #:use-module (guix scripts)
  #:export (guix-g-actions))

(define-command (guix-g-actions . args)
  (category development)
  (synopsis "run local actions")

  (g-actions-main args))
