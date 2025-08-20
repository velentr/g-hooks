;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-actions actions nix)
  #:use-module (g-actions paths)
  #:use-module (g-hooks utils)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:export (nix*
            nix-flake-update*))

;;; Commentary:
;;;
;;; Actions for manipulating nix flakes.
;;;
;;; Code:

(define-syntax-rule (nix* args ...)
  "Run nix with the given cli ARGS."
  (program nix "/bin/nix"
           "--extra-experimental-features"
           "flakes"
           "--extra-experimental-features"
           "nix-command"
           args ...))

(define* (nix-flake-update* #:key path lockfile (inputs '()))
  (let* ((path' (or path "."))
         (lockfile' (or lockfile (path* path' "flake.lock"))))
    (nix*
     "flake"
     "update"
     "--output-lock-file"
     #$lockfile'
     "--flake"
     #$(string-append "path:" path')
     #$@inputs)))
