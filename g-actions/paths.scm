;;; SPDX-FileCopyrightText: 2025 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (g-actions paths)
  #:export (~
            path
            path*
            xdg-config-home
            xdg-data-home))

;;; Commentary:
;;;
;;; Common path manipulation and constants.
;;;
;;; Code:

(define* (path components)
  "Make a path from COMPONENTS."
  (string-join components "/"))

(define* (path* . components)
  "Make a path from COMPONENTS."
  (path components))

(define* (~ . components)
  "Make a path from COMPONENTS relative to the current user's home directory."
  (let (($HOME (or (getenv "HOME")
                   (error "home directory is not set"))))
    (path (cons $HOME components))))

(define (make-xdg-directory envvar default)
  "Make a function for finding an XDG directory defined by ENVVAR, defaulting to
DEFAULT if the environment variable is not set."
  (let ((xdg-dir (delay (or (getenv envvar) default))))
    (lambda* (. components)
      (path (cons (force xdg-dir) components)))))

(define xdg-config-home
  (make-xdg-directory "XDG_CONFIG_HOME" (~ ".config")))

(define xdg-data-home
  (make-xdg-directory "XDG_DATA_HOME" (~ ".local/share")))
