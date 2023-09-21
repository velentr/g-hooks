#!/bin/sh

# SPDX-FileCopyrightText: 2023 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

top_srcdir=$(dirname "$0")

current_guix="$HOME/.config/guix/current/share/guile/site/3.0"

GUILE_LOAD_PATH="$top_srcdir:$current_guix:$GUILE_LOAD_PATH"
export GUILE_LOAD_PATH

PATH="$top_srcdir/scripts:$PATH"
export PATH

GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

exec "$@"
