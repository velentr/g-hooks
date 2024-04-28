#!/bin/sh

# SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

if [ -n "$(git status --porcelain)" ]; then
	echo "working tree is not clean"
	exit 1
fi

if [ -z "$1" ]; then
	echo "usage: make-release <version>"
	exit 1
fi

if ! echo "$1" | grep --extended-regexp --silent '^[0-9]+(\.[0-9]+){2}$'; then
	echo "new version must be X.Y.Z"
	exit 1
fi

newversion="$1"

sed --regexp-extended \
    --in-place \
    "s/\"(g-hooks version) [0-9]+\.[0-9]+\.[0-9]+\"/\"\1 $newversion\"/" \
    g-hooks/core.scm

git commit --all --signoff --message="core: Update to $newversion"
git tag v"$newversion"
