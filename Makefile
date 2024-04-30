# SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
#
# SPDX-License-Identifier: GPL-3.0-only

DESTDIR ?= /usr/local
GUILD ?= guild
GUILD_FLAGS ?=
GUILE_VERSION ?= 3.0
INSTALL ?= install
TARGET ?=

EXT_PATH ?= $(DESTDIR)/share/guix/extensions
SCM_PATH ?= $(DESTDIR)/share/guile/site/$(GUILE_VERSION)/g-hooks
SCM_COMPILED_PATH ?= $(DESTDIR)/lib/guile/$(GUILE_VERSION)/site-ccache/g-hooks

scm_sources = \
	g-hooks/core.scm \
	g-hooks/library.scm \
	g-hooks/utils.scm
objs = $(patsubst %.scm,%.go,$(scm_sources))

guild_target := $(if $(TARGET),--target=$(TARGET),)

all: $(objs)

install: $(objs)
	mkdir --parents $(SCM_PATH)
	$(foreach src,$(scm_sources),$(INSTALL) --mode=0644 $(src) $(SCM_PATH);)
	mkdir --parents $(SCM_COMPILED_PATH)
	$(foreach obj,$(objs),$(INSTALL) --mode=0644 $(obj) $(SCM_COMPILED_PATH);)
	mkdir --parents $(EXT_PATH)
	$(INSTALL) --mode=0644 guix/extensions/g-hooks.scm $(EXT_PATH)

clean:
	rm --force $(objs)

%.go: %.scm
	$(GUILD) compile --output=$@ $< $(guild_target) $(GUILD_FLAGS)

.PHONY: all clean
