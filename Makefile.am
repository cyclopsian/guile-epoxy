# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: Apache-2.0

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

BUILT_SOURCES = gl.c

if EPOXY_HAS_EGL
BUILT_SOURCES += egl.c
AM_CFLAGS += -DHAVE_EGL
endif

$(BUILT_SOURCES): %.c: registry/%.xml generate.scm
	$(GUILE) --no-auto-compile generate.scm -i epoxy/$*.h $* "$<" "$@"

guileextension_LTLIBRARIES = libguile-epoxy.la
guileextensiondir = $(GUILE_EXTENSION)

moddir = $(GUILE_SITE)
objdir = $(GUILE_SITE_CCACHE)

libguile_epoxy_la_SOURCES = guile-epoxy.c $(BUILT_SOURCES)
libguile_epoxy_la_CFLAGS = $(EPOXY_CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_epoxy_la_LIBADD = $(EPOXY_LIBS) $(GUILE_LIBS)
libguile_epoxy_la_LDFLAGS = -export-dynamic

SCM_BUILT = epoxy/config.install.scm
SCM_NOINST_BUILT = epoxy/config.scm
SOURCES = epoxy/gles2.scm
if EPOXY_HAS_EGL
SOURCES += epoxy/egl.scm
endif

BUILT_SOURCES += $(SCM_BUILT) $(SCM_NOINST_BUILT)

GOBJECTS = $(SCM_BUILT:.scm=.go) $(SOURCES:.scm=.go)

GUILE_WARNINGS = -Warity-mismatch \
		 -Wbad-case-datum \
		 -Wduplicate-case-datum \
		 -Wformat \
		 -Wmacro-use-before-definition \
		 -Wunbound-variable

$(GOBJECTS): %.go: %.scm $(guileextension_LTLIBRARIES) epoxy/config.scm
	echo $(guileextension_LTLIBRARIES)
	$(GUILD) compile -L $(abs_top_srcdir) -L $(abs_top_builddir) \
		$(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

epoxy/config.scm: epoxy/config.scm.in Makefile
	sed -e "s|@extensionlib\@|$(abs_top_builddir)/libguile-epoxy|" \
	    "$<" > "$@"

epoxy/config.install.scm: epoxy/config.scm.in Makefile
	sed -e "s|@extensionlib\@|$(guileextensiondir)/libguile-epoxy|" \
	    "$<" > "$@"

install-data-hook:
	cd $(DESTDIR)$(moddir)/epoxy && mv config.install.scm config.scm
	cd $(DESTDIR)$(objdir)/epoxy && mv config.install.go config.go

nobase_mod_DATA = $(SCM_BUILT) $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS)

CLEANFILES = $(BUILT_SOURCES) $(GOBJECTS) $(DOT_X_FILES)

EXTRA_DIST = $(SCM_BUILT) $(SOURCES) $(GOBJECTS)
