# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: Apache-2.0

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

SOURCES = guile-epoxy.c guile-epoxy-gl.c
GENERATED = gl.c

if EPOXY_HAS_EGL
SOURCES += guile-epoxy-egl.c
GENERATED += egl.c
AM_CFLAGS += -DHAVE_EGL
endif

$(GENERATED): %.c: registry/%.xml generate.scm
	$(GUILE) --no-auto-compile generate.scm -i epoxy/$*.h $* "$<" "$@"

guileextension_LTLIBRARIES = libguile-epoxy.la
guileextensiondir = $(GUILE_EXTENSION)

moddir = $(GUILE_SITE)
objdir = $(GUILE_SITE_CCACHE)

nodist_libguile_epoxy_la_SOURCES = $(GENERATED)
libguile_epoxy_la_SOURCES = $(SOURCES)
libguile_epoxy_la_CFLAGS = $(EPOXY_CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_epoxy_la_LIBADD = $(EPOXY_LIBS) $(GUILE_LIBS)
libguile_epoxy_la_LDFLAGS = -export-dynamic

DOT_X = $(SOURCES:.c=.x)
BUILT_SOURCES = $(DOT_X) $(GENERATED)

$(DOT_X): %.x : %.c
	$(GUILE_SNARF) $(libguile_epoxy_la_CFLAGS) $(CFLAGS) $< > $@ \
		|| { rm $@; false; }

SCMS = epoxy/common.scm \
       epoxy/gles2.scm epoxy/gl/commands.scm epoxy/gl/enums.scm
if EPOXY_HAS_EGL
SCMS += epoxy/egl.scm epoxy/egl/commands.scm epoxy/egl/enums.scm
endif

GOBJECTS = $(SCMS:.scm=.go)

GUILE_WARNINGS = -Warity-mismatch \
		 -Wbad-case-datum \
		 -Wduplicate-case-datum \
		 -Wformat \
		 -Wmacro-use-before-definition \
		 -Wunbound-variable

$(GOBJECTS): %.go: %.scm $(guileextension_LTLIBRARIES)
	./env $(GUILD) compile -L $(abs_top_srcdir) -L $(abs_top_builddir) \
		$(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

TESTS = tests/egl-gles2.scm
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = ./env $(GUILE) -s
check_SCRIPTS = $(TESTS:.scm=.go)

$(check_SCRIPTS): %.go: %.scm
	./env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

nobase_mod_DATA = $(SCMS) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS) $(check_SCRIPTS)

CLEANFILES = $(BUILT_SOURCES) $(GOBJECTS) $(check_SCRIPTS)

EXTRA_DIST = $(SCMS) $(TESTS) generate.scm LICENSES .reuse registry
