# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: Apache-2.0

AC_INIT([guile-epoxy], [1.0], [jason@cycles.network])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR([guile-epoxy.c])
AC_USE_SYSTEM_EXTENSIONS
AM_INIT_AUTOMAKE([gnu -Wall -Werror foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_FLAGS
AC_PATH_PROG(GUILE_SNARF,[guile-snarf-$GUILE_EFFECTIVE_VERSION])
AC_SUBST(GUILE_SNARF)

if test "$cross_compiling" != no; then
  GUILE_TARGET="--target=$host_alias"
  AC_SUBST([GUILE_TARGET])
fi

PKG_CHECK_MODULES(EPOXY, epoxy)
AC_SUBST(EPOXY_LIBS)
AC_SUBST(EPOXY_CFLAGS)

epoxy_has_egl=`$PKG_CONFIG --print-errors --variable=epoxy_has_egl epoxy`
AM_CONDITIONAL([EPOXY_HAS_EGL], [test "x$epoxy_has_egl" = "x1"])

AC_PROG_CC
AM_PROG_AR

LT_PREREQ([2.2])
LT_INIT([disable-static])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_OUTPUT
