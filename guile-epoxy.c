/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: Apache-2.0 */

#include <libguile.h>

SCM_API void scm_init_epoxy(void);

void init_gl(void *data);
#ifdef HAVE_GLX
void init_glx(void *data);
#endif
#ifdef HAVE_EGL
void init_egl(void *data);
#endif

void scm_init_epoxy(void) {
  scm_c_define_module("epoxy gl generated",  init_gl, NULL);
#ifdef HAVE_GLX
  scm_c_define_module("epoxy glx generated", init_glx, NULL);
#endif
#ifdef HAVE_EGL
  scm_c_define_module("epoxy egl generated", init_egl, NULL);
#endif
}
