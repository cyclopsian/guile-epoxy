/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: Apache-2.0 */

#include <libguile.h>

SCM_API void scm_init_epoxy(void);

void init_gl_commands(void *data);
void init_gl_enums(void *data);
#ifdef HAVE_EGL
void init_egl_commands(void *data);
void init_egl_enums(void *data);
#endif

void scm_init_epoxy(void) {
  scm_c_define_module("epoxy gl commands",  init_gl_commands, NULL);
  scm_c_define_module("epoxy gl enums",  init_gl_enums, NULL);
#ifdef HAVE_EGL
  scm_c_define_module("epoxy egl commands", init_egl_commands, NULL);
  scm_c_define_module("epoxy egl enums", init_egl_enums, NULL);
#endif
}
