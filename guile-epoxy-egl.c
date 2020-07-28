/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: Apache-2.0 */

#include <libguile.h>
#include <epoxy/egl.h>

SCM_API void scm_init_epoxy_egl_commands(void);
SCM_API void scm_init_epoxy_egl_enums(void);
SCM_API void scm_init_epoxy_egl(void);

#define FUNC_NAME s_scm_epoxy_has_egl_extension
SCM_DEFINE_PUBLIC(scm_epoxy_has_egl_extension,
    "epoxy-has-egl-extension", 2, 0, 0,
    (SCM dpy, SCM extension), "") {
  SCM_VALIDATE_POINTER(SCM_ARG1, dpy);
  void *c_dpy = SCM_POINTER_VALUE(dpy);
  SCM_VALIDATE_STRING(SCM_ARG2, extension);
  char *c_extension = scm_to_utf8_string(extension);
  bool result = epoxy_has_egl_extension((EGLDisplay) c_dpy, c_extension);
  free(c_extension);
  return scm_from_bool(result);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_epoxy_egl_version
SCM_DEFINE_PUBLIC(scm_epoxy_egl_version, "epoxy-egl-version", 1, 0, 0,
    (SCM dpy), "") {
  SCM_VALIDATE_POINTER(SCM_ARG1, dpy);
  void *c_dpy = SCM_POINTER_VALUE(dpy);
  return scm_from_int(epoxy_egl_version((EGLDisplay) c_dpy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_epoxy_has_egl
SCM_DEFINE_PUBLIC(scm_epoxy_has_egl, "epoxy-has-egl", 0, 0, 0,
    (), "") {
  return scm_from_bool(epoxy_has_egl());
}
#undef FUNC_NAME

void scm_init_epoxy_egl(void) {
#ifndef SCM_MAGIC_SNARFER
#include "guile-epoxy-egl.x"
#endif
}
