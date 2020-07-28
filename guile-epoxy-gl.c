/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: Apache-2.0 */

#include <libguile.h>
#include <epoxy/gl.h>

SCM_API void scm_init_epoxy_gl_commands(void);
SCM_API void scm_init_epoxy_gl_enums(void);
SCM_API void scm_init_epoxy_gl(void);

#define FUNC_NAME s_scm_epoxy_has_gl_extension
SCM_DEFINE_PUBLIC(scm_epoxy_has_gl_extension, "epoxy-has-gl-extension", 1, 0, 0,
    (SCM extension), "") {
  SCM_VALIDATE_STRING(SCM_ARG1, extension);
  char *c_extension = scm_to_utf8_string(extension);
  bool result = epoxy_has_gl_extension(c_extension);
  free(c_extension);
  return scm_from_bool(result);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_epoxy_is_desktop_gl
SCM_DEFINE_PUBLIC(scm_epoxy_is_desktop_gl, "epoxy-is-desktop-gl", 0, 0, 0,
    (), "") {
  return scm_from_bool(epoxy_is_desktop_gl());
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_epoxy_gl_version
SCM_DEFINE_PUBLIC(scm_epoxy_gl_version, "epoxy-gl-version", 0, 0, 0,
    (), "") {
  return scm_from_int(epoxy_gl_version());
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_epoxy_glsl_version
SCM_DEFINE_PUBLIC(scm_epoxy_glsl_version, "epoxy-glsl-version", 0, 0, 0,
    (), "") {
  return scm_from_int(epoxy_glsl_version());
}
#undef FUNC_NAME

void scm_init_epoxy_gl(void) {
#ifndef SCM_MAGIC_SNARFER
#include "guile-epoxy-gl.x"
#endif
}
