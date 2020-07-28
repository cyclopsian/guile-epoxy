/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: Apache-2.0 */

#include <libguile.h>
#include <epoxy/common.h>

SCM_API void scm_init_epoxy_common(void);

#define FUNC_NAME s_scm_epoxy_extension_in_string
SCM_DEFINE_PUBLIC(scm_epoxy_extension_in_string,
    "epoxy-extension-in-string", 2, 0, 0,
    (SCM extension_list, SCM ext), "") {
  SCM_VALIDATE_STRING(SCM_ARG1, extension_list);
  SCM_VALIDATE_STRING(SCM_ARG2, ext);
  scm_dynwind_begin(0);
  char *c_extension_list = scm_to_utf8_string(extension_list);
  scm_dynwind_free(c_extension_list);
  char *c_ext= scm_to_utf8_string(ext);
  scm_dynwind_free(c_ext);
  bool result = epoxy_extension_in_string(c_extension_list, c_ext);
  scm_dynwind_end();
  return scm_from_bool(result);
}
#undef FUNC_NAME

void scm_init_epoxy_common(void) {
#ifndef SCM_MAGIC_SNARFER
#include "guile-epoxy.x"
#endif
}

