;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: Apache-2.0

(define-module (epoxy egl commands))

(eval-when (expand load eval)
  (load-extension "libguile-epoxy" "scm_init_epoxy_egl_commands"))
