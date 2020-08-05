;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: Apache-2.0

(define-module (epoxy tests egl-gles2)
  #:use-module (epoxy egl)
  #:use-module (epoxy gles2)
  #:use-module (ice-9 binary-ports)
  #:use-module (oop goops)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-64)
  #:duplicates (merge-generics))

(module-set! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)
(test-runner-current (test-runner-create))

(test-group "epoxy-has-egl"
  (test-assert (epoxy-has-egl)))

(test-group "epoxy-require-egl"
  (test-assert (epoxy-require-egl)))

(test-group "epoxy-has-egl-extension"
  (test-assert (epoxy-has-egl-extension "EGL_MESA_platform_surfaceless")))

(test-group "epoxy-require-egl-extension"
  (test-assert (epoxy-require-egl-extension "EGL_MESA_platform_surfaceless")))

(test-group "epoxy-require-egl-extension invalid extension"
  (test-error (epoxy-require-egl-extension "INVALID_EXTENSION")))

(epoxy-require-egl-extension "EGL_MESA_platform_surfaceless")

(define config-attribs-desktop
  '(EGL_SURFACE_TYPE      EGL_PBUFFER_BIT
    EGL_RED_SIZE          8
    EGL_GREEN_SIZE        8
    EGL_BLUE_SIZE         8
    EGL_ALPHA_SIZE        8
    EGL_DEPTH_SIZE        0
    EGL_CONFORMANT        EGL_OPENGL_BIT
    EGL_RENDERABLE_TYPE   EGL_OPENGL_BIT
    EGL_NATIVE_RENDERABLE EGL_TRUE))

(define config-attribs
  (list EGL_SURFACE_TYPE      EGL_PBUFFER_BIT
        EGL_RED_SIZE          8
        EGL_GREEN_SIZE        8
        EGL_BLUE_SIZE         8
        EGL_ALPHA_SIZE        8
        EGL_DEPTH_SIZE        0
        EGL_CONFORMANT        EGL_OPENGL_ES2_BIT
        EGL_RENDERABLE_TYPE   EGL_OPENGL_ES2_BIT
        EGL_NATIVE_RENDERABLE EGL_TRUE))

(define ctx-attribs
  (list EGL_CONTEXT_MAJOR_VERSION 2))

(define ctx-attribs-desktop
  (list EGL_CONTEXT_MAJOR_VERSION 2))

(let ((disp (egl-get-platform-display EGL_PLATFORM_SURFACELESS_MESA)))
  (egl-initialize disp)
  (test-group "epoxy-egl-version"
    (test-assert (> (epoxy-egl-version disp) 0)))

  (test-group "epoxy-require-egl-version"
    (test-assert (epoxy-require-egl-version disp 10)))

  (test-group "epoxy-require-egl-version invalid"
    (test-error (epoxy-require-egl-version disp 999)))

  (let* ((config (egl-choose-config disp config-attribs-desktop 1))
         (context (egl-create-context disp (car config) ctx-attribs-desktop)))
    (egl-make-current disp #f #f context)
    (egl-bind-api EGL_OPENGL_API)

    (test-group "epoxy-has-gl-extension invalid extension"
      (test-equal (epoxy-has-gl-extension "INVALID_EXTENSION") #f))

    (test-group "epoxy-require-gl-extension invalid extension"
      (test-error (epoxy-require-gl-extension "INVALID_EXTENSION")))

    (test-group "epoxy-gl-version"
      (test-assert (> (epoxy-gl-version) 0)))

    (test-group "epoxy-require-gl-version"
      (test-assert (epoxy-require-gl-version 10)))

    (test-group "epoxy-require-gl-version invalid"
      (test-error (epoxy-require-gl-version 999)))

    (test-group "epoxy-glsl-version"
      (test-assert (> (epoxy-glsl-version) 0)))

    (test-group "epoxy-require-glsl-version"
      (test-assert (epoxy-require-glsl-version 10)))

    (test-group "epoxy-require-glsl-version invalid"
      (test-error (epoxy-require-glsl-version 999)))

    (egl-make-current disp #f #f #f)
    (egl-destroy-context disp context))

  (let* ((config (egl-choose-config disp config-attribs 1))
         (context (egl-create-context disp (car config) ctx-attribs)))
    (egl-make-current disp #f #f context)
    (egl-bind-api EGL_OPENGL_ES_API)

    (test-group "epoxy-has-gl-extension gles invalid extension"
      (test-equal (epoxy-has-gl-extension "INVALID_EXTENSION") #f))

    (test-group "epoxy-require-gl-extension gles invalid extension"
      (test-error (epoxy-require-gl-extension "INVALID_EXTENSION")))

    (test-group "epoxy-gl-version gles"
      (test-assert (> (epoxy-gl-version) 0)))

    (test-group "epoxy-require-gl-version gles"
      (test-assert (epoxy-require-gl-version 10)))

    (test-group "epoxy-require-gl-version gles invalid"
      (test-error (epoxy-require-gl-version 999)))

    (test-group "epoxy-glsl-version gles"
      (test-assert (> (epoxy-glsl-version) 0)))

    (test-group "epoxy-require-glsl-version gles"
      (test-assert (epoxy-require-glsl-version 10)))

    (test-group "epoxy-require-glsl-version gles invalid"
      (test-error (epoxy-require-glsl-version 999)))

    (egl-make-current disp #f #f #f)
    (egl-destroy-context disp context))

  (epoxy-require-egl-version disp 14)
  (egl-terminate disp))

(define width  64)
(define height 8)

(define vs-source "
precision mediump float;
attribute vec2 pos;
void main(void) {
  gl_Position = vec4(pos, 0, 1);
}
")
(define fs-source (format #f "
precision mediump float;
void main(void) {
  vec2 screenSize = vec2(~a, ~a);
  vec2 v = gl_FragCoord.xy / screenSize;
  v = v * vec2(1, 0.7) + vec2(0, 0.3);
  gl_FragColor = vec4(0, v, 1);
}
" width height ))

(test-begin "egl-gles2")
(let* ((disp (egl-get-platform-display EGL_PLATFORM_SURFACELESS_MESA))
       (config (begin (egl-initialize disp)
                      (egl-choose-config disp config-attribs 1)))
       (context (egl-create-context disp (car config) ctx-attribs)))
  (egl-make-current disp #f #f context)
  (egl-bind-api EGL_OPENGL_ES_API)
  (let ((fb (gl-create-framebuffer))
        (rb (gl-create-renderbuffer))
        (tex (gl-create-texture))
        (buf (gl-create-buffer))
        (prog (gl-create-program))
        (vs (gl-create-shader GL_VERTEX_SHADER))
        (fs (gl-create-shader GL_FRAGMENT_SHADER)))
    (gl-bind-framebuffer GL_FRAMEBUFFER fb)
    (gl-bind-renderbuffer GL_RENDERBUFFER rb)

    (gl-renderbuffer-storage GL_RENDERBUFFER GL_DEPTH_COMPONENT16 width height)
    (gl-bind-texture GL_TEXTURE_2D tex)
    (gl-tex-image-2d GL_TEXTURE_2D
                     0 GL_RGBA width height 0 GL_RGBA GL_UNSIGNED_BYTE)
    (gl-bind-texture GL_TEXTURE_2D)

    (gl-bind-buffer GL_ARRAY_BUFFER buf)
    (gl-buffer-data GL_ARRAY_BUFFER #f32(-1 -1  1 -1 -1  1
                                         -1  1  1 -1  1  1)
                    GL_STATIC_DRAW)

    (gl-framebuffer-renderbuffer
      GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER rb)
    (gl-framebuffer-texture-2d
      GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D tex 0)

    (gl-shader-source vs vs-source)
    (gl-compile-shader vs)
    (when (zero? (gl-get-shader-i vs GL_COMPILE_STATUS))
      (format (current-error-port) "Error compiling vertex shader:\n~a\n"
              (gl-get-shader-info-log vs)))
    (gl-shader-source fs fs-source)
    (gl-compile-shader fs)
    (when (zero? (gl-get-shader-i fs GL_COMPILE_STATUS))
      (format (current-error-port) "Error compiling fragment shader:\n~a\n"
              (gl-get-shader-info-log fs)))

    (gl-attach-shader prog vs)
    (gl-attach-shader prog fs)
    (gl-link-program prog)
    (when (zero? (gl-get-program-i prog GL_LINK_STATUS))
      (format (current-error-port) "Error linking program:\n~a\n"
              (gl-get-program-info-log prog)))
    (gl-use-program prog)

    (let ((pos-location (gl-get-attrib-location prog "pos")))
      (gl-vertex-attrib-pointer pos-location 2 GL_FLOAT #f 0 0)
      (gl-enable-vertex-attrib-array pos-location))

    (gl-viewport 0 0 width height)
    (gl-clear-color 0 0 0 1)
    (gl-clear-depth 1)
    (gl-clear GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)
    (gl-draw-arrays GL_TRIANGLES 0 6)
    (gl-finish)

    (let* ((stride (* width 4))
           (pixels (make-u8vector (* stride height))))
      (gl-read-pixels 0 0 width height GL_RGBA GL_UNSIGNED_BYTE pixels)
      (for-each
        (λ (y)
          (for-each
            (λ (x)
              (let* ((off (+ (* x 4) (* y stride)))
                     (r (u8vector-ref pixels (+ off 0)))
                     (g (u8vector-ref pixels (+ off 1)))
                     (b (u8vector-ref pixels (+ off 2))))
                (format #t "\x1b[48;2;~a;~a;~am " r g b)))
            (iota width))
          (format #t "\x1b[0m\n"))
        (iota height)))

    (delete fs)
    (delete vs)
    (delete prog)
    (delete buf)
    (delete tex)
    (delete rb)
    (delete fb))

  (egl-terminate disp))
(test-end "egl-gles2")

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
