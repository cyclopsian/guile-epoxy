;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: Apache-2.0

(define-module (epoxy egl)
  #:use-module (epoxy config)
  #:use-module (epoxy egl commands)
  #:use-module (epoxy egl enums)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (system foreign)
  #:use-module (system foreign-object)
  #:export (<egl-config> <egl-context> <egl-display>
            <egl-image>  <egl-surface> <egl-sync>

            egl-error egl-bind-api egl-bind-tex-image egl-choose-config
            egl-client-wait-sync egl-copy-buffers egl-create-context
            egl-create-image egl-create-pbuffer-from-client-buffer
            egl-create-pbuffer-surface egl-create-pixmap-surface
            egl-create-sync egl-create-window-surface egl-destroy-context
            egl-destroy-image egl-destroy-surface egl-destroy-sync
            egl-get-config-attrib egl-get-configs egl-get-current-context
            egl-get-current-display egl-get-current-surface egl-get-error
            egl-get-platform-display egl-get-sync-attrib egl-initialize
            egl-make-current egl-query-api egl-query-context egl-query-string
            egl-query-surface egl-release-tex-image egl-release-thread
            egl-surface-attrib egl-swap-buffers egl-swap-interval egl-terminate
            egl-wait-client egl-wait-gl egl-wait-native egl-wait-sync))

(module-use! (module-public-interface (current-module))
             (resolve-interface '(epoxy egl enums)))

(define-foreign-object-type <egl-config>
  make-egl-config (pointer))
(define-foreign-object-type <egl-context>
  make-egl-context (pointer))
(define-foreign-object-type <egl-display>
  make-egl-display (pointer))
(define-foreign-object-type <egl-image>
  make-egl-image (pointer))
(define-foreign-object-type <egl-surface>
  make-egl-surface (pointer))
(define-foreign-object-type <egl-sync>
  make-egl-sync (pointer))

(define (foreign->pointer f)
  (make-pointer (slot-ref f 'pointer)))

(define (foreign->nullable-pointer f)
  (if f (foreign->pointer f) %null-pointer))

(define egl-errors
  ((λ ()
     (define-syntax gen-error-alist
       (syntax-rules ()
         ((gen-error-alist)
          '())
         ((gen-error-alist sym rest ...)
          (cons (cons sym 'sym) (gen-error-alist rest ...)))))

     (gen-error-alist EGL_SUCCESS           EGL_NOT_INITIALIZED
                      EGL_BAD_ACCESS        EGL_BAD_ALLOC
                      EGL_BAD_ATTRIBUTE     EGL_BAD_CONTEXT
                      EGL_BAD_CONFIG        EGL_BAD_CURRENT_SURFACE
                      EGL_BAD_DISPLAY       EGL_BAD_SURFACE
                      EGL_BAD_MATCH         EGL_BAD_PARAMETER
                      EGL_BAD_NATIVE_PIXMAP EGL_BAD_NATIVE_WINDOW
                      EGL_CONTEXT_LOST))))

(define (egl-get-error)
  (assoc-ref egl-errors (eglGetError)))

(define* (egl-error #:optional subr)
  (scm-error 'egl-error subr "~a" (list (egl-get-error)) '()))

(define (egl-bind-api api)
  (unless (eglBindAPI api)
    (egl-error "egl-bind-api")))

(define-method
  (egl-bind-tex-image (disp <egl-display>) (surface <egl-surface>) buffer)
  (unless (eglBindTexImage
            (foreign->pointer disp) (foreign->pointer surface) buffer)
    (egl-error "egl-bind-tex-image")))

(define-method (egl-choose-config (disp <egl-display>) attrib-list num-configs)
  (let ((disp-ptr   (foreign->pointer disp))
        (attrib-ptr (bytevector->pointer
                      (any->s32vector (append attrib-list (list EGL_NONE)))))
        (num-config-vec (make-s32vector 1)))

    (unless num-configs
      (unless (eglChooseConfig disp-ptr attrib-ptr %null-pointer 0
                               (bytevector->pointer num-config-vec))
        (egl-error "egl-choose-config"))
      (set! num-configs (s32vector-ref num-config-vec 0)))

    (let ((configs (make-bytevector (* (sizeof '*) num-configs))))
      (unless (eglChooseConfig disp-ptr attrib-ptr (bytevector->pointer configs)
                               num-configs (bytevector->pointer num-config-vec))
        (egl-error "egl-choose-config"))
      (map make-egl-config
           (bytevector->uint-list configs (native-endianness) (sizeof '*))))))

(define-method (egl-choose-config (disp <egl-display>) attrib-list)
  (egl-choose-config disp attrib-list #f))

(define-method
  (egl-client-wait-sync (disp <egl-display>) (sync <egl-sync>) flags time)
  (unless (eglClientWaitSync
            (foreign->pointer disp) (foreign->pointer sync) flags time)
        (egl-error "egl-client-wait-sync")))

(define-method
  (egl-copy-buffers (disp <egl-display>) (surface <egl-surface>) target)
  (unless (eglCopyBuffers
            (foreign->pointer disp) (foreign->pointer surface) target)
    (egl-error "egl-copy-buffers")))

(define (check-optional-foreign ptr-obj class method)
  (cond
    ((eq? ptr-obj #f)
     %null-pointer)
    ((eq? (class-of ptr-obj) class)
     (foreign->pointer ptr-obj))
    (else
      (scm-error
        'wrong-type-arg method
        "Must be #f or an ~a ~a"
        (list (class-name class) ptr-obj)
        (list ptr-obj)))))

(define-method
  (egl-create-context
    (disp <egl-display>) (config <egl-config>) share-context attrib-list)
  (let* ((share-ptr (check-optional-foreign
                      share-context <egl-context> "egl-create-context"))
         (attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (ctx-ptr (pointer-address
                     (eglCreateContext
                       (foreign->pointer disp) (foreign->pointer config)
                       share-ptr attrib-ptr))))
    (if (zero? ctx-ptr)
      (egl-error "egl-create-context")
      (make-egl-context ctx-ptr))))

(define-method
  (egl-create-context (disp <egl-display>) (config <egl-config>) attrib-list)
  (egl-create-context disp config #f attrib-list))

(define-method
  (egl-create-image (disp <egl-display>) ctx target buffer attrib-list)
  (let* ((ctx-ptr
           (check-optional-foreign ctx <egl-context> "egl-create-image"))
         (attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (image-ptr (pointer-address
                      (eglCreateImage
                        (foreign->pointer disp)
                        ctx-ptr target
                        (if (pointer? buffer) buffer (make-pointer buffer))
                        attrib-ptr))))
    (if (zero? image-ptr)
      (egl-error "egl-create-image")
      (make-egl-image image-ptr))))

(define-method
  (egl-create-image (disp <egl-display>) target buffer attrib-list)
  (egl-create-image disp #f target buffer attrib-list))

(define-method
  (egl-create-pbuffer-from-client-buffer (disp <egl-display>) buftype buffer
                                         (config <egl-config>) attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (surface-ptr (pointer-address
                        (eglCreatePbufferFromClientBuffer
                          (foreign->pointer disp) buftype
                          (if (pointer? buffer) buffer (make-pointer buffer))
                          (foreign->pointer config) attrib-ptr))))
    (if (zero? surface-ptr)
      (egl-error "egl-create-pbuffer-from-client-buffer")
      (make-egl-surface surface-ptr))))

(define-method
  (egl-create-pbuffer-surface (disp <egl-display>) (config <egl-config>)
                                   attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (surface-ptr (pointer-address
                        (eglCreatePbufferSurface
                          (foreign->pointer disp)
                          (foreign->pointer config) attrib-ptr))))
    (if (zero? surface-ptr)
      (egl-error "egl-create-pbuffer-surface")
      (make-egl-surface surface-ptr))))

(define-method
  (egl-create-pixmap-surface (disp <egl-display>) (config <egl-config>) pixmap
                                   attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (surface-ptr (pointer-address
                        (eglCreatePlatformPixmapSurface
                          (foreign->pointer disp)
                          (foreign->pointer config)
                          (if (pointer? pixmap) pixmap (make-pointer pixmap))
                          attrib-ptr))))
    (if (zero? surface-ptr)
      (egl-error "egl-create-pixmap-surface")
      (make-egl-surface surface-ptr))))

(define-method
  (egl-create-pixmap-surface (disp <egl-display>) (config <egl-config>) pixmap)
  (egl-create-pixmap-surface disp config pixmap '()))

(define-method (egl-create-sync (disp <egl-display>) type attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (sync-ptr (pointer-address
                     (eglCreateSync
                       (foreign->pointer disp) type attrib-ptr))))
    (if (zero? sync-ptr)
      (egl-error "egl-create-sync")
      (make-egl-sync sync-ptr))))

(define-method
  (egl-create-window-surface (disp <egl-display>) (config <egl-config>) window
                                   attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (surface-ptr (pointer-address
                        (eglCreatePlatformWindowSurface
                          (foreign->pointer disp)
                          (foreign->pointer config)
                          (if (pointer? window) window (make-pointer window))
                          attrib-ptr))))
    (if (zero? surface-ptr)
      (egl-error "egl-create-window-surface")
      (make-egl-surface surface-ptr))))

(define-method
  (egl-create-window-surface (disp <egl-display>) (config <egl-config>) window)
  (egl-create-window-surface disp config window '()))

(define-method
  (egl-destroy-context (disp <egl-display>) (context <egl-context>))
  (if
    (eglDestroyContext (foreign->pointer disp) (foreign->pointer context))
    (slot-set! context 'pointer 0)
    (egl-error "egl-destroy-context")))

(define-method
  (egl-destroy-image (disp <egl-display>) (image <egl-image>))
  (if
    (eglDestroyImage (foreign->pointer disp) (foreign->pointer image))
    (slot-set! image 'pointer 0)
    (egl-error "egl-destroy-image")))

(define-method
  (egl-destroy-surface (disp <egl-display>) (surface <egl-surface>))
  (if
    (eglDestroySurface (foreign->pointer disp) (foreign->pointer surface))
    (slot-set! surface 'pointer 0)
    (egl-error "egl-destroy-surface")))

(define-method
  (egl-destroy-sync (disp <egl-display>) (sync <egl-sync>))
  (if
    (eglDestroySync (foreign->pointer disp) (foreign->pointer sync))
    (slot-set! sync 'pointer 0)
    (egl-error "egl-destroy-sync")))

(define-method
  (egl-get-config-attrib (disp <egl-display>) (config <egl-config>) attribute)
  (let ((value-vec (make-s32vector 1)))
    (if (eglGetConfigAttrib
              (foreign->pointer disp) (foreign->pointer config)
              attribute value-vec)
        (s32vector-ref value-vec 0)
        (egl-error "egl-get-config-attrib"))))

(define-method (egl-get-configs (disp <egl-display>))
  (let ((disp-ptr (foreign->pointer disp))
        (num-config-vec (make-s32vector 1)))
    (unless (eglGetConfigs disp-ptr %null-pointer 0
                           (bytevector->pointer num-config-vec))
      (egl-error "egl-get-configs"))

    (let* ((num-configs (s32vector-ref num-config-vec 0))
           (configs (make-bytevector (* (sizeof '*) num-configs))))
      (unless (eglGetConfigs disp-ptr (bytevector->pointer configs)
                             num-configs (bytevector->pointer num-config-vec))
        (egl-error "egl-get-configs"))
      (map make-egl-config
           (bytevector->uint-list configs (native-endianness) (sizeof '*))))))

(define (egl-get-current-context)
  (let ((ctx-ptr (eglGetCurrentContext)))
    (if (zero? ctx-ptr) #f (make-egl-context ctx-ptr))))

(define (egl-get-current-display)
  (let ((disp-ptr (eglGetCurrentDisplay)))
    (if (zero? disp-ptr) #f (make-egl-display disp-ptr))))

(define (egl-get-current-surface readdraw)
  (let ((surface-ptr (eglGetCurrentSurface readdraw)))
    (if (zero? surface-ptr) #f (make-egl-surface surface-ptr))))

(define-method (egl-get-platform-display platform disp attrib-list)
  (let* ((attrib-ptr (bytevector->pointer
                       (any->s32vector (append attrib-list (list EGL_NONE)))))
         (disp-ptr (pointer-address
                     (eglGetPlatformDisplay
                       platform
                       (if (pointer? disp) disp (make-pointer disp))
                       attrib-ptr))))
    (if (zero? disp-ptr)
      (egl-error "egl-get-platform-display")
      (make-egl-display disp-ptr))))

(define-method (egl-get-platform-display platform disp)
  (egl-get-platform-display platform disp '()))

(define-method
  (egl-get-sync-attrib (disp <egl-display>) (sync <egl-sync>) attribute)
  (let ((value-vec (make-s32vector 1)))
    (if (eglGetSyncAttrib
              (foreign->pointer disp) (foreign->pointer sync)
              attribute value-vec)
        (s32vector-ref value-vec 0)
        (egl-error "egl-get-sync-attrib"))))

(define-method (egl-initialize (disp <egl-display>))
  (let ((versions (make-s32vector 2)))
    (unless (eglInitialize
          (foreign->pointer disp)
          (bytevector->pointer versions)
          (bytevector->pointer versions (sizeof int)))
      (egl-error "egl-initialize"))
    (s32vector->list versions)))

(define-method
  (egl-make-current disp draw read context)
  (let ((disp-ptr (check-optional-foreign
                      disp <egl-display> "egl-make-current"))
        (draw-ptr (check-optional-foreign
                      draw <egl-surface> "egl-make-current"))
        (read-ptr (check-optional-foreign
                      read <egl-surface> "egl-make-current"))
        (context-ptr (check-optional-foreign
                      context <egl-context> "egl-make-current")))
    (unless
      (eglMakeCurrent disp-ptr draw-ptr read-ptr context-ptr)
      (egl-error "egl-make-current"))))

(define egl-query-api eglQueryAPI)

(define-method
  (egl-query-context (disp <egl-display>) (context <egl-context>) attribute)
  (let ((value-vec (make-s32vector 1)))
    (if (eglQueryContext
              (foreign->pointer disp) (foreign->pointer context)
              attribute value-vec)
        (s32vector-ref value-vec 0)
        (egl-error "egl-query-context"))))

(define-method (egl-query-string (disp <egl-display>) name)
  (let ((str-ptr (eglQueryString (foreign->pointer disp) name)))
    (if (zero? str-ptr)
        (egl-error "egl-query-string")
        (pointer->string str-ptr))))

(define-method
  (egl-query-surface (disp <egl-display>) (surface <egl-surface>) attribute)
  (let ((value-vec (make-s32vector 1)))
    (if (eglQuerySurface
              (foreign->pointer disp) (foreign->pointer surface)
              attribute value-vec)
        (s32vector-ref value-vec 0)
        (egl-error "egl-query-surface"))))

(define-method
  (egl-release-tex-image (disp <egl-display>) (surface <egl-surface>) buffer)
  (unless (eglReleaseTexImage
            (foreign->pointer disp) (foreign->pointer surface) buffer)
    (egl-error "egl-release-tex-image")))

(define (egl-release-thread)
  (unless (eglReleaseThread)
    (egl-error "egl-release-thread")))

(define-method
  (egl-surface-attrib (disp <egl-display>) (surface <egl-surface>)
                      attribute value)
  (unless
    (eglSurfaceAttrib
      (foreign->pointer disp) (foreign->pointer surface) attribute value)
    (egl-error "egl-surface-attrib")))

(define-method (egl-swap-buffers (disp <egl-display>) (surface <egl-surface>))
  (unless (eglSwapBuffers (foreign->pointer disp) (foreign->pointer surface))
    (egl-error "egl-swap-buffers")))

(define-method (egl-swap-interval (disp <egl-display>) interval)
  (unless (eglSwapInterval (foreign->pointer disp) interval)
    (egl-error "egl-swap-interval")))

(define-method (egl-terminate (disp <egl-display>))
  (if (eglTerminate (foreign->pointer disp))
      (slot-set! disp 'pointer 0)
      (egl-error "egl-terminate")))

(define (egl-wait-client)
  (unless (eglWaitClient)
    (egl-error "egl-wait-client")))

(define (egl-wait-gl)
  (unless (eglWaitGL)
    (egl-error "egl-wait-gl")))

(define (egl-wait-native engine)
  (unless (eglWaitNative engine)
    (egl-error "egl-wait-native")))

(define-method
  (egl-wait-sync (disp <egl-display>) (sync <egl-sync>) flags)
  (unless (eglWaitSync
            (foreign->pointer disp) (foreign->pointer sync) flags)
        (egl-error "egl-wait-sync")))