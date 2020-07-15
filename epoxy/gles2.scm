;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: Apache-2.0

(eval-when (expand load eval)
  (load-extension "libguile-epoxy" "scm_init_epoxy"))

(define-module (epoxy gles2)
  #:use-module (epoxy gl commands)
  #:use-module (epoxy gl enums)
  #:use-module (epoxy gl util)
  #:use-module (epoxy util)
  #:use-module (ice-9 binary-ports)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (system foreign)
  #:export (<gl-buffer> <gl-framebuffer> <gl-program>
            <gl-renderbuffer> <gl-shader> <gl-texture>
            object-id

            gl-errors gl-get-error gl-error gl-active-texture gl-attach-shader
            gl-bind-attrib-location gl-blend-color gl-blend-equation
            gl-blend-equation-separate gl-blend-func gl-blend-func-separate
            gl-buffer-data gl-buffer-sub-data gl-check-framebuffer-status
            gl-clear gl-clear-color gl-clear-depth gl-clear-stencil
            gl-color-mask gl-compile-shader gl-compressed-tex-image-2d
            gl-compressed-tex-sub-image-2d gl-copy-tex-image-2d
            gl-copy-tex-sub-image-2d gl-cull-face gl-depth-func gl-depth-mask
            gl-depth-range gl-detach-shader gl-disable
            gl-disable-vertex-attrib-array gl-draw-arrayfiless gl-draw-elements
            gl-enable gl-enable-vertex-attrib-array gl-finish gl-flush
            gl-framebuffer-renderbuffer gl-framebuffer-texture-2d gl-front-face
            gl-generate-mipmap gl-get-booleans gl-get-boolean gl-get-floats
            gl-get-float gl-get-integers gl-get-integer gl-get-active-attrib
            gl-get-active-uniform gl-get-attached-shaders
            gl-get-attrib-location gl-get-buffer-parameter-iv
            gl-get-buffer-parameter-i
            gl-get-framebuffer-attachment-parameter-iv
            gl-get-framebuffer-attachment-parameter-i gl-get-program-info-log
            gl-get-program-iv gl-get-program-i gl-get-renderbuffer-parameter-iv
            gl-get-renderbuffer-parameter-i gl-get-shader-info-log
            gl-get-shader-precision-format gl-get-shader-source
            gl-get-shader-iv gl-get-shader-i gl-get-string
            gl-get-tex-parameter-fv gl-get-tex-parameter-f
            gl-get-tex-parameter-iv gl-get-tex-parameter-i gl-get-uniform-fv
            gl-get-uniform-f gl-get-uniform-iv gl-get-uniform-i
            gl-get-uniform-location gl-get-vertex-attrib-fv
            gl-get-vertex-attrib-f gl-get-vertex-attrib-iv
            gl-get-vertex-attrib-i gl-get-vertex-attrib-pointer gl-hint
            gl-buffer?  gl-enabled?  gl-framebuffer?  gl-program?
            gl-renderbuffer?  gl-shader?  gl-texture?  gl-line-width
            gl-link-program gl-polygon-offset gl-read-pixels
            gl-release-shader-compiler gl-renderbuffer-storage
            gl-sample-coverage gl-scissor gl-shader-binary gl-shader-source
            gl-stencil-func gl-stencil-func-separate gl-stencil-mask
            gl-stencil-mask-separate gl-stencil-op gl-stencil-op-separate
            gl-tex-image-2d gl-tex-parameter gl-tex-sub-image-2d gl-uniform
            gl-uniform-1iv gl-uniform-1fv gl-uniform-1v gl-uniform-2iv
            gl-uniform-2fv gl-uniform-2v gl-uniform-3iv gl-uniform-3fv
            gl-uniform-3v gl-uniform-4iv gl-uniform-4fv gl-uniform-4v
            gl-uniform-matrix-2v gl-uniform-matrix-3v gl-uniform-matrix-4v
            gl-use-program gl-validate-program gl-vertex-attrib
            gl-vertex-attrib-pointer gl-viewport
            )
  #:re-export (epoxy-has-gl-extension epoxy-is-desktop-gl
               epoxy-gl-version epoxy-glsl-version

               initialize

               GL_DEPTH_BUFFER_BIT GL_STENCIL_BUFFER_BIT GL_COLOR_BUFFER_BIT
               GL_FALSE GL_TRUE GL_POINTS GL_LINES GL_LINE_LOOP GL_LINE_STRIP
               GL_TRIANGLES GL_TRIANGLE_STRIP GL_TRIANGLE_FAN GL_ZERO GL_ONE
               GL_SRC_COLOR GL_ONE_MINUS_SRC_COLOR GL_SRC_ALPHA
               GL_ONE_MINUS_SRC_ALPHA GL_DST_ALPHA GL_ONE_MINUS_DST_ALPHA
               GL_DST_COLOR GL_ONE_MINUS_DST_COLOR GL_SRC_ALPHA_SATURATE
               GL_FUNC_ADD GL_BLEND_EQUATION GL_BLEND_EQUATION_RGB
               GL_BLEND_EQUATION_ALPHA GL_FUNC_SUBTRACT
               GL_FUNC_REVERSE_SUBTRACT GL_BLEND_DST_RGB GL_BLEND_SRC_RGB
               GL_BLEND_DST_ALPHA GL_BLEND_SRC_ALPHA GL_CONSTANT_COLOR
               GL_ONE_MINUS_CONSTANT_COLOR GL_CONSTANT_ALPHA
               GL_ONE_MINUS_CONSTANT_ALPHA GL_BLEND_COLOR GL_ARRAY_BUFFER
               GL_ELEMENT_ARRAY_BUFFER GL_ARRAY_BUFFER_BINDING
               GL_ELEMENT_ARRAY_BUFFER_BINDING GL_STREAM_DRAW GL_STATIC_DRAW
               GL_DYNAMIC_DRAW GL_BUFFER_SIZE GL_BUFFER_USAGE
               GL_CURRENT_VERTEX_ATTRIB GL_FRONT GL_BACK GL_FRONT_AND_BACK
               GL_TEXTURE_2D GL_CULL_FACE GL_BLEND GL_DITHER GL_STENCIL_TEST
               GL_DEPTH_TEST GL_SCISSOR_TEST GL_POLYGON_OFFSET_FILL
               GL_SAMPLE_ALPHA_TO_COVERAGE GL_SAMPLE_COVERAGE GL_NO_ERROR
               GL_INVALID_ENUM GL_INVALID_VALUE GL_INVALID_OPERATION
               GL_OUT_OF_MEMORY GL_CW GL_CCW GL_LINE_WIDTH
               GL_ALIASED_POINT_SIZE_RANGE GL_ALIASED_LINE_WIDTH_RANGE
               GL_CULL_FACE_MODE GL_FRONT_FACE GL_DEPTH_RANGE
               GL_DEPTH_WRITEMASK GL_DEPTH_CLEAR_VALUE GL_DEPTH_FUNC
               GL_STENCIL_CLEAR_VALUE GL_STENCIL_FUNC GL_STENCIL_FAIL
               GL_STENCIL_PASS_DEPTH_FAIL GL_STENCIL_PASS_DEPTH_PASS
               GL_STENCIL_REF GL_STENCIL_VALUE_MASK GL_STENCIL_WRITEMASK
               GL_STENCIL_BACK_FUNC GL_STENCIL_BACK_FAIL
               GL_STENCIL_BACK_PASS_DEPTH_FAIL GL_STENCIL_BACK_PASS_DEPTH_PASS
               GL_STENCIL_BACK_REF GL_STENCIL_BACK_VALUE_MASK
               GL_STENCIL_BACK_WRITEMASK GL_VIEWPORT GL_SCISSOR_BOX
               GL_COLOR_CLEAR_VALUE GL_COLOR_WRITEMASK GL_UNPACK_ALIGNMENT
               GL_PACK_ALIGNMENT GL_MAX_TEXTURE_SIZE GL_MAX_VIEWPORT_DIMS
               GL_SUBPIXEL_BITS GL_RED_BITS GL_GREEN_BITS GL_BLUE_BITS
               GL_ALPHA_BITS GL_DEPTH_BITS GL_STENCIL_BITS
               GL_POLYGON_OFFSET_UNITS GL_POLYGON_OFFSET_FACTOR
               GL_TEXTURE_BINDING_2D GL_SAMPLE_BUFFERS GL_SAMPLES
               GL_SAMPLE_COVERAGE_VALUE GL_SAMPLE_COVERAGE_INVERT
               GL_NUM_COMPRESSED_TEXTURE_FORMATS GL_COMPRESSED_TEXTURE_FORMATS
               GL_DONT_CARE GL_FASTEST GL_NICEST GL_GENERATE_MIPMAP_HINT
               GL_BYTE GL_UNSIGNED_BYTE GL_SHORT GL_UNSIGNED_SHORT GL_INT
               GL_UNSIGNED_INT GL_FLOAT GL_FIXED GL_DEPTH_COMPONENT GL_ALPHA
               GL_RGB GL_RGBA GL_LUMINANCE GL_LUMINANCE_ALPHA
               GL_UNSIGNED_SHORT_4_4_4_4 GL_UNSIGNED_SHORT_5_5_5_1
               GL_UNSIGNED_SHORT_5_6_5 GL_FRAGMENT_SHADER GL_VERTEX_SHADER
               GL_MAX_VERTEX_ATTRIBS GL_MAX_VERTEX_UNIFORM_VECTORS
               GL_MAX_VARYING_VECTORS GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
               GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS GL_MAX_TEXTURE_IMAGE_UNITS
               GL_MAX_FRAGMENT_UNIFORM_VECTORS GL_SHADER_TYPE GL_DELETE_STATUS
               GL_LINK_STATUS GL_VALIDATE_STATUS GL_ATTACHED_SHADERS
               GL_ACTIVE_UNIFORMS GL_ACTIVE_UNIFORM_MAX_LENGTH
               GL_ACTIVE_ATTRIBUTES GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
               GL_SHADING_LANGUAGE_VERSION GL_CURRENT_PROGRAM GL_NEVER GL_LESS
               GL_EQUAL GL_LEQUAL GL_GREATER GL_NOTEQUAL GL_GEQUAL GL_ALWAYS
               GL_KEEP GL_REPLACE GL_INCR GL_DECR GL_INVERT GL_INCR_WRAP
               GL_DECR_WRAP GL_VENDOR GL_RENDERER GL_VERSION GL_EXTENSIONS
               GL_NEAREST GL_LINEAR GL_NEAREST_MIPMAP_NEAREST
               GL_LINEAR_MIPMAP_NEAREST GL_NEAREST_MIPMAP_LINEAR
               GL_LINEAR_MIPMAP_LINEAR GL_TEXTURE_MAG_FILTER
               GL_TEXTURE_MIN_FILTER GL_TEXTURE_WRAP_S GL_TEXTURE_WRAP_T
               GL_TEXTURE GL_TEXTURE_CUBE_MAP GL_TEXTURE_BINDING_CUBE_MAP
               GL_TEXTURE_CUBE_MAP_POSITIVE_X GL_TEXTURE_CUBE_MAP_NEGATIVE_X
               GL_TEXTURE_CUBE_MAP_POSITIVE_Y GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
               GL_TEXTURE_CUBE_MAP_POSITIVE_Z GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
               GL_MAX_CUBE_MAP_TEXTURE_SIZE GL_TEXTURE0 GL_TEXTURE1 GL_TEXTURE2
               GL_TEXTURE3 GL_TEXTURE4 GL_TEXTURE5 GL_TEXTURE6 GL_TEXTURE7
               GL_TEXTURE8 GL_TEXTURE9 GL_TEXTURE10 GL_TEXTURE11 GL_TEXTURE12
               GL_TEXTURE13 GL_TEXTURE14 GL_TEXTURE15 GL_TEXTURE16 GL_TEXTURE17
               GL_TEXTURE18 GL_TEXTURE19 GL_TEXTURE20 GL_TEXTURE21 GL_TEXTURE22
               GL_TEXTURE23 GL_TEXTURE24 GL_TEXTURE25 GL_TEXTURE26 GL_TEXTURE27
               GL_TEXTURE28 GL_TEXTURE29 GL_TEXTURE30 GL_TEXTURE31
               GL_ACTIVE_TEXTURE GL_REPEAT GL_CLAMP_TO_EDGE GL_MIRRORED_REPEAT
               GL_FLOAT_VEC2 GL_FLOAT_VEC3 GL_FLOAT_VEC4 GL_INT_VEC2
               GL_INT_VEC3 GL_INT_VEC4 GL_BOOL GL_BOOL_VEC2 GL_BOOL_VEC3
               GL_BOOL_VEC4 GL_FLOAT_MAT2 GL_FLOAT_MAT3 GL_FLOAT_MAT4
               GL_SAMPLER_2D GL_SAMPLER_CUBE GL_VERTEX_ATTRIB_ARRAY_ENABLED
               GL_VERTEX_ATTRIB_ARRAY_SIZE GL_VERTEX_ATTRIB_ARRAY_STRIDE
               GL_VERTEX_ATTRIB_ARRAY_TYPE GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
               GL_VERTEX_ATTRIB_ARRAY_POINTER
               GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
               GL_IMPLEMENTATION_COLOR_READ_TYPE
               GL_IMPLEMENTATION_COLOR_READ_FORMAT GL_COMPILE_STATUS
               GL_INFO_LOG_LENGTH GL_SHADER_SOURCE_LENGTH GL_SHADER_COMPILER
               GL_SHADER_BINARY_FORMATS GL_NUM_SHADER_BINARY_FORMATS
               GL_LOW_FLOAT GL_MEDIUM_FLOAT GL_HIGH_FLOAT GL_LOW_INT
               GL_MEDIUM_INT GL_HIGH_INT GL_FRAMEBUFFER GL_RENDERBUFFER
               GL_RGBA4 GL_RGB5_A1 GL_RGB565 GL_DEPTH_COMPONENT16
               GL_STENCIL_INDEX8 GL_RENDERBUFFER_WIDTH GL_RENDERBUFFER_HEIGHT
               GL_RENDERBUFFER_INTERNAL_FORMAT GL_RENDERBUFFER_RED_SIZE
               GL_RENDERBUFFER_GREEN_SIZE GL_RENDERBUFFER_BLUE_SIZE
               GL_RENDERBUFFER_ALPHA_SIZE GL_RENDERBUFFER_DEPTH_SIZE
               GL_RENDERBUFFER_STENCIL_SIZE
               GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
               GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
               GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
               GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
               GL_COLOR_ATTACHMENT0 GL_DEPTH_ATTACHMENT GL_STENCIL_ATTACHMENT
               GL_NONE GL_FRAMEBUFFER_COMPLETE
               GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
               GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
               GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS GL_FRAMEBUFFER_UNSUPPORTED
               GL_FRAMEBUFFER_BINDING GL_RENDERBUFFER_BINDING
               GL_MAX_RENDERBUFFER_SIZE GL_INVALID_FRAMEBUFFER_OPERATION
               )
  #:replace   (bind delete)
  #:re-export-and-replace (epoxy-extension-in-string))

(define gl-errors
  ((λ ()
     (define-syntax gen-error-alist
       (syntax-rules ()
         ((gen-error-alist)
          '())
         ((gen-error-alist sym rest ...)
          (cons (cons sym 'sym) (gen-error-alist rest ...)))))

     (gen-error-alist GL_NO_ERROR                      GL_INVALID_ENUM
                      GL_INVALID_VALUE                 GL_INVALID_OPERATION
                      GL_INVALID_FRAMEBUFFER_OPERATION GL_OUT_OF_MEMORY
                      GL_STACK_UNDERFLOW               GL_STACK_OVERFLOW))))

(define (gl-get-error)
  (assoc-ref gl-errors (glGetError)))

(define* (gl-error #:optional subr)
  (scm-error 'gl-error subr "~a" (list (gl-get-error)) '()))

(define-generic bind)
(define-generic delete)

(define-syntax define-gl-object
  (syntax-rules ()
    ((define-gl-object name gen bin del)
     (begin
       (define-class name () (id #:getter object-id))
       (define-method (initialize (obj name) args)
         (apply (case-lambda
                  (() (let ((id-vec (make-u32vector 1 0)))
                        (gen 1 (bytevector->pointer id-vec))
                        (slot-set! obj 'id (u32vector-ref id-vec 0))))
                  ((id) (slot-set! obj 'id id)))
                args))
       (define-method (bind target (obj name))
         (bin target (object-id obj)))
       (define-method (delete (obj name))
         (let ((id-vec (make-u32vector 1 (object-id obj))))
           (del 1 (bytevector->pointer id-vec))))))))

(define-gl-object <gl-buffer> glGenBuffers glBindBuffer glDeleteBuffers)
(define-gl-object <gl-framebuffer> glGenFramebuffers glBindFramebuffer glDeleteFramebuffers)
(define-gl-object <gl-renderbuffer> glGenRenderbuffers glBindRenderbuffer glDeleteRenderbuffers)
(define-gl-object <gl-texture> glGenTextures glBindTexture glDeleteTextures)

(define-class <gl-program> () id)
(define-method (initialize (program <gl-program>))
  (slot-set! program 'id (glCreateProgram)))
(define-method (delete (program <gl-program>))
  (glDeleteProgram (object-id program)))

(define-class <gl-shader> () id)
(define-method (initialize (shader <gl-shader>) args)
  (apply (λ (type) (slot-set! shader 'id (glCreateShader type))) args))
(define-method (delete (shader <gl-shader>))
  (glDeleteShader (object-id shader)))

(define gl-active-texture  glActiveTexture)

(define-method (gl-attach-shader (program <gl-program>) (shader <gl-shader>))
  (glAttachShader (object-id program) (object-id shader)))

(define-method (gl-bind-attrib-location (program <gl-program>) index name)
  (glBindAttribLocation (object-id program) index (string->pointer name)))

(define gl-blend-color glBlendColor)

(define gl-blend-equation glBlendEquation)

(define gl-blend-equation-separate glBlendEquationSeparate)

(define gl-blend-func glBlendFunc)

(define gl-blend-func-separate glBlendFuncSeparate)

(define (gl-buffer-data target size data usage)
  (glBufferData
    target size (if (bytevector? data) (bytevector->pointer data) data) usage))

(define (gl-buffer-sub-data target offset size data)
  (glBufferSubData
    target offset size (if (bytevector? data) (bytevector->pointer data) data)))

(define gl-check-framebuffer-status glCheckFramebufferStatus)

(define gl-clear glClear)

(define gl-clear-color glClearColor)

(define gl-clear-depth glClearDepth)

(define gl-clear-stencil glClearStencil)

(define gl-color-mask glColorMask)

(define-method (gl-compile-shader (shader <gl-shader>))
  (glCompileShader (object-id shader)))

(define (gl-compressed-tex-image-2d target level internalformat width height border size data)
  (glCompressedTexImage2D target level internalformat width height border size
                          (if (bytevector? data) (bytevector->pointer data) data)))

(define (gl-compressed-tex-sub-image-2d target level xoffset yoffset width height format size data)
  (glCompressedTexSubImage2D target level xoffset yoffset width height format size
                             (if (bytevector? data) (bytevector->pointer data) data)))

(define gl-copy-tex-image-2d glCopyTexImage2D)

(define gl-copy-tex-sub-image-2d glCopyTexSubImage2D)

(define gl-cull-face glCullFace)

(define gl-depth-func glDepthFunc)

(define gl-depth-mask glDepthMask)

(define gl-depth-range glDepthRangef)

(define-method (gl-detach-shader (program <gl-program>) (shader <gl-shader>))
  (glDetachShader (object-id program) (object-id shader)))

(define gl-disable glDisable)

(define gl-disable-vertex-attrib-array glDisableVertexAttribArray)

(define gl-draw-arrays glDrawArrays)

(define-method (gl-draw-elements mode count type indices)
  (glDrawElements mode count type
                  (if (bytevector? indices) (bytevector->pointer indices) indices)))

(define-method (gl-draw-elements mode indices)
  (if (u8vector? indices)
      (glDrawElements mode (u8vector-length indices) GL_UNSIGNED_BYTE indices)
      (let ((vec (if (u16vector? indices) indices (any->u16vector indices))))
        (glDrawElements mode (u16vector-length vec) GL_UNSIGNED_SHORT vec))))

(define gl-enable glEnable)

(define gl-enable-vertex-attrib-array glEnableVertexAttribArray)

(define gl-finish glFinish)

(define gl-flush glFlush)

(define-method (gl-framebuffer-renderbuffer target attachment rdtarget (rd <gl-renderbuffer>))
  (glFramebufferRenderbuffer target attachment rdtarget (object-id rd)))

(define-method (gl-framebuffer-texture-2d target attachment textarget (texture <gl-texture>) level)
  (glFramebufferTexture2D target attachment textarget (object-id texture) level))

(define gl-front-face glFrontFace)

(define gl-generate-mipmap glGenerateMipmap)

(define (gl-get-booleans pname count)
  (let ((vec (make-u8vector count 0)))
    (glGetBooleanv pname (bytevector->pointer vec))
    vec))

(define (gl-get-boolean pname)
  (u8vector-ref (gl-get-booleans pname 1) 0))

(define (gl-get-floats pname count)
  (let ((vec (make-f32vector count 0)))
    (glGetFloatv pname (bytevector->pointer vec))
    vec))

(define (gl-get-float pname)
  (f32vector-ref (gl-get-floats pname 1) 0))

(define (gl-get-integers pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetIntegerv pname (bytevector->pointer vec))
    vec))

(define (gl-get-integer pname)
  (s32vector-ref (gl-get-integers pname 1) 0))

(define-method (gl-get-active-attrib (program <gl-program>) index)
  (let* ((name-len (gl-get-program-i program GL_ACTIVE_UNIFORM_MAX_LENGTH))
         (name-vec (make-u8vector name-len))
         (size-vec (make-s32vector 1 0))
         (type-vec (make-u32vector 1 0))
         (len-vec (make-s32vector 1 0)))
    (glGetActiveAttrib (object-id program) index name-len
                       (bytevector->pointer len-vec)
                       (bytevector->pointer size-vec)
                       (bytevector->pointer type-vec)
                       (bytevector->pointer name-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (list (s32vector-ref size-vec 0) (u32vector-ref type-vec 0)
              (utf8->string name-vec)))))

(define-method (gl-get-active-uniform (program <gl-program>) index)
  (let* ((name-len (gl-get-program-i program GL_ACTIVE_UNIFORM_MAX_LENGTH))
         (name-vec (make-u8vector name-len))
         (size-vec (make-s32vector 1 0))
         (type-vec (make-u32vector 1 0))
         (len-vec (make-s32vector 1 0)))
    (glGetActiveUniform (object-id program) index name-len
                        (bytevector->pointer len-vec)
                        (bytevector->pointer size-vec)
                        (bytevector->pointer type-vec)
                        (bytevector->pointer name-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (list (s32vector-ref size-vec 0) (u32vector-ref type-vec 0)
              (utf8->string name-vec)))))

(define-method (gl-get-attached-shaders (program <gl-program>))
  (let* ((shaders-len (gl-get-program-i program GL_ATTACHED_SHADERS))
         (shaders-vec (make-u8vector shaders-len))
         (len-vec (make-s32vector 1 0)))
    (glGetAttachedShaders (object-id program) shaders-len
                          (bytevector->pointer len-vec)
                          (bytevector->pointer shaders-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (map (λ (id) (make <gl-shader> id)) (s32vector->list shaders-vec)))))

(define-method (gl-get-attrib-location (program <gl-program>) name)
  (glGetAttribLocation (object-id program) (string->pointer name)))

(define (gl-get-buffer-parameter-iv target value count)
  (let ((vec (make-s32vector count 0)))
    (glGetBufferParameteriv target value (bytevector->pointer vec))
    vec))

(define (gl-get-buffer-parameter-i target value)
  (s32vector-ref (gl-get-buffer-parameter-iv target value 1) 0))

(define-method (gl-get-framebuffer-attachment-parameter-iv target attachment pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetFramebufferAttachmentParameteriv target attachment pname (bytevector->pointer vec))
    vec))

(define-method (gl-get-framebuffer-attachment-parameter-i target attachment pname)
  (s32vector-ref (gl-get-framebuffer-attachment-parameter-iv target attachment pname 1) 0))

(define-method (gl-get-program-info-log (program <gl-program>))
  (let* ((log-len (gl-get-program-i program GL_INFO_LOG_LENGTH))
         (log-vec (make-u8vector log-len))
         (len-vec (make-s32vector 1 0)))
    (glGetProgramInfoLog (object-id program) log-len
                         (bytevector->pointer len-vec)
                         (bytevector->pointer log-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (utf8->string log-vec))))

(define-method (gl-get-program-iv (program <gl-program>) pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetProgramiv (object-id program) pname (bytevector->pointer vec))
    vec))

(define-method (gl-get-program-i (program <gl-program>) pname)
  (s32vector-ref (gl-get-program-iv program pname 1) 0))

(define-method (gl-get-renderbuffer-parameter-iv (renderbuffer <gl-renderbuffer>) pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetRenderbufferParameteriv (object-id renderbuffer) pname (bytevector->pointer vec))
    vec))

(define-method (gl-get-renderbuffer-parameter-i (renderbuffer <gl-renderbuffer>) pname)
  (s32vector-ref (gl-get-renderbuffer-parameter-iv renderbuffer pname 1) 0))

(define-method (gl-get-shader-info-log (shader <gl-shader>))
  (let* ((log-len (gl-get-shader-i shader GL_INFO_LOG_LENGTH))
         (log-vec (make-u8vector log-len))
         (len-vec (make-s32vector 1 0)))
    (glGetShaderInfoLog (object-id shader) log-len
                        (bytevector->pointer len-vec)
                        (bytevector->pointer log-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (utf8->string log-vec))))

(define (gl-get-shader-precision-format shader-type precision-type)
  (let ((vals (make-s32vector 3)))
    (glGetShaderPrecisionFormat
      shader-type precision-type
      (bytevector->pointer vals)
      (bytevector->pointer vals (* 2 (sizeof int))))
    (s32vector->list vals)))

(define-method (gl-get-shader-source (shader <gl-shader>))
  (let* ((src-len (gl-get-shader-i shader GL_SHADER_SOURCE_LENGTH))
         (src-vec (make-u8vector src-len))
         (len-vec (make-s32vector 1 0)))
    (glGetShaderSource (object-id shader) src-len
                       (bytevector->pointer len-vec)
                       (bytevector->pointer src-vec))
    (if (<= (s32vector-ref len-vec 0) 0)
        #f
        (utf8->string src-vec))))

(define-method (gl-get-shader-iv (shader <gl-shader>) pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetShaderiv (object-id shader) pname (bytevector->pointer vec))
    vec))

(define-method (gl-get-shader-i (shader <gl-shader>) pname)
  (s32vector-ref (gl-get-shader-iv shader pname 1) 0))

(define (gl-get-string name)
  (let ((ptr (glGetString name)))
    (if (null-pointer? ptr) #f (pointer->string ptr))))

(define (gl-get-tex-parameter-fv target pname count)
  (let ((vec (make-f32vector count 0)))
    (glGetTexParameterfv target pname (bytevector->pointer vec))
    vec))

(define (gl-get-tex-parameter-f target pname)
  (f32vector-ref (gl-get-tex-parameter-fv target pname 1) 0))

(define (gl-get-tex-parameter-iv target pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetTexParameteriv target pname (bytevector->pointer vec))
    vec))

(define (gl-get-tex-parameter-i target pname)
  (s32vector-ref (gl-get-tex-parameter-iv target pname 1) 0))

(define-method (gl-get-uniform-fv (program <gl-program>) location count)
  (let ((vec (make-f32vector count 0)))
    (glGetUniformfv (object-id program) location (bytevector->pointer vec))
    vec))

(define-method (gl-get-uniform-f (program <gl-program>) location)
  (f32vector-ref (gl-get-uniform-fv program location 1) 0))

(define-method (gl-get-uniform-iv (program <gl-program>) location count)
  (let ((vec (make-s32vector count 0)))
    (glGetUniformiv (object-id program) location (bytevector->pointer vec))
    vec))

(define-method (gl-get-uniform-i (program <gl-program>) location)
  (s32vector-ref (gl-get-uniform-iv program location 1) 0))

(define-method (gl-get-uniform-location (program <gl-program>) name)
  (glGetUniformLocation (object-id program) (string->pointer name)))

(define (gl-get-vertex-attrib-fv index pname count)
  (let ((vec (make-f32vector count 0)))
    (glGetVertexAttribfv index pname (bytevector->pointer vec))
    vec))

(define (gl-get-vertex-attrib-f index pname)
  (f32vector-ref (gl-get-vertex-attrib-fv index pname 1) 0))

(define (gl-get-vertex-attrib-iv index pname count)
  (let ((vec (make-s32vector count 0)))
    (glGetVertexAttribiv index pname (bytevector->pointer vec))
    vec))

(define (gl-get-vertex-attrib-i index pname)
  (s32vector-ref (gl-get-vertex-attrib-iv index pname 1) 0))

(define (gl-get-vertex-attrib-pointer index pname)
  (let ((ptr-vec (make-bytevector (sizeof '*))))
    (glGetVertexAttribPointerv index pname (bytevector->pointer ptr-vec))
    (bytevector-uint-ref ptr-vec 0 (native-endianness) (sizeof '*))))

(define gl-hint glHint)

(define (gl-buffer? buffer)
  (and (is-a? buffer <gl-buffer>) (glIsBuffer (object-id buffer))))

(define gl-enabled? glIsEnabled)

(define (gl-framebuffer? framebuffer)
  (and (is-a? framebuffer <gl-framebuffer>)
       (glIsFramebuffer (object-id framebuffer))))

(define (gl-program? program)
  (and (is-a? program <gl-program>) (glIsProgram (object-id program))))

(define (gl-renderbuffer? renderbuffer)
  (and (is-a? renderbuffer <gl-renderbuffer>)
       (glIsRenderbuffer (object-id renderbuffer))))

(define (gl-shader? shader)
  (and (is-a? shader <gl-shader>) (glIsShader (object-id shader))))

(define (gl-texture? texture)
  (and (is-a? texture <gl-texture>) (glIsTexture (object-id texture))))

(define gl-line-width glLineWidth)

(define-method (gl-link-program (program <gl-program>))
  (glLinkProgram (object-id program)))

(define gl-polygon-offset glPolygonOffset)

(define (gl-read-pixels x y width height format type data)
  (glReadPixels
    x y width height format type
    (if (bytevector? data) (bytevector->pointer data) data)))

(define gl-release-shader-compiler glReleaseShaderCompiler)

(define gl-renderbuffer-storage glRenderbufferStorage)

(define gl-sample-coverage glSampleCoverage)

(define gl-scissor glScissor)

(define-method (gl-shader-binary (shader <gl-shader>) binaryformat binary length)
  (let ((shader-vec (make-u32vector 1 (object-id shader))))
    (glShaderBinary
      1 (bytevector->pointer shader-vec) binaryformat
      (if (bytevector? binary) (bytevector->pointer binary) binary)
      length)))

(define-method (gl-shader-binary (shader <gl-shader>) binaryformat (bv <bytevector>))
  (gl-shader-binary
    shader binaryformat (bytevector->pointer bv) (bytevector-length bv)))

(define-method (gl-shader-binary (shader <gl-shader>) binaryformat (port <input-port>))
  (gl-shader-binary shader binaryformat (get-bytevector-all port)))

(define-method (gl-shader-source (shader <gl-shader>) source)
  (let ((string-vec (make-bytevector (sizeof '*)))
        (length-vec (make-s32vector 1 (string-length source))))
    (bytevector-uint-set!
      string-vec 0 (string->pointer source) (native-endianness) (sizeof '*))
    (glShaderSource (object-id shader) 1
                    (bytevector->pointer string-vec)
                    (bytevector->pointer length-vec))))

(define gl-stencil-func glStencilFunc)

(define gl-stencil-func-separate glStencilFuncSeparate)

(define gl-stencil-mask glStencilMask)

(define gl-stencil-mask-separate glStencilMaskSeparate)

(define gl-stencil-op glStencilOp)

(define gl-stencil-op-separate glStencilOpSeparate)

(define (gl-tex-image-2d target level internalformat width height border format type data)
  (glTexImage2D target level internalformat width height border format type
                (if (bytevector? data) (bytevector->pointer data) data)))

(define-method (gl-tex-parameter target pname (param <real>))
  (glTexParameterf target pname param))

(define-method (gl-tex-parameter target pname (param <integer>))
  (glTexParameteri target pname param))

(define (gl-tex-sub-image-2d target level xoffset yoffset width height format type data)
  (glTexSubImage2D target level xoffset yoffset width height format type
                   (if (bytevector? data) (bytevector->pointer data) data)))

(define-method (gl-uniform location (v0 <real>))
  (glUniform1f location v0))

(define-method (gl-uniform location (v0 <real>) (v1 <real>))
  (glUniform2f location v0 v1))

(define-method (gl-uniform location (v0 <real>) (v1 <real>) (v2 <real>))
  (glUniform3f location v0 v1 v2))

(define-method (gl-uniform location (v0 <real>) (v1 <real>) (v2 <real>) (v3 <real>))
  (glUniform4f location v0 v1 v2 v3))

(define-method (gl-uniform location (v0 <integer>))
  (glUniform1i location v0))

(define-method (gl-uniform location (v0 <integer>) (v1 <integer>))
  (glUniform2i location v0 v1))

(define-method (gl-uniform location (v0 <integer>) (v1 <integer>) (v2 <integer>))
  (glUniform3i location v0 v1 v2))

(define-method (gl-uniform location (v0 <integer>) (v1 <integer>) (v2 <integer>) (v3 <integer>))
  (glUniform4i location v0 v1 v2 v3))

(define gl-uniform-1iv glUniform1iv)

(define gl-uniform-1fv glUniform1fv)

(define-method (gl-uniform-1v location count (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform1iv location
                   (min count (s32vector-length value))
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform1fv location
                   (min count (f32vector-length value))
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-1v"
                 "Wrong type: ~a" (list value) (list value)))))

(define-method (gl-uniform-1v location (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform1iv location
                   (s32vector-length value)
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform1fv location
                   (f32vector-length value)
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-1v"
                 "Wrong type: ~a" (list value) (list value)))))

(define gl-uniform-2iv glUniform2iv)

(define gl-uniform-2fv glUniform2fv)

(define-method (gl-uniform-2v location count (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform2iv location
                   (min count (quotient (s32vector-length value) 2))
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform2fv location
                   (min count (quotient (f32vector-length value) 2))
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-2v"
                 "Wrong type: ~a" (list value) (list value)))))

(define-method (gl-uniform-2v location (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform2iv location
                   (quotient (s32vector-length value) 2)
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform2fv location
                   (quotient (f32vector-length value) 2)
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-2v"
                 "Wrong type: ~a" (list value) (list value)))))

(define gl-uniform-3iv glUniform3iv)

(define gl-uniform-3fv glUniform3fv)

(define-method (gl-uniform-3v location count (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform3iv location
                   (min count (quotient (s32vector-length value) 3))
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform3fv location
                   (min count (quotient (f32vector-length value) 3))
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-3v"
                 "Wrong type: ~a" (list value) (list value)))))

(define-method (gl-uniform-3v location (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform3iv location
                   (quotient (s32vector-length value) 3)
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform3fv location
                   (quotient (f32vector-length value) 3)
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-3v"
                 "Wrong type: ~a" (list value) (list value)))))

(define gl-uniform-4iv glUniform4iv)

(define gl-uniform-4fv glUniform4fv)

(define-method (gl-uniform-4v location count (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform4iv location
                   (min count (quotient (s32vector-length value) 4))
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform4fv location
                   (min count (quotient (f32vector-length value) 4))
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-4v"
                 "Wrong type: ~a" (list value) (list value)))))

(define-method (gl-uniform-4v location (value <uvec>))
  (cond
    ((s32vector? value)
     (glUniform4iv location
                   (quotient (s32vector-length value) 4)
                   (bytevector->pointer value)))
    ((f32vector? value)
     (glUniform4fv location
                   (quotient (f32vector-length value) 4)
                   (bytevector->pointer value)))
    (else
      (scm-error 'wrong-type-arg "gl-uniform-4v"
                 "Wrong type: ~a" (list value) (list value)))))

(define-method (gl-uniform-matrix-2v location count transpose (value <foreign>))
  (glUniformMatrix2fv location count transpose value))

(define-method (gl-uniform-matrix-2v location count transpose (value <uvec>))
  (glUniformMatrix2fv location
                      (min count (quotient (f32vector-length value) 4))
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-2v location (count <integer>) (value <uvec>))
  (gl-uniform-matrix-2v location count #f value))

(define-method (gl-uniform-matrix-2v location (transpose <boolean>) (value <uvec>))
  (glUniformMatrix2fv location
                      (quotient (f32vector-length value) 4)
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-2v location (value <uvec>))
  (gl-uniform-matrix-2v location #f value))

(define-method (gl-uniform-matrix-3v location count transpose (value <foreign>))
  (glUniformMatrix3fv location count transpose value))

(define-method (gl-uniform-matrix-3v location count transpose (value <uvec>))
  (glUniformMatrix3fv location
                      (min count (quotient (f32vector-length value) 9))
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-3v location (count <integer>) (value <uvec>))
  (gl-uniform-matrix-3v location count #f value))

(define-method (gl-uniform-matrix-3v location (transpose <boolean>) (value <uvec>))
  (glUniformMatrix3fv location
                      (quotient (f32vector-length value) 9)
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-3v location (value <uvec>))
  (gl-uniform-matrix-3v location #f value))

(define-method (gl-uniform-matrix-4v location count transpose (value <foreign>))
  (glUniformMatrix4fv location count transpose value))

(define-method (gl-uniform-matrix-4v location count transpose (value <uvec>))
  (glUniformMatrix4fv location
                      (min count (quotient (f32vector-length value) 16))
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-4v location (count <integer>) (value <uvec>))
  (gl-uniform-matrix-4v location count #f value))

(define-method (gl-uniform-matrix-4v location (transpose <boolean>) (value <uvec>))
  (glUniformMatrix4fv location
                      (quotient (f32vector-length value) 16)
                      transpose
                      (bytevector->pointer value)))

(define-method (gl-uniform-matrix-4v location (value <uvec>))
  (gl-uniform-matrix-4v location #f value))

(define-method (gl-use-program (program <gl-program>))
  (glUseProgram (object-id program)))

(define-method (gl-validate-program (program <gl-program>))
  (glValidateProgram (object-id program)))

(define-method (gl-vertex-attrib index v0)
  (glVertexAttrib1f index v0))

(define-method (gl-vertex-attrib index v0 v1)
  (glVertexAttrib2f index v0 v1))

(define-method (gl-vertex-attrib index v0 v1 v2)
  (glVertexAttrib3f index v0 v1 v2))

(define-method (gl-vertex-attrib index v0 v1 v2 v3)
  (glVertexAttrib4f index v0 v1 v2 v3))

(define (gl-vertex-attrib-pointer index size type normalized stride offset)
  (glVertexAttribPointer index size type normalized stride (make-pointer offset)))

(define gl-viewport glViewport)
