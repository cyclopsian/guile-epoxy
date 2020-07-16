;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: Apache-2.0

(use-modules (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 hash-table)
             (ice-9 match)
             (ice-9 receive)
             (ice-9 regex)
             (oop goops)
             (srfi srfi-1)
             (srfi srfi-26)
             (sxml fold)
             (sxml match)
             (sxml simple)
             (texinfo string-utils))

(define-class <gl-command> ()
              (name    #:init-keyword #:name)
              (ptype   #:init-keyword #:ptype)
              (fptype  #:init-keyword #:fptype)
              (params  #:init-keyword #:params  #:init-value '()))
(define-class <gl-param> ()
              (name   #:init-keyword #:name)
              (ptype  #:init-keyword #:ptype)
              (fptype #:init-keyword #:fptype)
              (index  #:init-keyword #:index))

(define-syntax sxml-match-children-internal
  (syntax-rules ()
    ((sxml-match-children func children ((name args ...) body ...) ...)
     (func
       (λ (elem)
         (match
           (car elem)
           ((quote name) (sxml-match elem ((name args ...) body ...))) ...
           (else #f)))
      (filter (negate string?) children)))))

(define-syntax sxml-match-children
  (syntax-rules ()
    ((sxml-match-children args ...)
     (sxml-match-children-internal for-each args ...))))

(define-syntax sxml-match-children-map
  (syntax-rules ()
    ((sxml-match-children-map args ...)
     (sxml-match-children-internal map args ...))))

(define typedef-re (make-regexp "^typedef *"))

(define (make-typedef type)
  (sxml-match
    type
    ((type ,pre (name ,name) ,post)
     (let* ((match (regexp-exec typedef-re pre))
            (def (if match (regexp-substitute #f match 'pre "" 'post) pre)))
       (cons name (string-trim-both def))))
    (,_ #f)))

(define (make-value value)
  (string->number
    (cond
      ((string-prefix? "0x" value)
        (string-append "#" (substring value 1)))
      (else value))))

(define (make-enum enum)
  (sxml-match
    enum
    ((enum (@ (value ,value) (name ,name)))
     (let ((num (make-value value)))
       (if num (cons name num) #f)))
    (,_ #f)))

(define make-type
  (compose (cut string-join <> " ") (cut map string-trim-both <>) list))

(define (make-ptype-and-name elems)
  (sxml-match
    elems
    ((list ,pre (ptype ,ptype) ,post (name ,name) . ,_)
     (list name ptype (make-type pre ptype post)))
    ((list ,pre (ptype ,ptype) (name ,name) . ,_)
     (list name ptype (make-type pre ptype)))
    ((list (ptype ,ptype) ,post (name ,name) . ,_)
     (list name ptype (make-type ptype post)))
    ((list (ptype ,ptype) (name ,name) . ,_)
     (list name ptype ptype))
    ((list ,ptype (name ,name) . ,_)
     (list name (make-type ptype) (make-type ptype)))))

(define (make-param elems index)
  (match-let
    (((name ptype fptype) (make-ptype-and-name elems)))
    (make <gl-param> #:name name #:ptype ptype #:fptype fptype #:index index)))

(define (make-params elems)
  (reverse
    (fold-values
      (λ (elem acc)
         (let
           ((param (sxml-match
                     elem
                     ((param . ,rest)
                      (make-param rest (length acc)))
                     (,_ #f))))
           (if param (cons param acc) acc)))
      elems '())))


(define (make-command elem)
  (sxml-match
    elem
    ((command (proto . ,proto) . ,rest)
     (match-let
       (((name ptype fptype) (make-ptype-and-name proto)))
       (make <gl-command>
             #:name   name
             #:ptype  ptype
             #:fptype fptype
             #:params (make-params rest))))))

(define (format-args params)
  (if (null? params)
    '("void")
    (let ((len (length params)))
      (append
        (map
          (cut string-append "SCM " <>)
          (map (cut slot-ref <> 'name)
               (if (> len 10) (take params 9) params)))
        (if (> len 10) '("SCM rest") '())))))

(define* (unique lst #:optional (pred identity))
  (let ((ht (make-hash-table)))
    (fold-right (λ (e acc)
             (let ((key (pred e)))
               (if (hash-ref ht key)
                   acc
                   (begin
                     (hash-set! ht key e)
                     (cons e acc)))))
          '() lst)))

(define blocked-funcs
  '("glXAssociateDMPbufferSGIX"
    "glXCreateGLXVideoSourceSGIX"))

(define type-mappings
  (alist->hash-table
    '(("GLenum"        . "uint")
      ("GLboolean"     . "bool")
      ("GLbitfield"    . "uint")
      ("GLbyte"        . "char")
      ("GLubyte"       . "uchar")
      ("GLshort"       . "short")
      ("GLushort"      . "ushort")
      ("GLint"         . "int")
      ("GLuint"        . "uint")
      ("GLclampx"      . "int32")
      ("GLsizei"       . "int")
      ("GLfloat"       . "double")
      ("GLclampf"      . "double")
      ("GLdouble"      . "double")
      ("GLclampd"      . "double")
      ("GLchar"        . "char")
      ("GLcharARB"     . "char")
      ("GLhandleARB"   . "uint") ; TODO - ptr on apple?
      ("GLhalf"        . "uint16")
      ("GLhalfARB"     . "uint16")
      ("GLfixed"       . "int32")
      ("GLintptr"      . "ssize_t")
      ("GLintptrARB"   . "ssize_t")
      ("GLsizeiptr"    . "ssize_t")
      ("GLsizeiptrARB" . "ssize_t")
      ("GLint64"       . "int64")
      ("GLint64EXT"    . "int64")
      ("GLuint64"      . "uint64")
      ("GLuint64EXT"   . "uint64")
      ("GLvdpauSurfaceNV" . "ssize_t")
      ("GLhalfNV"      . "ushort")
      ("EGLNativeDisplayType" . "size_t")
      ("EGLNativePixmapType"  . "size_t")
      ("EGLNativeWindowType"  . "size_t")
      ("EGLint"        . "int32")
      ("EGLBoolean"    . "bool")
      ("EGLAttribKHR"  . "ssize_t")
      ("EGLAttrib"     . "ssize_t")
      ("EGLenum"       . "uint")
      ("EGLTimeKHR"    . "uint64")
      ("EGLTime"       . "uint64")
      ("EGLTimeNV"     . "uint64")
      ("EGLuint64NV"   . "uint64")
      ("EGLuint64KHR"  . "uint64")
      ("EGLnsecsANDROID" . "int64")
      ("EGLNativeFileDescriptorKHR" . "int")
      ("EGLsizeiANDROID" . "ssize_t")
      ("int"           . "int")
      ("unsigned int"  . "uint")
      ("int64_t"       . "int64")
      ("float"         . "double")
      ("Bool"          . "bool")
      ("Status"        . "int")
      ("Font"          . "ulong")
      ("Window"        . "ulong")
      ("Pixmap"        . "ulong")
      ("Colormap"      . "ulong")
      ("GLXFBConfigID" . "ulong")
      ("GLXContextID"  . "ulong")
      ("GLXPixmap"     . "ulong")
      ("GLXDrawable"   . "ulong")
      ("GLXWindow"     . "ulong")
      ("GLXPbuffer"    . "ulong")
      ("GLXVideoCaptureDeviceNV" . "ulong")
      ("GLXVideoDeviceNV" . "uint")
      ("GLXVideoSourceSGIX" . "ulong")
      ("GLXFBConfigIDSGIX" . "ulong")
      ("GLXPbufferSGIX" . "ulong")
      )))

(define enum-booleans
  '("GL_TRUE" "GL_FALSE" "GL_TRUE_EXT" "GL_FALSE_EXT" "EGL_TRUE" "EGL_FALSE"))

(define (format-call-arg param)
  (string-append "_" (slot-ref param 'name)))

(define (format-param-unwrap param param-count)
  (let ((name   (slot-ref param 'name))
        (index  (slot-ref param 'index))
        (fptype (slot-ref param 'fptype)))
    (format
      #f "  ~24a _~24a = (~@*~a) scm_to_~2@*~a(~a);"
      fptype name (hash-ref type-mappings fptype "pointer")
      (if (and (> param-count 10) (>= index 9))
        (format #f "scm_list_ref(rest, scm_from_ulong(~a))" (- index 9))
        name))))

(define (format-body cmd)
  (let* ((name   (slot-ref cmd 'name))
         (fptype (slot-ref cmd 'fptype))
         (params (slot-ref cmd 'params))
         (param-count (length params))
         (has-ret (not (equal? fptype "void"))))
    (string-join
      (list
        (if (> param-count 10)
          (format #f
"  if (scm_to_ulong(scm_length(rest)) != ~a)
    scm_wrong_num_args(scm_from_utf8_string(\"~a\"));\n"
             (- param-count 9) name)
          "")
        (string-join (map (cut format-param-unwrap <> param-count) params) "\n")
        (format #f "  ~a~a(~a);"
                (if has-ret (string-append fptype " __ret = ") "")
                name
                (string-join (map format-call-arg params) ", "))
        (if has-ret
            (let ((scm-type (hash-ref type-mappings fptype)))
              (if scm-type
                  (format #f "  return scm_from_~a(__ret);" scm-type)
                  (format #f "  return scm_from_pointer((void *)__ret, NULL);")))
            "  return SCM_UNSPECIFIED;"))
      "\n")))

(define (generate namespace includes)
  (receive
    (typedefs enums commands)
    (sxml-match
      (caddr (xml->sxml #:trim-whitespace? #t))
      ((registry  . ,children)
       (fold-values
         (λ (elem ts es cs)
            (sxml-match
              elem
              ((types    . ,types)
               (values (filter identity (map make-typedef types)) es cs))
              ((enums    . ,enums)
               (values ts
                       (append es (filter identity (map make-enum enums))) cs))
              ((commands . ,cmds)
               (values ts es (map make-command cmds)))
              (,_ (values ts es cs))))
         children '() '() '())))
    (set! commands (unique commands (cut slot-ref <> 'name)))
    (set! enums (unique enums car))
    (set! commands
      (filter (λ (c) (not (member (slot-ref c 'name) blocked-funcs))) commands))
    (display "#include <libguile.h>")
    (for-each
      (λ (inc) (format #t "\n#include <~a>\n" inc))
      includes)
    (newline)
    (for-each
      (λ (cmd)
         (format #t "
static SCM scm_~a(~a) {
~a
}
" (slot-ref cmd 'name)
  (string-join (format-args (slot-ref cmd 'params)) ", ")
  (format-body cmd)))
      commands)
    (format #t "\n\nvoid init_~a_enums(void *data) {\n  (void) data;"
            namespace)
    (for-each
      (match-lambda
        ((name . value)
         (format #t "
  static const char *s_~a = \"~@*~a\";
  scm_c_define(s_~@*~a, scm_from_~a(~@*~a));
  scm_c_export(s_~@*~a, NULL);"
                 name
                 (cond
                   ((member name enum-booleans) "bool")
                   ((> value #xffffffff) "uint64")
                   ((< value 0) "int")
                   (else "uint")))))
      enums)
    (newline)
    (format #t "}\n\nvoid init_~a_commands(void *data) {\n  (void) data;"
            namespace)
    (define (print-cmd-def name params)
      (let ((len (length params)))
        (apply format #t "
  static const char *s_~a = \"~@*~a\";
  scm_c_define_gsubr(s_~@*~a, ~a, 0, ~a, scm_~@*~a);
  scm_c_export(s_~@*~a, NULL);"
          name (if (> len 10) '(9 1) `(,len 0)))))

    (for-each
      (λ (cmd)
         (let ((name   (slot-ref cmd 'name))
               (params (slot-ref cmd 'params)))
           (print-cmd-def name params)))
      commands)
    (format #t "\n}\n\n")))

(define usage-text "\
[OPTIONS] NAME INPUT OUTPUT

Supported options:
-i, --include [file,...]   Include additional files
-h, --help                 Display this help
")

(define (main args)
  (let*
    ((option-spec '((help       (single-char #\h) (value #f))
                    (include    (single-char #\i) (value #t))))
     (options (getopt-long args option-spec))
     (rest    (cdar options))
     (includes    (filter (negate string-null?)
                          (string-split (option-ref options 'include "") #\,)))
     (help-wanted (option-ref options 'help #f)))

    (when help-wanted
      (format #t "~a ~a" (car args) usage-text)
      (quit 0))

    (match
      rest
      ((name input output)
       (let
         ((outport (match output
                          ("-" (current-output-port))
                          (f (open-output-file f))))
          (inport  (match input
                          ("-" (current-input-port))
                          (f (open-input-file f)))))
         (with-input-from-port
           inport
           (cut with-output-to-port outport
                (cut generate name includes)))))
      (_
        (format #t "~a ~a" (car args) usage-text)
        (quit 1)))))

(main (command-line))
(quit 0)
