# guile-epoxy

[![License: Apache 2.0][license-img]][license-spdx]

A set of [GNU Guile][guile] bindings for the [libepoxy][epoxy] OpenGL library.
These bindings are provided as an alternative to [guile-opengl][guile-opengl].
Currently, idiomatic Guile bindings are only written for EGL 1.5 and OpenGL ES
2.0 (contributions welcome).

# Building

Requirements:

- autotools
- guile >=3.0
- libepoxy

Build steps:

```sh
autoreconf -fiv
./configure
make
make install
```

## Usage

See the program in the `tests` folder for sample code. The Mesa GL
implementation is required to run the tests.

[guile]: https://www.gnu.org/software/guile/
[epoxy]: https://github.com/anholt/libepoxy
[guile-opengl]: https://www.gnu.org/software/guile-opengl/

[license-img]:  https://img.shields.io/badge/License-Apache%202.0-blue.svg?logo=apache
[license-spdx]: https://spdx.org/licenses/Apache-2.0.html
