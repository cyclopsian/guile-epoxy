# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: Apache-2.0

project(
  'guile-epoxy',
  'c',
  version: '1.0',
  license: 'Apache-2.0',
  meson_version: '>=0.54',
  default_options: [
    'c_std=gnu11',
    'warning_level=2',
    'werror=true',
  ],
)

host_system = host_machine.system()

epoxy          = dependency('epoxy')
guile          = dependency('guile-3.0')
guile_exe_path = guile.get_variable(pkgconfig: 'guile')
guile_exe      = find_program(guile_exe_path, native: true)

guile_ver = guile.name().split('-').get(1)

root = '/' / get_option('prefix')
guile_moddir = root / get_option('datadir') / 'guile/site' / guile_ver
extdir = get_option('libdir') / 'guile' / guile_ver / 'extensions'
guile_godir = root / get_option('libdir') / 'guile' / guile_ver / 'site-ccache'

guild_exe = find_program(guile.get_variable(pkgconfig: 'guild'), native: true)
guild_warnings = [
  '-Warity-mismatch',
  '-Wbad-case-datum',
  '-Wduplicate-case-datum',
  '-Wformat',
  '-Wmacro-use-before-definition',
  '-Wunbound-variable',
]

guile_epoxy_srcs = [
  'guile-epoxy.c'
]

specs = ['gl']
guile_epoxy_args = []

egl_option = get_option('egl')
if egl_option.enabled() or (egl_option.auto() \
  and not ['windows', 'darwin'].contains(host_system))
  specs += 'egl'
  guile_epoxy_args += '-DHAVE_EGL'
endif

foreach spec : specs
  guile_epoxy_srcs += custom_target(
    spec + '_c',
    input: 'registry' / '@0@.xml'.format(spec),
    output: '@BASENAME@.c',
    command: [guile_exe, '--no-auto-compile',
              files('generate.scm'),
              '-i', 'epoxy/@0@.h'.format(spec),
              spec, '@INPUT@', '@OUTPUT@']
  )
endforeach

guile_epoxy = library(
  'guile-epoxy',
  guile_epoxy_srcs,
  dependencies: [guile, epoxy],
  c_args: guile_epoxy_args,
  install: true,
  install_dir: extdir
)

scmdir = meson.current_source_dir()
scmbuilddir = meson.current_build_dir()

subdir('epoxy')
