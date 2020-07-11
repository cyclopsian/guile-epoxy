# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: Apache-2.0

fs = import('fs')

guile_epoxy_scm_srcs = [
  'egl.scm',
  'gles2.scm'
]

install_data(guile_epoxy_scm_srcs, install_dir: guile_moddir / 'epoxy')

scm_compiles = []
foreach src : guile_epoxy_scm_srcs
  scm_compiles += [[fs.stem(src), src]]
endforeach

guile_epoxy_config = configure_file(
  input: 'config.scm.in',
  output: 'config.scm',
  configuration: configuration_data({
    'extensionlib': guile_epoxy.full_path(),
  }),
  install: true,
  install_dir: guile_moddir / 'epoxy'
)
scm_compiles += [['config', guile_epoxy_config]]

foreach src : scm_compiles
  custom_target(
    'epoxy_@0@_go'.format(src[0]),
    input: src[1],
    output: '@BASENAME@.go',
    depends: [guile_epoxy],
    depend_files: [guile_epoxy_config],
    command: [guild_exe, 'compile'] + guild_warnings \
      + ['-L', scmdir, '-L', scmbuilddir, '@INPUT@', '-o', '@OUTPUT@'],
    install: true,
    install_dir: guile_godir / 'epoxy'
  )
endforeach
