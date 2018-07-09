
project('libwidgetextra',
  'cpp',
  version: '1.0.0',
  meson_version: '>=0.47.0',
)

conf = configuration_data()

# Get wxWidgets
wx_args = []
if get_option('buildtype').contains('deb')
  wx_args += '--debug=yes'
  add_project_arguments('-DwxDEBUG_LEVEL=1', language: 'cpp')
else
  add_project_arguments('-DwxDEBUG_LEVEL=0', language: 'cpp')
endif
if get_option('wx-version') != ''
  wx_args += '--version=' + get_option('wx-version')
endif
if get_option('force-gtk3')
  wx_args += '--toolkit=gtk3'
endif

wxwidgets = dependency('wxwidgets', modules: wx_args)




libwidgetextra = library('widgetextra',
  'NonGuiThread.cpp',
  dependencies: [
    wxwidgets,
  ],
)

import('pkgconfig').generate(libwidgetextra,
  name: 'libwidgetextra',
  description: 'Extras for wxwidgets',
)

widgetextra_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with: libwidgetextra,
  version: meson.project_version(),
  dependencies: [],
)
