
project('libnyquist',
  [ 'c', 'cpp' ],
  version: '0.1',
  meson_version: '>=0.47.0',
)

cc = meson.get_compiler('c')
conf = configuration_data()

check_headers = [
  'malloc.h',
  'stat.h',
  'stdlib.h',
  'sys/stat.h',
  'sys/types.h',
]

foreach header : check_headers
  macro_name = 'HAS_' + header.to_upper().underscorify()
  conf.set(macro_name, cc.has_header(header))
endforeach

check_functions = [
  'ftime',
  'gettimeofday',
  'vsnprintf',
]

foreach function : check_functions
  macro_name = 'HAS_' + function.to_upper().underscorify()
  conf.set(macro_name, cc.has_function(function))
endforeach


libsndfile = cc.find_library('sndfile')
portaudio  = dependency('portaudio-2.0')

# configure_file(
#   output: 'config.h',
#   configuration: conf,
# )


rootInclude = include_directories('.')

subdir('nyquist')

libnyquist_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with: libnyquist,
  version: meson.project_version(),
  dependencies: [
    libsndfile,
    portaudio,
  ],
)
