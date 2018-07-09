
project('audacity',
  [ 'c', 'cpp' ],
  version: '2.3.0',
  meson_version: '>=0.47.0',
  default_options: [
    'c_std=gnu11',
    'cpp_std=gnu++11',
    'warning_level=1',
  ],
  subproject_dir: 'lib-src',
)


cc = meson.get_compiler('c')
cx = meson.get_compiler('cpp')

i18n = import('i18n')

conf = configuration_data()
desktop_conf = configuration_data()

################################################################################
# Project info

# allow the user to specify options to configure that change the
# name "audacity" anywhere it appears in a pathname.   This allows
# multiple versions of Audacity to be installed concurrently
# without collision
project_name = get_option('override_project_name')
if project_name == ''
  project_name = meson.project_name()
endif

conf.set_quoted ('AUDACITY_NAME', project_name)
desktop_conf.set('AUDACITY_NAME', project_name)

conf.set_quoted('INSTALL_PREFIX', get_option('prefix'))

conf.set('BUILDING_AUDACITY', true)

conf.set('VERSION', meson.project_version())

conf.set_quoted('LIBDIR', join_paths(get_option('prefix'), get_option('libdir')))

gettext_package = project_name
conf.set('GETTEXT_PACKAGE', gettext_package)

################################################################################
# Compiler opions

rdynamic_supported = cc.has_link_argument('-rdynamic')
if rdynamic_supported
  add_project_arguments('-rdynamic', language: [ 'c', 'cpp' ])
endif


if get_option('universal-binary')
  if host_machine.os().contains('darwin')
    CPPFLAGS='''${CPPFLAGS}
    -mmacosx-version-min=10.4
    -arch i386
    -arch ppc
    -isysroot /Developer/SDKs/MacOSX10.4u.sdk
    -Xarch_i386 -DAPPLE_I386 -Xarch_ppc -DAPPLE_PPC'''
  endif
endif


if get_option('sse')

  sse_ok = (
    cc.has_argument('-msse') and
    cx.has_argument('-msse') and
    cc.compiles('''
      #include <xmmintrin.h>

      int main() {
        __m128 v, w;
        float p[8];
        v = _mm_loadu_ps(p);
        _mm_storeu_ps(p, v);
        w = _mm_loadl_pi(w, (const __m64*)p);
        w = _mm_loadh_pi(w, (const __m64*)p);
        _mm_storel_pi((__m64 *)p, w);
        _mm_storeh_pi((__m64 *)p, w);
        v = _mm_add_ps(v, w);
        v = _mm_sub_ps(v, w);
        v = _mm_mul_ps(v, w);
        v = _mm_shuffle_ps(v, w, _MM_SHUFFLE(0, 1, 2, 3));
        w = _mm_set1_ps(0.0f);
        return 0;
      }
    ''')
  )

  if not sse_ok
    error('sse required but not available!')
  endif

  add_project_arguments('-msse', language: [ 'c', 'cpp' ])
endif
conf.set('ENABLE_SSE', get_option('sse'))


if get_option('debug')
  conf.set('wxDEBUG_LEVEL', 1)
else
  conf.set('wxDEBUG_LEVEL', *)
endif


check_headers = [
  'alloca.h',
  'dlfcn.h',
  'inttypes.h',
  'libudev.h',
  'memory.h',
  'nanosle.h',
  'stdint.h',
  'stdlib.h',
  'string.h',
  'strings.h',
  'unistd.h',
  'sys/stat.h',
  'sys/types.h',
]

foreach header : check_headers
  macro_name = 'HAVE_' + header.to_upper().underscorify()
  conf.set(macro_name, cc.has_header(header) ? 1 : false)
endforeach

check_functions = [
  'clock_gettime',
  'dcgettext',
  'gettext',
  'iconv',
  'lrint',
  'lrintf',
  'nanosleep',
  # MacOSX specific
  'cflocalecopycurrent',
  'cfpreferencescopyappvalue',
]

foreach function : check_functions
  macro_name = 'HAVE_' + function.to_upper().underscorify()
  conf.set(macro_name, cc.has_function(function) ? 1 : false)
endforeach

conf.set('ENABLE_NLS', true)


# todo gl_VISIBILITY

conf.set('HAVE_VISIBILITY', 1)

add_project_arguments([
    '-Wno-ignored-qualifiers',
    '-Wno-cast-function-type',
    '-Wno-non-virtual-dtor',
    '-Wno-misleading-indentation',
    '-Wno-deprecated-declarations',
  ],
  language: [ 'c', 'cpp', ],
)


################################################################################
# Dependencies

mimetypes = []


wx_args = []
wx_args += '--static='  + (get_option('static-wx')  ? 'yes' : 'no')
wx_args += '--unicode=' + (get_option('unicode')    ? 'yes' : 'no')
wx_args += '--debug='   + (get_option('debug')      ? 'yes' : 'no')

if get_option('wx-version') != ''
  wx_args += '--version=' + get_option('wx-version')
endif
if get_option('force-gtk3')
  wx_args += '--toolkit=gtk3'
endif

wxwidgets = dependency('wxwidgets', modules: wx_args)



expat       = dependency('expat')

# FFmpeg
libavcodec  = dependency('libavcodec',  version: '>=51.53')
libavformat = dependency('libavformat', version: '>=52.12')
libavutil   = dependency('libavutil',   version: '>=52.12')
use_ffmpeg = (libavcodec.found() and libavformat.found() and libavutil.found())
conf.set('USE_FFMPEG', use_ffmpeg)
conf.set('DISABLE_DYNAMIC_LOADING_FFMPEG', get_option('dynamic-loading'))
if use_ffmpeg
  mimetypes += [
    'audio/aac' ,
    'audio/ac3' ,
    'audio/mp4' ,
    'audio/x-ms-wma',
    'video/mpeg',
  ]
endif

gstreamer   = dependency('gstreamer-1.0')
gstreamerapp= dependency('gstreamer-app-1.0')
conf.set('USE_GSTREAMER', gstreamer.found() and gstreamerapp.found())

lame        = cc.find_library('mp3lame')
conf.set('DISABLE_DYNAMIC_LOADING_LAME', get_option('dynamic-loading'))

libflac     = dependency('flac',          version: '>=1.3.0')
libflacpp   = dependency('flac++',        version: '>=1.3.0')
use_flac = libflac.found() and libflacpp.found()
conf.set10('USE_LIBFLAC', use_flac)
if use_flac
  mimetypes += [
    'audio/flac',
    'audio/x-flac',
  ]
endif

id3tag      = dependency('id3tag',        required: false)
conf.set('USE_LIBID3TAG', id3tag.found())

mad         = dependency('mad',           version: '>=0.14.2',required: false)
conf.set('USE_LIBMAD', mad.found())
if mad.found()
  mimetypes += 'audio/mpeg'
endif

sbsms       = dependency('sbsms',         version: '>=2.0.2', required: false)
conf.set10('USE_SBSMS', sbsms.found())

sndfile     = dependency('sndfile',       version: '>=1.0.0', required: false)
if sndfile.found()
  mimetypes += [
    'audio/basic',
    'audio/x-aiff',
    'audio/x-wav',
  ]
endif

soundtouch  = dependency('soundtouch',    version: '>=1.3.0', required: false)
conf.set10('USE_SOUNDTOUCH', soundtouch.found())

soxr        = dependency('soxr',          version: '>=0.0.5', required: false)
twolame     = dependency('twolame',       version: '>=0.3.9', required: false)
conf.set10('USE_LIBTWOLAME', twolame.found())

vamp        = dependency('vamp',          version: '>=2.0',   required: false)
vamphost    = dependency('vamp-hostsdk',  version: '>=2.0',   required: false)
conf.set10('USE_VAMP', vamp.found() and vamphost.found())

vorbis      = dependency('vorbis',        version: '>=0.3.9', required: false)
vorbisenc   = dependency('vorbisenc',     version: '>=0.3.9', required: false)
vorbisfile  = dependency('vorbisfile',    version: '>=0.3.9', required: false)
use_vorbis = vorbis.found() and vorbisenc.found() and vorbisfile.found()
conf.set('USE_LIBVORBIS', use_vorbis)
if use_vorbis
  mimetypes += [ 'application/ogg', 'audio/x-vorbis+ogg' ]
endif


libogg      = dependency('ogg',           version: '>=0.3.9', required: false)

lv2         = dependency('lv2',           version: '>=0.8.2', required: false)
lilv        = dependency('lilv-0',        version: '>=0.16',  required: false)
suil        = dependency('suil-0',        version: '>=0.8.2', required: false)
conf.set10('USE_LV2', lv2.found() and lilv.found() and suil.found())

portaudio   = dependency('portaudio-2.0', version: '>=19',    required: false)
portaudiocpp= dependency('portaudiocpp',  version: '>=12',    required: false)
portmidi    = cc.find_library('portmidi', required: false)
conf.set('USE_PORTMIDI', portmidi.found())

portsmf     = dependency('portSMF',       required: false)
conf.set('USE_MIDI', portsmf.found())

portmixer   = dependency('portmixer',     required: false)
conf.set('USE_PORTMIXER', portmixer.found())


filedialog  = subproject('FileDialog',
  default_options: [ 'default_library=static', ]
).get_variable('filedialog_dep')

widgetextra = subproject('lib-widget-extra',
  default_options: [ 'default_library=static', ]
).get_variable('widgetextra_dep')

libnyquist  =  subproject('libnyquist',
  default_options: [ 'default_library=static', ]
).get_variable('libnyquist_dep')
conf.set10('USE_NYQUIST', libnyquist.found())

# subproject('lib-src/mod-nyq-bench')
# subproject('lib-src/mod-script-pipe')
# subproject('lib-src/mod-null')

if not expat.found()
   error('Audacity requires expat to be enabled.')
endif

# if not widgetextra.found()
#    error('Audacity requires libwidgetextra to be enabled.')
# endif

if not sndfile.found()
   error('Audacity requires libsndfile to be enabled')
endif

if not portaudio.found()
   error('Audacity requires portaudio to be enabled')
endif

if not soxr.found()
   error('Audacity requires libsoxr to be enabled')
endif









################################################################################
# Os-specific configuration

os = host_machine.system()

platform_osx    = os.contains('darwin') or os.contains('rhapsody')
platform_win32  = os.contains('cygwin') or os.contains('mingw') or os.contains('windows')
platform_linux  = (not platform_osx and not platform_win32)

if platform_osx
  # Mac OS X configuration
  CDEPEND = 'AudacityHeaders.h.gch'
  PRECOMP_CFLAGS = '-include AudacityHeaders.h'

  # LIBS="-framework AudioUnit -framework AudioToolbox $LIBS"
  # LIBS="-framework CoreAudio $LIBS -lz"

  extra_targets='../Audacity.app'
  extra_include_dir = include_directories('mac')

  config_header = 'configunix.h'

  use_audiounits = get_option('audiounits')
  use_quicktime  = get_option('quicktime')
else
  use_audiounits = false
  use_quicktime  = false
endif

if platform_win32
  # Windows/CygWin configuration

  # LIBS="-lkernel32 -luser32 -lgdi32 -lwinspool -lcomdlg32 -ladvapi32 -lshell32
  # -lole32 -loleaut32 -luuid -lodbc32 -lodbccp32 -lwsock32 -lwinmm"

  config_header = 'configwin.h'

endif
conf.set('__CYGWIN__', platform_win32)
conf.set('_FILE_OFFSET_BITS', platform_win32 ? 32 : false)

if platform_linux
  # Unix configuration

  config_header = 'configunix.h'

  dl = cc.find_library('dl')

  use_gtk3 = true
  # use_gtk3 = cx.run('''
  # #include <wx/wx.h>
  # int main() {
  # #if defined(__WXGTK3__)
  #   return 0;
  # #else
  #   return 1;
  # #endif
  # }
  # ''', dependencies: wxwidgets).returncode() == 0

  gtk_dep_name    = use_gtk3 ? 'gtk+-3.0' : 'gtk+-2.0'
  gtk_dep_version = use_gtk3 ? '>=3.10.0' : '>=2.24.0'

  gtk = dependency(gtk_dep_name, version: gtk_dep_version)

  conf.set('HAVE_GTK', gtk.found() ? 1 : false)

  alsa  = dependency('alsa',      required: false)
  jack  = dependency('jack',      required: false)
  hpi   = cc.find_library('hpi',  required: false)


  rt    = cc.find_library('rt')
  udev  = cc.find_library('udev')

endif


conf.set10('USE_LADSPA', get_option('ladspa'))

conf.set('USE_AUDIO_UNITS', use_audiounits)
conf.set('USE_QUICKTIME',   use_quicktime)
conf.set10('USE_VST',         get_option('vst'))


desktop_conf.set('MIMETYPES', ';'.join(mimetypes))


configure_file(
  input : 'config.h.in',
  output: config_header,
  configuration: conf,
)

# Configure finished
################################################################################
# Build

rootInclude = include_directories('.', 'include')


install_subdir('include',
  install_dir: join_paths(get_option('datadir'), project_name),
)
install_subdir('nyquist',
  install_dir: join_paths(get_option('datadir'), project_name),
)
install_subdir('plug-ins',
  install_dir: join_paths(get_option('datadir'), project_name),
)
install_subdir('presets',
  install_dir: join_paths(get_option('datadir'), project_name),
  strip_directory: true,
)
install_data([ 'LICENSE.txt', 'README.txt' ],
  install_dir: join_paths(get_option('datadir'), 'doc', project_name),
)


subdir('help')
subdir('images')
# subdir('lib-src')
subdir('po')
subdir('src')
# subdir('tests')

# subdir('lib-src/mod-nyq-bench')
# subdir('lib-src/mod-script-pipe')
# subdir('lib-src/mod-null')







message('\n'.join([
  '',
  'Configuration finished:',

# for lib in $LIBRARIES ; do
#    eval LIB_USE_LOCAL=\$${lib}_USE_LOCAL
#    eval LIB_USE_SYSTEM=\$${lib}_USE_SYSTEM
#    if test "$LIB_USE_LOCAL" = "yes" ; then
#       echo "$lib: using LOCAL libraries"
#    elif test "$LIB_USE_SYSTEM" = "yes" ; then
#       echo "$lib: using SYSTEM libraries"
#    else
#       echo "$lib: disabled"
#    fi
# done

  'Build module mod-nyq-pipe: @0@'.format(false),
  '',
  'Build module mod-nyq-bench: @0@'.format(false),
  '',
  'Ladspa plugin support: @0@'.format(get_option('ladspa')),
  '',
  'Audiounit plugin support: @0@'.format(use_audiounits),
  '',
  'VST plugin support: @0@'.format(get_option('vst')),
]))
