
project('audacity',
  [ 'c', 'cpp' ],
  version: '2.3.0',
  meson_version: '>=0.47.0',
  default_options: [
    'c_std=gnu11',
    'cpp_std=gnu++11',
    'warning_level=2',
  ]
)


cc = meson.get_compiler('c')
cx = meson.get_compiler('cpp')
conf = configuration_data()

################################################################################
# Project info




# AC_SUBST(MIMETYPES)


# allow the user to specify options to configure that change the
# name "audacity" anywhere it appears in a pathname.   This allows
# multiple versions of Audacity to be installed concurrently
# without collision
project_name = get_option('override_project_name')

conf.set_quoted('AUDACITY_NAME', project_name)


conf.set_quoted('INSTALL_PREFIX', get_option('prefix'))

conf.set('BUILDING_AUDACITY', true)


rdynamic_supported = cc.has_link_argument('-rdynamic')

if rdynamic_supported
  add_project_arguments('-rdynamic', language: [ 'c', 'cpp' ])
endif



################################################################################
# Compiler opions

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

conf.set('HAVE_ALLOCA_H', cc.has_header('alloca.h'))

# todo gl_VISIBILITY



################################################################################
# Dependencies

wx_args = []
wx_args += '--static='  + (get_option('static-wx')  ? 'yes' : 'no')
wx_args += '--unicode=' + (get_option('unicode')    ? 'yes' : 'no')
wx_args += '--debug='   + (get_option('debug')      ? 'yes' : 'no')

if get_option('wx-version') != ''
  wx_args += '--version=' + get_option('wx-version')
endif

wxwidgets = dependency('wxwidgets', modules: wx_args)






# subproject('lib-src/FileDialog')
# subproject('lib-src/mod-nyq-bench')
# subproject('lib-src/mod-script-pipe')
# subproject('lib-src/mod-null')

rootInclude = include_directories('.')

