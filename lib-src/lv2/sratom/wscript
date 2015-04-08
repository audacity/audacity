#!/usr/bin/env python
import os
import subprocess
import waflib.Options as Options
import waflib.extras.autowaf as autowaf

# Library and package version (UNIX style major, minor, micro)
# major increment <=> incompatible changes
# minor increment <=> compatible changes (additions)
# micro increment <=> no interface changes
SRATOM_VERSION       = '0.4.6'
SRATOM_MAJOR_VERSION = '0'

# Mandatory waf variables
APPNAME = 'sratom'        # Package name for waf dist
VERSION = SRATOM_VERSION  # Package version for waf dist
top     = '.'             # Source directory
out     = 'build'         # Build directory

def options(opt):
    opt.load('compiler_c')
    autowaf.set_options(opt)
    opt.add_option('--test', action='store_true', dest='build_tests',
                   help="Build unit tests")
    opt.add_option('--static', action='store_true', dest='static',
                   help="Build static library")
    opt.add_option('--no-shared', action='store_true', dest='no_shared',
                   help='Do not build shared library')

def configure(conf):
    conf.load('compiler_c')
    autowaf.configure(conf)
    autowaf.set_c99_mode(conf)
    autowaf.display_header('Sratom Configuration')

    conf.env.BUILD_TESTS  = Options.options.build_tests
    conf.env.BUILD_SHARED = not Options.options.no_shared
    conf.env.BUILD_STATIC = Options.options.static

    if not conf.env.BUILD_SHARED and not conf.env.BUILD_STATIC:
        conf.fatal('Neither a shared nor a static build requested')

    # Check for gcov library (for test coverage)
    if conf.env.BUILD_TESTS:
        conf.check_cc(lib='gcov',
                      define_name='HAVE_GCOV',
                      mandatory=False)

    autowaf.check_pkg(conf, 'lv2', uselib_store='LV2',
                      atleast_version='1.8.1', mandatory=True)
    autowaf.check_pkg(conf, 'serd-0', uselib_store='SERD',
                      atleast_version='0.14.0', mandatory=True)
    autowaf.check_pkg(conf, 'sord-0', uselib_store='SORD',
                      atleast_version='0.12.0', mandatory=True)

    autowaf.define(conf, 'SRATOM_VERSION', SRATOM_VERSION)
    autowaf.set_lib_env(conf, 'sratom', SRATOM_VERSION)
    conf.write_config_header('sratom_config.h', remove=False)

    autowaf.display_msg(conf, "Unit tests", str(conf.env.BUILD_TESTS))
    print('')

lib_source = ['src/sratom.c']

def build(bld):
    # C Headers
    includedir = '${INCLUDEDIR}/sratom-%s/sratom' % SRATOM_MAJOR_VERSION
    bld.install_files(includedir, bld.path.ant_glob('sratom/*.h'))

    # Pkgconfig file
    autowaf.build_pc(bld, 'SRATOM', SRATOM_VERSION, SRATOM_MAJOR_VERSION,
                     ['SERD', 'SORD', 'LV2'],
                     {'SRATOM_MAJOR_VERSION' : SRATOM_MAJOR_VERSION})

    libflags = ['-fvisibility=hidden']
    libs     = ['m']
    defines  = []
    if bld.env.MSVC_COMPILER:
        libflags = []
        libs     = []
        defines  = ['snprintf=_snprintf']

    # Shared Library
    if bld.env.BUILD_SHARED:
        obj = bld(features        = 'c cshlib',
                  export_includes = ['.'],
                  source          = lib_source,
                  includes        = ['.', './src'],
                  lib             = libs,
                  name            = 'libsratom',
                  target          = 'sratom-%s' % SRATOM_MAJOR_VERSION,
                  vnum            = SRATOM_VERSION,
                  install_path    = '${LIBDIR}',
                  defines         = defines + ['SRATOM_SHARED', 'SRATOM_INTERNAL'],
                  cflags          = libflags)
        autowaf.use_lib(bld, obj, 'SERD SORD LV2')

    # Static library
    if bld.env.BUILD_STATIC:
        obj = bld(features        = 'c cstlib',
                  export_includes = ['.'],
                  source          = lib_source,
                  includes        = ['.', './src'],
                  lib             = libs,
                  name            = 'libsratom_static',
                  target          = 'sratom-%s' % SRATOM_MAJOR_VERSION,
                  vnum            = SRATOM_VERSION,
                  install_path    = '${LIBDIR}',
                  defines         = defines + ['SRATOM_INTERNAL'])
        autowaf.use_lib(bld, obj, 'SERD SORD LV2')

    if bld.env.BUILD_TESTS:
        test_libs   = libs
        test_cflags = ['']
        if bld.is_defined('HAVE_GCOV'):
            test_libs   += ['gcov']
            test_cflags += ['-fprofile-arcs', '-ftest-coverage']

        # Static library (for unit test code coverage)
        obj = bld(features     = 'c cstlib',
                  source       = lib_source,
                  includes     = ['.', './src'],
                  lib          = test_libs,
                  name         = 'libsratom_profiled',
                  target       = 'sratom_profiled',
                  install_path = '',
                  defines      = defines + ['SRATOM_INTERNAL'],
                  cflags       = test_cflags)
        autowaf.use_lib(bld, obj, 'SERD SORD LV2')

        # Unit test program
        obj = bld(features     = 'c cprogram',
                  source       = 'tests/sratom_test.c',
                  includes     = ['.', './src'],
                  use          = 'libsratom_profiled',
                  lib          = test_libs,
                  target       = 'sratom_test',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags)
        autowaf.use_lib(bld, obj, 'SERD SORD LV2')

    # Documentation
    autowaf.build_dox(bld, 'SRATOM', SRATOM_VERSION, top, out)

    bld.add_post_fun(autowaf.run_ldconfig)
    if bld.env.DOCS:
        bld.add_post_fun(fix_docs)

def test(ctx):
    autowaf.pre_test(ctx, APPNAME)
    os.environ['PATH'] = '.' + os.pathsep + os.getenv('PATH')
    autowaf.run_tests(ctx, APPNAME, ['sratom_test'], dirs=['./src','./tests'])
    autowaf.post_test(ctx, APPNAME)

def lint(ctx):
    subprocess.call('cpplint.py --filter=+whitespace/comments,-whitespace/tab,-whitespace/braces,-whitespace/labels,-build/header_guard,-readability/casting,-readability/todo,-build/include src/* sratom/*', shell=True)

def fix_docs(ctx):
    if ctx.cmd == 'build':
        autowaf.make_simple_dox(APPNAME)

def upload_docs(ctx):
    os.system("rsync -ravz --delete -e ssh build/doc/html/ drobilla@drobilla.net:~/drobilla.net/docs/sratom/")
