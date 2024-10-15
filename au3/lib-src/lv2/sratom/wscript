#!/usr/bin/env python

import os
import subprocess

from waflib import Logs, Options
from waflib.extras import autowaf

# Library and package version (UNIX style major, minor, micro)
# major increment <=> incompatible changes
# minor increment <=> compatible changes (additions)
# micro increment <=> no interface changes
SRATOM_VERSION       = '0.6.4'
SRATOM_MAJOR_VERSION = '0'

# Mandatory waf variables
APPNAME = 'sratom'        # Package name for waf dist
VERSION = SRATOM_VERSION  # Package version for waf dist
top     = '.'             # Source directory
out     = 'build'         # Build directory

# Release variables
uri          = 'http://drobilla.net/sw/sratom'
dist_pattern = 'http://download.drobilla.net/sratom-%d.%d.%d.tar.bz2'
post_tags    = ['Hacking', 'LAD', 'LV2', 'RDF', 'Sratom']

def options(ctx):
    ctx.load('compiler_c')
    ctx.add_flags(
        ctx.configuration_options(),
        {'static':    'build static library',
         'no-shared': 'do not build shared library'})

def configure(conf):
    conf.load('compiler_c', cache=True)
    conf.load('autowaf', cache=True)
    autowaf.set_c_lang(conf, 'c99')

    conf.env.BUILD_SHARED = not Options.options.no_shared
    conf.env.BUILD_STATIC = Options.options.static

    if not conf.env.BUILD_SHARED and not conf.env.BUILD_STATIC:
        conf.fatal('Neither a shared nor a static build requested')

    conf.check_pkg('lv2 >= 1.16.0', uselib_store='LV2')
    conf.check_pkg('serd-0 >= 0.30.0', uselib_store='SERD')
    conf.check_pkg('sord-0 >= 0.14.0', uselib_store='SORD')

    autowaf.set_lib_env(conf, 'sratom', SRATOM_VERSION)
    conf.write_config_header('sratom_config.h', remove=False)

    autowaf.display_summary(conf, {'Unit tests': bool(conf.env.BUILD_TESTS)})

lib_source = ['src/sratom.c']

def build(bld):
    # C Headers
    includedir = '${INCLUDEDIR}/sratom-%s/sratom' % SRATOM_MAJOR_VERSION
    bld.install_files(includedir, bld.path.ant_glob('sratom/*.h'))

    # Pkgconfig file
    autowaf.build_pc(bld, 'SRATOM', SRATOM_VERSION, SRATOM_MAJOR_VERSION, [],
                     {'SRATOM_MAJOR_VERSION' : SRATOM_MAJOR_VERSION,
                      'SRATOM_PKG_DEPS' : 'lv2 serd-0 sord-0'})

    libflags = ['-fvisibility=hidden']
    libs     = ['m']
    defines  = []
    if bld.env.MSVC_COMPILER:
        libflags = []
        libs     = []
        defines  = []

    # Shared Library
    if bld.env.BUILD_SHARED:
        obj = bld(features        = 'c cshlib',
                  export_includes = ['.'],
                  source          = lib_source,
                  includes        = ['.', './src'],
                  lib             = libs,
                  uselib          = 'SERD SORD LV2',
                  name            = 'libsratom',
                  target          = 'sratom-%s' % SRATOM_MAJOR_VERSION,
                  vnum            = SRATOM_VERSION,
                  install_path    = '${LIBDIR}',
                  defines         = defines + ['SRATOM_SHARED', 'SRATOM_INTERNAL'],
                  cflags          = libflags)

    # Static library
    if bld.env.BUILD_STATIC:
        obj = bld(features        = 'c cstlib',
                  export_includes = ['.'],
                  source          = lib_source,
                  includes        = ['.', './src'],
                  lib             = libs,
                  uselib          = 'SERD SORD LV2',
                  name            = 'libsratom_static',
                  target          = 'sratom-%s' % SRATOM_MAJOR_VERSION,
                  vnum            = SRATOM_VERSION,
                  install_path    = '${LIBDIR}',
                  defines         = defines + ['SRATOM_INTERNAL'])

    if bld.env.BUILD_TESTS:
        test_libs   = libs
        test_cflags = ['']
        test_linkflags  = ['']
        if not bld.env.NO_COVERAGE:
            test_cflags    += ['--coverage']
            test_linkflags += ['--coverage']

        # Static library (for unit test code coverage)
        obj = bld(features     = 'c cstlib',
                  source       = lib_source,
                  includes     = ['.', './src'],
                  lib          = test_libs,
                  uselib       = 'SERD SORD LV2',
                  name         = 'libsratom_profiled',
                  target       = 'sratom_profiled',
                  install_path = '',
                  defines      = defines + ['SRATOM_INTERNAL'],
                  cflags       = test_cflags,
                  linkflags    = test_linkflags)

        # Unit test program
        obj = bld(features     = 'c cprogram',
                  source       = 'tests/sratom_test.c',
                  includes     = ['.', './src'],
                  use          = 'libsratom_profiled',
                  lib          = test_libs,
                  uselib       = 'SERD SORD LV2',
                  target       = 'sratom_test',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags,
                  linkflags    = test_linkflags)

    # Documentation
    autowaf.build_dox(bld, 'SRATOM', SRATOM_VERSION, top, out)

    bld.add_post_fun(autowaf.run_ldconfig)

def test(tst):
    import sys

    if sys.platform == 'win32' and '/DNDEBUG' not in tst.env.CFLAGS:
        # FIXME: Sort out DLL memory freeing situation in next major version
        Logs.warn("Skipping tests for Windows debug build")
        return

    with tst.group('Integration') as check:
        check(['./sratom_test'])

def lint(ctx):
    "checks code for style issues"
    import subprocess
    cmd = ("clang-tidy -p=. -header-filter=.* -checks=\"*," +
           "-bugprone-suspicious-string-compare," +
           "-clang-analyzer-alpha.*," +
           "-hicpp-signed-bitwise," +
           "-llvm-header-guard," +
           "-readability-else-after-return\" " +
           "$(find .. -name '*.c')")
    subprocess.call(cmd, cwd='build', shell=True)
