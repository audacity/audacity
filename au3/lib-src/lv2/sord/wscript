#!/usr/bin/env python

import glob
import os
import sys

from waflib import Logs, Options
from waflib.extras import autowaf

# Library and package version (UNIX style major, minor, micro)
# major increment <=> incompatible changes
# minor increment <=> compatible changes (additions)
# micro increment <=> no interface changes
SORD_VERSION       = '0.16.4'
SORD_MAJOR_VERSION = '0'

# Mandatory waf variables
APPNAME = 'sord'        # Package name for waf dist
VERSION = SORD_VERSION  # Package version for waf dist
top     = '.'           # Source directory
out     = 'build'       # Build directory

# Release variables
uri          = 'http://drobilla.net/sw/sord'
dist_pattern = 'http://download.drobilla.net/sord-%d.%d.%d.tar.bz2'
post_tags    = ['Hacking', 'RDF', 'Sord']

def options(ctx):
    ctx.load('compiler_c')
    ctx.load('compiler_cxx')
    opt = ctx.configuration_options()

    ctx.add_flags(
        opt,
        {'no-utils':     'do not build command line utilities',
         'static':       'build static library',
         'no-shared':    'do not build shared library',
         'static-progs': 'build programs as static binaries'})

    opt.add_option('--dump', type='string', default='', dest='dump',
                   help='dump debugging output (iter, search, write, all)')

def configure(conf):
    conf.load('compiler_c', cache=True)
    if Options.options.build_tests:
        try:
            conf.load('compiler_cxx', cache=True)
        except:
            Logs.warn("No C++ compiler, sordmm.hpp compile test skipped")
            pass

    conf.load('autowaf', cache=True)
    autowaf.set_c_lang(conf, 'c99')

    conf.env.BUILD_UTILS  = not Options.options.no_utils
    conf.env.BUILD_SHARED = not Options.options.no_shared
    conf.env.STATIC_PROGS = Options.options.static_progs
    conf.env.BUILD_STATIC = (Options.options.static or
                             Options.options.static_progs)

    conf.check_pkg('serd-0 >= 0.30.0', uselib_store='SERD')
    conf.check_pkg('libpcre', uselib_store='PCRE', mandatory=False)

    if conf.env.HAVE_PCRE:
        if conf.check(cflags=['-pthread'], mandatory=False):
            conf.env.PTHREAD_CFLAGS    = ['-pthread']
            if conf.env.CC_NAME != 'clang':
                conf.env.PTHREAD_LINKFLAGS = ['-pthread']
        elif conf.check(linkflags=['-lpthread'], mandatory=False):
            conf.env.PTHREAD_CFLAGS    = []
            conf.env.PTHREAD_LINKFLAGS = ['-lpthread']
        else:
            conf.env.PTHREAD_CFLAGS    = []
            conf.env.PTHREAD_LINKFLAGS = []

    # Parse dump options and define things accordingly
    dump = Options.options.dump.split(',')
    all = 'all' in dump
    if all or 'iter' in dump:
        conf.define('SORD_DEBUG_ITER', 1)
    if all or 'search' in dump:
        conf.define('SORD_DEBUG_SEARCH', 1)
    if all or 'write' in dump:
        conf.define('SORD_DEBUG_WRITE', 1)

    autowaf.set_lib_env(conf, 'sord', SORD_VERSION)
    conf.write_config_header('sord_config.h', remove=False)

    autowaf.display_summary(
        conf,
        {'Static library': bool(conf.env.BUILD_STATIC),
         'Shared library': bool(conf.env.BUILD_SHARED),
         'Utilities':      bool(conf.env.BUILD_UTILS),
         'Unit tests':     bool(conf.env.BUILD_TESTS),
         'Debug dumping':  dump})

def build(bld):
    # C/C++ Headers
    includedir = '${INCLUDEDIR}/sord-%s/sord' % SORD_MAJOR_VERSION
    bld.install_files(includedir, bld.path.ant_glob('sord/*.h'))
    bld.install_files(includedir, bld.path.ant_glob('sord/*.hpp'))

    # Pkgconfig file
    autowaf.build_pc(bld, 'SORD', SORD_VERSION, SORD_MAJOR_VERSION, [],
                     {'SORD_MAJOR_VERSION' : SORD_MAJOR_VERSION,
                      'SORD_PKG_DEPS'      : 'serd-0'})

    source = 'src/sord.c src/syntax.c'

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
                  source          = source,
                  includes        = ['.', './src'],
                  export_includes = ['.'],
                  name            = 'libsord',
                  target          = 'sord-%s' % SORD_MAJOR_VERSION,
                  vnum            = SORD_VERSION,
                  install_path    = '${LIBDIR}',
                  libs            = libs,
                  uselib          = 'SERD',
                  defines         = defines + ['SORD_SHARED', 'SORD_INTERNAL'],
                  cflags          = libflags)

    # Static Library
    if bld.env.BUILD_STATIC:
        obj = bld(features        = 'c cstlib',
                  source          = source,
                  includes        = ['.', './src'],
                  export_includes = ['.'],
                  name            = 'libsord_static',
                  target          = 'sord-%s' % SORD_MAJOR_VERSION,
                  vnum            = SORD_VERSION,
                  install_path    = '${LIBDIR}',
                  libs            = libs,
                  uselib          = 'SERD',
                  defines         = ['SORD_INTERNAL'])

    if bld.env.BUILD_TESTS:
        test_libs      = libs
        test_cflags    = ['']
        test_linkflags = ['']
        if not bld.env.NO_COVERAGE:
            test_cflags    += ['--coverage']
            test_linkflags += ['--coverage']

        # Profiled static library for test coverage
        obj = bld(features     = 'c cstlib',
                  source       = source,
                  includes     = ['.', './src'],
                  name         = 'libsord_profiled',
                  target       = 'sord_profiled',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags,
                  linkflags    = test_linkflags,
                  lib          = test_libs,
                  uselib       = 'SERD')

        # Unit test program
        obj = bld(features     = 'c cprogram',
                  source       = 'src/sord_test.c',
                  includes     = ['.', './src'],
                  use          = 'libsord_profiled',
                  lib          = test_libs,
                  target       = 'sord_test',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags,
                  linkflags    = test_linkflags,
                  uselib       = 'SERD')

        # Static profiled sordi for tests
        obj = bld(features     = 'c cprogram',
                  source       = 'src/sordi.c',
                  includes     = ['.', './src'],
                  use          = 'libsord_profiled',
                  lib          = test_libs,
                  target       = 'sordi_static',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags,
                  linkflags    = test_linkflags,
                  uselib       = 'SERD')

        # C++ build test
        if bld.env.COMPILER_CXX:
            obj = bld(features     = 'cxx cxxprogram',
                      source       = 'src/sordmm_test.cpp',
                      includes     = ['.', './src'],
                      use          = 'libsord_profiled',
                      lib          = test_libs,
                      target       = 'sordmm_test',
                      install_path = '',
                      defines      = defines,
                      cxxflags     = test_cflags,
                      linkflags    = test_linkflags,
                      uselib       = 'SERD')

    # Utilities
    if bld.env.BUILD_UTILS:
        utils = ['sordi']
        if bld.env.HAVE_PCRE:
            utils += ['sord_validate']
        for i in utils:
            obj = bld(features     = 'c cprogram',
                      source       = 'src/%s.c' % i,
                      includes     = ['.', './src'],
                      use          = 'libsord',
                      lib          = libs,
                      uselib       = 'SERD',
                      target       = i,
                      install_path = '${BINDIR}',
                      defines      = defines)
            if not bld.env.BUILD_SHARED or bld.env.STATIC_PROGS:
                obj.use = 'libsord_static'
            if bld.env.STATIC_PROGS:
                obj.env.SHLIB_MARKER = obj.env.STLIB_MARKER
                obj.linkflags        = ['-static', '-Wl,--start-group']
            if i == 'sord_validate':
                obj.uselib    += ' PCRE'
                obj.cflags    = bld.env.PTHREAD_CFLAGS
                obj.linkflags = bld.env.PTHREAD_LINKFLAGS

    # Documentation
    autowaf.build_dox(bld, 'SORD', SORD_VERSION, top, out)

    # Man pages
    bld.install_files('${MANDIR}/man1', bld.path.ant_glob('doc/*.1'))

    bld.add_post_fun(autowaf.run_ldconfig)
    if bld.env.DOCS:
        bld.add_post_fun(fix_docs)

def lint(ctx):
    "checks code for style issues"
    import subprocess
    cmd = ("clang-tidy -p=. -header-filter=.* -checks=\"*," +
           "-cert-dcl03-c," +
           "-clang-analyzer-alpha.*," +
           "-google-readability-todo," +
           "-llvm-header-guard," +
           "-llvm-include-order," +
           "-misc-static-assert," +
           "-misc-unused-parameters," +
           "-readability-else-after-return\" " +
           "../src/*.c")
    subprocess.call(cmd, cwd='build', shell=True)

def fix_docs(ctx):
    if ctx.cmd == 'build':
        autowaf.make_simple_dox(APPNAME)

def upload_docs(ctx):
    os.system('rsync -ravz --delete -e ssh build/doc/html/ drobilla@drobilla.net:~/drobilla.net/docs/sord/')
    for page in glob.glob('doc/*.[1-8]'):
        os.system('soelim %s | pre-grohtml troff -man -wall -Thtml | post-grohtml > build/%s.html' % (page, page))
        os.system('rsync -avz --delete -e ssh build/%s.html drobilla@drobilla.net:~/drobilla.net/man/' % page)

def test(tst):
    import tempfile
    try:
        test_dir = os.path.join
        os.mkdir('tests')
        for i in glob.glob('tests/*.*'):
            os.remove(i)
    except:
        pass

    if sys.platform == 'win32' and '/DNDEBUG' not in tst.env.CFLAGS:
        # FIXME: Sort out DLL memory freeing situation in next major version
        Logs.warn("Skipping tests for Windows debug build")
        return

    srcdir = tst.path.abspath()
    sordi = './sordi_static'
    base = 'http://example.org/'
    snippet = '<{0}s> <{0}p> <{0}o> .\n'.format(base)
    manifest = 'file://%s/tests/manifest.ttl' % srcdir

    with tst.group('Unit') as check:
        check(['./sord_test'])

    with tst.group('GoodCommands') as check:
        check([sordi, manifest])
        check([sordi, '%s/tests/UTF-8.ttl' % srcdir])
        check([sordi, '-v'])
        check([sordi, '-h'])
        check([sordi, '-s', '<foo> a <#Thingie> .', 'file:///test'])
        check([sordi, os.devnull], stdout=os.devnull)
        with tempfile.TemporaryFile(mode='r+') as stdin:
            stdin.write(snippet + '\n')
            check([sordi, '-', 'http://example.org/'], stdin=stdin)
            check([sordi, '-o', 'turtle', '-', 'http://example.org/'], stdin=stdin)

    with tst.group('BadCommands', expected=1) as check:
        check([sordi])
        check([sordi, 'ftp://example.org/unsupported.ttl'])
        check([sordi, '-i'])
        check([sordi, '-o'])
        check([sordi, '-z'])
        check([sordi, '-p'])
        check([sordi, '-c'])
        check([sordi, '-i illegal'])
        check([sordi, '-o illegal'])
        check([sordi, '-i turtle'])
        check([sordi, '-i ntriples'])
        check([sordi, '/no/such/file'])

    with tst.group('IoErrors', expected=1) as check:
        check([sordi, 'file://%s/' % srcdir], name='Read directory')
        if os.path.exists('/dev/full'):
            check([sordi, manifest], stdout='/dev/full', name='Write error')

    with tst.group('good', verbosity=0) as check:
        suite_base = 'http://www.w3.org/2001/sw/DataAccess/df1/'
        good_tests = glob.glob(os.path.join(srcdir, 'tests', 'test-*.ttl'))
        for test in good_tests:
            path = os.path.relpath(test, srcdir)
            base_uri = suite_base + path.replace('\\', '/')

            out_path = path + '.out'
            check([sordi, test, base_uri], stdout=out_path)

            check_path = test.replace('.ttl', '.out')
            out_lines = sorted(open(out_path).readlines())
            cmp_lines = sorted(open(check_path).readlines())
            check(lambda: out_lines == cmp_lines,
                  name='%s check' % path)

def posts(ctx):
    path = str(ctx.path.abspath())
    autowaf.news_to_posts(
        os.path.join(path, 'NEWS'),
        {'title'        : 'Sord',
         'description'  : autowaf.get_blurb(os.path.join(path, 'README')),
         'dist_pattern' : 'http://download.drobilla.net/sord-%s.tar.bz2'},
        { 'Author' : 'drobilla',
          'Tags'   : 'Hacking, RDF, Sord' },
        os.path.join(out, 'posts'))
