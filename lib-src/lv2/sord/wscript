#!/usr/bin/env python
import glob
import os
import subprocess
import waflib.Logs as Logs
import waflib.Options as Options
import waflib.extras.autowaf as autowaf

# Library and package version (UNIX style major, minor, micro)
# major increment <=> incompatible changes
# minor increment <=> compatible changes (additions)
# micro increment <=> no interface changes
SORD_VERSION       = '0.12.0'
SORD_MAJOR_VERSION = '0'

# Mandatory waf variables
APPNAME = 'sord'        # Package name for waf dist
VERSION = SORD_VERSION  # Package version for waf dist
top     = '.'           # Source directory
out     = 'build'       # Build directory

def options(opt):
    opt.load('compiler_c')
    opt.load('compiler_cxx')
    autowaf.set_options(opt)
    opt.add_option('--no-utils', action='store_true', dest='no_utils',
                   help='Do not build command line utilities')
    opt.add_option('--test', action='store_true', dest='build_tests',
                   help='Build unit tests')
    opt.add_option('--static', action='store_true', dest='static',
                   help='Build static library')
    opt.add_option('--no-shared', action='store_true', dest='no_shared',
                   help='Do not build shared library')
    opt.add_option('--static-progs', action='store_true', dest='static_progs',
                   help='Build programs as static binaries')
    opt.add_option('--dump', type='string', default='', dest='dump',
                   help='Dump debugging output (iter, search, write, all)')

def configure(conf):
    conf.load('compiler_c')
    if Options.options.build_tests:
        try:
            conf.load('compiler_cxx')
        except:
            Logs.warn("No C++ compiler, sordmm.hpp compile test skipped")
            pass
        
    autowaf.configure(conf)
    autowaf.set_c99_mode(conf)
    autowaf.display_header('Sord configuration')

    conf.env.BUILD_TESTS  = Options.options.build_tests
    conf.env.BUILD_UTILS  = not Options.options.no_utils
    conf.env.BUILD_SHARED = not Options.options.no_shared
    conf.env.STATIC_PROGS = Options.options.static_progs
    conf.env.BUILD_STATIC = (Options.options.static or
                             Options.options.static_progs)

    if conf.env.BUILD_TESTS:
        conf.check(lib         = 'gcov',
                   define_name = 'HAVE_GCOV',
                   mandatory   = False)

    autowaf.check_pkg(conf, 'serd-0', uselib_store='SERD',
                      atleast_version='0.18.0', mandatory=True)
    autowaf.check_pkg(conf, 'libpcre', uselib_store='PCRE', mandatory=False)

    # Parse dump options and define things accordingly
    dump = Options.options.dump.split(',')
    all = 'all' in dump
    if all or 'iter' in dump:
        autowaf.define(conf, 'SORD_DEBUG_ITER', 1)
    if all or 'search' in dump:
        autowaf.define(conf, 'SORD_DEBUG_SEARCH', 1)
    if all or 'write' in dump:
        autowaf.define(conf, 'SORD_DEBUG_WRITE', 1)

    autowaf.define(conf, 'SORD_VERSION', SORD_VERSION)
    autowaf.set_lib_env(conf, 'sord', SORD_VERSION)
    conf.write_config_header('sord_config.h', remove=False)

    autowaf.display_msg(conf, 'Utilities', bool(conf.env.BUILD_UTILS))
    autowaf.display_msg(conf, 'Unit tests', bool(conf.env.BUILD_TESTS))
    autowaf.display_msg(conf, 'Debug dumping', dump)
    print('')

def build(bld):
    # C/C++ Headers
    includedir = '${INCLUDEDIR}/sord-%s/sord' % SORD_MAJOR_VERSION
    bld.install_files(includedir, bld.path.ant_glob('sord/*.h'))
    bld.install_files(includedir, bld.path.ant_glob('sord/*.hpp'))

    # Pkgconfig file
    autowaf.build_pc(bld, 'SORD', SORD_VERSION, SORD_MAJOR_VERSION, 'SERD',
                     {'SORD_MAJOR_VERSION' : SORD_MAJOR_VERSION})

    source = 'src/sord.c src/syntax.c'

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
                  source          = source,
                  includes        = ['.', './src'],
                  export_includes = ['.'],
                  name            = 'libsord',
                  target          = 'sord-%s' % SORD_MAJOR_VERSION,
                  vnum            = SORD_VERSION,
                  install_path    = '${LIBDIR}',
                  libs            = libs,
                  defines         = defines + ['SORD_SHARED', 'SORD_INTERNAL'],
                  cflags          = libflags)
        autowaf.use_lib(bld, obj, 'SERD')
    
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
                  defines         = ['SORD_INTERNAL'])
        autowaf.use_lib(bld, obj, 'SERD')

    if bld.env.BUILD_TESTS:
        test_libs   = libs
        test_cflags = ['']
        if bld.is_defined('HAVE_GCOV'):
            test_libs   += ['gcov']
            test_cflags += ['-fprofile-arcs', '-ftest-coverage']

        # Profiled static library for test coverage
        obj = bld(features     = 'c cstlib',
                  source       = source,
                  includes     = ['.', './src'],
                  name         = 'libsord_profiled',
                  target       = 'sord_profiled',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags,
                  lib          = test_libs)
        autowaf.use_lib(bld, obj, 'SERD')

        # Unit test program
        obj = bld(features     = 'c cprogram',
                  source       = 'src/sord_test.c',
                  includes     = ['.', './src'],
                  use          = 'libsord_profiled',
                  lib          = test_libs,
                  target       = 'sord_test',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags)
        autowaf.use_lib(bld, obj, 'SERD')

        # Static profiled sordi for tests
        obj = bld(features     = 'c cprogram',
                  source       = 'src/sordi.c',
                  includes     = ['.', './src'],
                  use          = 'libsord_profiled',
                  lib          = test_libs,
                  target       = 'sordi_static',
                  install_path = '',
                  defines      = defines,
                  cflags       = test_cflags)
        autowaf.use_lib(bld, obj, 'SERD')

        # C++ build test
        if bld.env.COMPILER_CXX:
            obj = bld(features     = 'cxx cxxprogram',
                      source       = 'src/sordmm_test.cpp',
                      includes     = ['.', './src'],
                      use          = 'libsord_profiled',
                      lib          = test_libs,
                      target       = 'sordmm_test',
                      install_path = '',
                      defines      = defines)
            autowaf.use_lib(bld, obj, 'SERD')

    # Utilities
    if bld.env.BUILD_UTILS:
        for i in ['sordi', 'sord_validate']:
            obj = bld(features     = 'c cprogram',
                      source       = 'src/%s.c' % i,
                      includes     = ['.', './src'],
                      use          = 'libsord',
                      lib          = libs, 
                      target       = i,
                      install_path = '${BINDIR}',
                      defines      = defines)
            if not bld.env.BUILD_SHARED or bld.env.STATIC_PROGS:
                obj.use = 'libsord_static'
            if bld.env.STATIC_PROGS:
                obj.env.SHLIB_MARKER = obj.env.STLIB_MARKER
                obj.linkflags        = ['-static', '-Wl,--start-group']
            autowaf.use_lib(bld, obj, 'SERD PCRE')

    # Documentation
    autowaf.build_dox(bld, 'SORD', SORD_VERSION, top, out)

    # Man pages
    bld.install_files('${MANDIR}/man1', bld.path.ant_glob('doc/*.1'))

    bld.add_post_fun(autowaf.run_ldconfig)
    if bld.env.DOCS:
        bld.add_post_fun(fix_docs)

def lint(ctx):
    subprocess.call('cpplint.py --filter=+whitespace/comments,-whitespace/tab,-whitespace/braces,-whitespace/labels,-build/header_guard,-readability/casting,-readability/todo,-build/include src/*.* sord/* src/zix/*.*', shell=True)

def fix_docs(ctx):
    if ctx.cmd == 'build':
        autowaf.make_simple_dox(APPNAME)

def upload_docs(ctx):
    os.system('rsync -ravz --delete -e ssh build/doc/html/ drobilla@drobilla.net:~/drobilla.net/docs/sord/')

def test(ctx):
    blddir = autowaf.build_dir(APPNAME, 'tests')
    try:
        os.makedirs(blddir)
    except:
        pass

    for i in glob.glob(blddir + '/*.*'):
        os.remove(i)

    srcdir   = ctx.path.abspath()
    orig_dir = os.path.abspath(os.curdir)

    os.chdir(srcdir)

    good_tests = glob.glob('tests/test-*.ttl')
    good_tests.sort()

    os.chdir(orig_dir)

    autowaf.pre_test(ctx, APPNAME)

    os.environ['PATH'] = '.' + os.pathsep + os.getenv('PATH')

    nul = os.devnull

    autowaf.run_tests(ctx, APPNAME, [
            'sordi_static file://%s/tests/manifest.ttl > %s' % (srcdir, nul),
            'sordi_static %s/tests/UTF-8.ttl > %s' % (srcdir, nul),
            'sordi_static -v > %s' % nul,
            'sordi_static -h > %s' % nul,
            'sordi_static -s "<foo> a <#Thingie> ." file:///test > %s' % nul,
            'sordi_static %s > %s' % (nul, nul)],
                      0, name='sordi-cmd-good')

    autowaf.run_tests(ctx, APPNAME, [
            'sordi_static > %s' % nul,
            'sordi_static ftp://example.org/unsupported.ttl > %s' % nul,
            'sordi_static -i > %s' % nul,
            'sordi_static -o > %s' % nul,
            'sordi_static -z > %s' % nul,
            'sordi_static -p > %s' % nul,
            'sordi_static -c > %s' % nul,
            'sordi_static -i illegal > %s' % nul,
            'sordi_static -o illegal > %s' % nul,
            'sordi_static -i turtle > %s' % nul,
            'sordi_static /no/such/file > %s' % nul],
                      1, name='sordi-cmd-bad')

    autowaf.run_tests(ctx, APPNAME, ['sord_test'])

    commands = []
    for test in good_tests:
        base_uri = 'http://www.w3.org/2001/sw/DataAccess/df1/' + test.replace('\\', '/')
        commands += [ 'sordi_static "%s" "%s" > %s.out' % (
                os.path.join(srcdir, test), base_uri, test) ]

    autowaf.run_tests(ctx, APPNAME, commands, 0, name='good')

    Logs.pprint('BOLD', '\nVerifying turtle => ntriples')
    for test in good_tests:
        out_filename = test + '.out'
        cmp_filename = srcdir + '/' + test.replace('.ttl', '.out')
        if not os.access(out_filename, os.F_OK):
            Logs.pprint('RED', 'FAIL: %s output is missing' % test)
        else:
            out_lines = sorted(open(out_filename).readlines())
            cmp_lines = sorted(open(cmp_filename).readlines())
            if out_lines != cmp_lines:
                Logs.pprint('RED', 'FAIL: %s is incorrect' % out_filename)
            else:
                Logs.pprint('GREEN', 'Pass: %s' % test)

    autowaf.post_test(ctx, APPNAME)
