#!/usr/bin/env python
import glob
import os
import shutil
import subprocess
import waflib.Logs as Logs
import waflib.Options as Options
import waflib.extras.autowaf as autowaf

# Library and package version (UNIX style major, minor, micro)
# major increment <=> incompatible changes
# minor increment <=> compatible changes (additions)
# micro increment <=> no interface changes
SERD_VERSION       = '0.18.2'
SERD_MAJOR_VERSION = '0'

# Mandatory waf variables
APPNAME = 'serd'        # Package name for waf dist
VERSION = SERD_VERSION  # Package version for waf dist
top     = '.'           # Source directory
out     = 'build'       # Build directory

def options(opt):
    opt.load('compiler_c')
    autowaf.set_options(opt)
    opt.add_option('--no-utils', action='store_true', dest='no_utils',
                   help='Do not build command line utilities')
    opt.add_option('--test', action='store_true', dest='build_tests',
                   help='Build unit tests')
    opt.add_option('--stack-check', action='store_true', dest='stack_check',
                   help='Include runtime stack sanity checks')
    opt.add_option('--static', action='store_true', dest='static',
                   help='Build static library')
    opt.add_option('--no-shared', action='store_true', dest='no_shared',
                   help='Do not build shared library')
    opt.add_option('--static-progs', action='store_true', dest='static_progs',
                   help='Build programs as static binaries')
    opt.add_option('--largefile', action='store_true', dest='largefile',
                   help='Build with large file support on 32-bit systems')

def configure(conf):
    conf.load('compiler_c')
    autowaf.configure(conf)
    autowaf.set_c99_mode(conf)
    autowaf.display_header('Serd Configuration')

    conf.env.BUILD_TESTS  = Options.options.build_tests
    conf.env.BUILD_UTILS  = not Options.options.no_utils
    conf.env.BUILD_SHARED = not Options.options.no_shared
    conf.env.STATIC_PROGS = Options.options.static_progs
    conf.env.BUILD_STATIC = (Options.options.static or
                             Options.options.static_progs)

    if not conf.env.BUILD_SHARED and not conf.env.BUILD_STATIC:
        conf.fatal('Neither a shared nor a static build requested')

    if Options.options.stack_check:
        autowaf.define(conf, 'SERD_STACK_CHECK', SERD_VERSION)

    if Options.options.largefile:
        conf.env.append_unique('DEFINES', ['_FILE_OFFSET_BITS=64'])

    if conf.env.BUILD_TESTS:
        conf.check(lib         = 'gcov',
                   define_name = 'HAVE_GCOV',
                   mandatory   = False)

    conf.check(function_name = 'fmax',
               header_name   = 'math.h',
               define_name   = 'HAVE_FMAX',
               lib           = ['m'],
               mandatory     = False)

    conf.check(function_name = 'posix_memalign',
               header_name   = 'stdlib.h',
               define_name   = 'HAVE_POSIX_MEMALIGN',
               defines       = ['_POSIX_C_SOURCE=201112L'],
               mandatory     = False)

    conf.check(function_name = 'posix_fadvise',
               header_name   = 'fcntl.h',
               define_name   = 'HAVE_POSIX_FADVISE',
               defines       = ['_POSIX_C_SOURCE=201112L'],
               mandatory     = False)

    conf.check(function_name = 'fileno',
               header_name   = 'stdio.h',
               define_name   = 'HAVE_FILENO',
               defines       = ['_POSIX_C_SOURCE=201112L'],
               mandatory     = False)

    autowaf.define(conf, 'SERD_VERSION', SERD_VERSION)
    autowaf.set_lib_env(conf, 'serd', SERD_VERSION)
    conf.write_config_header('serd_config.h', remove=False)

    autowaf.display_msg(conf, 'Utilities', str(conf.env.BUILD_UTILS))
    autowaf.display_msg(conf, 'Unit tests', str(conf.env.BUILD_TESTS))
    print('')

lib_source = [
    'src/env.c',
    'src/node.c',
    'src/reader.c',
    'src/string.c',
    'src/uri.c',
    'src/writer.c',
]

def build(bld):
    # C Headers
    includedir = '${INCLUDEDIR}/serd-%s/serd' % SERD_MAJOR_VERSION
    bld.install_files(includedir, bld.path.ant_glob('serd/*.h'))

    # Pkgconfig file
    autowaf.build_pc(bld, 'SERD', SERD_VERSION, SERD_MAJOR_VERSION, [],
                     {'SERD_MAJOR_VERSION' : SERD_MAJOR_VERSION})

    libflags = ['-fvisibility=hidden']
    libs     = ['m']
    defines  = []
    if bld.env.MSVC_COMPILER:
        libflags = []
        libs     = []
        defines  = ['snprintf=_snprintf']

    # Shared Library
    if bld.env.BUILD_SHARED:
        bld(features        = 'c cshlib',
            export_includes = ['.'],
            source          = lib_source,
            includes        = ['.', './src'],
            lib             = libs,
            name            = 'libserd',
            target          = 'serd-%s' % SERD_MAJOR_VERSION,
            vnum            = SERD_VERSION,
            install_path    = '${LIBDIR}',
            defines         = defines + ['SERD_SHARED', 'SERD_INTERNAL'],
            cflags          = libflags)

    # Static library
    if bld.env.BUILD_STATIC:
        bld(features        = 'c cstlib',
            export_includes = ['.'],
            source          = lib_source,
            includes        = ['.', './src'],
            lib             = libs,
            name            = 'libserd_static',
            target          = 'serd-%s' % SERD_MAJOR_VERSION,
            vnum            = SERD_VERSION,
            install_path    = '${LIBDIR}',
            defines         = defines + ['SERD_INTERNAL'])

    if bld.env.BUILD_TESTS:
        test_libs   = libs
        test_cflags = ['']
        if bld.is_defined('HAVE_GCOV'):
            test_libs   += ['gcov']
            test_cflags += ['-fprofile-arcs', '-ftest-coverage']

        # Profiled static library for test coverage
        bld(features     = 'c cstlib',
            source       = lib_source,
            includes     = ['.', './src'],
            lib          = test_libs,
            name         = 'libserd_profiled',
            target       = 'serd_profiled',
            install_path = '',
            defines      = defines + ['SERD_INTERNAL'],
            cflags       = test_cflags)

        # Static profiled serdi for tests
        bld(features     = 'c cprogram',
            source       = 'src/serdi.c',
            includes     = ['.', './src'],
            use          = 'libserd_profiled',
            lib          = test_libs,
            target       = 'serdi_static',
            install_path = '',
            defines      = defines,
            cflags       = test_cflags)

        # Unit test program
        bld(features     = 'c cprogram',
            source       = 'tests/serd_test.c',
            includes     = ['.', './src'],
            use          = 'libserd_profiled',
            lib          = test_libs,
            target       = 'serd_test',
            install_path = '',
            defines      = defines,
            cflags       = test_cflags)

    # Utilities
    if bld.env.BUILD_UTILS:
        obj = bld(features     = 'c cprogram',
                  source       = 'src/serdi.c',
                  target       = 'serdi',
                  includes     = ['.', './src'],
                  use          = 'libserd',
                  lib          = libs,
                  install_path = '${BINDIR}')
        if not bld.env.BUILD_SHARED or bld.env.STATIC_PROGS:
            obj.use = 'libserd_static'
        if bld.env.STATIC_PROGS:
            obj.env.SHLIB_MARKER = obj.env.STLIB_MARKER
            obj.linkflags        = ['-static']

    # Documentation
    autowaf.build_dox(bld, 'SERD', SERD_VERSION, top, out)

    # Man page
    bld.install_files('${MANDIR}/man1', 'doc/serdi.1')

    bld.add_post_fun(autowaf.run_ldconfig)
    if bld.env.DOCS:
        bld.add_post_fun(fix_docs)

def lint(ctx):
    subprocess.call('cpplint.py --filter=+whitespace/comments,-whitespace/tab,-whitespace/braces,-whitespace/labels,-build/header_guard,-readability/casting,-readability/todo,-build/include src/* serd/*', shell=True)

def amalgamate(ctx):
    shutil.copy('serd/serd.h', 'build/serd.h')
    amalgamation = open('build/serd.c', 'w')

    serd_internal_h = open('src/serd_internal.h')
    for l in serd_internal_h:
        if l == '#include "serd/serd.h"\n':
            amalgamation.write('#include "serd.h"\n')
        else:
            amalgamation.write(l)
    serd_internal_h.close()

    for f in lib_source:
        fd = open(f)
        amalgamation.write('\n/**\n   @file %s\n*/' % f)
        header = True
        for l in fd:
            if header:
                if l == '*/\n':
                    header = False
            else:
                if l != '#include "serd_internal.h"\n':
                    amalgamation.write(l)
        fd.close()
    amalgamation.close()

    for i in ['c', 'h']:
        Logs.info('Wrote build/serd.%s' % i)

def fix_docs(ctx):
    if ctx.cmd == 'build':
        autowaf.make_simple_dox(APPNAME)

def upload_docs(ctx):
    os.system('rsync -ravz --delete -e ssh build/doc/html/ drobilla@drobilla.net:~/drobilla.net/docs/serd/')

def file_equals(patha, pathb, subst_from='', subst_to=''):
    fa = open(patha, 'rU')
    fb = open(pathb, 'rU')
    for line in fa:
        if line.replace(subst_from, subst_to) != fb.readline().replace(subst_from, subst_to):
            return False
    fa.close()
    fb.close()
    return True

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

    bad_tests = glob.glob('tests/bad-*.ttl')
    bad_tests.sort()

    os.chdir(orig_dir)

    autowaf.pre_test(ctx, APPNAME)

    os.environ['PATH'] = '.' + os.pathsep + os.getenv('PATH')

    autowaf.run_tests(ctx, APPNAME, ['serd_test'], dirs=['.'])

    autowaf.run_tests(ctx, APPNAME, [
            'serdi_static -o turtle %s/tests/base.ttl "base.ttl" > tests/base.ttl.out' % srcdir],
                      0, name='base')

    if not file_equals('%s/tests/base.ttl' % srcdir, 'tests/base.ttl.out'):
        Logs.pprint('RED', 'FAIL: build/tests/base.ttl.out is incorrect')

    nul = os.devnull
    autowaf.run_tests(ctx, APPNAME, [
            'serdi_static file://%s/tests/manifest.ttl > %s' % (srcdir, nul),
            'serdi_static %s/tests/UTF-8.ttl > %s' % (srcdir, nul),
            'serdi_static -v > %s' % nul,
            'serdi_static -h > %s' % nul,
            'serdi_static -s "<foo> a <#Thingie> ." > %s' % nul,
            'serdi_static %s > %s' % (nul, nul)],
                      0, name='serdi-cmd-good')

    autowaf.run_tests(ctx, APPNAME, [
            'serdi_static -q file://%s/tests/bad-id-clash.ttl > %s' % (srcdir, nul),
            'serdi_static > %s' % nul,
            'serdi_static ftp://example.org/unsupported.ttl > %s' % nul,
            'serdi_static -i > %s' % nul,
            'serdi_static -o > %s' % nul,
            'serdi_static -z > %s' % nul,
            'serdi_static -p > %s' % nul,
            'serdi_static -c > %s' % nul,
            'serdi_static -r > %s' % nul,
            'serdi_static -i illegal > %s' % nul,
            'serdi_static -o illegal > %s' % nul,
            'serdi_static -i turtle > %s' % nul,
            'serdi_static /no/such/file > %s' % nul],
                      1, name='serdi-cmd-bad')

    commands = []
    for test in good_tests:
        base_uri = 'http://www.w3.org/2001/sw/DataAccess/df1/' + test.replace('\\', '/')
        commands += [ 'serdi_static -f "%s" "%s" > %s.out' % (
                os.path.join(srcdir, test), base_uri, test) ]

    autowaf.run_tests(ctx, APPNAME, commands, 0, name='good')

    Logs.pprint('BOLD', '\nVerifying turtle => ntriples')
    for test in good_tests:
        out_filename = test + '.out'
        if not os.access(out_filename, os.F_OK):
            Logs.pprint('RED', 'FAIL: %s output is missing' % test)
        elif not file_equals(srcdir + '/' + test.replace('.ttl', '.out'),
                             test + '.out'):
            Logs.pprint('RED', 'FAIL: %s is incorrect' % out_filename)
        else:
            Logs.pprint('GREEN', 'Pass: %s' % test)

    commands = []
    for test in bad_tests:
        commands += [ 'serdi_static "%s" "http://www.w3.org/2001/sw/DataAccess/df1/%s" > %s.out' % (os.path.join(srcdir, test), test.replace('\\', '/'), test) ]

    autowaf.run_tests(ctx, APPNAME, commands, 1, name='bad')

    thru_tests = good_tests
    thru_tests.remove(os.path.join('tests', 'test-id.ttl')) # IDs are mapped so files won't be identical

    commands = []
    num = 0
    for test in thru_tests:
        num += 1
        flags = ''
        if (num % 2 == 0):
            flags += '-b'
        if (num % 5 == 0):
            flags += ' -f'
        if (num % 3 == 0):
            flags += ' -r http://www.w3.org/'
        if (num % 7 == 0):
            flags += ' -e'
        base_uri = 'http://www.w3.org/2001/sw/DataAccess/df1/' + test.replace('\\', '/')
        out_filename = test + '.thru'
        commands += [
            '%s %s -i ntriples -o turtle -p foo "%s" "%s" | %s -i turtle -o ntriples -c foo - "%s" > %s.thru' % (
                'serdi_static', flags.ljust(5),
                os.path.join(srcdir, test), base_uri,
                'serdi_static', base_uri, test) ]

    autowaf.run_tests(ctx, APPNAME, commands, 0, name='turtle-round-trip')
    Logs.pprint('BOLD', '\nVerifying ntriples => turtle => ntriples')
    for test in thru_tests:
        out_filename = test + '.thru'
        if not os.access(out_filename, os.F_OK):
            Logs.pprint('RED', 'FAIL: %s output is missing' % test)
        elif not file_equals(srcdir + '/' + test.replace('.ttl', '.out'),
                             test + '.thru',
                             '_:docid', '_:genid'):
            Logs.pprint('RED', 'FAIL: %s is incorrect' % out_filename)
        else:
            Logs.pprint('GREEN', 'Pass: %s' % test)

    autowaf.post_test(ctx, APPNAME)
