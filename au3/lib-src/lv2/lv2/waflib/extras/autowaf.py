import glob
import os
import subprocess
import sys

from waflib import Build, Context, Logs, Options, Utils
from waflib.TaskGen import feature, before, after

global g_is_child
g_is_child = False

# Only run autowaf hooks once (even if sub projects call several times)
global g_step
g_step = 0

global line_just
line_just = 40

# Compute dependencies globally
# import preproc
# preproc.go_absolute = True

# Test context that inherits build context to make configuration available
class TestContext(Build.BuildContext):
    "Run tests"
    cmd = 'test'
    fun = 'test'

@feature('c', 'cxx')
@after('apply_incpaths')
def include_config_h(self):
    self.env.append_value('INCPATHS', self.bld.bldnode.abspath())

def set_options(opt, debug_by_default=False, test=False):
    "Add standard autowaf options if they havn't been added yet"
    global g_step
    if g_step > 0:
        return

    opts = opt.get_option_group('Configuration options')

    # Standard directory options
    opts.add_option('--bindir', type='string',
                    help="executable programs [default: PREFIX/bin]")
    opts.add_option('--configdir', type='string',
                    help="configuration data [default: PREFIX/etc]")
    opts.add_option('--datadir', type='string',
                    help="shared data [default: PREFIX/share]")
    opts.add_option('--includedir', type='string',
                    help="header files [default: PREFIX/include]")
    opts.add_option('--libdir', type='string',
                    help="libraries [default: PREFIX/lib]")
    opts.add_option('--mandir', type='string',
                    help="manual pages [default: DATADIR/man]")
    opts.add_option('--docdir', type='string',
                    help="HTML documentation [default: DATADIR/doc]")

    # Build options
    if debug_by_default:
        opts.add_option('--optimize', action='store_false', default=True,
                        dest='debug', help="build optimized binaries")
    else:
        opts.add_option('-d', '--debug', action='store_true', default=False,
                        dest='debug', help="build debuggable binaries")
        opts.add_option('--pardebug', action='store_true', default=False,
                        dest='pardebug',
                        help="build debug libraries with D suffix")

    opts.add_option('-s', '--strict', action='store_true', default=False,
                    dest='strict',
                    help="use strict compiler flags and show all warnings")
    opts.add_option('--ultra-strict', action='store_true', default=False,
                    dest='ultra_strict',
                    help="use extremely strict compiler flags (likely noisy)")
    opts.add_option('--docs', action='store_true', default=False, dest='docs',
                    help="build documentation (requires doxygen)")

    # Test options
    if test:
        test_opts = opt.add_option_group('Test options', '')
        opts.add_option('-T', '--test', action='store_true', dest='build_tests',
                        help='build unit tests')
        opts.add_option('--no-coverage', action='store_true',
                        dest='no_coverage',
                        help='do not instrument code for test coverage')
        test_opts.add_option('--wrapper', type='string',
                             dest='test_wrapper',
                             help='command prefix for tests (e.g. valgrind)')
        test_opts.add_option('--verbose-tests', action='store_true',
                             default=False, dest='verbose_tests',
                             help='always show test output')

    g_step = 1

def add_flags(opt, flags):
    for name, desc in flags.items():
        opt.add_option('--' + name, action='store_true',
                       dest=name.replace('-', '_'), help=desc)

def get_check_func(conf, lang):
    if lang == 'c':
        return conf.check_cc
    elif lang == 'cxx':
        return conf.check_cxx
    else:
        Logs.error("Unknown header language `%s'" % lang)

def check_header(conf, lang, name, define='', mandatory=True):
    "Check for a header"
    check_func = get_check_func(conf, lang)
    if define != '':
        check_func(header_name=name,
                   define_name=define,
                   mandatory=mandatory)
    else:
        check_func(header_name=name, mandatory=mandatory)

def check_function(conf, lang, name, **args):
    "Check for a function"
    header_names = Utils.to_list(args['header_name'])
    includes = ''.join(['#include <%s>\n' % x for x in header_names])
    fragment = '''
%s
int main() { return !(void(*)())(%s); }
''' % (includes, name)

    check_func  = get_check_func(conf, lang)
    args['msg'] = 'Checking for %s' % name
    check_func(fragment=fragment, **args)

def nameify(name):
    return (name.replace('/', '_').replace('++', 'PP')
            .replace('-', '_').replace('.', '_'))

def define(conf, var_name, value):
    conf.define(var_name, value)
    conf.env[var_name] = value

def check_pkg(conf, name, **args):
    "Check for a package iff it hasn't been checked for yet"
    if args['uselib_store'].lower() in conf.env['AUTOWAF_LOCAL_LIBS']:
        return

    class CheckType:
        OPTIONAL = 1
        MANDATORY = 2

    var_name = 'CHECKED_' + nameify(args['uselib_store'])
    check = var_name not in conf.env
    mandatory = 'mandatory' not in args or args['mandatory']
    if not check and 'atleast_version' in args:
        # Re-check if version is newer than previous check
        checked_version = conf.env['VERSION_' + name]
        if checked_version and checked_version < args['atleast_version']:
            check = True
    if not check and mandatory and conf.env[var_name] == CheckType.OPTIONAL:
        # Re-check if previous check was optional but this one is mandatory
        check = True
    if check:
        found = None
        pkg_var_name = 'PKG_' + name.replace('-', '_')
        pkg_name = name
        if conf.env.PARDEBUG:
            args['mandatory'] = False  # Smash mandatory arg
            found = conf.check_cfg(package=pkg_name + 'D',
                                   args="--cflags --libs", **args)
            if found:
                pkg_name += 'D'
        if mandatory:
            args['mandatory'] = True  # Unsmash mandatory arg
        if not found:
            found = conf.check_cfg(package=pkg_name, args="--cflags --libs",
                                   **args)
        if found:
            conf.env[pkg_var_name] = pkg_name
        if 'atleast_version' in args:
            conf.env['VERSION_' + name] = args['atleast_version']
    if mandatory:
        conf.env[var_name] = CheckType.MANDATORY
    else:
        conf.env[var_name] = CheckType.OPTIONAL

    if not conf.env.MSVC_COMPILER and 'system' in args and args['system']:
        includes = conf.env['INCLUDES_' + nameify(args['uselib_store'])]
        for path in includes:
            if 'COMPILER_CC' in conf.env:
                conf.env.append_value('CFLAGS', ['-isystem', path])
            if 'COMPILER_CXX' in conf.env:
                conf.env.append_value('CXXFLAGS', ['-isystem', path])

        conf.env.append_value('CXXFLAGS', ['-isystem', '/usr/local/include'])

def normpath(path):
    if sys.platform == 'win32':
        return os.path.normpath(path).replace('\\', '/')
    else:
        return os.path.normpath(path)

def configure(conf):
    global g_step
    if g_step > 1:
        return

    def append_cxx_flags(flags):
        conf.env.append_value('CFLAGS', flags)
        conf.env.append_value('CXXFLAGS', flags)

    if Options.options.docs:
        conf.load('doxygen')

    try:
        conf.load('clang_compilation_database')
    except Exception:
        pass

    prefix = normpath(os.path.abspath(os.path.expanduser(conf.env['PREFIX'])))

    conf.env['DOCS'] = Options.options.docs and conf.env.DOXYGEN
    conf.env['DEBUG'] = Options.options.debug or Options.options.pardebug
    conf.env['PARDEBUG'] = Options.options.pardebug
    conf.env['PREFIX'] = prefix

    def config_dir(var, opt, default):
        if opt:
            conf.env[var] = normpath(opt)
        else:
            conf.env[var] = normpath(default)

    opts = Options.options

    config_dir('BINDIR',     opts.bindir,     os.path.join(prefix,  'bin'))
    config_dir('SYSCONFDIR', opts.configdir,  os.path.join(prefix,  'etc'))
    config_dir('DATADIR',    opts.datadir,    os.path.join(prefix,  'share'))
    config_dir('INCLUDEDIR', opts.includedir, os.path.join(prefix,  'include'))
    config_dir('LIBDIR',     opts.libdir,     os.path.join(prefix,  'lib'))

    datadir = conf.env['DATADIR']
    config_dir('MANDIR', opts.mandir, os.path.join(datadir, 'man'))
    config_dir('DOCDIR', opts.docdir, os.path.join(datadir, 'doc'))

    if Options.options.debug:
        if conf.env['MSVC_COMPILER']:
            conf.env['CFLAGS']    = ['/Od', '/Z7', '/MTd', '/FS']
            conf.env['CXXFLAGS']  = ['/Od', '/Z7', '/MTd', '/FS']
            conf.env['LINKFLAGS'] = ['/DEBUG', '/MANIFEST']
        else:
            conf.env['CFLAGS']   = ['-O0', '-g']
            conf.env['CXXFLAGS'] = ['-O0', '-g']
    else:
        if conf.env['MSVC_COMPILER']:
            conf.env['CFLAGS']   = ['/MD', '/FS', '/DNDEBUG']
            conf.env['CXXFLAGS'] = ['/MD', '/FS', '/DNDEBUG']
        else:
            append_cxx_flags(['-DNDEBUG'])

    if conf.env.MSVC_COMPILER:
        Options.options.no_coverage = True
        if Options.options.strict:
            conf.env.append_value('CFLAGS', ['/Wall'])
            conf.env.append_value('CXXFLAGS', ['/Wall'])
    else:
        if Options.options.ultra_strict:
            Options.options.strict = True
            conf.env.append_value('CFLAGS', ['-Wredundant-decls',
                                             '-Wstrict-prototypes',
                                             '-Wmissing-prototypes',
                                             '-Wcast-qual'])
            conf.env.append_value('CXXFLAGS', ['-Wcast-qual'])

        if Options.options.strict:
            conf.env.append_value('CFLAGS', ['-pedantic', '-Wshadow'])
            if conf.env.DEST_OS != "darwin":
                conf.env.append_value('LINKFLAGS', ['-Wl,--no-undefined'])
            conf.env.append_value('CXXFLAGS', ['-Wnon-virtual-dtor',
                                               '-Woverloaded-virtual'])
            append_cxx_flags(['-Wall',
                              '-Wcast-align',
                              '-Wextra',
                              '-Wmissing-declarations',
                              '-Wno-unused-parameter',
                              '-Wstrict-overflow',
                              '-Wundef',
                              '-Wwrite-strings',
                              '-fstrict-overflow'])

            # Add less universal flags after checking they work
            extra_flags = ['-Wlogical-op',
                           '-Wsuggest-attribute=noreturn',
                           '-Wunsafe-loop-optimizations']
            if conf.check_cc(cflags=['-Werror'] + extra_flags, mandatory=False,
                             msg="Checking for extra C warning flags"):
                conf.env.append_value('CFLAGS', extra_flags)
            if 'COMPILER_CXX' in conf.env:
                if conf.check_cxx(cxxflags=['-Werror'] + extra_flags,
                                  mandatory=False,
                                  msg="Checking for extra C++ warning flags"):
                    conf.env.append_value('CXXFLAGS', extra_flags)

    if not conf.env['MSVC_COMPILER']:
        append_cxx_flags(['-fshow-column'])

    conf.env.NO_COVERAGE = True
    conf.env.BUILD_TESTS = False
    try:
        conf.env.BUILD_TESTS = Options.options.build_tests
        conf.env.NO_COVERAGE = Options.options.no_coverage
        if not Options.options.no_coverage:
            # Set up unit test code coverage
            if conf.is_defined('CLANG'):
                for cov in [conf.env.CC[0].replace('clang', 'llvm-cov'),
                            'llvm-cov']:
                    if conf.find_program(cov, var='LLVM_COV', mandatory=False):
                        break
            else:
                conf.check_cc(lib='gcov', define_name='HAVE_GCOV',
                              mandatory=False)
    except Exception:
        pass  # Test options do not exist

    # Define version in configuration
    appname = getattr(Context.g_module, Context.APPNAME, 'noname')
    version = getattr(Context.g_module, Context.VERSION, '0.0.0')
    defname = appname.upper().replace('-', '_').replace('.', '_')
    define(conf, defname + '_VERSION', version)

    conf.env.prepend_value('CFLAGS', '-I' + os.path.abspath('.'))
    conf.env.prepend_value('CXXFLAGS', '-I' + os.path.abspath('.'))
    g_step = 2

def display_summary(conf, msgs=None):
    global g_is_child
    if not g_is_child:
        display_msg(conf, "Install prefix", conf.env['PREFIX'])
        if 'COMPILER_CC' in conf.env:
            display_msg(conf, "C Flags", ' '.join(conf.env['CFLAGS']))
        if 'COMPILER_CXX' in conf.env:
            display_msg(conf, "C++ Flags", ' '.join(conf.env['CXXFLAGS']))
        display_msg(conf, "Debuggable", bool(conf.env['DEBUG']))
        display_msg(conf, "Build documentation", bool(conf.env['DOCS']))

    if msgs is not None:
        display_msgs(conf, msgs)

def set_c_lang(conf, lang):
    "Set a specific C language standard, like 'c99' or 'c11'"
    if conf.env.MSVC_COMPILER:
        # MSVC has no hope or desire to compile C99, just compile as C++
        conf.env.append_unique('CFLAGS', ['-TP'])
    else:
        flag = '-std=%s' % lang
        conf.check(cflags=['-Werror', flag],
                   msg="Checking for flag '%s'" % flag)
        conf.env.append_unique('CFLAGS', [flag])

def set_cxx_lang(conf, lang):
    "Set a specific C++ language standard, like 'c++11', 'c++14', or 'c++17'"
    if conf.env.MSVC_COMPILER:
        if lang != 'c++14':
            lang = 'c++latest'
        conf.env.append_unique('CXXFLAGS', ['/std:%s' % lang])
    else:
        flag = '-std=%s' % lang
        conf.check(cxxflags=['-Werror', flag],
                   msg="Checking for flag '%s'" % flag)
        conf.env.append_unique('CXXFLAGS', [flag])

def set_modern_c_flags(conf):
    "Use the most modern C language available"
    if 'COMPILER_CC' in conf.env:
        if conf.env.MSVC_COMPILER:
            # MSVC has no hope or desire to compile C99, just compile as C++
            conf.env.append_unique('CFLAGS', ['-TP'])
        else:
            for flag in ['-std=c11', '-std=c99']:
                if conf.check(cflags=['-Werror', flag], mandatory=False,
                              msg="Checking for flag '%s'" % flag):
                    conf.env.append_unique('CFLAGS', [flag])
                    break

def set_modern_cxx_flags(conf, mandatory=False):
    "Use the most modern C++ language available"
    if 'COMPILER_CXX' in conf.env:
        if conf.env.MSVC_COMPILER:
            conf.env.append_unique('CXXFLAGS', ['/std:c++latest'])
        else:
            for lang in ['c++14', 'c++1y', 'c++11', 'c++0x']:
                flag = '-std=%s' % lang
                if conf.check(cxxflags=['-Werror', flag], mandatory=False,
                              msg="Checking for flag '%s'" % flag):
                    conf.env.append_unique('CXXFLAGS', [flag])
                    break

def set_local_lib(conf, name, has_objects):
    var_name = 'HAVE_' + nameify(name.upper())
    define(conf, var_name, 1)
    if has_objects:
        if type(conf.env['AUTOWAF_LOCAL_LIBS']) != dict:
            conf.env['AUTOWAF_LOCAL_LIBS'] = {}
        conf.env['AUTOWAF_LOCAL_LIBS'][name.lower()] = True
    else:
        if type(conf.env['AUTOWAF_LOCAL_HEADERS']) != dict:
            conf.env['AUTOWAF_LOCAL_HEADERS'] = {}
        conf.env['AUTOWAF_LOCAL_HEADERS'][name.lower()] = True

def append_property(obj, key, val):
    if hasattr(obj, key):
        setattr(obj, key, getattr(obj, key) + val)
    else:
        setattr(obj, key, val)

def use_lib(bld, obj, libs):
    abssrcdir = os.path.abspath('.')
    libs_list = libs.split()
    for l in libs_list:
        in_headers = l.lower() in bld.env['AUTOWAF_LOCAL_HEADERS']
        in_libs    = l.lower() in bld.env['AUTOWAF_LOCAL_LIBS']
        if in_libs:
            append_property(obj, 'use', ' lib%s ' % l.lower())
            append_property(obj, 'framework', bld.env['FRAMEWORK_' + l])
        if in_headers or in_libs:
            if bld.env.MSVC_COMPILER:
                inc_flag = '/I' + os.path.join(abssrcdir, l.lower())
            else:
                inc_flag = '-iquote ' + os.path.join(abssrcdir, l.lower())
            for f in ['CFLAGS', 'CXXFLAGS']:
                if inc_flag not in bld.env[f]:
                    bld.env.prepend_value(f, inc_flag)
        else:
            append_property(obj, 'uselib', ' ' + l)

@feature('c', 'cxx')
@before('apply_link')
def version_lib(self):
    if self.env.DEST_OS == 'win32':
        self.vnum = None  # Prevent waf from automatically appending -0
    if self.env['PARDEBUG']:
        applicable = ['cshlib', 'cxxshlib', 'cstlib', 'cxxstlib']
        if [x for x in applicable if x in self.features]:
            self.target = self.target + 'D'

def set_lib_env(conf, name, version):
    "Set up environment for local library as if found via pkg-config."
    NAME         = name.upper()
    major_ver    = version.split('.')[0]
    pkg_var_name = 'PKG_' + name.replace('-', '_') + '_' + major_ver
    lib_name     = '%s-%s' % (name, major_ver)
    if conf.env.PARDEBUG:
        lib_name += 'D'
    conf.env[pkg_var_name]       = lib_name
    conf.env['INCLUDES_' + NAME] = ['${INCLUDEDIR}/%s-%s' % (name, major_ver)]
    conf.env['LIBPATH_' + NAME]  = [conf.env.LIBDIR]
    conf.env['LIB_' + NAME]      = [lib_name]

    conf.define(NAME + '_VERSION', version)

def set_line_just(conf, width):
    global line_just
    line_just = max(line_just, width)
    conf.line_just = line_just

def display_header(title):
    global g_is_child
    if g_is_child:
        Logs.pprint('BOLD', title)

def display_msg(conf, msg, status=None, color=None):
    color = 'CYAN'
    if type(status) == bool and status:
        color  = 'GREEN'
        status = 'yes'
    elif type(status) == bool and not status or status == "False":
        color  = 'YELLOW'
        status = 'no'
    Logs.pprint('BOLD', '%s' % msg.ljust(conf.line_just), sep='')
    Logs.pprint('BOLD', ":", sep='')
    Logs.pprint(color, status)

def display_msgs(conf, msgs):
    for k, v in msgs.items():
        display_msg(conf, k, v)

def link_flags(env, lib):
    return ' '.join(map(lambda x: env['LIB_ST'] % x,
                        env['LIB_' + lib]))

def compile_flags(env, lib):
    return ' '.join(map(lambda x: env['CPPPATH_ST'] % x,
                        env['INCLUDES_' + lib]))

def set_recursive():
    global g_is_child
    g_is_child = True

def is_child():
    global g_is_child
    return g_is_child

def build_pc(bld, name, version, version_suffix, libs, subst_dict={}):
    """Build a pkg-config file for a library.

    name           -- uppercase variable name     (e.g. 'SOMENAME')
    version        -- version string              (e.g. '1.2.3')
    version_suffix -- name version suffix         (e.g. '2')
    libs           -- string/list of dependencies (e.g. 'LIBFOO GLIB')
    """

    pkg_prefix       = bld.env['PREFIX']
    if pkg_prefix[-1] == '/':
        pkg_prefix = pkg_prefix[:-1]

    target = name.lower()
    if version_suffix != '':
        target += '-' + version_suffix

    if bld.env['PARDEBUG']:
        target += 'D'

    target += '.pc'

    libdir = bld.env['LIBDIR']
    if libdir.startswith(pkg_prefix):
        libdir = libdir.replace(pkg_prefix, '${exec_prefix}')

    includedir = bld.env['INCLUDEDIR']
    if includedir.startswith(pkg_prefix):
        includedir = includedir.replace(pkg_prefix, '${prefix}')

    obj = bld(features='subst',
              source='%s.pc.in' % name.lower(),
              target=target,
              install_path=os.path.join(bld.env['LIBDIR'], 'pkgconfig'),
              exec_prefix='${prefix}',
              PREFIX=pkg_prefix,
              EXEC_PREFIX='${prefix}',
              LIBDIR=libdir,
              INCLUDEDIR=includedir)

    if type(libs) != list:
        libs = libs.split()

    subst_dict[name + '_VERSION'] = version
    subst_dict[name + '_MAJOR_VERSION'] = version[0:version.find('.')]
    for i in libs:
        subst_dict[i + '_LIBS']   = link_flags(bld.env, i)
        lib_cflags = compile_flags(bld.env, i)
        if lib_cflags == '':
            lib_cflags = ' '
        subst_dict[i + '_CFLAGS'] = lib_cflags

    obj.__dict__.update(subst_dict)

def build_dir(name, subdir):
    if is_child():
        return os.path.join('build', name, subdir)
    else:
        return os.path.join('build', subdir)


def make_simple_dox(name):
    "Clean up messy Doxygen documentation after it is built"
    name = name.lower()
    NAME = name.upper()
    try:
        top = os.getcwd()
        os.chdir(build_dir(name, 'doc/html'))
        page = 'group__%s.html' % name
        if not os.path.exists(page):
            return
        for i in [
            ['%s_API ' % NAME, ''],
            ['%s_DEPRECATED ' % NAME, ''],
            ['group__%s.html' % name, ''],
            ['&#160;', ''],
            [r'<script.*><\/script>', ''],
            [r'<hr\/><a name="details" id="details"><\/a><h2>.*<\/h2>', ''],
            [r'<link href=\"tabs.css\" rel=\"stylesheet\" type=\"text\/css\"\/>',
             ''],
            [r'<img class=\"footer\" src=\"doxygen.png\" alt=\"doxygen\"\/>',
             'Doxygen']]:
            os.system("sed -i 's/%s/%s/g' %s" % (i[0], i[1], page))
        os.rename('group__%s.html' % name, 'index.html')
        for i in (glob.glob('*.png') +
                  glob.glob('*.html') +
                  glob.glob('*.js') +
                  glob.glob('*.css')):
            if i != 'index.html' and i != 'style.css':
                os.remove(i)
        os.chdir(top)
        os.chdir(build_dir(name, 'doc/man/man3'))
        for i in glob.glob('*.3'):
            os.system("sed -i 's/%s_API //' %s" % (NAME, i))
        for i in glob.glob('_*'):
            os.remove(i)
        os.chdir(top)
    except Exception as e:
        Logs.error("Failed to fix up %s documentation: %s" % (name, e))


def build_dox(bld, name, version, srcdir, blddir, outdir='', versioned=True):
    """Build Doxygen API documentation"""
    if not bld.env['DOCS']:
        return

    # Doxygen paths in are relative to the doxygen file, not build directory
    if is_child():
        src_dir = os.path.join(srcdir, name.lower())
    else:
        src_dir = srcdir

    subst_tg = bld(features='subst',
                   source='doc/reference.doxygen.in',
                   target='doc/reference.doxygen',
                   install_path='',
                   name='doxyfile')

    subst_dict = {
        name + '_VERSION': version,
        name + '_SRCDIR': os.path.abspath(src_dir),
        name + '_DOC_DIR': ''
    }

    subst_tg.__dict__.update(subst_dict)

    subst_tg.post()

    docs = bld(features='doxygen',
               doxyfile='doc/reference.doxygen')

    docs.post()

    outname = name.lower()
    if versioned:
        outname += '-%d' % int(version[0:version.find('.')])
    bld.install_files(
        os.path.join('${DOCDIR}', outname, outdir, 'html'),
        bld.path.get_bld().ant_glob('doc/html/*'))
    for i in range(1, 8):
        bld.install_files('${MANDIR}/man%d' % i,
                          bld.path.get_bld().ant_glob('doc/man/man%d/*' % i,
                                                      excl='**/_*'))


def build_version_files(header_path, source_path, domain, major, minor, micro):
    """Generate version code header"""
    header_path = os.path.abspath(header_path)
    source_path = os.path.abspath(source_path)
    text  = "int " + domain + "_major_version = " + str(major) + ";\n"
    text += "int " + domain + "_minor_version = " + str(minor) + ";\n"
    text += "int " + domain + "_micro_version = " + str(micro) + ";\n"
    try:
        o = open(source_path, 'w')
        o.write(text)
        o.close()
    except IOError:
        Logs.error('Failed to open %s for writing\n' % source_path)
        sys.exit(-1)

    text  = "#ifndef __" + domain + "_version_h__\n"
    text += "#define __" + domain + "_version_h__\n"
    text += "extern const char* " + domain + "_revision;\n"
    text += "extern int " + domain + "_major_version;\n"
    text += "extern int " + domain + "_minor_version;\n"
    text += "extern int " + domain + "_micro_version;\n"
    text += "#endif /* __" + domain + "_version_h__ */\n"
    try:
        o = open(header_path, 'w')
        o.write(text)
        o.close()
    except IOError:
        Logs.warn('Failed to open %s for writing\n' % header_path)
        sys.exit(-1)

    return None

def build_i18n_pot(bld, srcdir, dir, name, sources, copyright_holder=None):
    Logs.info('Generating pot file from %s' % name)
    pot_file = '%s.pot' % name

    cmd = ['xgettext',
           '--keyword=_',
           '--keyword=N_',
           '--keyword=S_',
           '--from-code=UTF-8',
           '-o', pot_file]

    if copyright_holder:
        cmd += ['--copyright-holder="%s"' % copyright_holder]

    cmd += sources
    Logs.info('Updating ' + pot_file)
    subprocess.call(cmd, cwd=os.path.join(srcdir, dir))

def build_i18n_po(bld, srcdir, dir, name, sources, copyright_holder=None):
    pwd = os.getcwd()
    os.chdir(os.path.join(srcdir, dir))
    pot_file = '%s.pot' % name
    po_files = glob.glob('po/*.po')
    for po_file in po_files:
        cmd = ['msgmerge',
               '--update',
               po_file,
               pot_file]
        Logs.info('Updating ' + po_file)
        subprocess.call(cmd)
    os.chdir(pwd)

def build_i18n_mo(bld, srcdir, dir, name, sources, copyright_holder=None):
    pwd = os.getcwd()
    os.chdir(os.path.join(srcdir, dir))
    po_files = glob.glob('po/*.po')
    for po_file in po_files:
        mo_file = po_file.replace('.po', '.mo')
        cmd = ['msgfmt',
               '-c',
               '-f',
               '-o',
               mo_file,
               po_file]
        Logs.info('Generating ' + po_file)
        subprocess.call(cmd)
    os.chdir(pwd)

def build_i18n(bld, srcdir, dir, name, sources, copyright_holder=None):
    build_i18n_pot(bld, srcdir, dir, name, sources, copyright_holder)
    build_i18n_po(bld, srcdir, dir, name, sources, copyright_holder)
    build_i18n_mo(bld, srcdir, dir, name, sources, copyright_holder)

def cd_to_build_dir(ctx, appname):
    top_level = (len(ctx.stack_path) > 1)
    if top_level:
        os.chdir(os.path.join('build', appname))
    else:
        os.chdir('build')
    Logs.pprint('GREEN', ("Waf: Entering directory `%s'" %
                          os.path.abspath(os.getcwd())))

def cd_to_orig_dir(ctx, child):
    if child:
        os.chdir(os.path.join('..', '..'))
    else:
        os.chdir('..')

def pre_test(ctx, appname, dirs=['src']):
    if not hasattr(ctx, 'autowaf_tests_total'):
        ctx.autowaf_tests_total        = 0
        ctx.autowaf_tests_failed       = 0
        ctx.autowaf_local_tests_total  = 0
        ctx.autowaf_local_tests_failed = 0
        ctx.autowaf_tests              = {}

    ctx.autowaf_tests[appname] = {'total': 0, 'failed': 0}

    cd_to_build_dir(ctx, appname)
    if not ctx.env.NO_COVERAGE:
        diropts  = ''
        for i in dirs:
            diropts += ' -d ' + i
        clear_log = open('lcov-clear.log', 'w')
        try:
            try:
                # Clear coverage data
                subprocess.call(('lcov %s -z' % diropts).split(),
                                stdout=clear_log, stderr=clear_log)
            except Exception:
                Logs.warn('Failed to run lcov, no coverage report generated')
        finally:
            clear_log.close()

def post_test(ctx, appname, dirs=['src'], remove=['*boost*', 'c++*']):
    if not ctx.env.NO_COVERAGE:
        diropts  = ''
        for i in dirs:
            diropts += ' -d ' + i
        coverage_log           = open('lcov-coverage.log', 'w')
        coverage_lcov          = open('coverage.lcov', 'w')
        coverage_stripped_lcov = open('coverage-stripped.lcov', 'w')
        try:
            try:
                base = '.'
                if g_is_child:
                    base = '..'

                # Generate coverage data
                lcov_cmd = 'lcov -c %s -b %s' % (diropts, base)
                if ctx.env.LLVM_COV:
                    lcov_cmd += ' --gcov-tool %s' % ctx.env.LLVM_COV[0]
                subprocess.call(lcov_cmd.split(),
                                stdout=coverage_lcov, stderr=coverage_log)

                # Strip unwanted stuff
                subprocess.call(
                    ['lcov', '--remove', 'coverage.lcov'] + remove,
                    stdout=coverage_stripped_lcov, stderr=coverage_log)

                # Generate HTML coverage output
                if not os.path.isdir('coverage'):
                    os.makedirs('coverage')
                subprocess.call(
                    'genhtml -o coverage coverage-stripped.lcov'.split(),
                    stdout=coverage_log, stderr=coverage_log)

            except Exception:
                Logs.warn('Failed to run lcov, no coverage report generated')
        finally:
            coverage_stripped_lcov.close()
            coverage_lcov.close()
            coverage_log.close()

    if ctx.autowaf_tests[appname]['failed'] > 0:
        Logs.pprint('RED', '\nSummary:  %d / %d %s tests failed' % (
            ctx.autowaf_tests[appname]['failed'],
            ctx.autowaf_tests[appname]['total'],
            appname))
    else:
        Logs.pprint('GREEN', '\nSummary:  All %d %s tests passed' % (
            ctx.autowaf_tests[appname]['total'], appname))

    if not ctx.env.NO_COVERAGE:
        Logs.pprint('GREEN', 'Coverage: <file://%s>\n'
                    % os.path.abspath('coverage/index.html'))

    Logs.pprint('GREEN', ("Waf: Leaving directory `%s'" %
                          os.path.abspath(os.getcwd())))
    top_level = (len(ctx.stack_path) > 1)
    if top_level:
        cd_to_orig_dir(ctx, top_level)

def run_test(ctx,
             appname,
             test,
             desired_status=0,
             dirs=['src'],
             name='',
             header=False,
             quiet=False):
    """Run an individual test.

    `test` is either a shell command string, or a list of [name, return status]
    for displaying tests implemented in the calling Python code.
    """

    ctx.autowaf_tests_total += 1
    ctx.autowaf_local_tests_total += 1
    ctx.autowaf_tests[appname]['total'] += 1

    out = (None, None)
    if type(test) == list:
        name       = test[0]
        returncode = test[1]
    elif callable(test):
        returncode = test()
    else:
        s = test
        if isinstance(test, type([])):
            s = ' '.join(test)
        if header and not quiet:
            Logs.pprint('Green', '\n** Test %s' % s)
        cmd = test
        if Options.options.test_wrapper:
            cmd = Options.options.test_wrapper + ' ' + test
        if name == '':
            name = test

        proc = subprocess.Popen(cmd, shell=True,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out = proc.communicate()
        returncode = proc.returncode

    success = desired_status is None or returncode == desired_status
    if success:
        if not quiet:
            Logs.pprint('GREEN', '** Pass %s' % name)
    else:
        Logs.pprint('RED', '** FAIL %s' % name)
        ctx.autowaf_tests_failed += 1
        ctx.autowaf_tests[appname]['failed'] += 1
        if type(test) != list and not callable(test):
            Logs.pprint('RED', test)

    if Options.options.verbose_tests and type(test) != list and not callable(test):
        sys.stdout.write(out[0])
        sys.stderr.write(out[1])

    return (success, out)

def tests_name(ctx, appname, name='*'):
    if name == '*':
        return appname
    else:
        return '%s.%s' % (appname, name)

def begin_tests(ctx, appname, name='*'):
    ctx.autowaf_local_tests_failed = 0
    ctx.autowaf_local_tests_total  = 0
    Logs.pprint('GREEN', '\n** Begin %s tests' % (
        tests_name(ctx, appname, name)))

    class Handle:
        def __enter__(self):
            pass

        def __exit__(self, type, value, traceback):
            end_tests(ctx, appname, name)

    return Handle()

def end_tests(ctx, appname, name='*'):
    failures = ctx.autowaf_local_tests_failed
    if failures == 0:
        Logs.pprint('GREEN', '** Passed all %d %s tests' % (
            ctx.autowaf_local_tests_total, tests_name(ctx, appname, name)))
    else:
        Logs.pprint('RED', '** Failed %d / %d %s tests' % (
            failures, ctx.autowaf_local_tests_total, tests_name(ctx, appname, name)))

def run_tests(ctx,
              appname,
              tests,
              desired_status=0,
              dirs=['src'],
              name='*',
              headers=False):
    begin_tests(ctx, appname, name)

    diropts  = ''
    for i in dirs:
        diropts += ' -d ' + i

    for i in tests:
        run_test(ctx, appname, i, desired_status, dirs, i, headers)

    end_tests(ctx, appname, name)

def run_ldconfig(ctx):
    should_run = (ctx.cmd == 'install' and
                  not ctx.env['RAN_LDCONFIG'] and
                  ctx.env['LIBDIR'] and
                  'DESTDIR' not in os.environ and
                  not Options.options.destdir)

    if should_run:
        try:
            Logs.info("Waf: Running `/sbin/ldconfig %s'" % ctx.env['LIBDIR'])
            subprocess.call(['/sbin/ldconfig', ctx.env['LIBDIR']])
            ctx.env['RAN_LDCONFIG'] = True
        except Exception:
            pass

def get_rdf_news(name,
                 in_files,
                 top_entries=None,
                 extra_entries=None,
                 dev_dist=None):
    import rdflib
    from time import strptime

    doap = rdflib.Namespace('http://usefulinc.com/ns/doap#')
    dcs  = rdflib.Namespace('http://ontologi.es/doap-changeset#')
    rdfs = rdflib.Namespace('http://www.w3.org/2000/01/rdf-schema#')
    foaf = rdflib.Namespace('http://xmlns.com/foaf/0.1/')
    rdf  = rdflib.Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
    m    = rdflib.ConjunctiveGraph()

    try:
        for i in in_files:
            m.parse(i, format='n3')
    except Exception:
        Logs.warn('Error parsing data, unable to generate NEWS')
        return

    proj = m.value(None, rdf.type, doap.Project)
    for f in m.triples([proj, rdfs.seeAlso, None]):
        if f[2].endswith('.ttl'):
            m.parse(f[2], format='n3')

    entries = {}
    for r in m.triples([proj, doap.release, None]):
        release   = r[2]
        revision  = m.value(release, doap.revision, None)
        date      = m.value(release, doap.created, None)
        blamee    = m.value(release, dcs.blame, None)
        changeset = m.value(release, dcs.changeset, None)
        dist      = m.value(release, doap['file-release'], None)

        if not dist:
            Logs.warn('No file release for %s %s' % (proj, revision))
            dist = dev_dist

        if revision and date and blamee and changeset:
            entry             = {}
            entry['name']     = str(name)
            entry['revision'] = str(revision)
            entry['date']     = strptime(str(date), '%Y-%m-%d')
            entry['status']   = 'stable' if dist != dev_dist else 'unstable'
            entry['dist']     = str(dist)
            entry['items']    = []

            for i in m.triples([changeset, dcs.item, None]):
                item = str(m.value(i[2], rdfs.label, None))
                entry['items'] += [item]
                if dist and top_entries is not None:
                    if not str(dist) in top_entries:
                        top_entries[str(dist)] = {'items': []}
                    top_entries[str(dist)]['items'] += [
                        '%s: %s' % (name, item)]

            if extra_entries and dist:
                for i in extra_entries[str(dist)]:
                    entry['items'] += extra_entries[str(dist)]['items']

            entry['blamee_name'] = str(m.value(blamee, foaf.name, None))
            entry['blamee_mbox'] = str(m.value(blamee, foaf.mbox, None))

            entries[(str(date), str(revision))] = entry
        else:
            Logs.warn('Ignored incomplete %s release description' % name)

    return entries

def write_news(entries, out_file):
    import textwrap
    from time import strftime

    if len(entries) == 0:
        return

    news = open(out_file, 'w')
    for e in sorted(entries.keys(), reverse=True):
        entry = entries[e]
        news.write('%s (%s) %s;\n' % (entry['name'], entry['revision'], entry['status']))
        for item in entry['items']:
            wrapped = textwrap.wrap(item, width=79)
            news.write('\n  * ' + '\n    '.join(wrapped))

        news.write('\n\n --')
        news.write(' %s <%s>' % (entry['blamee_name'],
                                 entry['blamee_mbox'].replace('mailto:', '')))

        news.write('  %s\n\n' % (
            strftime('%a, %d %b %Y %H:%M:%S +0000', entry['date'])))

    news.close()

def write_posts(entries, meta, out_dir, status='stable'):
    "write news posts in Pelican Markdown format"
    from time import strftime
    try:
        os.mkdir(out_dir)
    except Exception:
        pass

    for i in entries:
        entry    = entries[i]
        revision = i[1]
        if entry['status'] != status:
            continue

        date_str     = strftime('%Y-%m-%d', entry['date'])
        datetime_str = strftime('%Y-%m-%d %H:%M', entry['date'])

        path  = os.path.join(out_dir, '%s-%s-%s.md' % (
            date_str, entry['name'], revision.replace('.', '-')))
        post  = open(path, 'w')
        title = entry['title'] if 'title' in entry else entry['name']
        post.write('Title: %s %s\n' % (title, revision))
        post.write('Date: %s\n' % datetime_str)
        post.write('Slug: %s-%s\n' % (entry['name'], revision.replace('.', '-')))
        for k in meta:
            post.write('%s: %s\n' % (k, meta[k]))
        post.write('\n')

        url = entry['dist']
        if entry['status'] == status:
            post.write('[%s %s](%s) has been released.' % (
                (entry['name'], revision, url)))

        if 'description' in entry:
            post.write('  ' + entry['description'])

        post.write('\n')
        if (len(entry['items']) > 0 and
            not (len(entry['items']) == 1 and
                 entry['items'][0] == 'Initial release')):
            post.write('\nChanges:\n\n')
            for i in entry['items']:
                post.write(' * %s\n' % i)

        post.close()

def get_blurb(in_file):
    "Get the first paragram of a Markdown formatted file, skipping the title"
    f = open(in_file, 'r')
    f.readline()  # Title
    f.readline()  # Title underline
    f.readline()  # Blank
    out = ''
    line = f.readline()
    while len(line) > 0 and line != '\n':
        out += line.replace('\n', ' ')
        line = f.readline()
    return out.strip()

def get_news(in_file, entry_props={}):
    """Get NEWS entries in the format expected by write_posts().

    Properties that should be set on every entry can be passed in
    `entry_props`.  If `entry_props` has a 'dist_pattern' value, it is used to
    set the 'dist' entry of entries by substituting the version number.
    """

    import re
    import rfc822

    f       = open(in_file, 'r')
    entries = {}
    while True:
        # Read header line
        head    = f.readline()
        matches = re.compile(r'([^ ]*) \((.*)\) ([a-zA-z]*);').match(head)
        if matches is None:
            break

        entry             = {}
        entry['name']     = matches.group(1)
        entry['revision'] = matches.group(2)
        entry['status']   = matches.group(3)
        entry['items']    = []
        if 'dist_pattern' in entry_props:
            entry['dist'] = entry_props['dist_pattern'] % entry['revision']

        # Read blank line after header
        if f.readline() != '\n':
            raise SyntaxError('expected blank line after NEWS header')

        def add_item(item):
            if len(item) > 0:
                entry['items'] += [item.replace('\n', ' ').strip()]

        # Read entries for this revision
        item = ''
        line = ''
        while line != '\n':
            line = f.readline()
            if line.startswith('  * '):
                add_item(item)
                item = line[3:].lstrip()
            else:
                item += line.lstrip()
        add_item(item)

        # Read footer line
        foot    = f.readline()
        matches = re.compile(' -- (.*) <(.*)>  (.*)').match(foot)
        entry['date']        = rfc822.parsedate(matches.group(3))
        entry['blamee_name'] = matches.group(1)
        entry['blamee_mbox'] = matches.group(2)
        entry.update(entry_props)
        entries[(entry['date'], entry['revision'])] = entry

        # Skip trailing blank line before next entry
        f.readline()

    f.close()

    return entries

def news_to_posts(news_file, entry_props, post_meta, default_post_dir):
    post_dir = os.getenv('POST_DIR')
    if not post_dir:
        post_dir = default_post_dir
        sys.stderr.write('POST_DIR not set in environment, writing to %s\n' % post_dir)
    else:
        sys.stderr.write('writing posts to %s\n' % post_dir)

    entries = get_news(news_file, entry_props)
    write_posts(entries, post_meta, post_dir)

def run_script(cmds):
    for cmd in cmds:
        subprocess.check_call(cmd, shell=True)

def release(name, version, dist_name=None):
    if dist_name is None:
        dist_name = name.lower()

    dist = '%s-%s.tar.bz2' % (dist_name or name.lower(), version)
    try:
        os.remove(dist)
        os.remove(dist + '.sig')
    except Exception:
        pass

    status = subprocess.check_output('git status --porcelain', shell=True)
    if status:
        Logs.error('error: git working copy is dirty\n' + status)
        raise Exception('git working copy is dirty')

    head = subprocess.check_output('git show -s --oneline', shell=True)
    head_summary = head[8:].strip().lower()
    expected_summary = '%s %s' % (name.lower(), version)
    if head_summary != expected_summary:
        raise Exception('latest commit "%s" does not match "%s"' % (
            head_summary, expected_summary))

    run_script(['./waf configure --docs',
                './waf',
                './waf distcheck',
                './waf posts',
                'gpg -b %s' % dist,
                'git tag -s v%s -m "%s %s"' % (version, name, version)])
