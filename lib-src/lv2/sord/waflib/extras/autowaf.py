import glob
import os
import subprocess
import sys
import time

from waflib import Configure, ConfigSet, Build, Context, Logs, Options, Utils
from waflib.TaskGen import feature, before, after

NONEMPTY = -10

if sys.platform == 'win32':
    lib_path_name = 'PATH'
elif sys.platform == 'darwin':
    lib_path_name = 'DYLD_LIBRARY_PATH'
else:
    lib_path_name = 'LD_LIBRARY_PATH'

# Compute dependencies globally
# import preproc
# preproc.go_absolute = True

@feature('c', 'cxx')
@after('apply_incpaths')
def include_config_h(self):
    self.env.append_value('INCPATHS', self.bld.bldnode.abspath())

class OptionsContext(Options.OptionsContext):
    def __init__(self, **kwargs):
        super(OptionsContext, self).__init__(**kwargs)
        set_options(self)

    def configuration_options(self):
        return self.get_option_group('Configuration options')

    def add_flags(self, group, flags):
        """Tersely add flags (a dictionary of longname:desc) to a group"""
        for name, desc in flags.items():
            group.add_option('--' + name, action='store_true',
                             dest=name.replace('-', '_'), help=desc)

def set_options(opt, debug_by_default=False):
    "Add standard autowaf options"
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
    opts.add_option('-S', '--ultra-strict', action='store_true', default=False,
                    dest='ultra_strict',
                    help="use extremely strict compiler flags (likely noisy)")
    opts.add_option('--docs', action='store_true', default=False, dest='docs',
                    help="build documentation (requires doxygen)")
    opts.add_option('-w', '--werror', action='store_true', dest='werror',
                    help="Treat warnings as errors")

    # Test options
    if hasattr(Context.g_module, 'test'):
        test_opts = opt.add_option_group('Test options', '')
        opts.add_option('-T', '--test', action='store_true', dest='build_tests',
                        help='build unit tests')
        opts.add_option('--no-coverage', action='store_true',
                        dest='no_coverage',
                        help='do not instrument code for test coverage')
        test_opts.add_option('--wrapper', type='string',
                             dest='test_wrapper',
                             help='command prefix for tests (e.g. valgrind)')
        test_opts.add_option('--test-filter', type='string',
                             dest='test_filter',
                             help='regular expression for tests to run')

    # Run options
    run_opts = opt.add_option_group('Run options')
    run_opts.add_option('--cmd', type='string', dest='cmd',
                        help='command to run from build directory')

class ConfigureContext(Configure.ConfigurationContext):
    """configures the project"""

    def __init__(self, **kwargs):
        self.line_just = 45
        if hasattr(Context.g_module, 'line_just'):
            self.line_just = Context.g_module.line_just

        super(ConfigureContext, self).__init__(**kwargs)
        self.run_env = ConfigSet.ConfigSet()
        self.system_include_paths = set()

    def pre_recurse(self, node):
        if len(self.stack_path) == 1:
            Logs.pprint('BOLD', 'Configuring %s' % node.parent.srcpath())
        super(ConfigureContext, self).pre_recurse(node)

    def store(self):
        self.env.AUTOWAF_RUN_ENV = self.run_env.get_merged_dict()
        for path in sorted(self.system_include_paths):
            if 'COMPILER_CC' in self.env:
                self.env.append_value('CFLAGS', ['-isystem', path])
            if 'COMPILER_CXX' in self.env:
                self.env.append_value('CXXFLAGS', ['-isystem', path])

        super(ConfigureContext, self).store()

    def check_pkg(self, *args, **kwargs):
        return check_pkg(self, *args, **kwargs)

    def check_function(self, *args, **kwargs):
        return check_function(self, *args, **kwargs)

    def build_path(self, path='.'):
        """Return `path` within the build directory"""
        return str(self.path.get_bld().make_node(path))

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

def check_pkg(conf, spec, **kwargs):
    "Check for a package iff it hasn't been checked for yet"

    if (kwargs['uselib_store'].lower() in conf.env['AUTOWAF_LOCAL_LIBS'] or
        kwargs['uselib_store'].lower() in conf.env['AUTOWAF_LOCAL_HEADERS']):
        return

    import re
    match = re.match('([^ ]*) >= [0-9\.]*', spec)
    args = []
    if match:
        name = match.group(1)
        args = [spec]
    elif spec.find(' ') == -1:
        name = spec
    else:
        Logs.error("Invalid package spec: %s" % spec)

    found = None
    pkg_var_name = 'PKG_' + name.replace('-', '_')
    pkg_name = name
    args += kwargs.get('args', [])

    if conf.env.PARDEBUG:
        kwargs['mandatory'] = False  # Smash mandatory arg
        found = conf.check_cfg(package=pkg_name + 'D',
                               args=args + ['--cflags', '--libs'])
        if found:
            pkg_name += 'D'

        args['mandatory'] = mandatory  # Unsmash mandatory arg

    if not found:
        found = conf.check_cfg(package=spec,
                               args=args + ['--cflags', '--libs'],
                               **kwargs)

    if not conf.env.MSVC_COMPILER and 'system' in kwargs and kwargs['system']:
        conf.system_include_paths.update(
            conf.env['INCLUDES_' + nameify(kwargs['uselib_store'])])

def normpath(path):
    if sys.platform == 'win32':
        return os.path.normpath(path).replace('\\', '/')
    else:
        return os.path.normpath(path)

def configure(conf):
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
            append_cxx_flags(['/MD', '/FS', '/DNDEBUG'])
        else:
            append_cxx_flags(['-DNDEBUG'])

    if conf.env.MSVC_COMPILER:
        Options.options.no_coverage = True
        append_cxx_flags(['/nologo',
                          '/FS',
                          '/D_CRT_SECURE_NO_WARNINGS',
                          '/experimental:external',
                          '/external:W0',
                          '/external:anglebrackets'])
        conf.env.append_value('LINKFLAGS', '/nologo')
        if Options.options.strict or Options.options.ultra_strict:
            ms_strict_flags = ['/Wall',
                               '/wd4061',
                               '/wd4200',
                               '/wd4514',
                               '/wd4571',
                               '/wd4625',
                               '/wd4626',
                               '/wd4706',
                               '/wd4710',
                               '/wd4820',
                               '/wd5026',
                               '/wd5027',
                               '/wd5045']
            conf.env.append_value('CFLAGS', ms_strict_flags)
            conf.env.append_value('CXXFLAGS', ms_strict_flags)
            conf.env.append_value('CXXFLAGS', ['/EHsc'])
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
                              '-Wno-parentheses',
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

    if Options.options.werror:
        if conf.env.MSVC_COMPILER:
            append_cxx_flags('/WX')
        else:
            append_cxx_flags('-Werror')

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
    conf.define(defname + '_VERSION', version)
    conf.env[defname + '_VERSION'] = version

    conf.env.prepend_value('CFLAGS', '-I' + os.path.abspath('.'))
    conf.env.prepend_value('CXXFLAGS', '-I' + os.path.abspath('.'))

def display_summary(conf, msgs=None):
    if len(conf.stack_path) == 1:
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
        conf.env.append_unique('CFLAGS', ['/TP'])
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
            conf.env.append_unique('CFLAGS', ['/TP'])
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
    conf.define(var_name, 1)
    conf.env[var_name] = 1
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

@feature('c', 'cxx')
@before('apply_link')
def version_lib(self):
    if self.env.DEST_OS == 'win32':
        self.vnum = None  # Prevent waf from automatically appending -0
    if self.env['PARDEBUG']:
        applicable = ['cshlib', 'cxxshlib', 'cstlib', 'cxxstlib']
        if [x for x in applicable if x in self.features]:
            self.target = self.target + 'D'

def set_lib_env(conf,
                name,
                version,
                has_objects=True,
                include_path=None,
                lib_path=None):
    "Set up environment for local library as if found via pkg-config."
    NAME         = name.upper()
    major_ver    = version.split('.')[0]
    pkg_var_name = 'PKG_' + name.replace('-', '_') + '_' + major_ver
    lib_name     = '%s-%s' % (name, major_ver)

    if lib_path is None:
        lib_path = str(conf.path.get_bld())

    if include_path is None:
        include_path = str(conf.path)

    if conf.env.PARDEBUG:
        lib_name += 'D'

    conf.env[pkg_var_name]       = lib_name
    conf.env['INCLUDES_' + NAME] = [include_path]
    conf.env['LIBPATH_' + NAME]  = [lib_path]
    if has_objects:
        conf.env['LIB_' + NAME] = [lib_name]

    conf.run_env.append_unique(lib_path_name, [lib_path])
    conf.define(NAME + '_VERSION', version)

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

def build_pc(bld, name, version, version_suffix, libs, subst_dict={}):
    """Build a pkg-config file for a library.

    name           -- uppercase variable name     (e.g. 'SOMENAME')
    version        -- version string              (e.g. '1.2.3')
    version_suffix -- name version suffix         (e.g. '2')
    libs           -- string/list of dependencies (e.g. 'LIBFOO GLIB')
    """

    pkg_prefix       = bld.env['PREFIX']
    if len(pkg_prefix) > 1 and pkg_prefix[-1] == '/':
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
    finally:
        os.chdir(top)

def build_dox(bld, name, version, srcdir, blddir, outdir='', versioned=True):
    """Build Doxygen API documentation"""
    if not bld.env['DOCS']:
        return

    # Doxygen paths in are relative to the doxygen file
    src_dir = bld.path.srcpath()
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

class ExecutionEnvironment:
    """Context that sets system environment variables for program execution"""
    def __init__(self, changes):
        self.original_environ = os.environ.copy()

        self.diff = {}
        for path_name, paths in changes.items():
            value = os.pathsep.join(paths)
            if path_name in os.environ:
                value += os.pathsep + os.environ[path_name]

            self.diff[path_name] = value

        os.environ.update(self.diff)

    def __str__(self):
        return '\n'.join({'%s="%s"' % (k, v) for k, v in self.diff.items()})

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        os.environ = self.original_environ

class RunContext(Build.BuildContext):
    "runs an executable from the build directory"
    cmd = 'run'

    def execute(self):
        self.restore()
        if not self.all_envs:
            self.load_envs()

        with ExecutionEnvironment(self.env.AUTOWAF_RUN_ENV) as env:
            if Options.options.verbose:
                Logs.pprint('GREEN', str(env) + '\n')

            if Options.options.cmd:
                Logs.pprint('GREEN', 'Running %s' % Options.options.cmd)
                subprocess.call(Options.options.cmd, shell=True)
            else:
                Logs.error("error: Missing --cmd option for run command")

def show_diff(from_lines, to_lines, from_filename, to_filename):
    import difflib
    import sys

    same = True
    for line in difflib.unified_diff(
            from_lines, to_lines,
            fromfile=os.path.abspath(from_filename),
            tofile=os.path.abspath(to_filename)):
        sys.stderr.write(line)
        same = False

    return same

def test_file_equals(patha, pathb):
    import filecmp
    import io

    for path in (patha, pathb):
        if not os.access(path, os.F_OK):
            Logs.pprint('RED', 'error: missing file %s' % path)
            return False

    if filecmp.cmp(patha, pathb, shallow=False):
        return True

    with io.open(patha, 'rU', encoding='utf-8') as fa:
        with io.open(pathb, 'rU', encoding='utf-8') as fb:
            return show_diff(fa.readlines(), fb.readlines(), patha, pathb)

def bench_time():
    if hasattr(time, 'perf_counter'): # Added in Python 3.3
        return time.perf_counter()
    else:
        return time.time()

class TestOutput:
    """Test output that is truthy if result is as expected"""
    def __init__(self, expected, result=None):
        self.stdout = self.stderr = None
        self.expected = expected
        self.result = result

    def __bool__(self):
        return self.expected is None or self.result == self.expected

    __nonzero__ = __bool__

def is_string(s):
    if sys.version_info[0] < 3:
        return isinstance(s, basestring)
    return isinstance(s, str)

class TestScope:
    """Scope for running tests that maintains pass/fail statistics"""
    def __init__(self, tst, name, defaults):
        self.tst = tst
        self.name = name
        self.defaults = defaults
        self.n_failed = 0
        self.n_total = 0

    def run(self, test, **kwargs):
        if type(test) == list and 'name' not in kwargs:
            import pipes
            kwargs['name'] = ' '.join(map(pipes.quote, test))

        if Options.options.test_filter and 'name' in kwargs:
            import re
            found = False
            for scope in self.tst.stack:
                if re.search(Options.options.test_filter, scope.name):
                    found = True
                    break

            if (not found and
                not re.search(Options.options.test_filter, self.name) and
                not re.search(Options.options.test_filter, kwargs['name'])):
                return True

        if callable(test):
            output = self._run_callable(test, **kwargs)
        elif type(test) == list:
            output = self._run_command(test, **kwargs)
        else:
            raise Exception("Unknown test type")

        if not output:
            self.tst.log_bad('FAILED', kwargs['name'])

        return self.tst.test_result(output)

    def _run_callable(self, test, **kwargs):
        expected = kwargs['expected'] if 'expected' in kwargs else True
        return TestOutput(expected, test())

    def _run_command(self, test, **kwargs):
        if 'stderr' in kwargs and kwargs['stderr'] == NONEMPTY:
            # Run with a temp file for stderr and check that it is non-empty
            import tempfile
            with tempfile.TemporaryFile() as stderr:
                kwargs['stderr'] = stderr
                output = self.run(test, **kwargs)
                stderr.seek(0, 2) # Seek to end
                return (output if not output else
                        self.run(
                            lambda: stderr.tell() > 0,
                            name=kwargs['name'] + ' error message'))

        try:
            # Run with stdout and stderr set to the appropriate streams
            out_stream = self._stream('stdout', kwargs)
            err_stream = self._stream('stderr', kwargs)
            return self._exec(test, **kwargs)
        finally:
            out_stream = out_stream.close() if out_stream else None
            err_stream = err_stream.close() if err_stream else None

    def _stream(self, stream_name, kwargs):
        s = kwargs[stream_name] if stream_name in kwargs else None
        if is_string(s):
            kwargs[stream_name] = open(s, 'wb')
            return kwargs[stream_name]
        return None

    def _exec(self,
              test,
              expected=0,
              name='',
              stdin=None,
              stdout=None,
              stderr=None,
              verbosity=1):
        def stream(s):
            return open(s, 'wb') if type(s) == str else s

        if verbosity > 1:
            self.tst.log_good('RUN     ', name)

        if Options.options.test_wrapper:
            import shlex
            test = shlex.split(Options.options.test_wrapper) + test

        output = TestOutput(expected)
        with open(os.devnull, 'wb') as null:
            out = null if verbosity < 3 and not stdout else stdout
            err = null if verbosity < 2 and not stderr else stderr
            proc = subprocess.Popen(test, stdin=stdin, stdout=out, stderr=err)
            output.stdout, output.stderr = proc.communicate()
            output.result = proc.returncode

        if output and verbosity > 0:
            self.tst.log_good('      OK', name)

        return output

class TestContext(Build.BuildContext):
    "runs test suite"
    fun = cmd = 'test'

    def __init__(self, **kwargs):
        super(TestContext, self).__init__(**kwargs)
        self.start_time = bench_time()
        self.max_depth = 1

        defaults = {'verbosity': Options.options.verbose}
        self.stack = [TestScope(self, Context.g_module.APPNAME, defaults)]

    def defaults(self):
        return self.stack[-1].defaults

    def finalize(self):
        if self.stack[-1].n_failed > 0:
            sys.exit(1)

        super(TestContext, self).finalize()

    def __call__(self, test, **kwargs):
        return self.stack[-1].run(test, **self.args(**kwargs))

    def file_equals(self, from_path, to_path, **kwargs):
        kwargs.update({'expected': True,
                       'name': '%s == %s' % (from_path, to_path)})
        return self(lambda: test_file_equals(from_path, to_path), **kwargs)

    def log_good(self, title, fmt, *args):
        Logs.pprint('GREEN', '[%s] %s' % (title.center(10), fmt % args))

    def log_bad(self, title, fmt, *args):
        Logs.pprint('RED', '[%s] %s' % (title.center(10), fmt % args))

    def pre_recurse(self, node):
        wscript_module = Context.load_module(node.abspath())
        group_name = wscript_module.APPNAME
        self.stack.append(TestScope(self, group_name, self.defaults()))
        self.max_depth = max(self.max_depth, len(self.stack) - 1)

        bld_dir = node.get_bld().parent
        if bld_dir != self.path.get_bld():
            Logs.info('')

        self.original_dir = os.getcwd()
        Logs.info("Waf: Entering directory `%s'\n", bld_dir)
        os.chdir(str(bld_dir))

        if not self.env.NO_COVERAGE and str(node.parent) == Context.top_dir:
            self.clear_coverage()

        self.log_good('=' * 10, 'Running %s tests', group_name)
        super(TestContext, self).pre_recurse(node)

    def test_result(self, success):
        self.stack[-1].n_total += 1
        self.stack[-1].n_failed += 1 if not success else 0
        return success

    def pop(self):
        scope = self.stack.pop()
        self.stack[-1].n_total += scope.n_total
        self.stack[-1].n_failed += scope.n_failed
        return scope

    def post_recurse(self, node):
        super(TestContext, self).post_recurse(node)

        scope = self.pop()
        duration = (bench_time() - self.start_time) * 1000.0
        is_top = str(node.parent) == str(Context.top_dir)

        if is_top and self.max_depth > 1:
            Logs.info('')

        self.log_good('=' * 10, '%d tests from %s ran (%d ms total)',
                      scope.n_total, scope.name, duration)

        if not self.env.NO_COVERAGE:
            if is_top:
                self.gen_coverage()

            if os.path.exists('coverage/index.html'):
                self.log_good('REPORT', '<file://%s>',
                              os.path.abspath('coverage/index.html'))

        successes = scope.n_total - scope.n_failed
        Logs.pprint('GREEN', '[  PASSED  ] %d tests' % successes)
        if scope.n_failed > 0:
            Logs.pprint('RED', '[  FAILED  ] %d tests' % scope.n_failed)
        if is_top:
            Logs.info("\nWaf: Leaving directory `%s'" % os.getcwd())

        os.chdir(self.original_dir)

    def execute(self):
        self.restore()
        if not self.all_envs:
            self.load_envs()

        if not self.env.BUILD_TESTS:
            self.fatal('Configuration does not include tests')

        with ExecutionEnvironment(self.env.AUTOWAF_RUN_ENV) as env:
            if self.defaults()['verbosity'] > 0:
                Logs.pprint('GREEN', str(env) + '\n')
            self.recurse([self.run_dir])

    def src_path(self, path):
        return os.path.relpath(os.path.join(str(self.path), path))

    def args(self, **kwargs):
        all_kwargs = self.defaults().copy()
        all_kwargs.update(kwargs)
        return all_kwargs

    def group(self, name, **kwargs):
        return TestGroup(
            self, self.stack[-1].name, name, **self.args(**kwargs))

    def set_test_defaults(self, **kwargs):
        """Set default arguments to be passed to all tests"""
        self.stack[-1].defaults.update(kwargs)

    def clear_coverage(self):
        """Zero old coverage data"""
        try:
            with open('cov-clear.log', 'w') as log:
                subprocess.call(['lcov', '-z', '-d', str(self.path)],
                                stdout=log, stderr=log)

        except Exception:
            Logs.warn('Failed to run lcov to clear old coverage data')

    def gen_coverage(self):
        """Generate coverage data and report"""
        try:
            with open('cov.lcov', 'w') as out:
                with open('cov.log', 'w') as err:
                    subprocess.call(['lcov', '-c', '--no-external',
                                     '--rc', 'lcov_branch_coverage=1',
                                     '-b', '.',
                                     '-d', str(self.path)],
                                    stdout=out, stderr=err)

            if not os.path.isdir('coverage'):
                os.makedirs('coverage')

            with open('genhtml.log', 'w') as log:
                subprocess.call(['genhtml',
                                 '-o', 'coverage',
                                 '--rc', 'genhtml_branch_coverage=1',
                                 'cov.lcov'],
                                stdout=log, stderr=log)

            summary = subprocess.check_output(
                ['lcov', '--summary',
                 '--rc', 'lcov_branch_coverage=1',
                 'cov.lcov'],
                stderr=subprocess.STDOUT).decode('ascii')

            import re
            lines = re.search('lines\.*: (.*)%.*', summary).group(1)
            functions = re.search('functions\.*: (.*)%.*', summary).group(1)
            branches = re.search('branches\.*: (.*)%.*', summary).group(1)
            self.log_good('COVERAGE', '%s%% lines, %s%% functions, %s%% branches',
                          lines, functions, branches)

        except Exception:
            Logs.warn('Failed to run lcov to generate coverage report')

class TestGroup:
    def __init__(self, tst, suitename, name, **kwargs):
        self.tst = tst
        self.suitename = suitename
        self.name = name
        self.kwargs = kwargs
        self.start_time = bench_time()
        tst.stack.append(TestScope(tst, name, tst.defaults()))

    def label(self):
        return self.suitename + '.%s' % self.name if self.name else ''

    def args(self, **kwargs):
        all_kwargs = self.tst.args(**self.kwargs)
        all_kwargs.update(kwargs)
        return all_kwargs

    def __enter__(self):
        if 'verbosity' in self.kwargs and self.kwargs['verbosity'] > 0:
            self.tst.log_good('-' * 10, self.label())
        return self

    def __call__(self, test, **kwargs):
        return self.tst(test, **self.args(**kwargs))

    def file_equals(self, from_path, to_path, **kwargs):
        return self.tst.file_equals(from_path, to_path, **kwargs)

    def __exit__(self, type, value, traceback):
        duration = (bench_time() - self.start_time) * 1000.0
        scope = self.tst.pop()
        n_passed = scope.n_total - scope.n_failed
        if scope.n_failed == 0:
            self.tst.log_good('-' * 10, '%d tests from %s (%d ms total)',
                              scope.n_total, self.label(), duration)
        else:
            self.tst.log_bad('-' * 10, '%d/%d tests from %s (%d ms total)',
                             n_passed, scope.n_total, self.label(), duration)

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

def run_script(cmds):
    for cmd in cmds:
        subprocess.check_call(cmd, shell=True)
