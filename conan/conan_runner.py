import argparse
import os
import venv
import subprocess
import re
import sys
import hashlib
from contextlib import contextmanager
import configparser

required_conan_version = (1, 58, 0)

class ConanVenv(venv.EnvBuilder):
    def post_setup(self, context):
        os.environ['VIRTUAL_ENV'] = context.env_dir
        if sys.platform == 'win32':
            python_exe = os.path.join(context.bin_path, 'python.exe')
        else:
            python_exe = os.path.join(context.bin_path, 'python')
        cmd = [python_exe, '-m', 'pip', 'install', 'conan==1.59.0']
        subprocess.check_call(cmd)


def get_python():
    if os.environ['VIRTUAL_ENV']:
        if sys.platform == 'win32':
            return os.path.join(os.environ['VIRTUAL_ENV'], 'Scripts', 'python.exe')
        else:
            return os.path.join(os.environ['VIRTUAL_ENV'], 'bin', 'python')
    else:
        return sys.executable


def get_conan():
    if 'VIRTUAL_ENV' in os.environ:
        if sys.platform == 'win32':
            return os.path.join(os.environ['VIRTUAL_ENV'], 'Scripts', 'conan.exe')
        else:
            return os.path.join(os.environ['VIRTUAL_ENV'], 'bin', 'conan')
    else:
        if sys.platform == 'win32':
            return 'conan.exe'
        else:
            return 'conan'


def get_root_dir():
    return os.path.dirname(os.path.abspath(__file__))


def get_conanfile_path():
    return os.path.join(get_root_dir(), 'conanfile.py')


def get_conanfile_hash():
    with open(get_conanfile_path(), 'rb') as f:
        return hashlib.sha256(f.read()).hexdigest()


def version_tuple(v):
    return tuple(map(int, (v.split("."))))


def init_args():
    parser = argparse.ArgumentParser(description='Audacity Conan runner')

    parser.add_argument('-b', '--build-dir', help='Build directory (${CMAKE_BINARY_DIR})', required=True)
    parser.add_argument('-o', '--options', help='Conan options', nargs='*')
    parser.add_argument('--force-build', help='Force build', action='store_true')
    parser.add_argument('--compiler', help='Compiler', required=True)
    parser.add_argument('--compiler-version', help='Compiler version', required=True)
    parser.add_argument('--target-arch', help='Target architecture', default="x86_64")
    parser.add_argument('--build-arch', help='Build architecture', default="x86_64")
    parser.add_argument('--download-cache', help='Conan download cache', default=None)
    parser.add_argument('--build-types', help='Build types', default=['Release'], nargs="*")
    parser.add_argument('--lib-dir', help='Directory to copy the shared libraries to', default=None)
    parser.add_argument('--min-os-version', help='Minimum OS version', default=None)
    parser.add_argument('--disallow-prebuilt', help='Disallow prebuilt dependencies', action='store_true')

    return parser.parse_args()


def get_conan_venv(build_dir):
    return os.path.abspath(os.path.join(build_dir, 'conan_venv'))


def get_conan_version():
    try:
        with subprocess.Popen([get_conan(), '--version'], stdout=subprocess.PIPE) as proc:
            return version_tuple(re.search(r'[0-9]+\.[0-9]+\.[0-9]+', proc.stdout.read().decode('utf-8')).group(0))
    except FileNotFoundError:
        return None


def build_venv(conan_venv):
    print(f'Creating Conan virtual environment {conan_venv}')
    builder = ConanVenv(with_pip=True)
    builder.create(conan_venv)
    return get_conan_version()


def upgrade_conan():
    subprocess.check_call([get_python(), '-m', 'pip', 'install', '--upgrade', 'conan'])
    return get_conan_version()


def validate_conan(build_dir):
    conan_venv = get_conan_venv(build_dir)

    if os.path.isdir(conan_venv):
        print(f'Using Conan from virtual environment {conan_venv}')
        os.environ['VIRTUAL_ENV'] = conan_venv

        conan_version = get_conan_version()

        if conan_version is None:
            conan_version = build_venv(conan_venv)
        elif conan_version < required_conan_version:
            conan_version = upgrade_conan()

        if conan_version is None or conan_version < required_conan_version:
            raise RuntimeError(f'Conan version {required_conan_version} is required')

        print(f'CONAN_USER_HOME is {os.path.abspath(build_dir)}')
        os.environ['CONAN_USER_HOME'] = os.path.abspath(build_dir)

        return conan_version
    else:
        conan_version = get_conan_version()

        if conan_version is None:
            build_venv(conan_venv)
            conan_version = validate_conan(build_dir)
        elif conan_version < required_conan_version:
            raise RuntimeError(f'Conan version {conan_version} is too old, please upgrade to {required_conan_version} or newer')

        return conan_version


def validate_remotes():
    with subprocess.Popen([get_conan(), 'remote', 'list'], stdout=subprocess.PIPE) as proc:
        remotes = proc.stdout.read().decode('utf-8')

        if 'audacity:' in remotes:
            print('Removing old audacity remote...')
            subprocess.check_call([get_conan(), 'remote', 'remove', 'audacity'])

        if 'conan-center-cache' in remotes:
            print('Removing old conan-center-cache remote...')
            subprocess.check_call([get_conan(), 'remote', 'remove', 'conan-center-cache'])

        if 'audacity-recipes:' not in remotes:
            print('Adding audacity-recipes remote...')
            subprocess.check_call([get_conan(), 'remote', 'add', 'audacity-recipes', 'https://artifactory.audacityteam.org/artifactory/api/conan/audacity-recipes'])

        if 'audacity-binaries:' not in remotes:
            print('Adding audacity-binaries remote...')
            subprocess.check_call([get_conan(), 'remote', 'add', 'audacity-binaries', 'https://artifactory.audacityteam.org/artifactory/api/conan/audacity-binaries'])


def generate_args_string(args):
    if args.options is None:
        return ''

    return ';'.join(args.options) + ';' + get_conanfile_hash()


def get_profile_os(args):
    try:
        return {
            'win32': 'Windows',
            'cygwin':'Windows',
            'darwin': 'Macos',
            'linux': 'Linux',
            'freebsd': 'FreeBSD',
            'openbsd': 'OpenBSD'
        }[sys.platform]
    except KeyError:
        return sys.platform

def get_conan_compiler(args):
    if args.compiler == 'MSVC':
        return 'Visual Studio'
    elif args.compiler == 'GNU':
        return 'gcc'
    elif args.compiler == 'Clang':
        return 'clang'
    elif args.compiler == 'AppleClang':
        return 'apple-clang'
    else:
        return args.compiler

def get_conan_compiler_version(args):
    if args.compiler == 'MSVC':
        ver = version_tuple(args.compiler_version)

        if ver[0] < 19:
            raise RuntimeError(f'Visual Studio {args.compiler_version} is not supported')
        else:
            return ver[1] // 10 - 2 + 16
    else:
        compiler_version = version_tuple(args.compiler_version)

        if args.compiler == 'AppleClang' and compiler_version[0] < 13:
            return f'{compiler_version[0]}.{compiler_version[1]}'

        return version_tuple(args.compiler_version)[0]

def get_conan_arch(arch: str):
    lower_arch = arch.lower()
    if lower_arch == 'x86_64' or lower_arch == 'amd64' or lower_arch == 'x64':
        return 'x86_64'
    elif lower_arch == 'x86':
        return 'x86'
    elif lower_arch == 'arm64':
        return 'armv8'
    elif lower_arch == 'arm':
        return 'armv7'
    else:
        return lower_arch


def generate_profile(args, host_profile: bool, build_type: str):
    profile = configparser.ConfigParser(allow_no_value=True, delimiters='=')
    profile.optionxform = str

    profile['settings'] = {
        'os': get_profile_os(args),
        'arch': get_conan_arch(args.target_arch if host_profile else args.build_arch),
        'compiler': get_conan_compiler(args),
        'compiler.version': get_conan_compiler_version(args),
        'cppstd': '17',
        '&:build_type': build_type,
        'build_type': 'Debug' if build_type == 'Debug' else 'RelWithDebInfo'
    }

    profile['options'] = {}
    profile['env'] = {}
    profile['conf'] = {}
    profile['tool_requires'] = {}

    if args.compiler == 'MSVC':
        profile['settings']['compiler.runtime'] = "MDd" if build_type == "Debug" else "MD"
    elif args.compiler == 'GNU':
        profile['settings']['compiler.libcxx'] = 'libstdc++11'
    elif args.compiler == 'Clang' or args.compiler == 'AppleClang':
        profile['settings']['compiler.libcxx'] = 'libc++'

    if args.min_os_version is not None:
        profile['settings']['os.version'] = args.min_os_version

    if host_profile:
        profile_overrides_path = os.path.join(get_root_dir(), f'profile_overrides_{build_type.lower()}.txt')

        if os.path.isfile(profile_overrides_path):
            profile.read(profile_overrides_path)
        for option in args.options:
            option_name, option_value = option.split('=', 1)
            profile['options'][f'&:{option_name}'] = option_value
        if args.lib_dir is not None:
            profile['options']['&:lib_dir'] = args.lib_dir
        if args.target_arch != args.build_arch:
            if sys.platform == 'darwin':
                profile['env']['CONAN_CMAKE_SYSTEM_NAME'] ='Darwin'
                profile['env']['CONAN_CMAKE_SYSTEM_PROCESSOR'] = "x86_64" if args.target_arch == "x86_64" else "arm64"
                profile['conf']['tools.apple:sdk_path'] = subprocess.check_output(['xcrun', '--sdk', 'macosx', '--show-sdk-path']).decode('utf-8').strip()


    if args.download_cache:
        profile['conf']['tools.files.download:download_cache']= args.download_cache

    profile_name = f'profile-host-{build_type.lower()}.profile' if host_profile else f'profile-build.profile'

    with open(os.path.join(args.build_dir, profile_name), 'w') as f:
        profile.write(f)

    return os.path.join(args.build_dir, profile_name)


@contextmanager
def update_global_config(args):
    old_download_cache = None

    if args.download_cache:
        try:
            old_download_cache = subprocess.check_output([get_conan(), 'config', 'get', 'storage.download_cache']).decode('utf-8').strip()
        except subprocess.CalledProcessError:
            pass

        subprocess.check_call([get_conan(), 'config', 'set', f'storage.download_cache={args.download_cache}'])

    try:
        print('Updating global Conan config...', flush=True)
        yield None
    finally:
        print('Restoring global Conan config...', flush=True)
        if old_download_cache is not None:
            print('\tRestoring old download cache...', flush=True)
            subprocess.check_call([get_conan(), 'config', 'set', f'storage.download_cache={old_download_cache}'])
        elif args.download_cache:
            print('\tRemoving download cache from global Conan config...', flush=True)
            subprocess.check_call([get_conan(), 'config', 'rm', 'storage.download_cache'])


if __name__ == '__main__':
    args = init_args()

    args_file_path = os.path.join(args.build_dir, 'conan_args.txt')
    args_string = generate_args_string(args)

    os.environ['CONAN_REVISIONS_ENABLED']='1'

    build_profile = generate_profile(args, False, 'Release')
    host_profiles = [(build_type, generate_profile(args, True, build_type)) for build_type in args.build_types]

    if os.path.isfile(args_file_path):
        with open(args_file_path, 'r') as args_file:
            old_args = args_file.read()
            if old_args == generate_args_string(args):
                print('Skipping conan install, arguments and requirements are the same')
                sys.exit(0)

    conan_version = validate_conan(args.build_dir)
    print(f'Using Conan version {".".join(map(str, conan_version))} ({get_conan()})', flush=True)
    validate_remotes()

    with update_global_config(args):
        for build_type, host_profile in host_profiles:
            conan_options = [
                get_conan(), 'install', get_root_dir(),
                '--build' if args.force_build else '--build=missing',
                '--install-folder', os.path.join(args.build_dir, f'conan-install-{build_type.lower()}'),
                '--output-folder', args.build_dir,
                '--remote', 'audacity-recipes' if (args.force_build or args.disallow_prebuilt) else 'audacity-binaries',
                '--profile:build', build_profile,
                '--profile:host', host_profile,
            ]

            subprocess.check_call(conan_options)

    with open(args_file_path, 'w') as args_file:
        args_file.write(args_string)
