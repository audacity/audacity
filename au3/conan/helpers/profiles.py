import configparser
import sys
import os

from helpers.version import version_tuple
from helpers.directories import directories

__arch_map = {
    'x86_64': 'x86_64',
    'amd64': 'x86_64',
    'x64': 'x86_64',
    'x86': 'x86',
    'arm64': 'armv8',
    'aarch64': 'armv8',
    'arm': 'armv7',
}

__compilers_map = {
    'MSVC': 'msvc',
    'GNU': 'gcc',
    'Clang': 'clang',
    'AppleClang': 'apple-clang',
}

def get_profile_os() -> str:
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


def get_conan_compiler(cmake_compiler: str) -> str:
    return __compilers_map.get(cmake_compiler, cmake_compiler)


def get_conan_compiler_version(cmake_compiler: str, cmake_compiler_version: str) -> str:
    compiler_version = version_tuple(cmake_compiler_version)

    if cmake_compiler == 'MSVC':
        if compiler_version < (19, 20, 0, 0):
            raise RuntimeError(f'Visual Studio {compiler_version} is not supported')
        else:
            return str(compiler_version[0]) + str(compiler_version[1] // 10) # 19.35.32217.1 -> 193
    else:
        if cmake_compiler == 'AppleClang' and compiler_version[0] < 13:
            return f'{compiler_version[0]}.{compiler_version[1]}'

        return compiler_version[0]


def get_conan_arch(cmake_arch: str):
    arch = cmake_arch.lower()
    return __arch_map.get(arch, arch)


class Profiles:
    compiler = None

    build_profile_path = None

    base_host_profile_path = None
    options_profile_path = None

    host_profile_paths = []

    def __init__(self, args):
        if not os.path.isdir(directories.profiles_dir):
            os.makedirs(directories.profiles_dir)

        self.compiler = get_conan_compiler(args.compiler)

        if not args.build_profile:
            self.build_profile_path = os.path.join(directories.profiles_dir, 'build.profile')
            if not os.path.isfile(self.build_profile_path):
                print('Generating build profile...')
                self.__detect_base_profile(args, self.build_profile_path, False)
        else:
            self.build_profile_path = args.build_profile

        if not args.host_profile:
            self.base_host_profile_path = os.path.join(directories.profiles_dir, 'host.profile')
            if not os.path.isfile(self.base_host_profile_path):
                print('Generating base host profile...')
                self.__detect_base_profile(args, self.base_host_profile_path, True)
        else:
            self.base_host_profile_path = args.host_profile

        self.options_profile_path = os.path.join(directories.profiles_dir, f'options.profile')
        print('Generating options profile...')
        self.__create_options_profile(args, self.options_profile_path)

        for build_type in ('Debug', 'Release', 'RelWithDebInfo', 'MinSizeRel'):
            profile_path = os.path.join(directories.profiles_dir, f'host-{build_type.lower()}.profile')
            if not os.path.isfile(profile_path):
                print(f'Generating host-{build_type.lower()} profile...')
                self.__create_host_profile(build_type, profile_path)
            self.host_profile_paths.append((build_type, profile_path))

    def __create_host_profile(self, build_type:str, profile_path:str):
        profile = self.__create_profile_stub()

        profile['settings'] = {}
        profile['settings']['&:build_type'] = build_type
        profile['settings']['build_type'] = "Debug" if build_type == "Debug" else "RelWithDebInfo"
        if self.compiler == 'msvc':
            profile['settings']['compiler.runtime_type'] = "Debug" if build_type == "Debug" else "Release"

        overrides_profile = os.path.join(directories.root_dir, f'profile_overrides_{build_type.lower()}.txt')

        with open(profile_path, 'w') as f:
            f.write('# This file is auto-generated. \n\n')

            f.write(f'include({self.base_host_profile_path})\n')
            f.write(f'include({self.options_profile_path})\n')

            if os.path.isfile(overrides_profile):
                f.write(f'include({overrides_profile})\n')

            f.write('\n')
            profile.write(f)

    def __create_profile_stub(self):
        profile = configparser.ConfigParser(allow_no_value=True, delimiters='=')
        profile.optionxform = str
        return profile

    def __create_options_profile(self, args, profile_path:str):
        profile = self.__create_profile_stub()

        profile['options'] = {}

        if args.options:
            for option in args.options:
                option_name, option_value = option.split('=', 1)
                profile['options'][f'&:{option_name}'] = option_value

        if args.lib_dir:
            profile['options']['&:lib_dir'] = args.lib_dir

        with open(profile_path, 'w') as f:
            f.write('# This file is auto-generated. \n\n')
            profile.write(f)

    def __detect_base_profile(self, args, profile_path:str, is_host_profile:bool):
        profile = self.__create_profile_stub()

        profile['settings'] = {
            'os': get_profile_os(),
            'arch': get_conan_arch(args.target_arch if is_host_profile else args.build_arch),
            'compiler': get_conan_compiler(args.compiler),
            'compiler.version': get_conan_compiler_version(args.compiler, args.compiler_version),
            'compiler.cppstd': '17',
        }

        if not is_host_profile:
            profile['settings']['build_type'] = 'Release'

        profile['options'] = {}
        profile['conf'] = {}

        if args.compiler == 'MSVC':
            profile['settings']['compiler.runtime'] = "dynamic"
            if not is_host_profile:
                profile['settings']['compiler.runtime_type'] = "Release"
        elif args.compiler == 'GNU':
            profile['settings']['compiler.libcxx'] = 'libstdc++11'
        elif args.compiler == 'Clang' or args.compiler == 'AppleClang':
            profile['settings']['compiler.libcxx'] = 'libc++'

        if args.min_os_version and is_host_profile:
            # Only set the minimum OS version for host profiles
            profile['settings']['os.version'] = args.min_os_version

        if sys.platform == 'darwin':
            import subprocess
            profile['conf']['tools.apple:sdk_path'] = subprocess.check_output(['xcrun', '--sdk', 'macosx', '--show-sdk-path']).decode('utf-8').strip()
            if not is_host_profile:
                # For build tools - use a higher version of macOS
                profile['settings']['os.version'] = "10.15" if get_conan_arch(args.build_arch) == 'x86_64' else "11.0"

        if args.download_cache:
            profile['conf']['tools.files.download:download_cache']= args.download_cache

        with open(profile_path, 'w') as f:
            f.write('# This file is auto-generated. \n\n')
            profile.write(f)
