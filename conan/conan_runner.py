import sys
import os
sys.dont_write_bytecode = True
sys.path.append(os.path.join(os.path.dirname(__file__), '..', "scripts"))

import argparse
import glob
import hashlib
import subprocess

from utils.files import safe_rm_tree

from helpers.conan_environment import get_conan, validate_conan,  ConanEnv
from helpers.directories import directories
from helpers.remotes import validate_remotes
from helpers.profiles import Profiles

required_conan_version = (2, 0, 0)
runner_version = 2


def get_conanfile_path():
    return os.path.join(directories.root_dir, 'conanfile.py')

def get_conanfile_hash():
    with open(get_conanfile_path(), 'rb') as f:
        return hashlib.sha256(f.read()).hexdigest()

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
    parser.add_argument('--build-profile', help='Build profile', default=None)
    parser.add_argument('--host-profile', help='Host profile', default=None)

    return parser.parse_args()


def get_conan_venv(build_dir):
    return os.path.abspath(os.path.join(build_dir, 'conan_venv'))


def generate_args_string(args):
    if args.options is None:
        return ''

    return ';'.join(args.options) + ';' + get_conanfile_hash() + ';' + str(runner_version)


def migrate(args):
    old_profiles = ('profile-build.profile',
                    'profile-host-release.profile', 'profile-host-debug.profile',
                    'profile-host-minsizerel.profile', 'profile-host-relwithdebinfo.profile')

    for profile in old_profiles:
        path = os.path.join(args.build_dir, profile)
        if os.path.isfile(path):
            os.remove(path)

    for dir in glob.glob(os.path.join(directories.build_dir, 'conan-install-*')):
        safe_rm_tree(dir)


if __name__ == '__main__':
    args = init_args()
    directories.update(args)

    migrate(args)

    args_file_path = os.path.join(args.build_dir, 'conan_args.txt')
    args_string = generate_args_string(args)

    if os.path.isfile(args_file_path):
        with open(args_file_path, 'r') as args_file:
            old_args = args_file.read()
            if old_args == args_string:
                print('Skipping conan install, arguments and requirements are the same')
                sys.exit(0)

    conan_version = validate_conan(required_conan_version)
    with ConanEnv():
        print(f'Using Conan version {".".join(map(str, conan_version))} ({get_conan()})', flush=True)
        validate_remotes()
        safe_rm_tree(os.path.join(directories.build_dir, 'generators'))

        profiles = Profiles(args)

        for build_type, host_profile in profiles.host_profile_paths:
            conan_options = [
                get_conan(), 'install', directories.root_dir,
                '--build='*'' if args.force_build else '--build=missing',
                '--output-folder', args.build_dir,
                '--remote', 'audacity-recipes-conan2' if (args.force_build or args.disallow_prebuilt) else 'audacity-binaries-conan2',
                '--profile:build', profiles.build_profile_path,
                '--profile:host', host_profile,
            ]

            subprocess.check_call(conan_options)

    with open(args_file_path, 'w') as args_file:
        args_file.write(args_string)
