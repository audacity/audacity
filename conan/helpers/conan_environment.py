import os
import sys
import subprocess
import venv

from helpers.directories import directories
from helpers.version import version_tuple


def __get_venv_tool_path(tool_name:str):
    if sys.platform == 'win32':
        return os.path.join(os.environ['VIRTUAL_ENV'], 'Scripts', f'{tool_name}.exe')
    else:
        return os.path.join(os.environ['VIRTUAL_ENV'], 'bin', tool_name)


def get_python():
    if os.environ['VIRTUAL_ENV']:
        return __get_venv_tool_path('python')
    else:
        return sys.executable


class ConanEnv:
    old_env_path = None
    old_home_path = None

    def __init__(self, env_path:str=None, home_path:str=None):
        self.env_path = env_path

        if not self.env_path:
            expected_path = directories.env_dir
            if os.path.exists(expected_path):
                self.env_path = expected_path

        self.home_path = home_path

        if not self.home_path and directories.conan_home_dir:
            self.home_path = directories.conan_home_dir

    def __enter__(self):
        if self.env_path and 'VIRTUAL_ENV' in os.environ:
            self.old_env_path = os.environ['VIRTUAL_ENV']

        if self.home_path and 'CONAN_HOME' in os.environ:
            self.old_home_path = os.environ['CONAN_HOME']

        if self.env_path:
            os.environ['VIRTUAL_ENV'] = self.env_path

        if self.home_path:
            os.environ['CONAN_HOME'] = self.home_path

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.env_path:
            if self.old_env_path:
                os.environ['VIRTUAL_ENV'] = self.old_env_path
            else:
                del os.environ['VIRTUAL_ENV']

        if self.home_path:
            if self.old_home_path:
                os.environ['CONAN_HOME'] = self.old_home_path
            else:
                del os.environ['CONAN_HOME']


class ConanVenvBuilder(venv.EnvBuilder):
    def post_setup(self, context):
        with ConanEnv(context.env_dir):
            cmd = [get_python(), '-m', 'pip', 'install', 'conan']
            subprocess.check_call(cmd)


def get_conan():
    if 'VIRTUAL_ENV' in os.environ:
        return __get_venv_tool_path('conan')
    elif os.path.isdir(directories.env_dir):
        with ConanEnv():
            return __get_venv_tool_path('conan')
    else:
        if sys.platform == 'win32':
            return 'conan.exe'
        else:
            return 'conan'


def get_conan_version():
    try:
        version_string = subprocess.check_output([get_conan(), '--version']).decode('utf-8').strip()
        return version_tuple(version_string.split(' ')[-1])
    except FileNotFoundError:
        return None


def upgrade_conan():
    subprocess.check_call([get_python(), '-m', 'pip', 'install', '--upgrade', 'conan'])
    return get_conan_version()


def create_conan_venv(conan_venv:str=None):
    if not conan_venv:
        conan_venv = directories.env_dir

    print(f'Creating Conan virtual environment {conan_venv}')
    builder = ConanVenvBuilder(with_pip=True)
    builder.create(conan_venv)

    directories.conan_home_dir = os.path.join(directories.build_dir, 'conan', 'home')
    os.makedirs(directories.conan_home_dir, exist_ok=True)

    with ConanEnv(conan_venv):
        return get_conan_version()


def validate_conan(required_conan_version:tuple):
    conan_venv = directories.env_dir

    if os.path.isdir(conan_venv):
        print(f'Using Conan from local environment {conan_venv}')
        with ConanEnv(conan_venv):
            conan_version = get_conan_version()

            if conan_version is None:
                conan_version = create_conan_venv(conan_venv)
            elif conan_version < required_conan_version:
                conan_version = upgrade_conan()

            # Something went wrong during `pip install`?
            if conan_version is None or conan_version < required_conan_version:
                raise RuntimeError(f'Conan version {required_conan_version} is required')

            if not directories.conan_home_dir:
                default_home_dir = os.path.join(directories.build_dir, 'conan', 'home')
                if os.path.isdir(default_home_dir):
                    directories.conan_home_dir = default_home_dir


            return conan_version
    else:
        conan_version = get_conan_version()

        if conan_version is None:
            create_conan_venv(conan_venv)
            # Perfrorm full set of checks again
            return validate_conan(required_conan_version)
        elif conan_version < required_conan_version:
            raise RuntimeError(f'Conan version {conan_version} is too old, please upgrade to {required_conan_version} or newer')

        return conan_version
