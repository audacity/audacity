import os

class Directories:
    root_dir = os.path.realpath(os.path.join(os.path.dirname(__file__), '..'))

    build_dir = None
    lib_dir = None

    env_dir = None
    conan_home_dir = None

    profiles_dir = None

    def update(self, parsed_args):
        self.build_dir = os.path.realpath(parsed_args.build_dir)

        if parsed_args.lib_dir:
            self.lib_dir = os.path.realpath(parsed_args.lib_dir)

        self.env_dir = os.path.join(self.build_dir, 'conan', 'env')
        self.profiles_dir = os.path.join(self.build_dir, 'conan', 'profiles')


directories = Directories()
