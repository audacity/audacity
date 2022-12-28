from .otool import collect_rpaths
import os
import subprocess

class InstallNameTool():
    def __init__(self, frameworks_dir, path):
        self.__path = path

        if frameworks_dir == os.path.dirname(path):
            self.__loader_path = '@loader_path'
        else:
            self.__loader_path = os.path.join('@loader_path', os.path.relpath(frameworks_dir, os.path.dirname(path)))
        self.__args = []

        rpaths = collect_rpaths(path)

        for rpath in rpaths:
            self.__args.append('-delete_rpath')
            self.__args.append(rpath)


    def update_dependency(self, dependency):
        (name, lib_line, is_system) = dependency

        if is_system:
            return

        expected_lib_line = os.path.join(self.__loader_path, name)

        if lib_line != expected_lib_line:
            self.__args.append('-change')
            self.__args.append(lib_line)
            self.__args.append(expected_lib_line)

    def execute(self):
        if len(self.__args) == 0:
            return

        exec_args = ['install_name_tool'] + self.__args + [self.__path]

        print(f'Patching {self.__path}')

        try:
            subprocess.check_call(exec_args)
        except subprocess.CalledProcessError as err:
            print(f'=========\ninstall_name_tool failed with code {err.returncode}\n\tstdout: {err.stdout}\n\tstderr: {err.stderr}\n=========')
            if err.returncode == -9:
                print("install_name_tool was killed. Retrying with x86_64 architecture...")
                exec_args = ["arch", "-arch", "x86_64"] + exec_args
                try:
                    subprocess.run(exec_args, capture_output=True, check=True, text=True)
                except subprocess.CalledProcessError as inner_err:
                    print(f'=========\ninstall_name_tool failed with code {inner_err.returncode}\n\tstdout: {inner_err.stdout}\n\tstderr: {inner_err.stderr}\n=========')
                    raise
            else:
                raise

