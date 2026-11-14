import argparse
import os
import subprocess
import re

def is_system_lib(path):
    return path.startswith('/System/Library/') or path.startswith('/usr/lib/')


def macos_collect_rpaths(file):
    result = []

    with subprocess.Popen(['otool', '-l', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        for line_index in range(len(lines)):
            if lines[line_index] == 'cmd LC_RPATH':
                rpath_match = re.match(r'path\s+(.*)\s+\(', lines[line_index + 2])
                if rpath_match:
                    rpath = rpath_match.group(1)
                    if rpath not in result:
                        result.append(rpath)

            line_index = line_index + 1

    return result


def get_dylib_id(file):
    with subprocess.Popen(['otool', '-D', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        if len(lines) == 2:
            return lines[1].split('/')[-1]
    return ''


def macos_collect_dependencies(file):
    result = []
    dylib_id = get_dylib_id(file)

    with subprocess.Popen(['otool', '-L', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        for line in lines:
            match = re.match(r'(.*)\s+\(', line)

            if not match:
                continue

            lib_line = match.group(1)
            if not lib_line.startswith('@loader_path') and not is_system_lib(lib_line):
                name = lib_line.split('/')[-1]
                if name != dylib_id:
                    result.append((name, lib_line))

    return result


def run_install_name_tool(file, dep_path_prefix):
    dependencies = macos_collect_dependencies(file)
    rpaths = macos_collect_rpaths(file)

    if len(dependencies) == 0 and len(rpaths) == 0:
        return

    install_name_tool = ['install_name_tool']

    for name, lib_line in dependencies:
        install_name_tool.append('-change')
        install_name_tool.append(lib_line)
        install_name_tool.append(dep_path_prefix + '/' + name)

    for rpath in rpaths:
        install_name_tool.append('-delete_rpath')
        install_name_tool.append(rpath)

    install_name_tool.append(file)

    print(' '.join(install_name_tool))
    try:
        subprocess.check_call(install_name_tool)
    except subprocess.CalledProcessError as err:
        print(f'=========\ninstall_name_tool failed with code {err.returncode}\n\tstdout: {err.stdout}\n\tstderr: {err.stderr}\n=========')
        if err.returncode != -9:
            raise

        print("install_name_tool was killed. Retrying with x86_64 architecture...")
        install_name_tool = ["arch", "-arch", "x86_64"] + install_name_tool
        try:
            subprocess.run(install_name_tool, capture_output=True, check=True, text=True)
        except subprocess.CalledProcessError as inner_err:
            print(f'=========\ninstall_name_tool failed with code {inner_err.returncode}\n\tstdout: {inner_err.stdout}\n\tstderr: {inner_err.stderr}\n=========')
            raise


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('bundle', help='The bundle to fix')
    parser.add_argument('-config', help='Configuration type')
    args = parser.parse_args()

    if (args.config == "Debug"):
       exit(0)

    bundle = args.bundle

    if not os.path.exists(bundle):
        print('Bundle not found: ' + bundle)
        exit(1)

    if not os.path.isdir(bundle):
        run_install_name_tool(bundle, '@loader_path/../Frameworks')

        if bundle.find('modules') != -1:
            exit(0)

        bundle = os.path.join(os.path.dirname(args.bundle), '..', '..')
        bundle = os.path.abspath(bundle)

    print(f"Fixing bundle {bundle}")

    frameworks_dir = os.path.join(bundle, 'Contents', 'Frameworks')

    if not os.path.exists(frameworks_dir):
        print('Bundle does not contain a Frameworks directory: ' + bundle)
        exit(1)

    for file in os.listdir(frameworks_dir):
        file_path = os.path.join(frameworks_dir, file)
        if os.path.isfile(file_path):
            run_install_name_tool(file_path, '@loader_path')
