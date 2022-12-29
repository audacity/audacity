import subprocess
import re
import os

def is_system_lib(path):
    return path.startswith('/System/Library/') or path.startswith('/usr/lib/')

def collect_rpaths(file):
    result = []

    with subprocess.Popen(['otool', '-l', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        for line_index in range(len(lines)):
            if lines[line_index] == 'cmd LC_RPATH':
                rpath_match = re.match(r'path\s+(.*)\s+\(', lines[line_index + 2])
                if rpath_match:
                    rpath = rpath_match.group(1)
                    result.append(rpath)

            line_index = line_index + 1

    return result


def get_dylib_id(file):
    with subprocess.Popen(['otool', '-D', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        if len(lines) == 2:
            return lines[1].split('/')[-1]
    return ''


def collect_file_dependencies(file):
    result = []
    dylib_id = get_dylib_id(file)

    with subprocess.Popen(['otool', '-L', file], stdout=subprocess.PIPE) as p:
        lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
        for line in lines:
            match = re.match(r'(.*)\s+\(', line)
            if match:
                lib_line = match.group(1)
                name = lib_line.split('/')[-1]
                if name != dylib_id:
                    result.append((name, lib_line, is_system_lib(lib_line)))

    return result

def __resolve_lib(lib_name, lookup_paths):
    for path in lookup_paths:
        lib_path = os.path.join(path, lib_name)
        if os.path.exists(lib_path):
            return lib_path

    raise Exception(f'Could not find lib: {lib_name}')

def __recursive_collect_dependencies(name, file, lookup_paths, results):
    print(f"Collecting dependencies for {file}")
    dependencies = collect_file_dependencies(file)
    results[name] = {
        'file': file,
        'dependencies': dependencies
    }
    for name, _, is_system_lib in dependencies:
        if not is_system_lib and name not in results:
            lib_path = __resolve_lib(name, lookup_paths)
            if lib_path:
                __recursive_collect_dependencies(name, lib_path, lookup_paths, results)

def collect_dependencies(files, lookup_paths):
    results = {}
    for file in files:
        __recursive_collect_dependencies(os.path.basename(file), file, lookup_paths, results)

    return results

