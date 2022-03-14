import re
import subprocess
import sys
import pathlib
import os
import shutil
import copy
import glob

class OtoolRunner:
    cache = {}
    search_paths = []

    def __init__(self, paths):
        self.search_paths = paths

    def resolve(self, file):
        for path in self.search_paths:
            fullpath = os.path.join(path, file)
            if os.path.exists(fullpath):
                return fullpath

        return None

    def run(self, file):
        if file in self.cache:
            return self.cache[file]

        result = {
            'input': file,
            'dependencies': [],
            'rpath': []
        }

        print('Processing ' + file)

        fileName = file.split('/')[-1]

        with subprocess.Popen(['otool', '-L', file], stdout=subprocess.PIPE) as p:
            for bline in p.stdout.readlines():
                line = bline.decode('utf-8')
                m = re.match(r'\s+(.*)\s+\(', line)
                if m:
                    lib_line = m.group(1)
                    if lib_line.startswith('@') or not pathlib.Path(lib_line).is_absolute():
                        name = lib_line.split('/')[-1]

                        if name != fileName:
                            path = self.resolve(name)
                            if path:
                                result['dependencies'].append({
                                    'line': lib_line,
                                    'name': name,
                                    'path': path
                                })

        with subprocess.Popen(['otool', '-l', file], stdout=subprocess.PIPE) as p:
            lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
            for line_index in range(len(lines)):
                if lines[line_index] == 'cmd LC_RPATH':
                    rpath_match = re.match(r'path\s+(.*)\s+\(', lines[line_index + 2])
                    if rpath_match:
                        rpath = rpath_match.group(1)
                        result['rpath'].append(rpath)

                line_index = line_index + 1

        self.cache[file] = result
        return result

    def fixup(self, file, path):

        def add_rpath(rpath):
            if rpath in file['rpath']:
                return []
            return ['-add_rpath', rpath]

        install_name_tool = [
            'install_name_tool',
        ]

        for dep in file['dependencies']:
            old_line = dep['line']
            new_line = "@rpath/{}".format(dep['name'])
            if old_line != new_line:
                install_name_tool = install_name_tool + [
                    '-change', old_line, new_line
                ]

        if len(install_name_tool) > 1:
            install_name_tool = install_name_tool + \
            add_rpath('@executable_path/../Frameworks') + \
            add_rpath('@loader_path')

            install_name_tool.append(path)

            print("Patching {}".format(path));
            subprocess.check_call(install_name_tool)

class DumpbinRunner:
    cache = {}
    search_paths = []

    #dumpbin = r'C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.29.30133\bin\Hostx86\x86\dumpbin.exe'
    dumpbin = r'dumpbin.exe'

    def __init__(self, paths):
        self.search_paths = paths

    def resolve(self, file):
        for path in self.search_paths:
            fullpath = os.path.join(path, file)
            if os.path.exists(fullpath):
                return fullpath

        return None


    def run(self, file):
        if file in self.cache:
            return self.cache[file]

        result = {
            'input': file,
            'dependencies': []
        }

        with subprocess.Popen([self.dumpbin, '/DEPENDENTS', file], stdout=subprocess.PIPE) as p:
            lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
            for line in lines:
                if line.endswith('.dll'):
                    path = self.resolve(line)
                    if path:
                        result['dependencies'].append({
                            'name': line,
                            'path': os.path.normpath(path)
                        })

        self.cache[file] = result
        return result

    def fixup(self, file, path):
        pass

class LDDRunner:
    cache = {}
    search_paths = []

    def __init__(self, paths):
        self.search_paths = paths

    def resolve(self, file):
        for path in self.search_paths:
            fullpath = os.path.join(path, file)
            if os.path.exists(fullpath):
                return fullpath

        return None


    def run(self, file):
        if file in self.cache:
            return self.cache[file]

        result = {
            'input': file,
            'dependencies': []
        }

        ldd_env = {'LD_LIBRARY_PATH': ':'.join(self.search_paths)}

        with subprocess.Popen(['ldd', file], stdout=subprocess.PIPE, env=ldd_env) as p:
            lines = [line.decode('utf-8').strip() for line in p.stdout.readlines()]
            for line in lines:
                m = re.match('(.*) => .*', line)

                if m:
                    lib_name = m.group(1)
                    path = self.resolve(lib_name)
                    if path:
                        result['dependencies'].append({
                            'name': lib_name,
                            'path': os.path.normpath(path)
                        })

        self.cache[file] = result
        return result

    def fixup(self, file, path):
        pass

def parse_args():
    args = {
        'include_dirs': []
    }

    inputs = []

    argsCount = len(sys.argv)

    argIndex = 1

    while argIndex < argsCount:
        value = sys.argv[argIndex]
        argIndex = argIndex + 1

        if value == '-i':
            args['include_dirs'].append(sys.argv[argIndex])
            argIndex = argIndex + 1
        elif value == '-o':
            args['output'] = sys.argv[argIndex]
            argIndex = argIndex + 1
        elif value == '-g':
            inputs.extend(glob.glob(sys.argv[argIndex], recursive=True))
            argIndex = argIndex + 1
        else:
            inputs.append(value)

    args['inputs'] = [os.path.normpath(p) for p in inputs]

    return args


def collect_dependencies(runner, input_files):
    files = {}

    stack = copy.copy(input_files)

    while len(stack) > 0:
        file = stack.pop()

        if file in files:
            continue

        result = runner.run(file)
        files[file] = result

        for lib in result['dependencies']:
            stack.append(lib['path'])

    return files


def create_runner(include_dir):
    if sys.platform.startswith('darwin'):
        return OtoolRunner(include_dir)
    elif sys.platform.startswith('win32'):
        return DumpbinRunner(include_dir)
    elif sys.platform.startswith('cygwin'):
        return DumpbinRunner(include_dir)
    else:
        return LDDRunner(include_dir)


args = parse_args()

print('============== fixup.libs.py')
print(args)

print('============================')

runner = create_runner(args['include_dirs'])

files = collect_dependencies(runner, args['inputs'])

for path in files:
    file = files[path]

    if not os.path.exists(args['output']):
        os.mkdir(args['output'])

    if path not in args['inputs']:
        target_path = os.path.join(args['output'], path.split(os.sep)[-1])
        print("Copying {} -> {}".format(path, target_path))
        shutil.copy2(path, target_path, follow_symlinks=True)
        path = target_path

    runner.fixup(file, path)
