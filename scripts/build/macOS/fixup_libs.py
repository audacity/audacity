from linecache import cache
import re
import subprocess
import sys
import pathlib
import os
import shutil

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
                    print(f"\tFound RPATH command at line {line_index}");
                    rpath_match = re.match(r'path\s+(.*)\s+\(', lines[line_index + 2])
                    if rpath_match:
                        rpath = rpath_match.group(1)
                        result['rpath'].append(rpath)
                
                line_index = line_index + 1
            
        cache[file] = result
        return result



def parse_args():
    args = {
        'include_dirs': []
    }

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
        else:
            args['input'] = value

    return args

def collect_dependencies(runner, input_file):
    files = {}

    stack = [ input_file ]

    while len(stack) > 0:
        file = stack.pop()

        if file in files:
            continue
            
        result = runner.run(file)
        files[file] = result

        for lib in result['dependencies']:
            stack.append(lib['path'])

    return files

def add_rpath(file, rpath):
    if rpath in file['rpath']:
        return []
    
    return ['-add_rpath', rpath]


args = parse_args()

otool = OtoolRunner(args['include_dirs'])
files = collect_dependencies(otool, args['input'])

for path in files:
    file = files[path]

    if path != args['input']:
        target_path = os.path.join(args['output'], path.split('/')[-1])
        print(f"Copying {path} -> {target_path}")
        shutil.copy2(path, target_path, follow_symlinks=True)
        path = target_path
    
    install_name_tool = [
        'install_name_tool',
    ]

    for dep in file['dependencies']:
        old_line = dep['line']
        new_line = f"@rpath/{dep['name']}"
        if old_line != new_line:
            install_name_tool = install_name_tool + [
                '-change', old_line, new_line
            ]

    if len(install_name_tool) > 1:
        install_name_tool = install_name_tool + \
        add_rpath(file, '@executable_path/../Frameworks') + \
        add_rpath(file, '@loader_path')

        install_name_tool.append(path)

        print(f"Patching {path}");
        subprocess.check_call(install_name_tool)
