import argparse
import os
import linux
from qt import QtConfig, QmlImports
from utils import copy_files

def collect_plugins(category, plugin_dir, matcher = None):
    category_dir = os.path.join(plugin_dir, category)

    if not os.path.exists(category_dir):
        return []

    for name in os.listdir(category_dir):
        path = os.path.join(category_dir, name)
        if os.path.isfile(path) and (matcher is None or matcher(name)):
            yield (path, os.path.join('plugins', category))

def is_lib(name):
    return name.endswith('.so')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--executable-name', required=True, type=str)
    parser.add_argument('--lib-dir', required=True, type=str)
    parser.add_argument('--source', required=True, type=str)
    parser.add_argument('--destination', required=True, type=str)
    parser.add_argument('--qrc-files', type=str, nargs='*')
    args = parser.parse_args()

    if not os.path.exists(args.source):
        raise Exception('Source directory not found: {}'.format(args.source))

    if not os.path.exists(args.destination):
        os.makedirs(args.destination)

    modules_dir = os.path.join(args.source, 'modules')

    runtime_files = [ os.path.join(args.destination, 'bin', args.executable_name) ]
    qt_files = []

    # Check only the installed libraries and modules
    lib_dir = os.path.join(args.destination, args.lib_dir)
    for library in os.listdir(lib_dir):
        if library.startswith('lib-'):
            runtime_files.append(os.path.join(lib_dir, library))

    modules_dir = os.path.join(lib_dir, 'modules')
    if os.path.exists(modules_dir):
        for module in os.listdir(modules_dir):
            if module.endswith('.so'):
                runtime_files.append(os.path.join(modules_dir, module))

    qt_conf_path = os.path.join(args.source, 'bin', 'qt.conf')

    if os.path.exists(qt_conf_path):
        config = QtConfig(qt_conf_path)

        if not os.path.isdir(config.plugins_dir):
            raise Exception('Qt plugins path not found: {}'.format(config.plugins_dir))

        qt_files.extend(collect_plugins('iconengines', config.plugins_dir))
        qt_files.extend(collect_plugins('imageformats', config.plugins_dir))
        qt_files.extend(collect_plugins('styles', config.plugins_dir))
        qt_files.extend(collect_plugins('platforms', config.plugins_dir))

        if args.qrc_files:
            qml_imports = QmlImports(config)
            for qrc_file in args.qrc_files:
                qml_imports.scan_qrc(qrc_file)
            qt_files.extend(qml_imports.dynamic_libs)
            qt_files.extend(qml_imports.resource_files)

        runtime_files += [path for path, _ in qt_files if is_lib(path)]

        with open(os.path.join(args.destination, 'bin',  'qt.conf'), 'w') as f:
            f.write('\n'.join(['[Paths]', 'Prefix = .', f'Plugins = ../{args.lib_dir}/qt6/plugins', f'Qml2Imports = ../{args.lib_dir}/qt6/qml', '']))

    dependencies = linux.collect_dependencies(runtime_files, [os.path.join(args.source, args.lib_dir)])

    target_lib_dir = os.path.join(args.destination, args.lib_dir)
    copy_files([(dependency.path, '') for dependency in dependencies if not dependency.is_system_library], target_lib_dir)

    target_qt6_resources_dir = os.path.join(args.destination, args.lib_dir, 'qt6')
    copy_files(qt_files, target_qt6_resources_dir)


