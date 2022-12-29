import argparse
import os
from windows import dumpbin
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
            yield (path, os.path.join('qtplugins', category))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--use-objdump', action='store_true')
    parser.add_argument('--executable', required=True, type=str)
    parser.add_argument('--source', required=True, type=str)
    parser.add_argument('--destination', required=True, type=str)
    parser.add_argument('--dumpbin-hint', type=str)
    parser.add_argument('--qrc-files', type=str, nargs='*')
    args = parser.parse_args()

    if not os.path.exists(args.executable):
        raise Exception('Executable not found: {}'.format(args.executable))

    if not os.path.exists(args.source):
        raise Exception('Source directory not found: {}'.format(args.source))

    if not os.path.exists(args.destination):
        os.makedirs(args.destination)

    if args.dumpbin_hint:
        dumpbin.set_dumpbin_hint(args.dumpbin_hint)

    runtime_files = [(args.executable, "")]
    resource_files = []

    modules_dir = os.path.join(args.source, 'modules')
    if os.path.isdir(modules_dir):
        runtime_files.extend([(os.path.join(modules_dir, f), "modules") for f in os.listdir(modules_dir)])

    qt_conf_path = os.path.join(args.source, 'qt.conf')

    if os.path.exists(qt_conf_path):
        config = QtConfig(qt_conf_path)

        if not os.path.isdir(config.plugins_dir):
            raise Exception('Qt plugins path not found: {}'.format(config.plugins_dir))

        runtime_files.extend(collect_plugins('iconengines', config.plugins_dir))
        runtime_files.extend(collect_plugins('imageformats', config.plugins_dir))
        runtime_files.extend(collect_plugins('styles', config.plugins_dir))
        runtime_files.extend(collect_plugins('platforms', config.plugins_dir, lambda name: name.startswith('qwindows') or name.startswith('qdirect2d')))

        if args.qrc_files:
            qml_imports = QmlImports(config)
            for qrc_file in args.qrc_files:
                qml_imports.scan_qrc(qrc_file)
            runtime_files.extend(qml_imports.dynamic_libs)
            resource_files.extend(qml_imports.resource_files)

        with open(os.path.join(args.destination, 'qt.conf'), 'w') as f:
            f.write('\n'.join(['[Paths]', 'Prefix = .', 'Plugins = qtplugins', 'Qml2Imports = qml', '']))


    def __collect_dependencies(files, dirs):
        if args.use_objdump:
            return linux.collect_dependencies(files, dirs)
        else:
            return dumpbin.collect_dependencies(files, dirs)

    dependencies = __collect_dependencies([file[0] for file in runtime_files], [args.source])

    copy_files(runtime_files, args.destination)
    copy_files([(f.path, "") for f in dependencies if not f.is_system_library], args.destination)
    copy_files(resource_files, args.destination)

