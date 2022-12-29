import argparse
import os
import shutil
from macOS import otool, InstallNameTool
from qt import QtConfig, QmlImports
from utils import copy_files, copy_file

class MacosBundleFolder():
    def __init__(self, path):
        self.__path = path

    @property
    def contents(self):
        return os.path.join(self.__path, 'Contents')

    @property
    def macos(self):
        return os.path.join(self.contents, 'MacOS')

    @property
    def resources(self):
        return os.path.join(self.contents, 'Resources')

    @property
    def plugins(self):
        return os.path.join(self.contents, 'PlugIns')

    @property
    def audacity_plugins(self):
        return os.path.join(self.contents, 'plug-ins')

    @property
    def frameworks(self):
        return os.path.join(self.contents, 'Frameworks')

    @property
    def modules(self):
        return os.path.join(self.contents, 'modules')

    @property
    def nyquist(self):
        return os.path.join(self.contents, 'nyquist')

    @property
    def qt_conf(self):
        return os.path.join(self.resources, 'qt.conf')

    @property
    def info_plist(self):
        return os.path.join(self.contents, 'Info.plist')

    @property
    def qml_resources(self):
        return os.path.join(self.resources, 'qml')

def is_image_format_allowed(name):
    formats = ['svg', 'png', 'jpg', 'jpeg', 'gif', 'ico']
    return any(format in name for format in formats)


def collect_plugins(category, plugin_dir, matcher = None):
    category_dir = os.path.join(plugin_dir, category)

    if not os.path.exists(category_dir):
        return []

    for name in os.listdir(category_dir):
        path = os.path.join(category_dir, name)
        if os.path.isfile(path) and (matcher is None or matcher(name)):
            yield (path, os.path.join('PlugIns', category))


def is_lib(name):
    return name.endswith('.dylib')


def copy_folder(source, destination):
    if not os.path.exists(destination):
        os.makedirs(destination)

    for root, dirs, files in os.walk(source):
        for file in files:
            source_file = os.path.join(root, file)
            destination_file = os.path.join(destination, file)
            copy_file(source_file, destination_file)

        for dir in dirs:
            source_dir = os.path.join(root, dir)
            destination_dir = os.path.join(destination, dir)
            copy_folder(source_dir, destination_dir)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--source', required=True, type=str)
    parser.add_argument('--dest', required=True, type=str)
    parser.add_argument('--qrc-files', type=str, nargs='*')
    args = parser.parse_args()

    source_path = os.path.abspath(args.source)

    if not os.path.isdir(source_path) or not source_path.endswith('.app'):
        raise Exception('Source directory not found or not a bundle: {}'.format(args.source))

    dest_path = os.path.join(os.path.abspath(args.dest), os.path.basename(source_path))

    if not os.path.exists(dest_path):
        os.makedirs(dest_path)

    source = MacosBundleFolder(args.source)
    destination = MacosBundleFolder(dest_path)

    copy_folder(source.macos, destination.macos)
    copy_folder(source.resources, destination.resources)
    copy_folder(source.audacity_plugins, destination.audacity_plugins)
    copy_folder(source.nyquist, destination.nyquist)
    copy_folder(source.modules, destination.modules)

    if os.path.exists(destination.info_plist):
        os.remove(destination.info_plist)
    shutil.copy2(source.info_plist, destination.info_plist)

    runtime_files = [os.path.join(destination.macos, name) for name in os.listdir(destination.macos)]
    runtime_files.extend([os.path.join(destination.modules, name) for name in os.listdir(destination.modules)])

    qt_plugins = []
    qml_resources = []
    qml_plugins = []

    if os.path.exists(source.qt_conf):
        config = QtConfig(source.qt_conf)

        if not os.path.isdir(config.plugins_dir):
            raise Exception('Qt plugins path not found: {}'.format(config.plugins_dir))

        qt_plugins.extend(collect_plugins('iconengines', config.plugins_dir))
        qt_plugins.extend(collect_plugins('imageformats', config.plugins_dir, lambda name: is_image_format_allowed(name)))
        qt_plugins.extend(collect_plugins('styles', config.plugins_dir))
        qt_plugins.extend(collect_plugins('platforms', config.plugins_dir))

        if args.qrc_files:
            qml_imports = QmlImports(config)
            for qrc_file in args.qrc_files:
                qml_imports.scan_qrc(qrc_file)
            qml_plugins.extend(qml_imports.dynamic_libs)
            qml_resources.extend(qml_imports.resource_files)

        copy_files(qml_resources, destination.resources)
        copy_files(qml_plugins, destination.resources)
        copy_files(qt_plugins, destination.contents)

        runtime_files.extend([os.path.join(destination.contents, prefix, os.path.basename(name)) for name, prefix in qt_plugins])
        runtime_files.extend([os.path.join(destination.resources, prefix, os.path.basename(name)) for name, prefix in qml_plugins])

        with open(destination.qt_conf, 'w') as f:
            f.write('\n'.join(['[Paths]', f'Plugins = PlugIns', f'Imports = Resources/qml' f'Qml2Imports = Resources/qml', '']))

    dependencies = otool.collect_dependencies(runtime_files, [ source.frameworks ])

    if not os.path.isdir(destination.frameworks):
        os.makedirs(destination.frameworks)

    for name, dependency in dependencies.items():
        file_path = dependency['file']

        if otool.is_system_lib(dependency['file']):
            continue

        if file_path.startswith(source.frameworks):
            destination_path = os.path.join(destination.frameworks, os.path.basename(file_path))
            copy_file(file_path, destination_path)
            file_path = destination_path

        install_name_tool = InstallNameTool(destination.frameworks, file_path)

        for dep in dependency['dependencies']:
            install_name_tool.update_dependency(dep)

        install_name_tool.execute()
