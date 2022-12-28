import os
import platform
import subprocess
import json

def _is_library_file(file):
    return file.endswith('.dll') or file.endswith('.so') or file.endswith('.dylib')

class QmlModule():
    def __init__(self, module_dict):
        self._name = module_dict['name']
        print(f"Found QML module {self._name}")
        self._prefix = os.path.join('qml', module_dict['relativePath'])
        self._path = module_dict['path']
        self._components = module_dict['components'] if 'components' in module_dict else []
        self._configs = [ os.path.join(self._path, 'qmldir') ]
        if os.path.isfile(os.path.join(self._path, 'plugins.qmltypes')):
            self._configs.append(os.path.join(self._path, 'plugins.qmltypes'))
        self._dynamic_libs = [os.path.join(self._path, name) for name in os.listdir(self._path) if _is_library_file(name)]


    @property
    def resource_files(self):
        for file in self._configs:
            yield (file, self._prefix)
        for component in self._components:
            rel_path = os.path.relpath(component, self._path)
            prefix = os.path.join(self._prefix, os.path.dirname(rel_path))
            yield(component, prefix)

    @property
    def dynamic_libs(self):
        for lib in self._dynamic_libs:
            yield (lib, self._prefix)


class QmlImports():
    __import_scanner = None
    def __init__(self, config):
        self.config = config
        self._imports = {}

    @property
    def qmlscanner(self):
        if self.__import_scanner:
            return self.__import_scanner

        name = 'qmlimportscanner' + ('.exe' if platform.system() == "Windows" else '')

        if os.path.exists(os.path.join(self.config.bin_dir, name)):
            self.__import_scanner = os.path.join(self.config.bin_dir, name)
            return self.__import_scanner

        if os.path.exists(os.path.join(self.config.libexec_dir, name)):
            self.__import_scanner = os.path.join(self.config.libexec_dir, name)
            return self.__import_scanner

    def _process_imports(self, output_json):
        for module in output_json:
            if module['type'] != 'module' or 'relativePath' not in module:
                continue

            if module['name'] in self._imports:
                continue

            self._imports[module['name']] = QmlModule(module)


    def scan_qrc(self, qrc_file):
        data = subprocess.check_output([self.qmlscanner, '-qrcFiles', qrc_file, '-importPath', self.config.qml_dir])
        self._process_imports(json.loads(data))

    @property
    def resource_files(self):
        for module in self._imports.values():
            for file in module.resource_files:
                yield file

    @property
    def dynamic_libs(self):
        for module in self._imports.values():
            for lib in module.dynamic_libs:
                yield lib

