from configparser import ConfigParser
import os

class QtConfig():
    def __init__(self, config_file):
        self.config = ConfigParser()
        self.config.read(config_file)

        self._prefix = self.config.get('Paths', 'Prefix')
        if not os.path.isabs(self._prefix):
            self._prefix = os.path.join(os.path.dirname(config_file), self._prefix)

        if self.config.has_option('Paths', 'HostPrefix'):
            self._host_prefix = self.config.get('Paths', 'HostPrefix')
            if not os.path.isabs(self._host_prefix):
                self._host_prefix = os.path.join(os.path.dirname(config_file), self._host_prefix)
        else:
            self._host_prefix = self._prefix

    @property
    def bin_dir(self):
        return os.path.join(self._host_prefix, "bin")

    @property
    def libexec_dir(self):
        return os.path.join(self._host_prefix, "libexec")

    @property
    def plugins_dir(self):
        if self.config.has_option('Paths', 'Plugins'):
            return os.path.join(self._prefix, self.config.get('Paths', 'Plugins'))
        elif self.config.has_option('Paths', 'ArchData'):
            return os.path.join(self._prefix, self.config.get('Paths', 'ArchData'), "plugins")
        return os.path.join(self._prefix, "plugins")

    @property
    def qml_dir(self):
        if self.config.has_option('Paths', 'Qml2Imports'):
            return os.path.join(self._prefix, self.config.get('Paths', 'Qml2Imports'))
        elif self.config.has_option('Paths', 'ArchData'):
            return os.path.join(self._prefix, self.config.get('Paths', 'ArchData'), "qml")
        return os.path.join(self._prefix, "qml")
