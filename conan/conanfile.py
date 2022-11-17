from dataclasses import dataclass
from conan import ConanFile
from conan.tools.cmake import cmake_layout
from conan.tools.files import copy
from conan.errors import ConanInvalidConfiguration
import os
import subprocess
import textwrap

def run_patchelf(conanfile, args):
    if conanfile.settings.os != "Linux":
        raise ConanInvalidConfiguration("patchelf can only be run on Linux")

    patchelf_path = os.path.join(conanfile.dependencies.build["patchelf"].cpp_info.bindirs[0], "patchelf")
    return subprocess.check_output([patchelf_path] + args).decode("utf-8").strip()

def append_rpath(conanfile, file, rpath):
    if conanfile.settings.os != "Linux":
        raise ConanInvalidConfiguration("patchelf can only be run on Linux")

    old_rpath = run_patchelf(conanfile, ["--print-rpath", file])
    run_patchelf(conanfile, ["--set-rpath", f"{old_rpath}:{rpath}", file])

# A helper function that correctly copies the files from the Conan package to the
# correct location in the build tree
def global_copy_files(conanfile, dependency_info):
    if len(dependency_info.cpp_info.libdirs) == 0:
        return

    if conanfile.settings.os == "Windows":
        copy(conanfile, "*.dll", dependency_info.cpp_info.bindirs[0], f"{conanfile.build_folder}/{conanfile.settings.build_type}")
    elif conanfile.settings.os == "Macos":
        copied_files = copy(conanfile, "*.dylib*", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/Audacity.app/Contents/Frameworks")
    elif conanfile.settings.os:
        # On Linux we also set the correct rpath for the copied libraries

        lib_dir = conanfile.options.lib_dir if conanfile.options.lib_dir else "lib/audacity"

        print(f"Copying files from {dependency_info.cpp_info.libdirs[0]} to {conanfile.build_folder}/{lib_dir}", flush=True)

        copied_files = copy(conanfile, "*.so*", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/{lib_dir}")
        for file in copied_files:
            if not os.path.islink(file):
                run_patchelf(conanfile, ["--add-rpath", "$ORIGIN", file])


# Dataclass that holds the information about a dependency
@dataclass
class AudacityDependency:
    name: str
    version: str
    channel: str = None
    package_options: dict = None
    default_enabled: bool = False

    def apply_options(self, conanfile, package):
        if self.package_options is not None:
            for key, value in self.package_options.items():
                print(f"\t{self.name}:{key}={value}")
                setattr(package, key, value)

    def reference(self, conanfile):
        return f"{self.name}/{self.version}@{self.channel}" if self.channel else f"{self.name}/{self.version}"

    def copy_files(self, conanfile, dependency_info):
        global_copy_files(conanfile, dependency_info)

# Dataclass that holds the information about the wxWidgets dependency
@dataclass
class wxWidgetsAudacityDependency:
    name: str = "wxwidgets"
    default_enabled: bool = False

    def reference(self, conanfile):
        return f"{self.name}/3.1.3.4-audacity"

    def apply_options(self, conanfile, package):
        opts = [
            ("zlib", "zlib" if conanfile.options.use_zlib else "sys"),
            ("expat", "expat" if conanfile.options.use_expat else "sys"),
            ("png", "libpng" if conanfile.options.use_libpng else "sys"),
            ("jpeg", "libjpeg-turbo" if getattr(conanfile.options, "use_libjpeg-turbo") else "sys"),
            ("tiff", "off"),
            ("compatibility", 3.0),
            ("secretstore", False), ("opengl", False), ("propgrid", False), ("ribbon", False),
            ("richtext", False), ("stc", False), ("webview", False), ("help", False),
            ("html_help", False), ("fs_inet", False), ("protocol", False),
        ]

        for key, value in opts:
            print(f"\t{self.name}:{key}={value}")
            setattr(package, key, value)

    def copy_files(self, conanfile, dependency_info):
        if conanfile.settings.os == "Windows":
            copy(conanfile, "*.dll", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/{conanfile.settings.build_type}", keep_path=False)
        else:
            global_copy_files(conanfile, dependency_info)

# PortAudio has addittional options that need to be set
@dataclass
class PortAudioDependency(AudacityDependency):
    def __init__(self, package_options: dict = None):
        super().__init__("portaudio", "19.7.0", package_options=package_options)

    def apply_options(self, conanfile, package):
        super().apply_options(conanfile, package)

        if conanfile.settings.os == "Windows":
            package.with_asio = conanfile.options.use_asio
            package.with_wdmks = False

        if conanfile.settings.os != "Macos":
            package.with_jack = conanfile.options.use_jack


# Curl needs the propper TLS backend set
@dataclass
class CurlDependency(AudacityDependency):
    def __init__(self, package_options: dict = None):
        super().__init__("libcurl", "7.75.0", package_options=package_options)

    def apply_options(self, conanfile, package):
        super().apply_options(conanfile, package)

        if conanfile.settings.os == "Windows":
            package.with_ssl = "schannel"
        elif conanfile.settings.os == "Macos":
            package.with_ssl = "darwinssl"
        else:
            package.with_ssl = "openssl"



@dataclass
class Qt6Dependency(AudacityDependency):
    _options = {
        "opengl": "no",
        "openssl": False,
        "with_libjpeg": "libjpeg-turbo",
        "with_sqlite3": False,
        "with_pq": False,
        "with_odbc": False,
        "with_brotli": False,
        "with_openal": False,
        "with_md4c": False,
    }

    _enabled_modules = [
        "qtsvg", "qtdeclarative", "qttools", "qttranslations",
        "qtquicktimeline", "qtlottie",
        "qtimageformats", "qtlanguageserver", "qtshadertools"
    ]

    def __init__(self, package_options: dict = None):
        super().__init__("qt", "6.3.1", package_options=package_options)

    def reference(self, conanfile):
        return "qt/6.3.1@audacity/testing"

    def apply_options(self, conanfile, package):
        super().apply_options(conanfile, package)

        for key, value in self._options.items():
            print(f"\t{self.name}:{key}={value}")
            setattr(package, key, value)

        for module in self._enabled_modules:
            print(f"\tEnabling Qt module: {module}")
            setattr(package, module, True)

        if conanfile.settings.os != "Linux":
            conanfile.options["harfbuzz"].with_glib=False
        else:
            package.qtwayland = False

    @staticmethod
    def _content_template(path, folder, os_):
        return textwrap.dedent("""\
            [Paths]
            Prefix = {0}
            ArchData = {1}/archdatadir
            HostData = {1}/archdatadir
            Data = {1}/datadir
            Sysconf = {1}/sysconfdir
            LibraryExecutables = {1}/archdatadir/{2}
            HostLibraryExecutables = bin
            Plugins = {1}/archdatadir/plugins
            Imports = {1}/archdatadir/imports
            Qml2Imports = {1}/archdatadir/qml
            Translations = {1}/datadir/translations
            Documentation = {1}/datadir/doc""").format(path, folder,
                "bin" if os_ == "Windows" else "libexec")

    @staticmethod
    def _qtconf_path(conanfile):
        if conanfile.settings.os == "Windows":
            return f"{conanfile.build_folder}/{conanfile.settings.build_type}/qt.conf"
        elif conanfile.settings.os == "Macos":
            return f"{conanfile.build_folder}/Audacity.app/Contents/Resources/qt.conf"
        else:
            return f"{conanfile.build_folder}/bin/qt.conf"

    def __fix_windows_package(self, conanfile, dependency_info):
        if conanfile.settings.os != "Windows":
            return
        # On Windows, *:shared generates unusable Qt tooling
        # We need to copy few libraries into the package folder
        def __copy_dep(name):
            if conanfile.dependencies[name]:
                print(f"Copying {name} into the Qt package folder ({dependency_info.cpp_info.bindirs[0]})")
                copy(conanfile, "*.dll", conanfile.dependencies[name].cpp_info.bindirs[0], dependency_info.cpp_info.bindirs[0])

        print("Fixing Qt tooling on Windows...", flush=True)

        for dep in ["pcre2", "zlib", "double-conversion"]:
            __copy_dep(dep)

    def __fix_macos_package(self, conanfile, dependency_info):
        pass

    def __fix_linux_package(self, conanfile, dependency_info):
        if conanfile.settings.os == "Windows" or conanfile.settings.os == "Macos":
            return

        for root, dirs, files in os.walk(os.path.join(dependency_info.package_folder, "res", "archdatadir")):
            for file in files:
                if file.endswith(".so"):
                    print(f"Appending RPATH to {file}")
                    append_rpath(conanfile, os.path.join(root, file), f"$ORIGIN:{dependency_info.cpp_info.libdirs[0]}")

        if conanfile.dependencies["icu"]:
            icu_libdir= conanfile.dependencies["icu"].cpp_info.libdirs[0]

            for file in os.listdir(icu_libdir):
                icu_libpath = os.path.join(icu_libdir, file)
                if os.path.isfile(icu_libpath):
                    append_rpath(conanfile, icu_libpath, "$ORIGIN")


    def copy_files(self, conanfile, dependency_info):
        self.__fix_windows_package(conanfile, dependency_info)
        self.__fix_macos_package(conanfile, dependency_info)
        self.__fix_linux_package(conanfile, dependency_info)

        global_copy_files(conanfile, dependency_info)

        with open(self._qtconf_path(conanfile), "w") as f:
            f.write(self._content_template(
                dependency_info.package_folder.replace("\\", "/"),
                "res", conanfile.settings.os))

class AudacityConan(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"
    options = { "use_asio": [True, False], "use_jack": [True, False], "lib_dir": [None, "ANY"]}
    default_options = { "use_asio": False, "use_jack": False, "lib_dir": None }

    # List of Audacity dependencies
    _dependencies = [
        AudacityDependency("zlib", "1.2.11"),
        AudacityDependency("libpng", "1.6.37"),
        AudacityDependency("expat", "2.2.9", "audacity/stable"),
        AudacityDependency("libjpeg-turbo", "2.0.5", package_options={ "SIMD": False }),
        wxWidgetsAudacityDependency(),

        AudacityDependency("libmp3lame", "3.100"),
        AudacityDependency("mpg123", "1.29.3", package_options={ "network": False }),
        AudacityDependency("libmad", "0.15.2b-1", package_options={ "shared": False }),
        AudacityDependency("libid3tag", "0.15.2b", "audacity/stable", package_options={ "shared": False }),
        AudacityDependency("wavpack", "5.4.0"),
        AudacityDependency("ogg", "1.3.4"),
        AudacityDependency("flac", "1.3.3"),
        AudacityDependency("opus", "1.3.1"),
        AudacityDependency("vorbis", "1.3.7"),
        AudacityDependency("libsndfile", "1.0.31"),

        AudacityDependency("vst3sdk", "3.7.3"),

        AudacityDependency("libuuid", "1.0.3"),

        PortAudioDependency(),

        AudacityDependency("portmidi", "r234"),

        AudacityDependency("threadpool", "20140926"),
        CurlDependency(),

        AudacityDependency("rapidjson", "1.1.0"),

        AudacityDependency("breakpad", "0.1"),

        AudacityDependency("catch2", "2.13.8"),

        Qt6Dependency()
    ]

    options.update({f"use_{dependency.name}": [True, False] for dependency in _dependencies})
    default_options.update({f"use_{dependency.name}": dependency.default_enabled for dependency in _dependencies})

    _additional_cmake_modules = []

    def requirements(self):
        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                self.requires(dependency.reference(self))

    def build_requirements(self):
        if self.settings.os not in ["Windows", "Macos"]:
            self.build_requires("patchelf/0.13")

    def configure(self):
        self.options["*"].shared = True

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                print(f"Applying options for {dependency.name}...")
                dependency.apply_options(self, self.options[dependency.name])

    def layout(self):
        cmake_layout(self, build_folder="")

    def add_additional_cmake_module(self, module):
        if os.path.exists(module):
            self._additional_cmake_modules.append(module)

    def generate(self):
        deps_lookup = { dependency.name: dependency for dependency in self._dependencies }

        for dep in self.dependencies.host.values():
            print(f"Copying files for {dep.ref.name}...")
            if dep.ref.name in deps_lookup:
                deps_lookup[dep.ref.name].copy_files(self, dep)
            else:
                global_copy_files(self, dep)

        modules_dir = os.path.join(self.build_folder, self.folders.generators, "modules")

        print(f"Copying additional CMake modules to {modules_dir}...", flush=True)

        if not os.path.exists(modules_dir):
            os.makedirs(modules_dir)

        if self.settings.build_type == "RelWithDebInfo":
            file_name = os.path.join(modules_dir, "conan_modules.cmake")

            with open(file_name, "w") as f:
                for module in self._additional_cmake_modules:
                    module = module.replace("\\", "/")
                    f.write(f'include("{module}")\n')
