from dataclasses import dataclass
from conan import ConanFile
from conan.tools.cmake import cmake_layout, CMakeDeps
from conan.tools.files import copy
from conan.tools.build import cross_building
from conan.errors import ConanInvalidConfiguration
import os
import subprocess
import textwrap

# Why is it not Conan? Copied from cmake_layout().
def is_multi_config(conanfile):
    gen = conanfile.conf.get("tools.cmake.cmaketoolchain:generator", default=None)
    if gen:
        return "Visual" in gen or "Xcode" in gen or "Multi-Config" in gen
    else:
        compiler = conanfile.settings.get_safe("compiler")
        return compiler in ("Visual Studio", "msvc")


def is_linux_like(conanfile):
    return conanfile.settings.os not in ["Windows", "Macos"]


def run_patchelf(conanfile, args):
    if not is_linux_like(conanfile):
        raise ConanInvalidConfiguration("patchelf can only be run on Linux")

    patchelf_path = os.path.join(conanfile.dependencies.build["patchelf"].cpp_info.bindirs[0], "patchelf")
    return subprocess.check_output([patchelf_path] + args).decode("utf-8").strip()


def set_rpath(conanfile, file, rpath):
    return run_patchelf(conanfile, ["--set-rpath", rpath, file])


def append_rpath(conanfile, file, rpath):
    old_rpath = run_patchelf(conanfile, ["--print-rpath", file])
    run_patchelf(conanfile, ["--set-rpath", f"{old_rpath}:{rpath}", file])


def strip_debug_symbols(conanfile, file, keep_symbols):
    if not is_linux_like(conanfile):
        raise ConanInvalidConfiguration("patchelf can only be run on Linux")

    if keep_symbols:
        try:
            if os.path.isfile(f"{file}.debug"):
                os.remove(f"{file}.debug")
            subprocess.check_call(["objcopy", "--only-keep-debug", file, f"{file}.debug"])
        except subprocess.CalledProcessError:
            print(f"Could not copy debug symbols from {file}", flush=True)

    try:
        subprocess.check_call(["objcopy", "--strip-debug", "--strip-unneeded", file])
    except subprocess.CalledProcessError:
        print(f"Could not strip debug symbols from {file}", flush=True)
        return

    if keep_symbols and os.path.isfile(f"{file}.debug"):
        subprocess.check_call(["objcopy", "--add-gnu-debuglink", f"{file}.debug", file])


def get_build_folder(conanfile):
    build_folder = str(conanfile.build_folder)
    if is_multi_config(conanfile):
        return os.path.join(build_folder, str(conanfile.settings.build_type))
    else:
        return build_folder

def get_generators_folder(conanfile):
    build_folder = str(conanfile.build_folder)
    if not is_multi_config(conanfile):
        build_folder = os.path.join(build_folder, "..")
    return os.path.join(build_folder, "generators")


def get_macos_bundle_dir(conanfile, folder):
    return os.path.join(get_build_folder(conanfile), "Audacity.app", "Contents", folder)


def get_linux_libdir(conanfile, full_path=True):
    lib_dir = str(conanfile.options.lib_dir) if conanfile.options.lib_dir else os.path.join("lib", "audacity")
    if full_path:
        return os.path.join(get_build_folder(conanfile), lib_dir)
    else:
        return lib_dir


def safe_linux_copy(conanfile, source, destination, keep_debug_symbols=True):
    copied_files = copy(conanfile, "*.so*", source, destination)
    for file in copied_files:
        if not os.path.islink(file):
            set_rpath(conanfile, file, "$ORIGIN")
            if "libicu" not in file:
                strip_debug_symbols(conanfile, file, keep_debug_symbols)

# A helper function that correctly copies the files from the Conan package to the
# correct location in the build tree
def global_copy_files(conanfile, dependency_info):
    copy_from = dependency_info.cpp_info.libdirs if conanfile.settings.os != "Windows" else dependency_info.cpp_info.bindirs
    if len(copy_from) == 0:
        return

    if conanfile.settings.os == "Windows":
        copy(conanfile, "*.dll", dependency_info.cpp_info.bindirs[0], get_build_folder(conanfile))
    elif conanfile.settings.os == "Macos":
        copied_files = copy(conanfile, "*.dylib*", dependency_info.cpp_info.libdirs[0], get_macos_bundle_dir(conanfile, "Frameworks"))
    else:
        # On Linux we also set the correct rpath for the copied libraries
        target_dir = get_linux_libdir(conanfile)

        print(f"Copying files from {dependency_info.cpp_info.libdirs[0]} to {target_dir}", flush=True)
        safe_linux_copy(conanfile, dependency_info.cpp_info.libdirs[0], target_dir)


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

    def requires(self, conanfile):
        return conanfile.requires(self.reference(conanfile))

    def tool_requires(self, conanfile):
        pass

    def reference(self, conanfile):
        return f"{self.name}/{self.version}@{self.channel}" if self.channel else f"{self.name}/{self.version}"

    def copy_files(self, conanfile, dependency_info):
        global_copy_files(conanfile, dependency_info)

# Dataclass that holds the information about the wxWidgets dependency
@dataclass
class wxWidgetsAudacityDependency(AudacityDependency):
    def __init__(self, package_options: dict = None):
        super().__init__("wxwidgets", "3.1.3.4-audacity", package_options=package_options)

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

@dataclass
class CrashpadDependency(AudacityDependency):
    def __init__(self, version: str, package_options: dict = None):
        super().__init__(name="crashpad", version=version, package_options=package_options)

    def copy_files(self, conanfile, dependency_info):
        crashpad_handler_filename = "crashpad_handler"
        if conanfile.settings.os == "Windows":
            crashpad_handler_filename += ".exe"

        dst_path = f"{conanfile.build_folder}"
        if conanfile.settings.os == "Windows":
            dst_path += f"/{conanfile.settings.build_type}"
        elif conanfile.settings.os == "Macos":
            dst_path += "/Audacity.app/Contents/MacOS"
        else:
            dst_path += "/bin"

        copy(conanfile, crashpad_handler_filename, dependency_info.cpp_info.bindirs[0], dst_path, keep_path=False)
        super().copy_files(conanfile, dependency_info)

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
    def _content_template(conanfile, qt6_dependency_info):
        package_folder = qt6_dependency_info.package_folder.replace("\\", "/")
        host_prefix = package_folder if not cross_building(conanfile, skip_x64_x86=True) else conanfile.dependencies.direct_build["qt-tools"].package_folder

        if conanfile.settings.os in ["Windows", "Macos"]:
            return textwrap.dedent(f"""\
                [Paths]
                Prefix = {package_folder}
                Plugins = res/archdatadir/plugins
                Qml2Imports = res/archdatadir/qml
                Translations = res/datadir/translations
                Documentation = res/datadir/doc
                HostPrefix = {host_prefix}""")
        else:
            libdir = get_linux_libdir(conanfile, False)
            return textwrap.dedent(f"""\
                [Paths]
                Prefix = .
                Plugins = ../{libdir}/qt6/plugins
                Qml2Imports = ../{libdir}/qt6/qml
                HostPrefix = {host_prefix}""")

    @staticmethod
    def _qtconf_dir(conanfile):
        if conanfile.settings.os == "Windows":
            return get_build_folder(conanfile)
        elif conanfile.settings.os == "Macos":
            return os.path.join(get_macos_bundle_dir(conanfile, "Resources"))
        else:
            return os.path.join(get_build_folder(conanfile), "bin")

    @staticmethod
    def _qtconf_path(conanfile):
        return os.path.join(Qt6Dependency._qtconf_dir(conanfile),  "qt.conf")

    def __fix_windows_package(self, conanfile, dependency_info):
        if conanfile.settings.os != "Windows":
            return
        # On Windows, *:shared generates unusable Qt tooling
        # We need to copy few libraries into the package folder
        def __copy_dep(name):
            try:
                print(f"Copying {name} into the Qt package folder ({dependency_info.cpp_info.bindirs[0]})")
                copy(conanfile, "*.dll", conanfile.dependencies[name].cpp_info.bindirs[0], dependency_info.cpp_info.bindirs[0])
            finally:
                pass

        print("Fixing Qt tooling on Windows...", flush=True)

        for dep in ["pcre2", "zlib", "double-conversion"]:
            __copy_dep(dep)

    def __fix_macos_package(self, conanfile, dependency_info):
        def __copy_dep(name):
            try:
                print(f"Copying {name} into the Qt package folder ({dependency_info.cpp_info.libdirs[0]})")
                copy(conanfile, "*.dylib*", conanfile.dependencies[name].cpp_info.libdirs[0], dependency_info.cpp_info.libdirs[0])
            finally:
                pass

        for dep in ["pcre2", "zlib", "double-conversion"]:
            __copy_dep(dep)

    def __fix_linux_package(self, conanfile, dependency_info):
        if conanfile.settings.os in ["Windows", "Macos"]:
            return

        def __copy_dep(name):
            try:
                print(f"Copying {name} into the Qt package folder ({dependency_info.cpp_info.libdirs[0]})")
                safe_linux_copy(conanfile, conanfile.dependencies[name].cpp_info.libdirs[0], dependency_info.cpp_info.libdirs[0], False)
            finally:
                pass

        for dep in ["pcre2", "zlib", "double-conversion", "icu"]:
            __copy_dep(dep)

        libdir = get_linux_libdir(conanfile)
        arch_plugins_source = os.path.join(dependency_info.package_folder, "res", "archdatadir")
        arch_plugins_target = os.path.join(libdir, "qt6")
        # Copy all the plugins to the libdir/qt6 folder
        copy(conanfile, "*", arch_plugins_source, arch_plugins_target, keep_path=True)

        for root, dirs, files in os.walk(arch_plugins_target):
            for file in files:
                if file.endswith(".so"):
                    print(f"Setting RPATH of {file}")
                    relative_path = os.path.join("$ORIGIN", os.path.relpath(libdir, root))
                    set_rpath(conanfile, os.path.join(root, file), f"$ORIGIN:{relative_path}")
                    strip_debug_symbols(conanfile, os.path.join(root, file), True)

    def __fix_crossbuild(self, conanfile):
        if not cross_building(conanfile, skip_x64_x86=True):
            return
        host_tools = conanfile.dependencies.direct_build["qt-tools"].package_folder
        conanfile.append_to_pre_file(f'set(QT_HOST_PATH "{host_tools}" CACHE STRING "Path to the Qt host tools" FORCE)')


    def copy_files(self, conanfile, dependency_info):
        self.__fix_windows_package(conanfile, dependency_info)
        self.__fix_macos_package(conanfile, dependency_info)
        self.__fix_linux_package(conanfile, dependency_info)
        self.__fix_crossbuild(conanfile)

        global_copy_files(conanfile, dependency_info)

        os.makedirs(Qt6Dependency._qtconf_dir(conanfile), exist_ok=True)

        with open(self._qtconf_path(conanfile), "w") as f:
            f.write(self._content_template(conanfile, dependency_info))

    def tool_requires(self, conanfile):
        if cross_building(conanfile, skip_x64_x86=True):
            conanfile.tool_requires("qt-tools/6.3.1@audacity/testing")

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
        AudacityDependency("mpg123", "1.29.3", "audacity/testing", package_options={ "network": False }),
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

        CrashpadDependency("cci.20220219-audacity"),

        AudacityDependency("catch2", "2.13.8")
        Qt6Dependency(),
        AudacityDependency("kddockwidgets", "1.6.0", "audacity/testing")
    ]

    options.update({f"use_{dependency.name}": [True, False] for dependency in _dependencies})
    default_options.update({f"use_{dependency.name}": dependency.default_enabled for dependency in _dependencies})

    _pre_find_package_file = None
    _post_find_package_file = None


    def requirements(self):
        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                dependency.requires(self)

    def build_requirements(self):
        if self.settings.os not in ["Windows", "Macos"]:
            self.build_requires("patchelf/0.13")

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                dependency.tool_requires(self)

    def configure(self):
        self.options["*"].shared = True

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                print(f"Applying options for {dependency.name}...")
                dependency.apply_options(self, self.options[dependency.name])

    def layout(self):
        cmake_layout(self, build_folder="")
        self.folders.generators = "generators"

    def append_to_pre_file(self, text):
        if self._pre_find_package_file is None:
            self._pre_find_package_file = text
        else:
            self._pre_find_package_file += '\n' + text

    def append_to_post_file(self, text):
        if self._post_find_package_file is None:
            self._post_find_package_file = text
        else:
            self._post_find_package_file += '\n' + text

    def __get_dependency(self, name):
        try:
            return self.dependencies[name]
        except KeyError:
            return None

    def generate(self):
        deps_lookup = { dependency.name: dependency for dependency in self._dependencies }

        for dep in self.dependencies.host.values():
            print(f"Copying files for {dep.ref.name}...")
            if dep.ref.name in deps_lookup:
                deps_lookup[dep.ref.name].copy_files(self, dep)
            else:
                global_copy_files(self, dep)

        # ICU is a special case, because it's not a dependency of Audacity, but it's a dependency of Qt6
        # On Linux, ICU wont be able to locate ICU data library.
        if self.settings.os not in ["Windows", "Macos"]:
            icu_dep = self.__get_dependency("icu")
            if icu_dep:
                icu_libdir = icu_dep.cpp_info.libdirs[0]

                for file in os.listdir(icu_libdir):
                    icu_libpath = os.path.join(icu_libdir, file)
                    if os.path.isfile(icu_libpath):
                        append_rpath(self, icu_libpath, "$ORIGIN")

        deps = CMakeDeps(self)
        deps.generate()

        if self.settings.build_type == "RelWithDebInfo":
            if self._pre_find_package_file:
                file_name = os.path.join(get_generators_folder(self), "pre-find-package.cmake")
                with open(file_name, "w") as f:
                    f.write(self._pre_find_package_file)

            if self._post_find_package_file:
                file_name = os.path.join(get_generators_folder(self), "post-find-package.cmake")
                with open(file_name, "w") as f:
                    f.write(self._post_find_package_file)
