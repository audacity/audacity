from dataclasses import dataclass
from conan import ConanFile
from conan.tools.cmake import cmake_layout, CMakeDeps
from conan.tools.files import copy
from conan.tools.build import cross_building
from conan.errors import ConanInvalidConfiguration
import os
import subprocess
import textwrap

required_conan_version = ">=2.0.0"

# Fix expat rpah on macOS
def fix_expact_rpath(conanfile, filepath):
    if conanfile.settings.os != "Macos":
        return

    # Force expat ID to be @rpath/libexpat.dylib
    if 'expat' in filepath:
        filename = os.path.basename(filepath)
        conanfile.output.info(f"Setting id of {filepath} to @rpath/{filename}")
        subprocess.check_call(["install_name_tool", "-id", f"@rpath/{filename}", filepath])
        return

    deps = [dep.strip().split()[0] for dep in subprocess.check_output(["otool", "-L", filepath]).decode("utf-8").splitlines()]
    for dep in deps:
        if 'expat' not in dep:
            continue
        if not dep.startswith("@"):
            conanfile.output.info(f"=== Changing {dep} to @rpath/{os.path.basename(dep)} in {filepath} ===")
            subprocess.check_call(["install_name_tool", "-change", dep, f"@rpath/{os.path.basename(dep)}", filepath])

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
    conanfile.output.info(f"Running patchelf: {patchelf_path} {' '.join(args)}")
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
            conanfile.output.info(f"Could not copy debug symbols from {file}")

    try:
        subprocess.check_call(["objcopy", "--strip-debug", "--strip-unneeded", file])
    except subprocess.CalledProcessError:
        conanfile.output.info(f"Could not strip debug symbols from {file}")
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
    if conanfile.settings.os == "Windows":
        if len(dependency_info.cpp_info.bindirs) == 0:
            return
        copy(conanfile, "*.dll", dependency_info.cpp_info.bindirs[0], f"{conanfile.build_folder}/{conanfile.settings.build_type}")
    elif conanfile.settings.os == "Macos":
        if len(dependency_info.cpp_info.libdirs) == 0:
            return

        copied_files = copy(conanfile, "*.dylib*", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/Audacity.app/Contents/Frameworks")

        for file in copied_files:
            if not os.path.islink(file):
                try:
                    fix_expact_rpath(conanfile, file)
                except subprocess.CalledProcessError as e:
                    conanfile.output.error(f"Failed to set id of {file}: {e}")
    else:
        if len(dependency_info.cpp_info.libdirs) == 0:
            return
        # On Linux we also set the correct rpath for the copied libraries
        patchelf_path = os.path.join(conanfile.dependencies.build["patchelf"].cpp_info.bindirs[0], "patchelf")

        lib_dir = conanfile.options.lib_dir if conanfile.options.lib_dir else "lib/audacity"

        conanfile.output.info(f"Copying files from {dependency_info.cpp_info.libdirs[0]} to {conanfile.build_folder}/{lib_dir}")

        copied_files = copy(conanfile, "*.so*", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/{lib_dir}")
        for file in copied_files:
            if not os.path.islink(file):
                subprocess.check_call([patchelf_path, "--add-rpath", "$ORIGIN", file], stdin=subprocess.DEVNULL)

# Dataclass that holds the information about a dependency
@dataclass
class AudacityDependency:
    name: str
    version: str
    channel: str = None
    package_options: dict = None
    default_enabled: bool = False
    override: bool = False

    def apply_options(self, conanfile, package):
        if self.package_options is not None:
            for key, value in self.package_options.items():
                conanfile.output.info(f"\t{self.name}:{key}={value}")
                setattr(package, key, value)

    def requires(self, conanfile):
        return conanfile.requires(self.reference(conanfile))

    def tool_requires(self, conanfile):
        pass

    def reference(self, conanfile):
        return f"{self.name}/{self.version}@{self.channel}" if self.channel else f"{self.name}/{self.version}@audacity/stable"

    def copy_files(self, conanfile, dependency_info):
        global_copy_files(conanfile, dependency_info)

# Dataclass that holds the information about the wxWidgets dependency
@dataclass
class wxWidgetsAudacityDependency(AudacityDependency):
    def __init__(self, package_options: dict = None):
        super().__init__("wxwidgets", "3.1.3.4-audacity", package_options=package_options)
    override: bool = False

    def reference(self, conanfile):
        return f"{self.name}/3.1.3.4-audacity@audacity/stable"

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
            conanfile.output.info(f"\t{self.name}:{key}={value}")
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
        super().__init__("libcurl", "7.82.0", package_options=package_options)

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
        return "qt/6.3.1@audacity/stable"

    def apply_options(self, conanfile, package):
        super().apply_options(conanfile, package)

        for key, value in self._options.items():
            conanfile.output.info(f"\t{self.name}:{key}={value}")
            setattr(package, key, value)

        for module in self._enabled_modules:
            conanfile.output.info(f"\tEnabling Qt module: {module}")
            setattr(package, module, True)

        if conanfile.settings.os != "Linux":
            conanfile.options["harfbuzz"].with_glib=False
        else:
            package.qtwayland = False

        if conanfile.settings.os == 'Macos':
            conanfile.output.info("Disabling glib on macOS")
            setattr(package, "disabled_features", 'glib')

    @staticmethod
    def _content_template(conanfile, qt6_dependency_info):
        package_folder = qt6_dependency_info.package_folder.replace("\\", "/")
        host_prefix = package_folder if not cross_building(conanfile, skip_x64_x86=True) else conanfile.dependencies.direct_build["qt-tools"].package_folder

        return textwrap.dedent(f"""\
            [Paths]
            Prefix = {package_folder}
            Plugins = res/archdatadir/plugins
            Qml2Imports = res/archdatadir/qml
            Translations = res/datadir/translations
            Documentation = res/datadir/doc
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

    def __fix_crossbuild(self, conanfile):
        if not cross_building(conanfile, skip_x64_x86=True):
            return
        host_tools = conanfile.dependencies.direct_build["qt-tools"].package_folder
        conanfile.append_to_pre_file(f'set(QT_HOST_PATH "{host_tools}" CACHE STRING "Path to the Qt host tools" FORCE)')
        conanfile.append_to_post_file(f'include("{host_tools}/lib/cmake/Qt6LinguistTools/Qt6LinguistToolsMacros.cmake")')

    def copy_files(self, conanfile, dependency_info):
        self.__fix_crossbuild(conanfile)

        global_copy_files(conanfile, dependency_info)

        os.makedirs(Qt6Dependency._qtconf_dir(conanfile), exist_ok=True)

        with open(self._qtconf_path(conanfile), "w") as f:
            f.write(self._content_template(conanfile, dependency_info))

    def tool_requires(self, conanfile):
        if cross_building(conanfile, skip_x64_x86=True):
            conanfile.tool_requires("qt-tools/6.3.1@audacity/stable")

class AudacityConan(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    options = { "use_asio": [True, False], "use_jack": [True, False], "lib_dir": [None, "ANY"]}
    default_options = { "use_asio": False, "use_jack": False, "lib_dir": None }

    # List of Audacity dependencies
    _dependencies = [
        AudacityDependency("zlib", "1.2.13"),
        AudacityDependency("libpng", "1.6.39"),
        AudacityDependency("expat", "2.5.0"),
        AudacityDependency("libjpeg-turbo", "2.1.5"),
        wxWidgetsAudacityDependency(),

        AudacityDependency("libmp3lame", "3.100"),
        AudacityDependency("mpg123", "1.31.2", package_options={ "network": False }),
        AudacityDependency("libid3tag", "0.15.2b", package_options={ "shared": False }),
        AudacityDependency("wavpack", "5.6.0"),
        AudacityDependency("ogg", "1.3.5"),
        AudacityDependency("flac", "1.4.2"),
        AudacityDependency("opus", "1.4.0", override=True),
        AudacityDependency("opusfile", "0.12", package_options={ "shared": False, "http": False }),
        AudacityDependency("vorbis", "1.3.7"),
        AudacityDependency("libsndfile", "1.0.31", package_options={ "programs": False }),

        AudacityDependency("vst3sdk", "3.7.7"),

        AudacityDependency("libuuid", "1.0.3"),

        PortAudioDependency(),

        AudacityDependency("portmidi", "r234"),

        AudacityDependency("threadpool", "20140926"),
        CurlDependency(),

        AudacityDependency("rapidjson", "1.1.0"),

        AudacityDependency("breakpad", "2023.01.27"),

        CrashpadDependency("cci.20220219-audacity"),

        AudacityDependency("catch2", "2.13.8"),

        AudacityDependency("libwebp", "1.3.0"),
        AudacityDependency("libtiff", "4.5.0", package_options={
            "lzma": False, "jpeg": "libjpeg-turbo", "jbig": False,
            "libdeflate": False, "webp": False, }),

        Qt6Dependency(),
        AudacityDependency("kddockwidgets", "2.0.0-20230407-b3b45c7")
    ]

    options.update({f"use_{dependency.name}": [True, False] for dependency in _dependencies})
    default_options.update({f"use_{dependency.name}": dependency.default_enabled for dependency in _dependencies})

    _pre_find_package_file = None
    _post_find_package_file = None


    def requirements(self):
        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                if dependency.override:
                    self.requires(dependency.reference(self), override=True)
                else:
                    self.requires(dependency.reference(self))

    def build_requirements(self):
        if self.settings.os not in ["Windows", "Macos"]:
            self.build_requires("patchelf/0.13@audacity/stable")

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                dependency.tool_requires(self)

    def configure(self):
        self.options["*"].shared = True

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                self.output.info(f"Applying options for {dependency.name}...")
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
            self.output.info(f"Copying files for {dep.ref.name}...")
            if dep.ref.name in deps_lookup:
                deps_lookup[dep.ref.name].copy_files(self, dep)
            else:
                global_copy_files(self, dep)

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
