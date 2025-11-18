from dataclasses import dataclass
from conan import ConanFile
from conan.tools.cmake import cmake_layout
from conan.tools.files import copy
import os
import subprocess

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

    def reference(self, conanfile):
        return f"{self.name}/{self.version}@{self.channel}" if self.channel else f"{self.name}/{self.version}@audacity/stable"

    def copy_files(self, conanfile, dependency_info):
        global_copy_files(conanfile, dependency_info)

# Dataclass that holds the information about the wxWidgets dependency
@dataclass
class wxWidgetsAudacityDependency:
    name: str = "wxwidgets"
    default_enabled: bool = False
    override: bool = False

    def reference(self, conanfile):
        return f"{self.name}/3.1.3.8-audacity@audacity/stable"

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
        super().__init__("libcurl", "8.17.0", package_options=package_options)

    def apply_options(self, conanfile, package):
        super().apply_options(conanfile, package)

        if conanfile.settings.os == "Windows":
            package.with_ssl = "schannel"
        else:
            package.with_ssl = "openssl"



class AudacityConan(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps"
    options = { "use_asio": [True, False], "use_jack": [True, False], "lib_dir": [None, "ANY"]}
    default_options = { "use_asio": False, "use_jack": False, "lib_dir": None }

    # List of Audacity dependencies
    _dependencies = [
        AudacityDependency("zlib", "1.3.1"),
        AudacityDependency("libpng", "1.6.50"),
        AudacityDependency("expat", "2.5.0"),
        AudacityDependency("libjpeg-turbo", "2.1.5"),
        wxWidgetsAudacityDependency(),

        AudacityDependency("libmp3lame", "3.100"),
        AudacityDependency("mpg123", "1.31.2", package_options={ "network": False }),
        AudacityDependency("libid3tag", "0.15.2b", package_options={ "shared": False }),
        AudacityDependency("wavpack", "5.6.0"),
        AudacityDependency("ogg", "1.3.5"),
        AudacityDependency("flac", "1.4.2"),
        AudacityDependency("opus", "1.5.2", override=True),
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

        AudacityDependency("catch2", "2.13.8")
    ]

    options.update({f"use_{dependency.name}": [True, False] for dependency in _dependencies})
    default_options.update({f"use_{dependency.name}": dependency.default_enabled for dependency in _dependencies})

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

    def configure(self):
        self.options["*"].shared = True

        for dependency in self._dependencies:
            if getattr(self.options, f"use_{dependency.name}"):
                self.output.info(f"Applying options for {dependency.name}...")
                dependency.apply_options(self, self.options[dependency.name])

    def layout(self):
        cmake_layout(self, build_folder="")
        self.folders.generators = "generators"

    def generate(self):
        deps_lookup = { dependency.name: dependency for dependency in self._dependencies }

        for dep in self.dependencies.host.values():
            self.output.info(f"Copying files for {dep.ref.name}...")
            if dep.ref.name in deps_lookup:
                deps_lookup[dep.ref.name].copy_files(self, dep)
            else:
                global_copy_files(self, dep)
