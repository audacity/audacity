from dataclasses import dataclass
from conan import ConanFile
from conan.tools.cmake import cmake_layout
from conan.tools.files import copy
import os
import re
import subprocess

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
    elif conanfile.settings.os:
        if len(dependency_info.cpp_info.libdirs) == 0:
            return
        # On Linux we also set the correct rpath for the copied libraries
        patchelf_path = os.path.join(conanfile.dependencies.build["patchelf"].cpp_info.bindirs[0], "patchelf")

        lib_dir = conanfile.options.lib_dir if conanfile.options.lib_dir else "lib/audacity"

        print(f"Copying files from {dependency_info.cpp_info.libdirs[0]} to {conanfile.build_folder}/{lib_dir}", flush=True)

        copied_files = copy(conanfile, "*.so*", dependency_info.cpp_info.libdirs[0], f"{conanfile.build_folder}/{lib_dir}")
        for file in copied_files:
            if not os.path.islink(file):
                subprocess.check_call([patchelf_path, "--add-rpath", "$ORIGIN", file])

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

        CrashpadDependency("cci.20220219-audacity"),

        AudacityDependency("catch2", "2.13.8")
    ]

    options.update({f"use_{dependency.name}": [True, False] for dependency in _dependencies})
    default_options.update({f"use_{dependency.name}": dependency.default_enabled for dependency in _dependencies})

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
        self.folders.generators = "generators"

    def generate(self):
        deps_lookup = { dependency.name: dependency for dependency in self._dependencies }

        for dep in self.dependencies.host.values():
            print(f"Copying files for {dep.ref.name}...")
            if dep.ref.name in deps_lookup:
                deps_lookup[dep.ref.name].copy_files(self, dep)
            else:
                global_copy_files(self, dep)
