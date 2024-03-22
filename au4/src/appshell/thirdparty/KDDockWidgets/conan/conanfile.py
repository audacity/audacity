#
# This file is part of KDDockWidgets.
#
# SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only
#
# Contact KDAB at <info@kdab.com> for commercial licensing options.
#

from conans import ConanFile, CMake, tools

class KDDockWidgetsConan(ConanFile):
    name = "kddockwidgets"
    version = "1.4.0"
    default_user = "kdab"
    default_channel = "stable"
    license = ("https://raw.githubusercontent.com/KDAB/KDDockWidgets/master/LICENSES/GPL-2.0-only.txt",
               "https://raw.githubusercontent.com/KDAB/KDDockWidgets/master/LICENSES/GPL-3.0-only.txt")
    author = "Klaralvdalens Datakonsult AB (KDAB) info@kdab.com"
    url = "https://github.com/KDAB/KDDockWidgets"
    description = "Advanced Dock Widget Framework for Qt"
    generators = "cmake"
    topics = ("qt", "dockwidget" , "kdab")
    settings = "os", "compiler", "build_type", "arch"
    options = {
        "qt_version": "ANY",
        "build_static": [True, False],
        "build_examples": [True, False],
        "build_tests": [True, False],
        "build_python_bindings": [True, False],
        "build_for_qt6": [True, False],
    }

    default_options = {
        "qt_version": "qt/[>5.12.0]@kdab/stable",
        "build_static": False,
        "build_examples": True,
        "build_tests": False,
        "build_python_bindings": False,
        "build_for_qt6": False,
    }

    def requirements(self):
        # Check https://docs.conan.io/en/latest/reference/conanfile/attributes.html#version-ranges for more info about versioning
        self.requires(str(self.options.qt_version))

    def source(self):
        git = tools.Git(folder="")
        git.clone(self.url)

    def build(self, build_type="Release"):
        self.cmake = CMake(self, generator='Ninja', build_type=build_type)
        self.cmake.definitions["KDDockWidgets_STATIC"] = self.options.build_static
        self.cmake.definitions["KDDockWidgets_EXAMPLES"] = self.options.build_examples
        self.cmake.definitions["KDDockWidgets_TESTS"] = self.options.build_tests
        self.cmake.definitions["KDDockWidgets_PYTHON_BINDINGS"] = self.options.build_python_bindings
        self.cmake.definitions["KDDockWidgets_QT6"] = self.options.build_for_qt6
        self.cmake.configure()
        self.cmake.build()

    def package(self):
        self.cmake.install()

    def package_info(self):
        self.env_info.CMAKE_PREFIX_PATH.append(self.package_folder)

    def package_id(self):
        self.info.requires["qt"].minor_mode() # Check only the major and minor version!
