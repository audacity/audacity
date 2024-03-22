# SPDX-FileCopyrightText: 2014 David Faure <faure@kde.org>
#
# SPDX-License-Identifier: BSD-3-Clause

#[=======================================================================[.rst:
ECMGeneratePriFile
------------------

Generate a ``.pri`` file for the benefit of qmake-based projects.

As well as the function below, this module creates the cache variable
``ECM_MKSPECS_INSTALL_DIR`` and sets the default value to ``mkspecs/modules``.
This assumes Qt and the current project are both installed to the same
non-system prefix.  Packagers who use ``-DCMAKE_INSTALL_PREFIX=/usr`` will
certainly want to set ``ECM_MKSPECS_INSTALL_DIR`` to something like
``share/qt5/mkspecs/modules``.

The main thing is that this should be the ``modules`` subdirectory of either
the default qmake ``mkspecs`` directory or of a directory that will be in the
``$QMAKEPATH`` environment variable when qmake is run.

::

  ecm_generate_pri_file(BASE_NAME <baseName>
                        LIB_NAME <libName>
                        [VERSION <version>] # since 5.83
                        [DEPS "<dep> [<dep> [...]]"]
                        [FILENAME_VAR <filename_variable>]
                        [INCLUDE_INSTALL_DIR <dir>]
                        [LIB_INSTALL_DIR <dir>])

If your CMake project produces a Qt-based library, you may expect there to be
applications that wish to use it that use a qmake-based build system, rather
than a CMake-based one.  Creating a ``.pri`` file will make use of your
library convenient for them, in much the same way that CMake config files make
things convenient for CMake-based applications. ``ecm_generate_pri_file()``
generates just such a file.

VERSION specifies the version of the library the ``.pri`` file describes. If
not set, the value is taken from the context variable ``PROJECT_VERSION``.
This variable is usually set by the ``project(... VERSION ...)`` command or,
if CMake policy CMP0048 is not NEW, by :module:`ECMSetupVersion`.
For backward-compatibility with older ECM versions the
``PROJECT_VERSION_STRING`` variable as set by :module:`ECMSetupVersion`
will be preferred over ``PROJECT_VERSION`` if set, unless the minimum
required version of ECM is 5.83 and newer. Since 5.83.

BASE_NAME specifies the name qmake project (.pro) files should use to refer to
the library (eg: KArchive).  LIB_NAME is the name of the actual library to
link to (ie: the first argument to add_library()).  DEPS is a space-separated
list of the base names of other libraries (for Qt libraries, use the same
names you use with the ``QT`` variable in a qmake project file, such as "core"
for QtCore).  FILENAME_VAR specifies the name of a variable to store the path
to the generated file in.

INCLUDE_INSTALL_DIR is the path (relative to ``CMAKE_INSTALL_PREFIX``) that
include files will be installed to. It defaults to
``${INCLUDE_INSTALL_DIR}/<baseName>`` if the ``INCLUDE_INSTALL_DIR`` variable
is set. If that variable is not set, the ``CMAKE_INSTALL_INCLUDEDIR`` variable
is used instead, and if neither are set ``include`` is used.  LIB_INSTALL_DIR
operates similarly for the installation location for libraries; it defaults to
``${LIB_INSTALL_DIR}``, ``${CMAKE_INSTALL_LIBDIR}`` or ``lib``, in that order.

Example usage:

.. code-block:: cmake

  ecm_generate_pri_file(
      BASE_NAME KArchive
      LIB_NAME KF5KArchive
      DEPS "core"
      FILENAME_VAR pri_filename
      VERSION 4.2.0
  )
  install(FILES ${pri_filename} DESTINATION ${ECM_MKSPECS_INSTALL_DIR})

A qmake-based project that wished to use this would then do::

  QT += KArchive

in their ``.pro`` file.

Since pre-1.0.0.
#]=======================================================================]

# Replicate the logic from KDEInstallDirs.cmake as we can't depend on it
# Ask qmake if we're using the same prefix as Qt
set(_askqmake OFF)
if(NOT DEFINED KDE_INSTALL_USE_QT_SYS_PATHS)
    include(ECMQueryQmake)
    query_qmake(qt_install_prefix_dir QT_INSTALL_PREFIX TRY)
    if(qt_install_prefix_dir STREQUAL "${CMAKE_INSTALL_PREFIX}")
        set(_askqmake ON)
    endif()
endif()

if(KDE_INSTALL_USE_QT_SYS_PATHS OR _askqmake)
  include(ECMQueryQmake)
  query_qmake(qt_install_prefix_dir QT_INSTALL_PREFIX)
  query_qmake(qt_host_data_dir QT_HOST_DATA)
  if(qt_install_prefix_dir STREQUAL "${CMAKE_INSTALL_PREFIX}")
    file(RELATIVE_PATH qt_host_data_dir ${qt_install_prefix_dir} ${qt_host_data_dir})
  endif()
  if(qt_host_data_dir STREQUAL "")
    set(mkspecs_install_dir mkspecs/modules)
  else()
    set(mkspecs_install_dir ${qt_host_data_dir}/mkspecs/modules)
  endif()
  set(ECM_MKSPECS_INSTALL_DIR ${mkspecs_install_dir} CACHE PATH "The directory where mkspecs will be installed to.")
else()
  set(ECM_MKSPECS_INSTALL_DIR mkspecs/modules CACHE PATH "The directory where mkspecs will be installed to.")
endif()

function(ECM_GENERATE_PRI_FILE)
  set(options )
  set(oneValueArgs BASE_NAME LIB_NAME DEPS FILENAME_VAR INCLUDE_INSTALL_DIR LIB_INSTALL_DIR VERSION)
  set(multiValueArgs )

  cmake_parse_arguments(EGPF "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if(EGPF_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unknown keywords given to ECM_GENERATE_PRI_FILE(): \"${EGPF_UNPARSED_ARGUMENTS}\"")
  endif()

  if(ECM_GLOBAL_FIND_VERSION VERSION_LESS 5.83.0)
    set(_support_backward_compat_version_string_var TRUE)
  else()
    set(_support_backward_compat_version_string_var FALSE)
  endif()

  if(NOT EGPF_BASE_NAME)
    message(FATAL_ERROR "Required argument BASE_NAME missing in ECM_GENERATE_PRI_FILE() call")
  endif()
  if(NOT EGPF_LIB_NAME)
    message(FATAL_ERROR "Required argument LIB_NAME missing in ECM_GENERATE_PRI_FILE() call")
  endif()
  if(NOT EGPF_VERSION)
    if(_support_backward_compat_version_string_var)
      if(NOT PROJECT_VERSION_STRING AND NOT PROJECT_VERSION)
        message(FATAL_ERROR "Required variable PROJECT_VERSION_STRING or PROJECT_VERSION not set before ECM_GENERATE_PRI_FILE() call. Missing call of ecm_setup_version() or project(VERSION)?")
      endif()
    else()
      if(NOT PROJECT_VERSION)
        message(FATAL_ERROR "Required variable PROJECT_VERSION not set before ECM_GENERATE_PRI_FILE() call. Missing call of ecm_setup_version() or project(VERSION)?")
      endif()
    endif()
  endif()
  if(NOT EGPF_INCLUDE_INSTALL_DIR)
      if(INCLUDE_INSTALL_DIR)
          set(EGPF_INCLUDE_INSTALL_DIR "${INCLUDE_INSTALL_DIR}/${EGPF_BASE_NAME}")
      elseif(CMAKE_INSTALL_INCLUDEDIR)
          set(EGPF_INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_INCLUDEDIR}/${EGPF_BASE_NAME}")
      else()
          set(EGPF_INCLUDE_INSTALL_DIR "include/${EGPF_BASE_NAME}")
      endif()
  endif()
  if(NOT EGPF_LIB_INSTALL_DIR)
      if(LIB_INSTALL_DIR)
          set(EGPF_LIB_INSTALL_DIR "${LIB_INSTALL_DIR}")
      elseif(CMAKE_INSTALL_LIBDIR)
          set(EGPF_LIB_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}")
      else()
          set(EGPF_LIB_INSTALL_DIR "lib")
      endif()
  endif()

  if(EGPF_VERSION)
    set(PRI_VERSION "${EGPF_VERSION}")
  else()
    if(_support_backward_compat_version_string_var AND PROJECT_VERSION_STRING)
      set(PRI_VERSION "${PROJECT_VERSION_STRING}")
      if(NOT PROJECT_VERSION_STRING STREQUAL PROJECT_VERSION)
        message(DEPRECATION "ECM_GENERATE_PRI_FILE() will no longer support PROJECT_VERSION_STRING when the required minimum version of ECM is 5.83 or newer. Set VERSION parameter or use PROJECT_VERSION instead.")
      endif()
    else()
      set(PRI_VERSION "${PROJECT_VERSION}")
    endif()
  endif()

  string(REGEX REPLACE "^([0-9]+)\\.[0-9]+\\.[0-9]+.*" "\\1" PRI_VERSION_MAJOR "${PRI_VERSION}")
  string(REGEX REPLACE "^[0-9]+\\.([0-9]+)\\.[0-9]+.*" "\\1" PRI_VERSION_MINOR "${PRI_VERSION}")
  string(REGEX REPLACE "^[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" PRI_VERSION_PATCH "${PRI_VERSION}")

  # Prepare the right number of "../.." to go from ECM_MKSPECS_INSTALL_DIR to the install prefix
  # This allows to make the generated pri files relocatable (no absolute paths)
  if (IS_ABSOLUTE ${ECM_MKSPECS_INSTALL_DIR})
     set(BASEPATH ${CMAKE_INSTALL_PREFIX})
  else()
    string(REGEX REPLACE "[^/]+" ".." PRI_ROOT_RELATIVE_TO_MKSPECS ${ECM_MKSPECS_INSTALL_DIR})
    set(BASEPATH "$$PWD/${PRI_ROOT_RELATIVE_TO_MKSPECS}")
 endif()

  set(PRI_TARGET_BASENAME ${EGPF_BASE_NAME})
  set(PRI_TARGET_LIBNAME ${EGPF_LIB_NAME})
  set(PRI_TARGET_QTDEPS ${EGPF_DEPS})
  if(IS_ABSOLUTE "${EGPF_INCLUDE_INSTALL_DIR}")
      set(PRI_TARGET_INCLUDES "${EGPF_INCLUDE_INSTALL_DIR}")
  else()
      set(PRI_TARGET_INCLUDES "${BASEPATH}/${EGPF_INCLUDE_INSTALL_DIR}")
  endif()
  if(IS_ABSOLUTE "${EGPF_LIB_INSTALL_DIR}")
      set(PRI_TARGET_LIBS "${EGPF_LIB_INSTALL_DIR}")
  else()
      set(PRI_TARGET_LIBS "${BASEPATH}/${EGPF_LIB_INSTALL_DIR}")
  endif()
  set(PRI_TARGET_DEFINES "")

  set(PRI_FILENAME ${CMAKE_CURRENT_BINARY_DIR}/qt_${PRI_TARGET_BASENAME}.pri)
  if (EGPF_FILENAME_VAR)
     set(${EGPF_FILENAME_VAR} ${PRI_FILENAME} PARENT_SCOPE)
  endif()

  set(PRI_TARGET_MODULE_CONFIG "")
  # backward compat: it was not obvious LIB_NAME needs to be a target name,
  # and some projects where the target name was not the actual library output name
  # passed the output name for LIB_NAME, so .name & .module prperties are correctly set.
  # TODO: improve API dox, allow control over module name if target name != output name
  if(TARGET ${EGPF_LIB_NAME})
    get_target_property(target_type ${EGPF_LIB_NAME} TYPE)
    if (target_type STREQUAL "STATIC_LIBRARY")
        set(PRI_TARGET_MODULE_CONFIG "staticlib")
    endif()
  endif()

  file(GENERATE
     OUTPUT ${PRI_FILENAME}
     CONTENT
     "QT.${PRI_TARGET_BASENAME}.VERSION = ${PRI_VERSION}
QT.${PRI_TARGET_BASENAME}.MAJOR_VERSION = ${PRI_VERSION_MAJOR}
QT.${PRI_TARGET_BASENAME}.MINOR_VERSION = ${PRI_VERSION_MINOR}
QT.${PRI_TARGET_BASENAME}.PATCH_VERSION = ${PRI_VERSION_PATCH}
QT.${PRI_TARGET_BASENAME}.name = ${PRI_TARGET_LIBNAME}
QT.${PRI_TARGET_BASENAME}.module = ${PRI_TARGET_LIBNAME}
QT.${PRI_TARGET_BASENAME}.defines = ${PRI_TARGET_DEFINES}
QT.${PRI_TARGET_BASENAME}.includes = ${PRI_TARGET_INCLUDES}
QT.${PRI_TARGET_BASENAME}.private_includes =
QT.${PRI_TARGET_BASENAME}.libs = ${PRI_TARGET_LIBS}
QT.${PRI_TARGET_BASENAME}.depends = ${PRI_TARGET_QTDEPS}
QT.${PRI_TARGET_BASENAME}.module_config = ${PRI_TARGET_MODULE_CONFIG}
"
  )
endfunction()
