# SPDX-License-Identifier: GPL-3.0-only
# MuseScore-CLA-applies
#
# MuseScore
# Music Composition & Notation
#
# Copyright (C) 2021 MuseScore BVBA and others
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

set(MODULE global)

include(${CMAKE_CURRENT_LIST_DIR}/async/async.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/thirdparty/kors_logger/logger.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/thirdparty/kors_profiler/profiler/profiler.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/thirdparty/kors_modularity/modularity/modularity.cmake)

set(MODULE_SRC
    ${MODULARITY_SRC}
    ${ASYNC_SRC}
    ${KORS_LOGGER_SRC}
    ${KORS_PROFILER_SRC}
    ${CMAKE_CURRENT_LIST_DIR}/globaltypes.h
    ${CMAKE_CURRENT_LIST_DIR}/iapplication.h
    ${CMAKE_CURRENT_LIST_DIR}/iinteractive.h
    ${CMAKE_CURRENT_LIST_DIR}/iglobalconfiguration.h
    ${CMAKE_CURRENT_LIST_DIR}/log.h
    ${CMAKE_CURRENT_LIST_DIR}/logstream.h
    ${CMAKE_CURRENT_LIST_DIR}/logger.h
    ${CMAKE_CURRENT_LIST_DIR}/logremover.cpp
    ${CMAKE_CURRENT_LIST_DIR}/logremover.h
    ${CMAKE_CURRENT_LIST_DIR}/profiler.h
    ${CMAKE_CURRENT_LIST_DIR}/dataformatter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/dataformatter.h
    ${CMAKE_CURRENT_LIST_DIR}/muversion.cpp
    ${CMAKE_CURRENT_LIST_DIR}/muversion.h
    ${CMAKE_CURRENT_LIST_DIR}/stringutils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/stringutils.h
    ${CMAKE_CURRENT_LIST_DIR}/ptrutils.h
    ${CMAKE_CURRENT_LIST_DIR}/realfn.h
    ${CMAKE_CURRENT_LIST_DIR}/runtime.cpp
    ${CMAKE_CURRENT_LIST_DIR}/runtime.h
    ${CMAKE_CURRENT_LIST_DIR}/translation.cpp
    ${CMAKE_CURRENT_LIST_DIR}/translation.h
    ${CMAKE_CURRENT_LIST_DIR}/timer.h
    ${CMAKE_CURRENT_LIST_DIR}/progress.h
    ${CMAKE_CURRENT_LIST_DIR}/utils.cpp
    ${CMAKE_CURRENT_LIST_DIR}/utils.h
    ${CMAKE_CURRENT_LIST_DIR}/interpolation.cpp
    ${CMAKE_CURRENT_LIST_DIR}/interpolation.h
    ${CMAKE_CURRENT_LIST_DIR}/defer.h
    ${CMAKE_CURRENT_LIST_DIR}/containers.h
    ${CMAKE_CURRENT_LIST_DIR}/icryptographichash.h
    ${CMAKE_CURRENT_LIST_DIR}/allocator.cpp
    ${CMAKE_CURRENT_LIST_DIR}/allocator.h
    ${CMAKE_CURRENT_LIST_DIR}/dlib.h
    ${CMAKE_CURRENT_LIST_DIR}/iprocess.h
    ${CMAKE_CURRENT_LIST_DIR}/isysteminfo.h

    ${KORS_MODULARITY_SRC}
    ${CMAKE_CURRENT_LIST_DIR}/modularity/ioc.h
    ${CMAKE_CURRENT_LIST_DIR}/modularity/imoduleinterface.h
    ${CMAKE_CURRENT_LIST_DIR}/modularity/imodulesetup.h

    ${CMAKE_CURRENT_LIST_DIR}/types/bytearray.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/bytearray.h
    ${CMAKE_CURRENT_LIST_DIR}/types/string.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/string.h
    ${CMAKE_CURRENT_LIST_DIR}/types/datetime.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/datetime.h
    ${CMAKE_CURRENT_LIST_DIR}/types/flags.h
    ${CMAKE_CURRENT_LIST_DIR}/types/id.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/id.h
    ${CMAKE_CURRENT_LIST_DIR}/types/val.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/val.h
    ${CMAKE_CURRENT_LIST_DIR}/types/ret.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/ret.h
    ${CMAKE_CURRENT_LIST_DIR}/types/retval.h
    ${CMAKE_CURRENT_LIST_DIR}/types/uri.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/uri.h
    ${CMAKE_CURRENT_LIST_DIR}/types/sharedhashmap.h
    ${CMAKE_CURRENT_LIST_DIR}/types/sharedmap.h
    ${CMAKE_CURRENT_LIST_DIR}/types/translatablestring.h
    ${CMAKE_CURRENT_LIST_DIR}/types/mnemonicstring.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/mnemonicstring.h
    ${CMAKE_CURRENT_LIST_DIR}/types/version.cpp
    ${CMAKE_CURRENT_LIST_DIR}/types/version.h
    ${CMAKE_CURRENT_LIST_DIR}/types/number.h

    ${CMAKE_CURRENT_LIST_DIR}/io/ioenums.h
    ${CMAKE_CURRENT_LIST_DIR}/io/path.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/path.h
    ${CMAKE_CURRENT_LIST_DIR}/io/iodevice.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/iodevice.h
    ${CMAKE_CURRENT_LIST_DIR}/io/file.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/file.h
    ${CMAKE_CURRENT_LIST_DIR}/io/buffer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/buffer.h
    ${CMAKE_CURRENT_LIST_DIR}/io/ifilesystem.h
    ${CMAKE_CURRENT_LIST_DIR}/io/ioretcodes.h
    ${CMAKE_CURRENT_LIST_DIR}/io/fileinfo.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/fileinfo.h
    ${CMAKE_CURRENT_LIST_DIR}/io/dir.cpp
    ${CMAKE_CURRENT_LIST_DIR}/io/dir.h

    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmlstreamreader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmlstreamreader.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmlstreamwriter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmlstreamwriter.h
    ${CMAKE_CURRENT_LIST_DIR}/thirdparty/tinyxml/tinyxml2.cpp
    ${CMAKE_CURRENT_LIST_DIR}/thirdparty/tinyxml/tinyxml2.h
    ${CMAKE_CURRENT_LIST_DIR}/thirdparty/tinyxml/mu_patch.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/zipreader.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/zipreader.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/zipwriter.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/zipwriter.h

    ${CMAKE_CURRENT_LIST_DIR}/serialization/internal/zipcontainer.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/internal/zipcontainer.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/textstream.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/textstream.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/json.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/json.h
    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmldom.cpp
    ${CMAKE_CURRENT_LIST_DIR}/serialization/xmldom.h

    ${CMAKE_CURRENT_LIST_DIR}/concurrency/taskscheduler.h
    ${CMAKE_CURRENT_LIST_DIR}/concurrency/concurrent.h
)

if (GLOBAL_NO_INTERNAL)
    set(MODULE_DEF ${MODULE_DEF} -DGLOBAL_NO_INTERNAL)
else()
    set(MODULE_SRC ${MODULE_SRC}
        ${CMAKE_CURRENT_LIST_DIR}/globalmodule.cpp
        ${CMAKE_CURRENT_LIST_DIR}/globalmodule.h
        ${CMAKE_CURRENT_LIST_DIR}/settings.cpp
        ${CMAKE_CURRENT_LIST_DIR}/settings.h

        ${CMAKE_CURRENT_LIST_DIR}/api/iapiregister.h
        ${CMAKE_CURRENT_LIST_DIR}/api/iapiengine.h
        ${CMAKE_CURRENT_LIST_DIR}/api/internal/apiregister.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/internal/apiregister.h
        ${CMAKE_CURRENT_LIST_DIR}/api/apiobject.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/apiobject.h
        ${CMAKE_CURRENT_LIST_DIR}/api/jsretval.h
        ${CMAKE_CURRENT_LIST_DIR}/api/apiutils.h
        ${CMAKE_CURRENT_LIST_DIR}/api/logapi.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/logapi.h
        ${CMAKE_CURRENT_LIST_DIR}/api/interactiveapi.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/interactiveapi.h
        ${CMAKE_CURRENT_LIST_DIR}/api/processapi.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/processapi.h
        ${CMAKE_CURRENT_LIST_DIR}/api/filesystemapi.cpp
        ${CMAKE_CURRENT_LIST_DIR}/api/filesystemapi.h

        ${CMAKE_CURRENT_LIST_DIR}/internal/application.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/application.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/globalconfiguration.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/globalconfiguration.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/interactive.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/interactive.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/cryptographichash.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/cryptographichash.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/invoker.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/invoker.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/process.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/process.h
        ${CMAKE_CURRENT_LIST_DIR}/internal/systeminfo.cpp
        ${CMAKE_CURRENT_LIST_DIR}/internal/systeminfo.h

        ${CMAKE_CURRENT_LIST_DIR}/io/internal/filesystem.cpp
        ${CMAKE_CURRENT_LIST_DIR}/io/internal/filesystem.h

        ${CMAKE_CURRENT_LIST_DIR}/deprecated/xmlreader.cpp
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/xmlreader.h
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/xmlwriter.cpp
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/xmlwriter.h
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/qzip.cpp
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/qzipreader_p.h
        ${CMAKE_CURRENT_LIST_DIR}/deprecated/qzipwriter_p.h
        )

    include(GetPlatformInfo)

    if (OS_IS_MAC)
        set(MODULE_SRC ${MODULE_SRC}
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosinteractivehelper.mm
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosinteractivehelper.h
        )
        set_source_files_properties(
            ${CMAKE_CURRENT_LIST_DIR}/internal/platform/macos/macosinteractivehelper.mm
            PROPERTIES
            SKIP_UNITY_BUILD_INCLUSION ON
            SKIP_PRECOMPILE_HEADERS ON
        )
        find_library(AppKit NAMES AppKit)
        list(APPEND MODULE_LINK ${AppKit})
    endif()
endif()

set(FS_LIB )
# The use of `filesystem` is disabled until
# I figure out how to use it correctly for all platforms and compilers (including MinGW)
# (igor.korsukov@gmail.com)
#if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
#    set(FS_LIB stdc++fs)
#endif()

include(GetCompilerInfo)
set(Z_LIB )
if (CC_IS_MSVC)
    include(FindStaticLibrary) # TODO need rename and move to build/cmake
    set(Z_LIB zlibstat)
    set(Z_INCLUDE ${DEPENDENCIES_INC}/zlib)
elseif (CC_IS_EMSCRIPTEN)
    #zlib included in main linker
else ()
    set(Z_LIB z)
endif ()

list(APPEND MODULE_INCLUDE
    ${Z_INCLUDE}
    )

list(APPEND MODULE_LINK
    ${FS_LIB}
    ${Z_LIB}
)

if (MUE_ENABLE_CUSTOM_ALLOCATOR)
    set(MODULE_DEF ${MODULE_DEF} -DMUE_ENABLE_CUSTOM_ALLOCATOR)
endif()

if (MUE_ENABLE_LOGGER_DEBUGLEVEL)
    set(MODULE_DEF ${MODULE_DEF} -DMUE_ENABLE_LOGGER_DEBUGLEVEL)
endif()

include(SetupModule)

if (MUE_BUILD_GLOBAL_TESTS)
    add_subdirectory(tests)
endif()
