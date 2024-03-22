#
# SPDX-FileCopyrightText: 2016-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# Author: Allen Winter <allen.winter@kdab.com>
#
# SPDX-License-Identifier: BSD-3-Clause
#

# Assumes you've already found Qt and Qt_VERSION_MAJOR is set
#
# Create variables for all the various install paths for the Qt version in use
# Make sure to have found Qt before using this.
# sets variables like QT_INSTALL_PREFIX, QT_INSTALL_DATA, QT_INSTALL_DOCS, etc.
# run qmake -query to see a full list

if(NOT DEFINED Qt_VERSION_MAJOR)
    message(FATAL_ERROR "Please set Qt_VERSION_MAJOR first (ie. set(Qt_VERSION_MAJOR 5))")
endif()

if(TARGET Qt${Qt_VERSION_MAJOR}::qmake)
    get_target_property(QT_QMAKE_EXECUTABLE Qt${Qt_VERSION_MAJOR}::qmake LOCATION)
else()
    message(FATAL_ERROR "No supported Qt version found. Make sure you find Qt before calling this")
endif()

execute_process(
    COMMAND ${QT_QMAKE_EXECUTABLE} -query
    RESULT_VARIABLE return_code
    OUTPUT_VARIABLE ALL_VARS
)
if(NOT return_code EQUAL 0)
    message(WARNING "Failed call: ${QMAKE_EXECUTABLE} -query")
    message(FATAL_ERROR "QMake call failed: ${return_code}")
endif()

string(REPLACE "\n" ";" VARS_LIST ${ALL_VARS})
foreach(QVAL ${VARS_LIST})
    if(QVAL MATCHES "QT_INSTALL_")
        string(REPLACE ":" ";" QVAL_LIST ${QVAL})
        list(LENGTH QVAL_LIST listlen)
        list(GET QVAL_LIST 0 var)
        if(WIN32 AND ${listlen} GREATER 2)
            list(GET QVAL_LIST 2 path)
            list(GET QVAL_LIST 1 drive)
            set(path "${drive}:${path}")
        else()
            list(GET QVAL_LIST 1 path)
        endif()
        if(NOT ${var}) #if set already on the command line for example
            set(${var} ${path} CACHE PATH "Qt install path for ${var}")
        endif()
    endif()
endforeach()
