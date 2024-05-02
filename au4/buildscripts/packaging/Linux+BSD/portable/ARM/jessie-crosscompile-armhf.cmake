#Cross toolchain file for building QT project (specifically MuseScore)
#For use in Debian x86-64 + armhf with cross compiler

SET(CMAKE_SYSTEM_NAME Linux)
SET(CMAKE_SYSTEM_VERSION 1)
SET(ARCH armhf) # AppImage directory's name will include this string

# specify the cross compiler
SET(CMAKE_C_COMPILER   /usr/bin/arm-linux-gnueabihf-gcc)
SET(CMAKE_CXX_COMPILER /usr/bin/arm-linux-gnueabihf-g++)
SET(ENV{PKG_CONFIG_PATH} /usr/lib/arm-linux-gnueabihf/pkgconfig)

# don't set CMAKE_STRIP, because doing so somehow causes CMakeCache to report nothing for CMAKE_STRIP:FILEPATH
#SET(CMAKE_STRIP        /usr/bin/arm-linux-gnueabihf-strip)

# where is the target environment 
SET(CMAKE_FIND_ROOT_PATH /lib/arm-linux-gnueabihf /usr/lib/arm-linux-gnueabihf /usr/include/arm-linux-gnueabihf)
SET(CMAKE_LIBRARY_PATH /usr/include/arm-linux-gnueabihf)

# Qt include directories
SET(QT_INCLUDE_DIR           /usr/include/arm-linux-gnu)
SET(QT_QT_INCLUDE_DIR        ${QT_INCLUDE_DIR}/qt5)
SET(QT_QTCORE_INCLUDE_DIR    ${QT_QT_INCLUDE_DIR}/QtCore)
SET(QT_QTXML_INCLUDE_DIR     ${QT_QT_INCLUDE_DIR}/QtXml)
SET(QT_QTGUI_INCLUDE_DIR     ${QT_QT_INCLUDE_DIR}/QtGui)
SET(QT_QTNETWORK_INCLUDE_DIR ${QT_QT_INCLUDE_DIR}/QtNetwork)
SET(QT_QTUITOOLS_INCLUDE_DIR ${QT_QT_INCLUDE_DIR}/QtUiTools)
SET(QT_QTSCRIPT_INCLUDE_DIR  ${QT_QT_INCLUDE_DIR}/QtScript)
SET(QT_QTWEBKIT_INCLUDE_DIR  ${QT_QT_INCLUDE_DIR}/QtWebkit)
SET(QT_INCLUDES ${QT_INCLUDE_DIR} ${QT_QT_INCLUDE_DIR} ${QT_QTCORE_INCLUDE_DIR} ${QT_QTXML_INCLUDE_DIR} ${QT_GUI_INCLUDE_DIR} ${QT_QTNETWORK_INCLUDE_DIR} ${QT_QTWEBKIT_INCLUDE_DIR} )

# Qt libraries
SET(QT_LIBRARY_DIR  ${CMAKE_LIBRARY_PATH})
SET(QT_CROSS_LIBRARIES QtSvg5 QtGui5 QtCore5 QtXml5 QtNetwork5 QtWebKit5 QtXmlPatterns5 QtDeclarative5)

# Qt binaries
SET(QT_BINARY_DIR   /usr/lib/arm-linux-gnueabihf/qt5/bin)
SET(QT_MOC_EXECUTABLE  ${QT_BINARY_DIR}/moc)
SET(QT_UIC_EXECUTABLE  ${QT_BINARY_DIR}/uic)
SET(QT_QMAKE_EXECUTABLE  ${QT_BINARY_DIR}/qmake)
SET(QT_RCC_EXECUTABLE  ${QT_BINARY_DIR}/rcc)
SET(QT_LRELEASE_EXECUTABLE  ${QT_BINARY_DIR}/lrelease)

# search for programs in the build host directories
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)

# for libraries and headers in the target directories
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

# Audio library include dirs (not usre this is being used)
SET(LAME_INCLUDE_DIR /usr/include/lame)
