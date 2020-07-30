
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE

      # lilv

      ${TARGET_ROOT}/lilv/src/collections.c
      ${TARGET_ROOT}/lilv/src/instance.c
      ${TARGET_ROOT}/lilv/src/lib.c
      ${TARGET_ROOT}/lilv/src/node.c
      ${TARGET_ROOT}/lilv/src/plugin.c
      ${TARGET_ROOT}/lilv/src/pluginclass.c
      ${TARGET_ROOT}/lilv/src/port.c
      ${TARGET_ROOT}/lilv/src/query.c
      ${TARGET_ROOT}/lilv/src/scalepoint.c
      ${TARGET_ROOT}/lilv/src/state.c
      ${TARGET_ROOT}/lilv/src/ui.c
      ${TARGET_ROOT}/lilv/src/util.c
      ${TARGET_ROOT}/lilv/src/world.c
      ${TARGET_ROOT}/lilv/src/zix/tree.c

      # serd

      ${TARGET_ROOT}/serd/src/byte_source.c
      ${TARGET_ROOT}/serd/src/env.c
      ${TARGET_ROOT}/serd/src/n3.c
      ${TARGET_ROOT}/serd/src/node.c
      ${TARGET_ROOT}/serd/src/reader.c
      ${TARGET_ROOT}/serd/src/string.c
      ${TARGET_ROOT}/serd/src/uri.c
      ${TARGET_ROOT}/serd/src/writer.c
      
      # sord

      ${TARGET_ROOT}/sord/src/sord.c
      ${TARGET_ROOT}/sord/src/syntax.c
      ${TARGET_ROOT}/sord/src/zix/btree.c
      ${TARGET_ROOT}/sord/src/zix/digest.c
      ${TARGET_ROOT}/sord/src/zix/hash.c

      # sratom

      ${TARGET_ROOT}/sratom/src/sratom.c

      # suil

      ${TARGET_ROOT}/suil/src/host.c
      ${TARGET_ROOT}/suil/src/instance.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${TARGET_ROOT}/lilv/src
      ${TARGET_ROOT}/lilv/src/zix
      ${TARGET_ROOT}/serd/src
      ${TARGET_ROOT}/sord/src
      ${TARGET_ROOT}/sord/src/zix
      ${TARGET_ROOT}/sratom/src
      ${TARGET_ROOT}/suil/src
   PUBLIC
      ${_PUBDIR}
      ${TARGET_ROOT}/lv2
      ${TARGET_ROOT}/lilv
      ${TARGET_ROOT}/serd
      ${TARGET_ROOT}/sord
      ${TARGET_ROOT}/sratom
      ${TARGET_ROOT}/suil
)

list( APPEND DEFINES
   PRIVATE
      SUIL_INTERNAL
)

list( APPEND HEADERS

   # lv2

   ${TARGET_ROOT}/lv2/lv2/atom/atom.h
   ${TARGET_ROOT}/lv2/lv2/atom/forge.h
   ${TARGET_ROOT}/lv2/lv2/atom/util.h
   ${TARGET_ROOT}/lv2/lv2/buf-size/buf-size.h
   ${TARGET_ROOT}/lv2/lv2/core/attributes.h
   ${TARGET_ROOT}/lv2/lv2/core/lv2.h
   ${TARGET_ROOT}/lv2/lv2/core/lv2_util.h
   ${TARGET_ROOT}/lv2/lv2/data-access/data-access.h
   ${TARGET_ROOT}/lv2/lv2/dynmanifest/dynmanifest.h
   ${TARGET_ROOT}/lv2/lv2/event/event-helpers.h
   ${TARGET_ROOT}/lv2/lv2/event/event.h
   ${TARGET_ROOT}/lv2/lv2/instance-access/instance-access.h
   ${TARGET_ROOT}/lv2/lv2/log/log.h
   ${TARGET_ROOT}/lv2/lv2/log/logger.h
   ${TARGET_ROOT}/lv2/lv2/midi/midi.h
   ${TARGET_ROOT}/lv2/lv2/morph/morph.h
   ${TARGET_ROOT}/lv2/lv2/options/options.h
   ${TARGET_ROOT}/lv2/lv2/parameters/parameters.h
   ${TARGET_ROOT}/lv2/lv2/patch/patch.h
   ${TARGET_ROOT}/lv2/lv2/port-groups/port-groups.h
   ${TARGET_ROOT}/lv2/lv2/port-props/port-props.h
   ${TARGET_ROOT}/lv2/lv2/presets/presets.h
   ${TARGET_ROOT}/lv2/lv2/resize-port/resize-port.h
   ${TARGET_ROOT}/lv2/lv2/state/state.h
   ${TARGET_ROOT}/lv2/lv2/time/time.h
   ${TARGET_ROOT}/lv2/lv2/ui/ui.h
   ${TARGET_ROOT}/lv2/lv2/units/units.h
   ${TARGET_ROOT}/lv2/lv2/uri-map/uri-map.h
   ${TARGET_ROOT}/lv2/lv2/urid/urid.h
   ${TARGET_ROOT}/lv2/lv2/worker/worker.h
)

set( src "${TARGET_ROOT}/lv2" )
set( dst "${_PUBDIR}" )
set( ns "${dst}/lv2/lv2plug.in/ns" )
set( stamp "${_INTDIR}/.stamp.lv2" )

# Simulate the older directory structure (trailing "/" is important)
file( MAKE_DIRECTORY "${ns}" )
file( COPY "${src}/lv2/core/lv2.h" DESTINATION "${dst}" )
file( COPY "${src}/lv2/" DESTINATION "${ns}/ext" )
file( COPY "${src}/lv2/" DESTINATION "${ns}/extensions" )
file( COPY "${src}/lv2/core/" DESTINATION  "${ns}/lv2core" )

set( LILV_VERSION "0.24.4" )
set( SERD_VERSION "0.30.2" )
set( SORD_VERSION "0.16.4" )
set( SRATOM_VERSION "0.6.4" )
set( SUIL_VERSION "0.10.6" )

set( HAVE_LV2 1 )
set( HAVE_SERD 1 )
set( HAVE_SORD 1 )
set( HAVE_SRATOM 1 )

macro( bld name packages define sources )
   set( libs )
   set( missing )
   foreach( pkg ${packages} )
      if( NOT "${${pkg}_FOUND}" )
         set( missing ON )
         break()
      endif()

      list( APPEND libs
         PRIVATE
            "PkgConfig::${pkg}"
      )
   endforeach()

   if( NOT missing )
      list( APPEND DEFINES
         PRIVATE
            ${define}
      )

      add_library( ${name} MODULE "${sources}" )
      add_dependencies( ${TARGET} ${name} )

      set_target_properties( ${name}
         PROPERTIES
            LIBRARY_OUTPUT_DIRECTORY "${_DEST}/${_PKGLIB}"
            PREFIX ""
      )

      organize_source( "${TARGET_ROOT}" "" "${sources}" )
      target_compile_definitions( ${name} PRIVATE SUIL_SHARED ${DEFINES} )
      target_include_directories( ${name} PRIVATE ${INCLUDES} )
      target_link_libraries( ${name} PRIVATE ${libs} )
   endif()
endmacro()

if( WIN32 )
   set( LILV_PATH_SEP ";" )
   set( LILV_DIR_SEP "\\\\" )
   set( LILV_DEFAULT_LV2_PATH "%APPDATA%\\\\LV2;%COMMONPROGRAMFILES%\\\\LV2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "" )
   set( SUIL_MODULE_EXT ".dll" )
elseif( APPLE )
   set( LILV_PATH_SEP ":" )
   set( LILV_DIR_SEP "/" )
   set( LILV_DEFAULT_LV2_PATH "~/Library/Audio/Plug-Ins/LV2:~/.lv2:/usr/local/lib/lv2:/usr/lib/lv2:/Library/Audio/Plug-Ins/LV2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "lib" )
   set( SUIL_MODULE_EXT ".dylib" )
elseif( UNIX )
   set( LILV_PATH_SEP ":" )
   set( LILV_DIR_SEP "/" )
   set( LILV_DEFAULT_LV2_PATH "~/.lv2:/usr/lib/lv2:/usr/local/lib/lv2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "lib" )
   set( SUIL_MODULE_EXT ".so" )

   pkg_check_modules( X11 IMPORTED_TARGET "x11" )
   pkg_check_modules( GTK2 IMPORTED_TARGET "gtk+-2.0" )
   pkg_check_modules( GTK3 IMPORTED_TARGET "gtk+-3.0" )
   pkg_check_modules( GTK2X11 IMPORTED_TARGET "gtk+-x11-2.0" )
   pkg_check_modules( GTK3X11 IMPORTED_TARGET "gtk+-x11-3.0" )
   pkg_check_modules( QT4 IMPORTED_TARGET "QtGui >= 4.4.0" )
   pkg_check_modules( QT5 IMPORTED_TARGET "Qt5Widgets >= 5.1.0" )

   bld( "suil_x11"
        "X11"
        "SUIL_WITH_X11"
        "${TARGET_ROOT}/suil/src/x11.c" )
   bld( "suil_x11_in_gtk2"
        "X11;GTK2X11"
        "SUIL_WITH_X11_IN_GTK2"
        "${TARGET_ROOT}/suil/src/x11_in_gtk2.c" )
   bld( "suil_x11_in_gtk3"
        "X11;GTK3X11"
        "SUIL_WITH_X11_IN_GTK3"
        "${TARGET_ROOT}/suil/src/x11_in_gtk3.c" )
   bld( "suil_qt4_in_gtk2"
        "QT4;GTK2"
        "SUIL_WITH_QT4_IN_GTK2"
        "${TARGET_ROOT}/suil/src/qt4_in_gtk2.cpp" )
   bld( "suil_qt5_in_gtk2"
        "QT5;GTK2"
        "SUIL_WITH_QT5_IN_GTK2"
        "${TARGET_ROOT}/suil/src/qt5_in_gtk.cpp" )
   bld( "suil_qt5_in_gtk3"
        "QT5;GTK3"
        "SUIL_WITH_QT5_IN_GTK3"
        "${TARGET_ROOT}/suil/src/qt5_in_gtk.cpp" )
endif()

configure_file( lilv_config.h.in "${_PRVDIR}/lilv_config.h" )
configure_file( serd_config.h.in "${_PRVDIR}/serd_config.h" )
configure_file( sord_config.h.in "${_PRVDIR}/sord_config.h" )
configure_file( sratom_config.h.in "${_PRVDIR}/sratom_config.h" )
configure_file( suil_config.h.in "${_PRVDIR}/suil_config.h" )

organize_source( "${TARGET_ROOT}" "" "${SOURCES} ${HEADERS}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} ${HEADERS} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

