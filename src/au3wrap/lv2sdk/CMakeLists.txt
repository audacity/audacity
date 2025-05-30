#
# Audacity: A Digital Audio Editor
#
declare_module(lv2sdk)

message(STATUS "AU_MODULE_LV2_LV2SDK_PATH: ${AU_MODULE_LV2_LV2SDK_PATH}")

if(AU_MODULE_LV2_LV2SDK_PATH)
    set(LV2SDK_PATH ${AU_MODULE_LV2_LV2SDK_PATH})
else()
    set(LV2SDK_PATH ${CMAKE_SOURCE_DIR}/au3/lib-src/lv2)
endif()

message(STATUS "LV2SDK_PATH: ${LV2SDK_PATH}")


set(_PRVDIR ${CMAKE_CURRENT_BINARY_DIR}/private)
set(_PUBDIR ${CMAKE_CURRENT_BINARY_DIR}/public)
set(_DEST ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(_INTDIR ${CMAKE_CURRENT_BINARY_DIR})
set(_LIBDIR ${CMAKE_INSTALL_LIBDIR} )

if (OS_IS_WIN)
   set(_PKGLIB "")
elseif(OS_IS_MAC)
   set(_PKGLIB audacity.app/Contents/${_APPDIR}/Frameworks)
else()
   set(_PKGLIB ${_LIBDIR}/audacity)
endif()

set( SOURCES
   PRIVATE

      # lilv

      ${LV2SDK_PATH}/lilv/lilv/lilv.h
      ${LV2SDK_PATH}/lilv/src/collections.c
      ${LV2SDK_PATH}/lilv/src/instance.c
      ${LV2SDK_PATH}/lilv/src/lib.c
      ${LV2SDK_PATH}/lilv/src/lilv_internal.h
      ${LV2SDK_PATH}/lilv/src/node.c
      ${LV2SDK_PATH}/lilv/src/plugin.c
      ${LV2SDK_PATH}/lilv/src/pluginclass.c
      ${LV2SDK_PATH}/lilv/src/port.c
      ${LV2SDK_PATH}/lilv/src/query.c
      ${LV2SDK_PATH}/lilv/src/scalepoint.c
      ${LV2SDK_PATH}/lilv/src/state.c
      ${LV2SDK_PATH}/lilv/src/ui.c
      ${LV2SDK_PATH}/lilv/src/util.c
      ${LV2SDK_PATH}/lilv/src/world.c
      ${LV2SDK_PATH}/lilv/src/zix/common.h
      ${LV2SDK_PATH}/lilv/src/zix/tree.c
      ${LV2SDK_PATH}/lilv/src/zix/tree.h
      ${LV2SDK_PATH}/lilv/utils/bench.h
      ${LV2SDK_PATH}/lilv/utils/uri_table.h

      # serd

      ${LV2SDK_PATH}/serd/serd/serd.h
      ${LV2SDK_PATH}/serd/src/byte_source.c
      ${LV2SDK_PATH}/serd/src/env.c
      ${LV2SDK_PATH}/serd/src/n3.c
      ${LV2SDK_PATH}/serd/src/node.c
      ${LV2SDK_PATH}/serd/src/reader.c
      ${LV2SDK_PATH}/serd/src/reader.h
      ${LV2SDK_PATH}/serd/src/serd_internal.h
      ${LV2SDK_PATH}/serd/src/string.c
      ${LV2SDK_PATH}/serd/src/uri.c
      ${LV2SDK_PATH}/serd/src/writer.c
      
      # sord

      ${LV2SDK_PATH}/sord/sord/sord.h
      ${LV2SDK_PATH}/sord/src/sord.c
      ${LV2SDK_PATH}/sord/src/sord_internal.h
      ${LV2SDK_PATH}/sord/src/syntax.c
      ${LV2SDK_PATH}/sord/src/zix/btree.c
      ${LV2SDK_PATH}/sord/src/zix/btree.h
      ${LV2SDK_PATH}/sord/src/zix/common.h
      ${LV2SDK_PATH}/sord/src/zix/digest.c
      ${LV2SDK_PATH}/sord/src/zix/digest.h
      ${LV2SDK_PATH}/sord/src/zix/hash.c
      ${LV2SDK_PATH}/sord/src/zix/hash.h

      # sratom

      ${LV2SDK_PATH}/sratom/sratom/sratom.h
      ${LV2SDK_PATH}/sratom/src/sratom.c

      # suil

      ${LV2SDK_PATH}/suil/src/host.c
      ${LV2SDK_PATH}/suil/src/instance.c
      ${LV2SDK_PATH}/suil/src/suil_internal.h
      ${LV2SDK_PATH}/suil/suil/suil.h
)

set( INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${LV2SDK_PATH}/lilv/src
      ${LV2SDK_PATH}/lilv/src/zix
      ${LV2SDK_PATH}/serd/src
      ${LV2SDK_PATH}/sord/src
      ${LV2SDK_PATH}/sord/src/zix
      ${LV2SDK_PATH}/sratom/src
      ${LV2SDK_PATH}/suil/src
   PUBLIC
      ${_PUBDIR}
      ${LV2SDK_PATH}/lv2
      ${LV2SDK_PATH}/lilv
      ${LV2SDK_PATH}/serd
      ${LV2SDK_PATH}/sord
      ${LV2SDK_PATH}/sratom
      ${LV2SDK_PATH}/suil
)

set( DEFINES
   PRIVATE
      SUIL_INTERNAL
)

set( HEADERS

   # lv2

   ${LV2SDK_PATH}/lv2/lv2/atom/atom.h
   ${LV2SDK_PATH}/lv2/lv2/atom/forge.h
   ${LV2SDK_PATH}/lv2/lv2/atom/util.h
   ${LV2SDK_PATH}/lv2/lv2/buf-size/buf-size.h
   ${LV2SDK_PATH}/lv2/lv2/core/attributes.h
   ${LV2SDK_PATH}/lv2/lv2/core/lv2.h
   ${LV2SDK_PATH}/lv2/lv2/core/lv2_util.h
   ${LV2SDK_PATH}/lv2/lv2/data-access/data-access.h
   ${LV2SDK_PATH}/lv2/lv2/dynmanifest/dynmanifest.h
   ${LV2SDK_PATH}/lv2/lv2/event/event-helpers.h
   ${LV2SDK_PATH}/lv2/lv2/event/event.h
   ${LV2SDK_PATH}/lv2/lv2/instance-access/instance-access.h
   ${LV2SDK_PATH}/lv2/lv2/log/log.h
   ${LV2SDK_PATH}/lv2/lv2/log/logger.h
   ${LV2SDK_PATH}/lv2/lv2/midi/midi.h
   ${LV2SDK_PATH}/lv2/lv2/morph/morph.h
   ${LV2SDK_PATH}/lv2/lv2/options/options.h
   ${LV2SDK_PATH}/lv2/lv2/parameters/parameters.h
   ${LV2SDK_PATH}/lv2/lv2/patch/patch.h
   ${LV2SDK_PATH}/lv2/lv2/port-groups/port-groups.h
   ${LV2SDK_PATH}/lv2/lv2/port-props/port-props.h
   ${LV2SDK_PATH}/lv2/lv2/presets/presets.h
   ${LV2SDK_PATH}/lv2/lv2/resize-port/resize-port.h
   ${LV2SDK_PATH}/lv2/lv2/state/state.h
   ${LV2SDK_PATH}/lv2/lv2/time/time.h
   ${LV2SDK_PATH}/lv2/lv2/ui/ui.h
   ${LV2SDK_PATH}/lv2/lv2/units/units.h
   ${LV2SDK_PATH}/lv2/lv2/uri-map/uri-map.h
   ${LV2SDK_PATH}/lv2/lv2/urid/urid.h
   ${LV2SDK_PATH}/lv2/lv2/worker/worker.h
)

set( src ${LV2SDK_PATH}/lv2 )
set( dst ${_PUBDIR} )
set( ns ${dst}/lv2/lv2plug.in/ns )
set( stamp ${_INTDIR}/.stamp.lv2 )

# Simulate the older directory structure (trailing "/" is important)
file( MAKE_DIRECTORY ${ns} )
file( COPY ${src}/lv2/core/lv2.h DESTINATION ${dst} )
file( COPY ${src}/lv2/ DESTINATION ${ns}/ext )
file( COPY ${src}/lv2/ DESTINATION ${ns}/extensions )
file( COPY ${src}/lv2/core/ DESTINATION  ${ns}/lv2core )

set( LILV_VERSION 0.24.4 )
set( SERD_VERSION 0.30.2 )
set( SORD_VERSION 0.16.4 )
set( SRATOM_VERSION 0.6.4 )
set( SUIL_VERSION 0.10.6 )

set( HAVE_LV2 1 )
set( HAVE_SERD 1 )
set( HAVE_SORD 1 )
set( HAVE_SRATOM 1 )

if( OS_IS_WIN )
   set( LILV_PATH_SEP ";" )
   set( LILV_DIR_SEP "\\\\" )
   set( LILV_DEFAULT_LV2_PATH "%APPDATA%\\\\LV2;%COMMONPROGRAMFILES%\\\\LV2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "" )
   set( SUIL_MODULE_EXT ".dll" )
elseif( OS_IS_MAC )
   set( LILV_PATH_SEP ":" )
   set( LILV_DIR_SEP "/" )
   set( LILV_DEFAULT_LV2_PATH "~/Library/Audio/Plug-Ins/LV2:~/.lv2:/usr/local/lib/lv2:/usr/lib/lv2:/Library/Audio/Plug-Ins/LV2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "lib" )
   set( SUIL_MODULE_EXT ".dylib" )
elseif( OS_IS_LIN )
   set( LILV_PATH_SEP ":" )
   set( LILV_DIR_SEP "/" )
   set( LILV_DEFAULT_LV2_PATH "~/.lv2:/usr/lib/lv2:/usr/local/lib/lv2" )

   set( SUIL_MODULE_DIR "" )
   set( SUIL_DIR_SEP "" )
   set( SUIL_GTK2_LIB_NAME "libgtk-x11-2.0.so.0" )
   set( SUIL_GTK3_LIB_NAME "libgtk-x11-3.0.so.0" )
   set( SUIL_MODULE_PREFIX "lib" )
   set( SUIL_MODULE_EXT ".so" )
endif()

configure_file( lilv_config.h.in "${_PRVDIR}/lilv_config.h" )
configure_file( serd_config.h.in "${_PRVDIR}/serd_config.h" )
configure_file( sord_config.h.in "${_PRVDIR}/sord_config.h" )
configure_file( sratom_config.h.in "${_PRVDIR}/sratom_config.h" )
configure_file( suil_config.h.in "${_PRVDIR}/suil_config.h" )

set(MODULE_SRC ${SOURCES} ${HEADERS})
set(MODULE_DEF ${DEFINES})
set(MODULE_INCLUDE ${INCLUDES})
set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)

if (OS_IS_LIN)
   pkg_check_modules( X11 IMPORTED_TARGET "x11" )
   pkg_check_modules( GTK2 IMPORTED_TARGET "gtk+-2.0" )
   pkg_check_modules( GTK3 IMPORTED_TARGET "gtk+-3.0" )
   pkg_check_modules( GTK2X11 IMPORTED_TARGET "gtk+-x11-2.0" )
   pkg_check_modules( GTK3X11 IMPORTED_TARGET "gtk+-x11-3.0" )
   pkg_check_modules( QT4 IMPORTED_TARGET "QtGui >= 4.4.0" )
   pkg_check_modules( QT5 IMPORTED_TARGET "Qt5Widgets >= 5.1.0" )

   macro( bld name packages define sources )
      set( libs )
      set( missing )
      set( lib ${SUIL_MODULE_PREFIX}${name} )
      foreach( pkg ${packages} )
         if( NOT "${${pkg}_FOUND}" )
            set( missing ON )
            break()
         endif()

         list( APPEND libs
            PRIVATE
               PkgConfig::${pkg}
         )
      endforeach()

      if( NOT missing )
         list( APPEND DEFINES
            PRIVATE
               ${define}
         )

         add_library( ${lib} MODULE ${sources} )
         add_dependencies( ${MODULE} ${lib} )

         set_target_properties( ${lib}
            PROPERTIES
               LIBRARY_OUTPUT_DIRECTORY ${_DEST}/${_PKGLIB}
               PREFIX ""
         )

         target_compile_definitions( ${lib} PRIVATE SUIL_SHARED ${DEFINES} )
         target_include_directories( ${lib} PRIVATE ${INCLUDES} )
         target_link_libraries( ${lib} PRIVATE ${libs} )
      endif()
   endmacro()

   bld( "suil_x11"
        "X11"
        "SUIL_WITH_X11"
        "${LV2SDK_PATH}/suil/src/x11.c" )
   bld( "suil_x11_in_gtk2"
        "X11;GTK2X11"
        "SUIL_WITH_X11_IN_GTK2"
        "${LV2SDK_PATH}/suil/src/x11_in_gtk2.c" )
endif()
