#
# Audacity: A Digital Audio Editor
#
declare_module(lv2sdk)

FetchContent_Declare(
   lv2
   GIT_REPOSITORY https://gitlab.com/lv2/lv2.git
   # The latest tag, v1.18.10, doesn't have the Qt6UI type yet,
   # which we want to give a chance to.
   GIT_TAG 4b8760cec9636a1d9757afa79ceee2111b86e98b
)
FetchContent_MakeAvailable(lv2)

set( LILV_VERSION 0.24.26 )
FetchContent_Declare(
   lilv
   GIT_REPOSITORY https://gitlab.com/lv2/lilv.git
   GIT_TAG v${LILV_VERSION}
)
FetchContent_MakeAvailable(lilv)

FetchContent_Declare(
   zix
   GIT_REPOSITORY https://github.com/drobilla/zix.git
   GIT_TAG v0.6.2
)
FetchContent_MakeAvailable(zix)
   
set( SORD_VERSION 0.16.18 )
FetchContent_Declare(
   sord
   GIT_REPOSITORY https://github.com/drobilla/sord.git
   GIT_TAG v${SORD_VERSION}
)
FetchContent_MakeAvailable(sord)

set( SERD_VERSION 0.32.4 )
FetchContent_Declare(
   serd
   GIT_REPOSITORY https://github.com/drobilla/serd.git
   GIT_TAG v${SERD_VERSION}
)
FetchContent_MakeAvailable(serd)

set( SRATOM_VERSION 0.6.18 )
FetchContent_Declare(
   sratom
   GIT_REPOSITORY https://gitlab.com/lv2/sratom.git
   GIT_TAG v${SRATOM_VERSION}
)
FetchContent_MakeAvailable(sratom)

set( SUIL_VERSION 0.10.22 )
FetchContent_Declare(
   suil
   GIT_REPOSITORY https://gitlab.com/lv2/suil.git
   GIT_TAG v${SUIL_VERSION}
)
FetchContent_MakeAvailable(suil)

set( SOURCES
   PRIVATE

      # zix
      ${zix_SOURCE_DIR}/src/allocator.c
      ${zix_SOURCE_DIR}/src/bump_allocator.c
      ${zix_SOURCE_DIR}/src/digest.c
      ${zix_SOURCE_DIR}/src/errno_status.h
      ${zix_SOURCE_DIR}/src/hash.c
      ${zix_SOURCE_DIR}/src/path.c
      ${zix_SOURCE_DIR}/src/status.c
      ${zix_SOURCE_DIR}/src/system.c
      ${zix_SOURCE_DIR}/src/tree.c
      ${zix_SOURCE_DIR}/src/zix_config.h
      ${zix_SOURCE_DIR}/src/btree.c
      ${zix_SOURCE_DIR}/src/errno_status.c
      ${zix_SOURCE_DIR}/src/filesystem.c
      ${zix_SOURCE_DIR}/src/index_range.h
      ${zix_SOURCE_DIR}/src/path_iter.h
      ${zix_SOURCE_DIR}/src/ring.c
      ${zix_SOURCE_DIR}/src/string_view.c
      ${zix_SOURCE_DIR}/src/system.h
      # We only target Linux for now, but the darwin and win32 source files are
      # available if needed.
      ${zix_SOURCE_DIR}/src/posix/environment_posix.c
      ${zix_SOURCE_DIR}/src/posix/filesystem_posix.c
      ${zix_SOURCE_DIR}/src/posix/sem_posix.c
      ${zix_SOURCE_DIR}/src/posix/system_posix.c
      ${zix_SOURCE_DIR}/src/posix/thread_posix.c

      # lilv

      ${lilv_SOURCE_DIR}/src/collections.c
      ${lilv_SOURCE_DIR}/src/dylib.c
      ${lilv_SOURCE_DIR}/src/dylib.h
      ${lilv_SOURCE_DIR}/src/instance.c
      ${lilv_SOURCE_DIR}/src/lib.c
      ${lilv_SOURCE_DIR}/src/lilv_config.h
      ${lilv_SOURCE_DIR}/src/lilv_internal.h
      ${lilv_SOURCE_DIR}/src/node.c
      ${lilv_SOURCE_DIR}/src/plugin.c
      ${lilv_SOURCE_DIR}/src/pluginclass.c
      ${lilv_SOURCE_DIR}/src/port.c
      ${lilv_SOURCE_DIR}/src/query.c
      ${lilv_SOURCE_DIR}/src/scalepoint.c
      ${lilv_SOURCE_DIR}/src/state.c
      ${lilv_SOURCE_DIR}/src/ui.c
      ${lilv_SOURCE_DIR}/src/util.c
      ${lilv_SOURCE_DIR}/src/world.c

      # serd

      ${serd_SOURCE_DIR}/src/attributes.h
      ${serd_SOURCE_DIR}/src/base64.c
      ${serd_SOURCE_DIR}/src/base64.h
      ${serd_SOURCE_DIR}/src/byte_sink.h
      ${serd_SOURCE_DIR}/src/byte_source.c
      ${serd_SOURCE_DIR}/src/byte_source.h
      ${serd_SOURCE_DIR}/src/env.c
      ${serd_SOURCE_DIR}/src/n3.c
      ${serd_SOURCE_DIR}/src/node.c
      ${serd_SOURCE_DIR}/src/reader.c
      ${serd_SOURCE_DIR}/src/reader.h
      ${serd_SOURCE_DIR}/src/serd_config.h
      ${serd_SOURCE_DIR}/src/serd_internal.h
      ${serd_SOURCE_DIR}/src/serdi.c
      ${serd_SOURCE_DIR}/src/stack.h
      ${serd_SOURCE_DIR}/src/string.c
      ${serd_SOURCE_DIR}/src/string_utils.h
      ${serd_SOURCE_DIR}/src/system.c
      ${serd_SOURCE_DIR}/src/system.h
      ${serd_SOURCE_DIR}/src/try.h
      ${serd_SOURCE_DIR}/src/uri.c
      ${serd_SOURCE_DIR}/src/uri_utils.h
      ${serd_SOURCE_DIR}/src/warnings.h
      ${serd_SOURCE_DIR}/src/writer.c

      # sord

      ${sord_SOURCE_DIR}/src/sord.c
      ${sord_SOURCE_DIR}/src/sord_config.h
      ${sord_SOURCE_DIR}/src/sord_internal.h
      ${sord_SOURCE_DIR}/src/sord_validate.c
      ${sord_SOURCE_DIR}/src/sordi.c
      ${sord_SOURCE_DIR}/src/syntax.c

      # sratom

      ${sratom_SOURCE_DIR}/src/sratom.c

      # suil

      ${suil_SOURCE_DIR}/src/host.c
      ${suil_SOURCE_DIR}/src/instance.c
      ${suil_SOURCE_DIR}/src/suil_internal.h
)

set( INCLUDES
   PRIVATE
      ${lilv_SOURCE_DIR}/src
      ${zix_SOURCE_DIR}/src
      ${serd_SOURCE_DIR}/src
      ${sord_SOURCE_DIR}/src
      ${sratom_SOURCE_DIR}/src
      ${suil_SOURCE_DIR}/src
   PUBLIC
      ${zix_SOURCE_DIR}/include
      ${lv2_SOURCE_DIR}/include
      ${lilv_SOURCE_DIR}/include
      ${serd_SOURCE_DIR}/include
      ${sord_SOURCE_DIR}/include
      ${sratom_SOURCE_DIR}/include
      ${suil_SOURCE_DIR}/include
)

set( SUIL_DEFS
   PRIVATE
      HAVE_X11=OFF
      SUIL_MODULE_DIR=""
)

set(MODULE_SRC ${SOURCES})
set(MODULE_DEF ${SUIL_DEFS})
set(MODULE_INCLUDE ${INCLUDES})
set(MODULE_USE_PCH OFF)
set(MODULE_USE_UNITY OFF)

setup_module()

target_no_warning(${MODULE} -w)
