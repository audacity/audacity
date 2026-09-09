
set(TARGET pffft)
get_property(_src GLOBAL PROPERTY pffft_SOURCE_DIR)
set(TARGET_ROOT ${_src}/pffft)

add_library( ${TARGET} STATIC )

set(SOURCES
      ${TARGET_ROOT}/pffft.c
      ${TARGET_ROOT}/pffft.h
      ${TARGET_ROOT}/pfsimd_macros.h
)

set(INCLUDES
      ${TARGET_ROOT}
)

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PUBLIC ${INCLUDES} )
