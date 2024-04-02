
set(TARGET pffft)
set(TARGET_ROOT ${AUDACITY_ROOT}/lib-src/pffft)

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
