add_library( ${TARGET} STATIC )

set(SOURCES
      ${TARGET_ROOT}/pffft.c
      ${TARGET_ROOT}/pffft.h
      ${TARGET_ROOT}/pfsimd_macros.h
)

set(INCLUDES
      ${TARGET_ROOT}
)

set_target_properties( ${TARGET} PROPERTIES FOLDER "lib-src" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_include_directories( ${TARGET} PUBLIC ${INCLUDES} )
set_property(TARGET ${TARGET} PROPERTY POSITION_INDEPENDENT_CODE ON)
