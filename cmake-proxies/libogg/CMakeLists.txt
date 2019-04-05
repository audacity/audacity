#directory cmake-proxies/libogg
set(TARGET libogg)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})

project(${TARGET})

include(CheckIncludeFile)
include(CheckTypeSize)

check_include_file("inttypes.h" INCLUDE_INTTYPES_H)
check_include_file("stdint.h" INCLUDE_STDINT_H)
check_include_file("sys/types.h" INCLUDE_SYS_TYPES_H)

check_type_size("int16_t" INT16_SIZE LANGUAGE C)
check_type_size("uint16_t" UINT16_SIZE LANGUAGE C)
check_type_size("u_int16_t" U_INT16_SIZE LANGUAGE C)
check_type_size("int32_t" INT32_SIZE LANGUAGE C)
check_type_size("uint32_t" UINT32_SIZE LANGUAGE C)
check_type_size("u_int32_t" U_INT32_SIZE LANGUAGE C)
check_type_size("int64_t" INT64_SIZE LANGUAGE C)
check_type_size("short" SHORT_SIZE LANGUAGE C)
check_type_size("int" INT_SIZE LANGUAGE C)
check_type_size("long" LONG_SIZE LANGUAGE C)
check_type_size("long long" LONG_LONG_SIZE LANGUAGE C)

if(INT16_SIZE EQUAL 2)
    set(SIZE16 "int16_t")
elseif(SHORT_SIZE EQUAL 2)
    set(SIZE16 "short")
elseif(INT_SIZE EQUAL 2)
    set(SIZE16 "int")
else()
    message(FATAL_ERROR "No 16 bit type found on this platform!")
endif()

if(UINT16_SIZE EQUAL 2)
    set(USIZE16 "uint16_t")
elseif(SHORT_SIZE EQUAL 2)
    set(USIZE16 "unsigned short")
elseif(INT_SIZE EQUAL 2)
    set(USIZE16 "unsigned int")
elseif(U_INT_SIZE EQUAL 2)
    set(USIZE16 "u_int16_t")
else()
    message(FATAL_ERROR "No unsigned 16 bit type found on this platform!")
endif()

if(INT32_SIZE EQUAL 4)
    set(SIZE32 "int32_t")
elseif(SHORT_SIZE EQUAL 4)
    set(SIZE32 "short")
elseif(INT_SIZE EQUAL 4)
    set(SIZE32 "int")
elseif(LONG_SIZE EQUAL 4)
    set(SIZE16 "long")
else()
    message(FATAL_ERROR "No 32 bit type found on this platform!")
endif()

if(UINT32_SIZE EQUAL 4)
    set(USIZE32 "uint32_t")
elseif(SHORT_SIZE EQUAL 4)
    set(USIZE32 "unsigned short")
elseif(INT_SIZE EQUAL 4)
    set(USIZE32 "unsigned int")
elseif(LONG_SIZE EQUAL 4)
    set(USIZE32 "unsigned long")
elseif(U_INT_SIZE EQUAL 4)
    set(USIZE32 "u_int32_t")
else()
    message(FATAL_ERROR "No unsigned 32 bit type found on this platform!")
endif()

if(INT64_SIZE EQUAL 8)
    set(SIZE64 "int64_t")
elseif(INT_SIZE EQUAL 8)
    set(SIZE64 "int")
elseif(LONG_SIZE EQUAL 8)
    set(SIZE64 "long")
elseif(LONG_LONG_SIZE EQUAL 8)
    set(SIZE64 "long long")
else()
    message(FATAL_ERROR "No 64 bit type found on this platform!")
endif()

configure_file(
    "${LIB_SRC_DIRECTORY}libogg/include/ogg/config_types.h.in"
    "ogg/config_types.h")

add_library(${TARGET}
    "${LIB_SRC_DIRECTORY}libogg/src/bitwise.c"
    "${LIB_SRC_DIRECTORY}libogg/src/framing.c")

target_include_directories(${TARGET} PUBLIC
    "${TARGET_SOURCE}/include"
    "${CMAKE_CURRENT_BINARY_DIR}")
