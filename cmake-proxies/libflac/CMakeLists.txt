#directory cmake-proxies/libflac
cmake_minimum_required(VERSION 3.12)

set(TARGET libflac)
set(TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET})
project(${TARGET} VERSION 1.3.1)

option(FLAC_ENABLE_64_BIT_WORDS "Set FLAC__BYTES_PER_WORD to 8 (4 is the default)" OFF)
option(FLAC_WITH_ASM "Use any assembly optimization routines" ON)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    if(CMAKE_BULD_TYPE STREQUAL Release)
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -funroll-loops")
    endif()

    option(FLAC_ENABLE_SSP "Enable GNU GCC stack smash protection" OFF)
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "[xX]86(_64)?|(AMD|amd)64|i[346]86")
    option(FLAC_WITH_AVX "Enable AVX, AVX2 optimizations" ON)
    option(FLAC_WITH_SSE "Enable AVX, AVX2 optimizations" ON)
endif()

include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)
include(CheckSymbolExists)
include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)
include(TestBigEndian)

check_include_file("byteswap.h" HAVE_BYTESWAP_H)
check_include_file("inttypes.h" HAVE_INTTYPES_H)
check_include_file("stdint.h" HAVE_STDINT_H)
check_include_file("x86intrin.h" HAVE_X86INTRIN_H)
check_include_file("cpuid.h" HAVE_CPUID_H)
check_include_file("sys/param.h" HAVE_SYS_PARAM_H)

function(check_lround)
    set(CMAKE_REQUIRED_LIBRARIES m)
    check_function_exists(lround HAVE_LROUND)
endfunction()

check_lround()

check_c_source_compiles("int main() { return __builtin_bswap16 (0) ; }" HAVE_BSWAP16)
check_c_source_compiles("int main() { return __builtin_bswap32 (0) ; }" HAVE_BSWAP32)
check_c_source_compiles("
    int main()
    {
    #ifndef _FORTIFY_SOURCE
        return 0;
    #else
        this_is_an_error;
    #endif
    }"
    HAVENOT_FORTIFY_SOURCE)

test_big_endian(CPU_IS_BIG_ENDIAN)

check_c_compiler_flag(-Werror HAVE_WERROR_FLAG)
check_c_compiler_flag(-Wdeclaration-after-statement HAVE_DECL_AFTER_STMT_FLAG)
check_c_compiler_flag("-fstack-protector --param ssp-buffer-size=4" HAVE_SSP_FLAG)
check_c_compiler_flag("-mstackrealign" HAVE_STACKREALIGN_FLAG)
check_c_compiler_flag(-msse2 HAVE_MSSE2_FLAG)
check_cxx_compiler_flag(-Weffc++ HAVE_WEFFCXX_FLAG)

add_compile_definitions(
    VERSION="${PROJECT_VERSION}"
    $<$<BOOL:${HAVE_BYTESWAP_H}>:HAVE_BYTESWAP_H>
    $<$<BOOL:${HAVE_INTTYPES_H}>:HAVE_INTTYPES_H>
    $<$<BOOL:${HAVE_STDINT_H}>:HAVE_STDINT_H>
    $<$<BOOL:${MSVC}>:_CRT_SECURE_NO_WARNINGS>
    $<$<BOOL:${MSVC}>:_USE_MATH_DEFINES>
    $<$<BOOL:${HAVE_ICONV}>:HAVE_ICONV>
    $<$<BOOL:${HAVE_LROUND}>:HAVE_LROUND>
    $<$<BOOL:${HAVE_CPUID_H}>:HAVE_CPUID_H>
    $<$<BOOL:${HAVE_SYS_PARAM_H}>:HAVE_SYS_PARAM_H>
    $<$<BOOL:${ENABLE_64_BIT_WORDS}>:ENABLE_64_BIT_WORDS>
    HAVE_BSWAP16=$<BOOL:${HAVE_BSWAP16}>
    HAVE_BSWAP32=$<BOOL:${HAVE_BSWAP32}>
    ENABLE_64_BIT_WORDS=$<BOOL:${ENABLE_64_BIT_WORDS}>
    WORDS_BIGENDIAN=$<BOOL:${CPU_IS_BIG_ENDIAN}>
    CPU_IS_BIG_ENDIAN=$<BOOL:${CPU_IS_BIG_ENDIAN}>
    CPU_IS_LITTLE_ENDIAN=$<NOT:${CPU_IS_BIG_ENDIAN}>
    FLAC__HAS_X86INTRIN=$<BOOL:${HAVE_X86INTRIN_H}>
    FLAC__USE_AVX=$<BOOL:${WITH_AVX}>
    $<$<OR:$<CONFIG:Debug>,$<CONFIG:RelWithDebInfo>>:FLAC__OVERFLOW_DETECT>
    _ALL_SOURCE
    _DARWIN_C_SOURCE
    _GNU_SOURCE
    _POSIX_PTHREAD_SEMANTICS
    __STDC_WANT_IEC_60559_ATTRIBS_EXT__
    __STDC_WANT_IEC_60559_BFP_EXT__
    __STDC_WANT_IEC_60559_DFP_EXT__
    __STDC_WANT_IEC_60559_FUNCS_EXT__
    __STDC_WANT_IEC_60559_TYPES_EXT__
    __STDC_WANT_LIB_EXT2__
    __STDC_WANT_MATH_SPEC_FUNCS__
    _TANDEM_SOURCE
    $<$<AND:$<BOOL:${HAVENOT_FORTIFY_SOURCE}>,$<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>>:_FORTIFY_SOURCE=2>)

add_compile_options(
    $<$<BOOL:${MSVC}>:/wd4267>
    $<$<BOOL:${MSVC}>:/wd4996>
    $<$<BOOL:${ENABLE_WERROR}>:-Werror>
    $<$<AND:$<BOOL:${HAVE_SSP_FLAG}>,$<BOOL:${ENABLE_SSP}>>:"-fstack-protector --param ssp-buffer-size=4">
    $<$<AND:$<BOOL:${HAVE_MSSE2_FLAG}>,$<BOOL:FLAC_WITH_SSE>>:-msse2>)

if(CMAKE_SYSTEM_PROCESSOR STREQUAL "i686" AND HAVE_STACKREALIGN_FLAG)
    add_compile_options(-mstackrealign)
endif()
if(HAVE_WEFFCXX_FLAG)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Weffc++")
endif()
if(HAVE_DECL_AFTER_STMT_FLAG)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wdeclaration-after-statement")
endif()

if(CMAKE_SYSTEM_PROCESSOR MATCHES "[xX]86_64|(AMD|amd)64")
    if(CMAKE_SIZEOF_VOID_P EQUAL 4)
        set(IA32 TRUE)
    endif()
    add_compile_definitions(
        FLAC__CPU_X86_64
        FLAC__ALIGN_MALLOC_DATA)
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "([xX]|i[346])86")
    set(IA32 TRUE)
    add_compile_definitions(
        FLAC__CPU_IA32
        FLAC__ALIGN_MALLOC_DATA)
endif()

include(CheckLanguage)
check_language(ASM_NASM)
if(CMAKE_ASM_NASM_COMPILER)
    enable_language(ASM_NASM)
    add_compile_definitions(FLAC__HAS_NASM)
endif()

if(NOT WITH_ASM)
    add_compile_definitions(FLAC__NO_ASM)
endif()

if(WITH_ASM AND IA32 AND CMAKE_ASM_NASM_COMPILER)
    if(APPLE)
        set(CMAKE_ASM_NASM_FLAGS -dOBJ_FORMAT_macho)
    elseif(WIN32)
        set(CMAKE_ASM_NASM_FLAGS -dOBJ_FORMAT_win32)
    else()
        set(CMAKE_ASM_NASM_FLAGS -dOBJ_FORMAT_elf)
    endif()

    add_library(FLAC-asm STATIC
        "${TARGET_SOURCE}/src/libFLAC/ia32/cpu_asm.nasm"
        "${TARGET_SOURCE}/src/libFLAC/ia32/fixed_asm.nasm"
        "${TARGET_SOURCE}/src/libFLAC/ia32/lpc_asm.nasm")
    set_target_properties(FLAC-asm PROPERTIES COMPILE_OPTIONS "")
    target_include_directories(FLAC-asm PRIVATE "${TARGET_SOURCE}/src/libFLAC/ia32/")
endif()

add_library(FLAC-static STATIC
    "${TARGET_SOURCE}/src/libFLAC/bitmath.c"
    "${TARGET_SOURCE}/src/libFLAC/bitreader.c"
    "${TARGET_SOURCE}/src/libFLAC/bitwriter.c"
    "${TARGET_SOURCE}/src/libFLAC/cpu.c"
    "${TARGET_SOURCE}/src/libFLAC/crc.c"
    "${TARGET_SOURCE}/src/libFLAC/fixed.c"
    "${TARGET_SOURCE}/src/libFLAC/fixed_intrin_sse2.c"
    "${TARGET_SOURCE}/src/libFLAC/fixed_intrin_ssse3.c"
    "${TARGET_SOURCE}/src/libFLAC/float.c"
    "${TARGET_SOURCE}/src/libFLAC/format.c"
    "${TARGET_SOURCE}/src/libFLAC/lpc.c"
    "${TARGET_SOURCE}/src/libFLAC/lpc_intrin_sse.c"
    "${TARGET_SOURCE}/src/libFLAC/lpc_intrin_sse2.c"
    "${TARGET_SOURCE}/src/libFLAC/lpc_intrin_sse41.c"
    "${TARGET_SOURCE}/src/libFLAC/lpc_intrin_avx2.c"
    "${TARGET_SOURCE}/src/libFLAC/md5.c"
    "${TARGET_SOURCE}/src/libFLAC/memory.c"
    "${TARGET_SOURCE}/src/libFLAC/metadata_iterators.c"
    "${TARGET_SOURCE}/src/libFLAC/metadata_object.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_decoder.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_encoder.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_encoder_intrin_sse2.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_encoder_intrin_ssse3.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_encoder_intrin_avx2.c"
    "${TARGET_SOURCE}/src/libFLAC/stream_encoder_framing.c"
    "${TARGET_SOURCE}/src/libFLAC/window.c"
    "${TARGET_SOURCE}/src/libFLAC/ogg_decoder_aspect.c"
    "${TARGET_SOURCE}/src/libFLAC/ogg_encoder_aspect.c"
    "${TARGET_SOURCE}/src/libFLAC/ogg_helper.c"
    "${TARGET_SOURCE}/src/libFLAC/ogg_mapping.c")

include_directories(
    "${TARGET_SOURCE}/include"
    "${TARGET_SOURCE}/src/libFLAC/include")

target_compile_definitions(FLAC-static PUBLIC FLAC__NO_DLL FLAC__HAS_OGG)
target_include_directories(FLAC-static INTERFACE
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>")
target_link_libraries(FLAC-static PUBLIC
    libogg
    $<TARGET_NAME_IF_EXISTS:FLAC-asm>
    $<$<BOOL:${HAVE_LROUND}>:m>)
set_target_properties(FLAC-static PROPERTIES OSX_ARCHITECTURES "")

add_library(FLACXX-static STATIC
    "${TARGET_SOURCE}/src/libFLAC++/metadata.cpp"
    "${TARGET_SOURCE}/src/libFLAC++/stream_decoder.cpp"
    "${TARGET_SOURCE}/src/libFLAC++/stream_encoder.cpp")
target_compile_definitions(FLACXX-static PUBLIC FLAC__NO_DLL)
target_include_directories(FLACXX-static INTERFACE
    "$<BUILD_INTERFACE:${TARGET_SOURCE}/include>")
target_link_libraries(FLACXX-static PUBLIC FLAC-static)
set_target_properties(FLACXX-static PROPERTIES OSX_ARCHITECTURES "")
