// This header was generated from the FFMPEG headers
#pragma once

#include <errno.h>
#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <limits.h>
#include <stddef.h>
#include <math.h>
#include <stdarg.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#define AVCODEC_AVCODEC_H

#define AVUTIL_SAMPLEFMT_H

#define AVUTIL_AVUTIL_H

unsigned avutil_version(void);

const char* avutil_configuration(void);

const char* avutil_license(void);

enum AVMediaType {
    AVMEDIA_TYPE_UNKNOWN = -1,
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,
    AVMEDIA_TYPE_NB
};

const char* av_get_media_type_string(enum AVMediaType media_type);

#define FF_LAMBDA_SHIFT 7
#define FF_LAMBDA_SCALE (1 << FF_LAMBDA_SHIFT)
#define FF_QP2LAMBDA 118
#define FF_LAMBDA_MAX (256 * 128 - 1)

#define FF_QUALITY_SCALE FF_LAMBDA_SCALE

#define AV_NOPTS_VALUE          ((int64_t)UINT64_C(0x8000000000000000))

#define AV_TIME_BASE            1000000

#define AV_TIME_BASE_Q          (AVRational) { 1, AV_TIME_BASE }

enum AVPictureType {
    AV_PICTURE_TYPE_NONE = 0,
    AV_PICTURE_TYPE_I,
    AV_PICTURE_TYPE_P,
    AV_PICTURE_TYPE_B,
    AV_PICTURE_TYPE_S,
    AV_PICTURE_TYPE_SI,
    AV_PICTURE_TYPE_SP,
    AV_PICTURE_TYPE_BI,
};

char av_get_picture_type_char(enum AVPictureType pict_type);

#define AVUTIL_COMMON_H

#define AVUTIL_ATTRIBUTES_H

#    define AV_GCC_VERSION_AT_LEAST(x, y) 0

#    define av_always_inline inline

#    define av_extern_inline inline

#    define av_noinline

#    define av_pure

#    define av_const

#    define av_cold

#    define av_flatten

#    define attribute_deprecated

#    define AV_NOWARN_DEPRECATED(code) code

#    define av_unused

#    define av_used

#   define av_alias

#    define av_uninit(x) x

#    define av_builtin_constant_p(x) 0
#    define av_printf_format(fmtpos, attrpos)

#    define av_noreturn

#define AVUTIL_VERSION_H

#define AVUTIL_MACROS_H

#define AV_STRINGIFY(s)         AV_TOSTRING(s)
#define AV_TOSTRING(s) #s

#define AV_GLUE(a, b) a##b
#define AV_JOIN(a, b) AV_GLUE(a, b)

#define AV_PRAGMA(s) _Pragma(#s)

#define AV_VERSION_INT(a, b, c) (a << 16 | b << 8 | c)
#define AV_VERSION_DOT(a, b, c) a##.##b##.##c
#define AV_VERSION(a, b, c) AV_VERSION_DOT(a, b, c)

#define LIBAVUTIL_VERSION_MAJOR  52
#define LIBAVUTIL_VERSION_MINOR  92
#define LIBAVUTIL_VERSION_MICRO 100

#define LIBAVUTIL_VERSION_INT   AV_VERSION_INT(LIBAVUTIL_VERSION_MAJOR, \
                                               LIBAVUTIL_VERSION_MINOR, \
                                               LIBAVUTIL_VERSION_MICRO)
#define LIBAVUTIL_VERSION       AV_VERSION(LIBAVUTIL_VERSION_MAJOR,     \
                                           LIBAVUTIL_VERSION_MINOR,     \
                                           LIBAVUTIL_VERSION_MICRO)
#define LIBAVUTIL_BUILD         LIBAVUTIL_VERSION_INT

#define LIBAVUTIL_IDENT         "Lavu" AV_STRINGIFY(LIBAVUTIL_VERSION)

#define FF_API_GET_BITS_PER_SAMPLE_FMT (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_FIND_OPT                 (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_OLD_AVOPTIONS            (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_PIX_FMT                  (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_CONTEXT_SIZE             (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_PIX_FMT_DESC             (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_AV_REVERSE               (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_AUDIOCONVERT             (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_CPU_FLAG_MMX2            (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_SAMPLES_UTILS_RETURN_ZERO (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_LLS_PRIVATE              (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_LLS1                     (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_AVFRAME_LAVC             (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_VDPAU                    (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_GET_CHANNEL_LAYOUT_COMPAT (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_OLD_OPENCL               (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_XVMC                     (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_INTFLOAT                 (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_OPT_TYPE_METADATA        (LIBAVUTIL_VERSION_MAJOR < 54)
#define FF_API_AVFRAME_COLORSPACE       (LIBAVUTIL_VERSION_MAJOR >= 52)

#define FF_CONST_AVUTIL53

#   define AV_NE(be, le) (le)

#define RSHIFT(a, b) ((a) > 0 ? ((a) + ((1 << (b)) >> 1)) >> (b) : ((a) + ((1 << (b)) >> 1) - 1) >> (b))

#define ROUNDED_DIV(a, b) (((a) > 0 ? (a) + ((b) >> 1) : (a) - ((b) >> 1)) / (b))

#define FF_CEIL_RSHIFT(a, b) (!av_builtin_constant_p(b) ? -((-(a)) >> (b)) \
                              : ((a) + (1 << (b)) - 1) >> (b))
#define FFUDIV(a, b) (((a) > 0 ? (a) : (a) - (b) + 1) / (b))
#define FFUMOD(a, b) ((a) - (b) * FFUDIV(a, b))
#define FFABS(a) ((a) >= 0 ? (a) : (-(a)))
#define FFSIGN(a) ((a) > 0 ? 1 : -1)

#define FFMAX(a, b) ((a) > (b) ? (a) : (b))
#define FFMAX3(a, b, c) FFMAX(FFMAX(a, b), c)
#define FFMIN(a, b) ((a) > (b) ? (b) : (a))
#define FFMIN3(a, b, c) FFMIN(FFMIN(a, b), c)

#define FFSWAP(type, a, b) do{ type SWAP_tmp= b; b= a; a= SWAP_tmp; }while (0)
#define FF_ARRAY_ELEMS(a) (sizeof(a) / sizeof((a)[0]))
#define FFALIGN(x, a) (((x) + (a) - 1) & ~((a) - 1))

extern attribute_deprecated const uint8_t av_reverse[256];

#   define av_ceil_log2     av_ceil_log2_c
#   define av_clip          av_clip_c
#   define av_clip64        av_clip64_c
#   define av_clip_uint8    av_clip_uint8_c
#   define av_clip_int8     av_clip_int8_c
#   define av_clip_uint16   av_clip_uint16_c
#   define av_clip_int16    av_clip_int16_c
#   define av_clipl_int32   av_clipl_int32_c
#   define av_clip_uintp2   av_clip_uintp2_c
#   define av_sat_add32     av_sat_add32_c
#   define av_sat_dadd32    av_sat_dadd32_c
#   define av_clipf         av_clipf_c
#   define av_clipd         av_clipd_c
#   define av_popcount      av_popcount_c
#   define av_popcount64    av_popcount64_c

av_const int av_log2(unsigned v);

av_const int av_log2_16bit(unsigned v);

static av_always_inline av_const int av_clip_c(int a, int amin, int amax)
{
    if (a < amin) {
        return amin;
    } else if (a > amax) {
        return amax;
    } else {
        return a;
    }
}

static av_always_inline av_const int64_t av_clip64_c(int64_t a, int64_t amin, int64_t amax)
{
    if (a < amin) {
        return amin;
    } else if (a > amax) {
        return amax;
    } else {
        return a;
    }
}

static av_always_inline av_const uint8_t av_clip_uint8_c(int a)
{
    if (a & (~0xFF)) {
        return (-a) >> 31;
    } else {
        return a;
    }
}

static av_always_inline av_const int8_t av_clip_int8_c(int a)
{
    if ((a + 0x80) & ~0xFF) {
        return (a >> 31) ^ 0x7F;
    } else {
        return a;
    }
}

static av_always_inline av_const uint16_t av_clip_uint16_c(int a)
{
    if (a & (~0xFFFF)) {
        return (-a) >> 31;
    } else {
        return a;
    }
}

static av_always_inline av_const int16_t av_clip_int16_c(int a)
{
    if ((a + 0x8000) & ~0xFFFF) {
        return (a >> 31) ^ 0x7FFF;
    } else {
        return a;
    }
}

static av_always_inline av_const int32_t av_clipl_int32_c(int64_t a)
{
    if ((a + 0x80000000u) & ~UINT64_C(0xFFFFFFFF)) {
        return (int32_t)((a >> 63) ^ 0x7FFFFFFF);
    } else {
        return (int32_t)a;
    }
}

static av_always_inline av_const unsigned av_clip_uintp2_c(int a, int p)
{
    if (a & ~((1 << p) - 1)) {
        return -a >> 31 & ((1 << p) - 1);
    } else {
        return a;
    }
}

static av_always_inline int av_sat_add32_c(int a, int b)
{
    return av_clipl_int32((int64_t)a + b);
}

static av_always_inline int av_sat_dadd32_c(int a, int b)
{
    return av_sat_add32(a, av_sat_add32(b, b));
}

static av_always_inline av_const float av_clipf_c(float a, float amin, float amax)
{
    if (a < amin) {
        return amin;
    } else if (a > amax) {
        return amax;
    } else {
        return a;
    }
}

static av_always_inline av_const double av_clipd_c(double a, double amin, double amax)
{
    if (a < amin) {
        return amin;
    } else if (a > amax) {
        return amax;
    } else {
        return a;
    }
}

static av_always_inline av_const int av_ceil_log2_c(int x)
{
    return av_log2((x - 1) << 1);
}

static av_always_inline av_const int av_popcount_c(uint32_t x)
{
    x -= (x >> 1) & 0x55555555;
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x += x >> 8;
    return (x + (x >> 16)) & 0x3F;
}

static av_always_inline av_const int av_popcount64_c(uint64_t x)
{
    return av_popcount((uint32_t)x) + av_popcount((uint32_t)(x >> 32));
}

#define MKTAG(a, b, c, d) ((a) | ((b) << 8) | ((c) << 16) | ((unsigned)(d) << 24))
#define MKBETAG(a, b, c, d) ((d) | ((c) << 8) | ((b) << 16) | ((unsigned)(a) << 24))

#define GET_UTF8(val, GET_BYTE, ERROR) \
    val= GET_BYTE; \
    { \
        uint32_t top = (val & 128) >> 1; \
        if ((val & 0xc0) == 0x80 || val >= 0xFE) \
        ERROR \
        while (val & top) { \
            int tmp= GET_BYTE - 128; \
            if (tmp >> 6) \
            ERROR \
            val= (val << 6) +tmp; \
            top <<= 5; \
        } \
        val &= (top << 1) - 1; \
    }

#define GET_UTF16(val, GET_16BIT, ERROR) \
    val = GET_16BIT; \
    { \
        unsigned int hi = val - 0xD800; \
        if (hi < 0x800) { \
            val = GET_16BIT - 0xDC00; \
            if (val > 0x3FFU || hi > 0x3FFU) \
            ERROR \
            val += (hi << 10) +0x10000; \
        } \
    } \

#define PUT_UTF8(val, tmp, PUT_BYTE) \
    { \
        int bytes, shift; \
        uint32_t in = val; \
        if (in < 0x80) { \
            tmp = in; \
            PUT_BYTE \
        } else { \
            bytes = (av_log2(in) + 4) / 5; \
            shift = (bytes - 1) * 6; \
            tmp = (256 - (256 >> bytes)) | (in >> shift); \
            PUT_BYTE \
            while (shift >= 6) { \
                shift -= 6; \
                tmp = 0x80 | ((in >> shift) & 0x3f); \
                PUT_BYTE \
            } \
        } \
    }

#define PUT_UTF16(val, tmp, PUT_16BIT) \
    { \
        uint32_t in = val; \
        if (in < 0x10000) { \
            tmp = in; \
            PUT_16BIT \
        } else { \
            tmp = 0xD800 | ((in - 0x10000) >> 10); \
            PUT_16BIT \
                tmp = 0xDC00 | ((in - 0x10000) & 0x3FF); \
            PUT_16BIT \
        } \
    } \

#define AVUTIL_MEM_H

#define AVUTIL_ERROR_H

#define AVERROR(e) (e)
#define AVUNERROR(e) (e)

#define FFERRTAG(a, b, c, d) (-(int)MKTAG(a, b, c, d))

#define AVERROR_BSF_NOT_FOUND      FFERRTAG(0xF8, 'B', 'S', 'F')
#define AVERROR_BUG                FFERRTAG('B', 'U', 'G', '!')
#define AVERROR_BUFFER_TOO_SMALL   FFERRTAG('B', 'U', 'F', 'S')
#define AVERROR_DECODER_NOT_FOUND  FFERRTAG(0xF8, 'D', 'E', 'C')
#define AVERROR_DEMUXER_NOT_FOUND  FFERRTAG(0xF8, 'D', 'E', 'M')
#define AVERROR_ENCODER_NOT_FOUND  FFERRTAG(0xF8, 'E', 'N', 'C')
#define AVERROR_EOF                FFERRTAG('E', 'O', 'F', ' ')
#define AVERROR_EXIT               FFERRTAG('E', 'X', 'I', 'T')
#define AVERROR_EXTERNAL           FFERRTAG('E', 'X', 'T', ' ')
#define AVERROR_FILTER_NOT_FOUND   FFERRTAG(0xF8, 'F', 'I', 'L')
#define AVERROR_INVALIDDATA        FFERRTAG('I', 'N', 'D', 'A')
#define AVERROR_MUXER_NOT_FOUND    FFERRTAG(0xF8, 'M', 'U', 'X')
#define AVERROR_OPTION_NOT_FOUND   FFERRTAG(0xF8, 'O', 'P', 'T')
#define AVERROR_PATCHWELCOME       FFERRTAG('P', 'A', 'W', 'E')
#define AVERROR_PROTOCOL_NOT_FOUND FFERRTAG(0xF8, 'P', 'R', 'O')

#define AVERROR_STREAM_NOT_FOUND   FFERRTAG(0xF8, 'S', 'T', 'R')

#define AVERROR_BUG2               FFERRTAG('B', 'U', 'G', ' ')
#define AVERROR_UNKNOWN            FFERRTAG('U', 'N', 'K', 'N')
#define AVERROR_EXPERIMENTAL       (-0x2bb2afa8)

#define AV_ERROR_MAX_STRING_SIZE 64

int av_strerror(int errnum, char* errbuf, size_t errbuf_size);

static inline char* av_make_error_string(char* errbuf, size_t errbuf_size, int errnum)
{
    av_strerror(errnum, errbuf, errbuf_size);
    return errbuf;
}

#define av_err2str(errnum) \
    av_make_error_string((char[AV_ERROR_MAX_STRING_SIZE]) { 0 }, AV_ERROR_MAX_STRING_SIZE, errnum)

    #define DECLARE_ALIGNED(n, t, v)      t v
    #define DECLARE_ASM_CONST(n, t, v)    static const t v

    #define av_malloc_attrib

    #define av_alloc_size(...)

void* av_malloc(size_t size) av_malloc_attrib av_alloc_size(1);

av_alloc_size(1, 2) static inline void* av_malloc_array(size_t nmemb, size_t size)
{
    if (!size || nmemb >= INT_MAX / size) {
        return NULL;
    }
    return av_malloc(nmemb * size);
}

void* av_realloc(void* ptr, size_t size) av_alloc_size(2);

void* av_realloc_f(void* ptr, size_t nelem, size_t elsize);

int av_reallocp(void* ptr, size_t size);

av_alloc_size(2, 3) void* av_realloc_array(void* ptr, size_t nmemb, size_t size);

av_alloc_size(2, 3) int av_reallocp_array(void* ptr, size_t nmemb, size_t size);

void av_free(void* ptr);

void* av_mallocz(size_t size) av_malloc_attrib av_alloc_size(1);

void* av_calloc(size_t nmemb, size_t size) av_malloc_attrib;

av_alloc_size(1, 2) static inline void* av_mallocz_array(size_t nmemb, size_t size)
{
    if (!size || nmemb >= INT_MAX / size) {
        return NULL;
    }
    return av_mallocz(nmemb * size);
}

char* av_strdup(const char* s) av_malloc_attrib;

void* av_memdup(const void* p, size_t size);

void av_freep(void* ptr);

void av_dynarray_add(void* tab_ptr, int* nb_ptr, void* elem);

int av_dynarray_add_nofree(void* tab_ptr, int* nb_ptr, void* elem);

void* av_dynarray2_add(void** tab_ptr, int* nb_ptr, size_t elem_size, const uint8_t* elem_data);

static inline int av_size_mult(size_t a, size_t b, size_t* r)
{
    size_t t = a * b;

    if ((a | b) >= ((size_t)1 << (sizeof(size_t) * 4)) && a && t / a != b) {
        return AVERROR(EINVAL);
    }
    *r = t;
    return 0;
}

void av_max_alloc(size_t max);

void av_memcpy_backptr(uint8_t* dst, int back, int cnt);

void* av_fast_realloc(void* ptr, unsigned int* size, size_t min_size);

void av_fast_malloc(void* ptr, unsigned int* size, size_t min_size);

#define AVUTIL_RATIONAL_H

typedef struct AVRational {
    int num;
    int den;
} AVRational;

static inline AVRational av_make_q(int num, int den)
{
    AVRational r = { num, den };
    return r;
}

static inline int av_cmp_q(AVRational a, AVRational b)
{
    const int64_t tmp= a.num * (int64_t)b.den - b.num * (int64_t)a.den;

    if (tmp) {
        return (int)((tmp ^ a.den ^ b.den) >> 63) | 1;
    } else if (b.den && a.den) {
        return 0;
    } else if (a.num && b.num) {
        return (a.num >> 31) - (b.num >> 31);
    } else {
        return INT_MIN;
    }
}

static inline double av_q2d(AVRational a)
{
    return a.num / (double)a.den;
}

int av_reduce(int* dst_num, int* dst_den, int64_t num, int64_t den, int64_t max);

AVRational av_mul_q(AVRational b, AVRational c) av_const;

AVRational av_div_q(AVRational b, AVRational c) av_const;

AVRational av_add_q(AVRational b, AVRational c) av_const;

AVRational av_sub_q(AVRational b, AVRational c) av_const;

static av_always_inline AVRational av_inv_q(AVRational q)
{
    AVRational r = { q.den, q.num };
    return r;
}

AVRational av_d2q(double d, int max) av_const;

int av_nearer_q(AVRational q, AVRational q1, AVRational q2);

int av_find_nearest_q_idx(AVRational q, const AVRational* q_list);

#define AVUTIL_MATHEMATICS_H

#define AVUTIL_INTFLOAT_H

union av_intfloat32 {
    uint32_t i;
    float f;
};

union av_intfloat64 {
    uint64_t i;
    double f;
};

static av_always_inline float av_int2float(uint32_t i)
{
    union av_intfloat32 v;
    v.i = i;
    return v.f;
}

static av_always_inline uint32_t av_float2int(float f)
{
    union av_intfloat32 v;
    v.f = f;
    return v.i;
}

static av_always_inline double av_int2double(uint64_t i)
{
    union av_intfloat64 v;
    v.i = i;
    return v.f;
}

static av_always_inline uint64_t av_double2int(double f)
{
    union av_intfloat64 v;
    v.f = f;
    return v.i;
}

#define M_E            2.7182818284590452354
#define M_LN2          0.69314718055994530942
#define M_LN10         2.30258509299404568402
#define M_LOG2_10      3.32192809488736234787
#define M_PHI          1.61803398874989484820
#define M_PI           3.14159265358979323846
#define M_PI_2         1.57079632679489661923
#define M_SQRT1_2      0.70710678118654752440
#define M_SQRT2        1.41421356237309504880
#define NAN            av_int2float(0x7fc00000)
#define INFINITY       av_int2float(0x7f800000)

enum AVRounding {
    AV_ROUND_ZERO     = 0,
    AV_ROUND_INF      = 1,
    AV_ROUND_DOWN     = 2,
    AV_ROUND_UP       = 3,
    AV_ROUND_NEAR_INF = 5,
    AV_ROUND_PASS_MINMAX = 8192,
};

int64_t av_const av_gcd(int64_t a, int64_t b);

int64_t av_rescale(int64_t a, int64_t b, int64_t c) av_const;

int64_t av_rescale_rnd(int64_t a, int64_t b, int64_t c, enum AVRounding) av_const;

int64_t av_rescale_q(int64_t a, AVRational bq, AVRational cq) av_const;

int64_t av_rescale_q_rnd(int64_t a, AVRational bq, AVRational cq, enum AVRounding) av_const;

int av_compare_ts(int64_t ts_a, AVRational tb_a, int64_t ts_b, AVRational tb_b);

int64_t av_compare_mod(uint64_t a, uint64_t b, uint64_t mod);

int64_t av_rescale_delta(AVRational in_tb, int64_t in_ts,  AVRational fs_tb, int duration, int64_t* last, AVRational out_tb);

int64_t av_add_stable(AVRational ts_tb, int64_t ts, AVRational inc_tb, int64_t inc);

#define AVUTIL_LOG_H

typedef enum {
    AV_CLASS_CATEGORY_NA = 0,
    AV_CLASS_CATEGORY_INPUT,
    AV_CLASS_CATEGORY_OUTPUT,
    AV_CLASS_CATEGORY_MUXER,
    AV_CLASS_CATEGORY_DEMUXER,
    AV_CLASS_CATEGORY_ENCODER,
    AV_CLASS_CATEGORY_DECODER,
    AV_CLASS_CATEGORY_FILTER,
    AV_CLASS_CATEGORY_BITSTREAM_FILTER,
    AV_CLASS_CATEGORY_SWSCALER,
    AV_CLASS_CATEGORY_SWRESAMPLER,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT = 40,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_INPUT,
    AV_CLASS_CATEGORY_NB,
}AVClassCategory;

struct AVOptionRanges;

typedef struct AVClass {
    const char* class_name;

    const char*(* item_name)(void* ctx);

    const struct AVOption* option;

    int version;

    int log_level_offset_offset;

    int parent_log_context_offset;

    void*(* child_next)(void* obj, void* prev);

    const struct AVClass*(* child_class_next)(const struct AVClass* prev);

    AVClassCategory category;

    AVClassCategory (* get_category)(void* ctx);

    int (* query_ranges)(struct AVOptionRanges**, void* obj, const char* key, int flags);
} AVClass;

#define AV_LOG_QUIET    -8

#define AV_LOG_PANIC     0

#define AV_LOG_FATAL     8

#define AV_LOG_ERROR    16

#define AV_LOG_WARNING  24

#define AV_LOG_INFO     32

#define AV_LOG_VERBOSE  40

#define AV_LOG_DEBUG    48

#define AV_LOG_MAX_OFFSET (AV_LOG_DEBUG - AV_LOG_QUIET)

#define AV_LOG_C(x) (x << 8)

void av_log(void* avcl, int level, const char* fmt, ...) av_printf_format(3, 4);

void av_vlog(void* avcl, int level, const char* fmt, va_list vl);

int av_log_get_level(void);

void av_log_set_level(int level);

void av_log_set_callback(void (*callback)(void*, int, const char*, va_list));

void av_log_default_callback(void* avcl, int level, const char* fmt, va_list vl);

const char* av_default_item_name(void* ctx);
AVClassCategory av_default_get_category(void* ptr);

void av_log_format_line(void* ptr, int level, const char* fmt, va_list vl, char* line, int line_size, int* print_prefix);

#    define av_dlog(pctx, ...) do { if (0) av_log(pctx, AV_LOG_DEBUG, __VA_ARGS__); } while (0)

#define AV_LOG_SKIP_REPEATED 1

#define AV_LOG_PRINT_LEVEL 2

void av_log_set_flags(int arg);
int av_log_get_flags(void);

#define AVUTIL_PIXFMT_H

#define AVPALETTE_SIZE 1024
#define AVPALETTE_COUNT 256

enum AVPixelFormat {
    AV_PIX_FMT_NONE = -1,
    AV_PIX_FMT_YUV420P,
    AV_PIX_FMT_YUYV422,
    AV_PIX_FMT_RGB24,
    AV_PIX_FMT_BGR24,
    AV_PIX_FMT_YUV422P,
    AV_PIX_FMT_YUV444P,
    AV_PIX_FMT_YUV410P,
    AV_PIX_FMT_YUV411P,
    AV_PIX_FMT_GRAY8,
    AV_PIX_FMT_MONOWHITE,
    AV_PIX_FMT_MONOBLACK,
    AV_PIX_FMT_PAL8,
    AV_PIX_FMT_YUVJ420P,
    AV_PIX_FMT_YUVJ422P,
    AV_PIX_FMT_YUVJ444P,
    AV_PIX_FMT_XVMC_MPEG2_MC,
    AV_PIX_FMT_XVMC_MPEG2_IDCT,
#define AV_PIX_FMT_XVMC AV_PIX_FMT_XVMC_MPEG2_IDCT
    AV_PIX_FMT_UYVY422,
    AV_PIX_FMT_UYYVYY411,
    AV_PIX_FMT_BGR8,
    AV_PIX_FMT_BGR4,
    AV_PIX_FMT_BGR4_BYTE,
    AV_PIX_FMT_RGB8,
    AV_PIX_FMT_RGB4,
    AV_PIX_FMT_RGB4_BYTE,
    AV_PIX_FMT_NV12,
    AV_PIX_FMT_NV21,

    AV_PIX_FMT_ARGB,
    AV_PIX_FMT_RGBA,
    AV_PIX_FMT_ABGR,
    AV_PIX_FMT_BGRA,

    AV_PIX_FMT_GRAY16BE,
    AV_PIX_FMT_GRAY16LE,
    AV_PIX_FMT_YUV440P,
    AV_PIX_FMT_YUVJ440P,
    AV_PIX_FMT_YUVA420P,
    AV_PIX_FMT_VDPAU_H264,
    AV_PIX_FMT_VDPAU_MPEG1,
    AV_PIX_FMT_VDPAU_MPEG2,
    AV_PIX_FMT_VDPAU_WMV3,
    AV_PIX_FMT_VDPAU_VC1,
    AV_PIX_FMT_RGB48BE,
    AV_PIX_FMT_RGB48LE,

    AV_PIX_FMT_RGB565BE,
    AV_PIX_FMT_RGB565LE,
    AV_PIX_FMT_RGB555BE,
    AV_PIX_FMT_RGB555LE,

    AV_PIX_FMT_BGR565BE,
    AV_PIX_FMT_BGR565LE,
    AV_PIX_FMT_BGR555BE,
    AV_PIX_FMT_BGR555LE,

    AV_PIX_FMT_VAAPI_MOCO,
    AV_PIX_FMT_VAAPI_IDCT,
    AV_PIX_FMT_VAAPI_VLD,

    AV_PIX_FMT_YUV420P16LE,
    AV_PIX_FMT_YUV420P16BE,
    AV_PIX_FMT_YUV422P16LE,
    AV_PIX_FMT_YUV422P16BE,
    AV_PIX_FMT_YUV444P16LE,
    AV_PIX_FMT_YUV444P16BE,
    AV_PIX_FMT_VDPAU_MPEG4,
    AV_PIX_FMT_DXVA2_VLD,

    AV_PIX_FMT_RGB444LE,
    AV_PIX_FMT_RGB444BE,
    AV_PIX_FMT_BGR444LE,
    AV_PIX_FMT_BGR444BE,
    AV_PIX_FMT_GRAY8A,
    AV_PIX_FMT_BGR48BE,
    AV_PIX_FMT_BGR48LE,

    AV_PIX_FMT_YUV420P9BE,
    AV_PIX_FMT_YUV420P9LE,
    AV_PIX_FMT_YUV420P10BE,
    AV_PIX_FMT_YUV420P10LE,
    AV_PIX_FMT_YUV422P10BE,
    AV_PIX_FMT_YUV422P10LE,
    AV_PIX_FMT_YUV444P9BE,
    AV_PIX_FMT_YUV444P9LE,
    AV_PIX_FMT_YUV444P10BE,
    AV_PIX_FMT_YUV444P10LE,
    AV_PIX_FMT_YUV422P9BE,
    AV_PIX_FMT_YUV422P9LE,
    AV_PIX_FMT_VDA_VLD,

    AV_PIX_FMT_GBRP,
    AV_PIX_FMT_GBRP9BE,
    AV_PIX_FMT_GBRP9LE,
    AV_PIX_FMT_GBRP10BE,
    AV_PIX_FMT_GBRP10LE,
    AV_PIX_FMT_GBRP16BE,
    AV_PIX_FMT_GBRP16LE,

    AV_PIX_FMT_YUVA422P_LIBAV,
    AV_PIX_FMT_YUVA444P_LIBAV,

    AV_PIX_FMT_YUVA420P9BE,
    AV_PIX_FMT_YUVA420P9LE,
    AV_PIX_FMT_YUVA422P9BE,
    AV_PIX_FMT_YUVA422P9LE,
    AV_PIX_FMT_YUVA444P9BE,
    AV_PIX_FMT_YUVA444P9LE,
    AV_PIX_FMT_YUVA420P10BE,
    AV_PIX_FMT_YUVA420P10LE,
    AV_PIX_FMT_YUVA422P10BE,
    AV_PIX_FMT_YUVA422P10LE,
    AV_PIX_FMT_YUVA444P10BE,
    AV_PIX_FMT_YUVA444P10LE,
    AV_PIX_FMT_YUVA420P16BE,
    AV_PIX_FMT_YUVA420P16LE,
    AV_PIX_FMT_YUVA422P16BE,
    AV_PIX_FMT_YUVA422P16LE,
    AV_PIX_FMT_YUVA444P16BE,
    AV_PIX_FMT_YUVA444P16LE,

    AV_PIX_FMT_VDPAU,

    AV_PIX_FMT_XYZ12LE,
    AV_PIX_FMT_XYZ12BE,
    AV_PIX_FMT_NV16,
    AV_PIX_FMT_NV20LE,
    AV_PIX_FMT_NV20BE,

    AV_PIX_FMT_RGBA64BE_LIBAV,
    AV_PIX_FMT_RGBA64LE_LIBAV,
    AV_PIX_FMT_BGRA64BE_LIBAV,
    AV_PIX_FMT_BGRA64LE_LIBAV,

    AV_PIX_FMT_YVYU422,

    AV_PIX_FMT_VDA,

    AV_PIX_FMT_RGBA64BE=0x123,
    AV_PIX_FMT_RGBA64LE,
    AV_PIX_FMT_BGRA64BE,
    AV_PIX_FMT_BGRA64LE,
    AV_PIX_FMT_0RGB=0x123 + 4,
    AV_PIX_FMT_RGB0,
    AV_PIX_FMT_0BGR,
    AV_PIX_FMT_BGR0,
    AV_PIX_FMT_YUVA444P,
    AV_PIX_FMT_YUVA422P,

    AV_PIX_FMT_YUV420P12BE,
    AV_PIX_FMT_YUV420P12LE,
    AV_PIX_FMT_YUV420P14BE,
    AV_PIX_FMT_YUV420P14LE,
    AV_PIX_FMT_YUV422P12BE,
    AV_PIX_FMT_YUV422P12LE,
    AV_PIX_FMT_YUV422P14BE,
    AV_PIX_FMT_YUV422P14LE,
    AV_PIX_FMT_YUV444P12BE,
    AV_PIX_FMT_YUV444P12LE,
    AV_PIX_FMT_YUV444P14BE,
    AV_PIX_FMT_YUV444P14LE,
    AV_PIX_FMT_GBRP12BE,
    AV_PIX_FMT_GBRP12LE,
    AV_PIX_FMT_GBRP14BE,
    AV_PIX_FMT_GBRP14LE,
    AV_PIX_FMT_GBRAP,
    AV_PIX_FMT_GBRAP16BE,
    AV_PIX_FMT_GBRAP16LE,
    AV_PIX_FMT_YUVJ411P,

    AV_PIX_FMT_BAYER_BGGR8,
    AV_PIX_FMT_BAYER_RGGB8,
    AV_PIX_FMT_BAYER_GBRG8,
    AV_PIX_FMT_BAYER_GRBG8,
    AV_PIX_FMT_BAYER_BGGR16LE,
    AV_PIX_FMT_BAYER_BGGR16BE,
    AV_PIX_FMT_BAYER_RGGB16LE,
    AV_PIX_FMT_BAYER_RGGB16BE,
    AV_PIX_FMT_BAYER_GBRG16LE,
    AV_PIX_FMT_BAYER_GBRG16BE,
    AV_PIX_FMT_BAYER_GRBG16LE,
    AV_PIX_FMT_BAYER_GRBG16BE,

    AV_PIX_FMT_NB,

#define AVUTIL_OLD_PIX_FMTS_H

    PIX_FMT_NONE = AV_PIX_FMT_NONE,
    PIX_FMT_YUV420P,
    PIX_FMT_YUYV422,
    PIX_FMT_RGB24,
    PIX_FMT_BGR24,
    PIX_FMT_YUV422P,
    PIX_FMT_YUV444P,
    PIX_FMT_YUV410P,
    PIX_FMT_YUV411P,
    PIX_FMT_GRAY8,
    PIX_FMT_MONOWHITE,
    PIX_FMT_MONOBLACK,
    PIX_FMT_PAL8,
    PIX_FMT_YUVJ420P,
    PIX_FMT_YUVJ422P,
    PIX_FMT_YUVJ444P,
    PIX_FMT_XVMC_MPEG2_MC,
    PIX_FMT_XVMC_MPEG2_IDCT,
    PIX_FMT_UYVY422,
    PIX_FMT_UYYVYY411,
    PIX_FMT_BGR8,
    PIX_FMT_BGR4,
    PIX_FMT_BGR4_BYTE,
    PIX_FMT_RGB8,
    PIX_FMT_RGB4,
    PIX_FMT_RGB4_BYTE,
    PIX_FMT_NV12,
    PIX_FMT_NV21,

    PIX_FMT_ARGB,
    PIX_FMT_RGBA,
    PIX_FMT_ABGR,
    PIX_FMT_BGRA,

    PIX_FMT_GRAY16BE,
    PIX_FMT_GRAY16LE,
    PIX_FMT_YUV440P,
    PIX_FMT_YUVJ440P,
    PIX_FMT_YUVA420P,
    PIX_FMT_VDPAU_H264,
    PIX_FMT_VDPAU_MPEG1,
    PIX_FMT_VDPAU_MPEG2,
    PIX_FMT_VDPAU_WMV3,
    PIX_FMT_VDPAU_VC1,
    PIX_FMT_RGB48BE,
    PIX_FMT_RGB48LE,

    PIX_FMT_RGB565BE,
    PIX_FMT_RGB565LE,
    PIX_FMT_RGB555BE,
    PIX_FMT_RGB555LE,

    PIX_FMT_BGR565BE,
    PIX_FMT_BGR565LE,
    PIX_FMT_BGR555BE,
    PIX_FMT_BGR555LE,

    PIX_FMT_VAAPI_MOCO,
    PIX_FMT_VAAPI_IDCT,
    PIX_FMT_VAAPI_VLD,

    PIX_FMT_YUV420P16LE,
    PIX_FMT_YUV420P16BE,
    PIX_FMT_YUV422P16LE,
    PIX_FMT_YUV422P16BE,
    PIX_FMT_YUV444P16LE,
    PIX_FMT_YUV444P16BE,
    PIX_FMT_VDPAU_MPEG4,
    PIX_FMT_DXVA2_VLD,

    PIX_FMT_RGB444LE,
    PIX_FMT_RGB444BE,
    PIX_FMT_BGR444LE,
    PIX_FMT_BGR444BE,
    PIX_FMT_GRAY8A,
    PIX_FMT_BGR48BE,
    PIX_FMT_BGR48LE,

    PIX_FMT_YUV420P9BE,
    PIX_FMT_YUV420P9LE,
    PIX_FMT_YUV420P10BE,
    PIX_FMT_YUV420P10LE,
    PIX_FMT_YUV422P10BE,
    PIX_FMT_YUV422P10LE,
    PIX_FMT_YUV444P9BE,
    PIX_FMT_YUV444P9LE,
    PIX_FMT_YUV444P10BE,
    PIX_FMT_YUV444P10LE,
    PIX_FMT_YUV422P9BE,
    PIX_FMT_YUV422P9LE,
    PIX_FMT_VDA_VLD,

    PIX_FMT_GBRP,
    PIX_FMT_GBRP9BE,
    PIX_FMT_GBRP9LE,
    PIX_FMT_GBRP10BE,
    PIX_FMT_GBRP10LE,
    PIX_FMT_GBRP16BE,
    PIX_FMT_GBRP16LE,

    PIX_FMT_RGBA64BE=0x123,
    PIX_FMT_RGBA64LE,
    PIX_FMT_BGRA64BE,
    PIX_FMT_BGRA64LE,
    PIX_FMT_0RGB=0x123 + 4,
    PIX_FMT_RGB0,
    PIX_FMT_0BGR,
    PIX_FMT_BGR0,
    PIX_FMT_YUVA444P,
    PIX_FMT_YUVA422P,

    PIX_FMT_YUV420P12BE,
    PIX_FMT_YUV420P12LE,
    PIX_FMT_YUV420P14BE,
    PIX_FMT_YUV420P14LE,
    PIX_FMT_YUV422P12BE,
    PIX_FMT_YUV422P12LE,
    PIX_FMT_YUV422P14BE,
    PIX_FMT_YUV422P14LE,
    PIX_FMT_YUV444P12BE,
    PIX_FMT_YUV444P12LE,
    PIX_FMT_YUV444P14BE,
    PIX_FMT_YUV444P14LE,
    PIX_FMT_GBRP12BE,
    PIX_FMT_GBRP12LE,
    PIX_FMT_GBRP14BE,
    PIX_FMT_GBRP14LE,

    PIX_FMT_NB,
};

#define AV_PIX_FMT_Y400A AV_PIX_FMT_GRAY8A
#define AV_PIX_FMT_GBR24P AV_PIX_FMT_GBRP

#   define AV_PIX_FMT_NE(be, le) AV_PIX_FMT_##le

#define AV_PIX_FMT_RGB32   AV_PIX_FMT_NE(ARGB, BGRA)
#define AV_PIX_FMT_RGB32_1 AV_PIX_FMT_NE(RGBA, ABGR)
#define AV_PIX_FMT_BGR32   AV_PIX_FMT_NE(ABGR, RGBA)
#define AV_PIX_FMT_BGR32_1 AV_PIX_FMT_NE(BGRA, ARGB)
#define AV_PIX_FMT_0RGB32  AV_PIX_FMT_NE(0RGB, BGR0)
#define AV_PIX_FMT_0BGR32  AV_PIX_FMT_NE(0BGR, RGB0)

#define AV_PIX_FMT_GRAY16 AV_PIX_FMT_NE(GRAY16BE, GRAY16LE)
#define AV_PIX_FMT_RGB48  AV_PIX_FMT_NE(RGB48BE,  RGB48LE)
#define AV_PIX_FMT_RGB565 AV_PIX_FMT_NE(RGB565BE, RGB565LE)
#define AV_PIX_FMT_RGB555 AV_PIX_FMT_NE(RGB555BE, RGB555LE)
#define AV_PIX_FMT_RGB444 AV_PIX_FMT_NE(RGB444BE, RGB444LE)
#define AV_PIX_FMT_RGBA64 AV_PIX_FMT_NE(RGBA64BE, RGBA64LE)
#define AV_PIX_FMT_BGR48  AV_PIX_FMT_NE(BGR48BE,  BGR48LE)
#define AV_PIX_FMT_BGR565 AV_PIX_FMT_NE(BGR565BE, BGR565LE)
#define AV_PIX_FMT_BGR555 AV_PIX_FMT_NE(BGR555BE, BGR555LE)
#define AV_PIX_FMT_BGR444 AV_PIX_FMT_NE(BGR444BE, BGR444LE)
#define AV_PIX_FMT_BGRA64 AV_PIX_FMT_NE(BGRA64BE, BGRA64LE)

#define AV_PIX_FMT_YUV420P9  AV_PIX_FMT_NE(YUV420P9BE, YUV420P9LE)
#define AV_PIX_FMT_YUV422P9  AV_PIX_FMT_NE(YUV422P9BE, YUV422P9LE)
#define AV_PIX_FMT_YUV444P9  AV_PIX_FMT_NE(YUV444P9BE, YUV444P9LE)
#define AV_PIX_FMT_YUV420P10 AV_PIX_FMT_NE(YUV420P10BE, YUV420P10LE)
#define AV_PIX_FMT_YUV422P10 AV_PIX_FMT_NE(YUV422P10BE, YUV422P10LE)
#define AV_PIX_FMT_YUV444P10 AV_PIX_FMT_NE(YUV444P10BE, YUV444P10LE)
#define AV_PIX_FMT_YUV420P12 AV_PIX_FMT_NE(YUV420P12BE, YUV420P12LE)
#define AV_PIX_FMT_YUV422P12 AV_PIX_FMT_NE(YUV422P12BE, YUV422P12LE)
#define AV_PIX_FMT_YUV444P12 AV_PIX_FMT_NE(YUV444P12BE, YUV444P12LE)
#define AV_PIX_FMT_YUV420P14 AV_PIX_FMT_NE(YUV420P14BE, YUV420P14LE)
#define AV_PIX_FMT_YUV422P14 AV_PIX_FMT_NE(YUV422P14BE, YUV422P14LE)
#define AV_PIX_FMT_YUV444P14 AV_PIX_FMT_NE(YUV444P14BE, YUV444P14LE)
#define AV_PIX_FMT_YUV420P16 AV_PIX_FMT_NE(YUV420P16BE, YUV420P16LE)
#define AV_PIX_FMT_YUV422P16 AV_PIX_FMT_NE(YUV422P16BE, YUV422P16LE)
#define AV_PIX_FMT_YUV444P16 AV_PIX_FMT_NE(YUV444P16BE, YUV444P16LE)

#define AV_PIX_FMT_GBRP9     AV_PIX_FMT_NE(GBRP9BE,    GBRP9LE)
#define AV_PIX_FMT_GBRP10    AV_PIX_FMT_NE(GBRP10BE,    GBRP10LE)
#define AV_PIX_FMT_GBRP12    AV_PIX_FMT_NE(GBRP12BE,    GBRP12LE)
#define AV_PIX_FMT_GBRP14    AV_PIX_FMT_NE(GBRP14BE,    GBRP14LE)
#define AV_PIX_FMT_GBRP16    AV_PIX_FMT_NE(GBRP16BE,    GBRP16LE)
#define AV_PIX_FMT_GBRAP16   AV_PIX_FMT_NE(GBRAP16BE,   GBRAP16LE)

#define AV_PIX_FMT_BAYER_BGGR16 AV_PIX_FMT_NE(BAYER_BGGR16BE,    BAYER_BGGR16LE)
#define AV_PIX_FMT_BAYER_RGGB16 AV_PIX_FMT_NE(BAYER_RGGB16BE,    BAYER_RGGB16LE)
#define AV_PIX_FMT_BAYER_GBRG16 AV_PIX_FMT_NE(BAYER_GBRG16BE,    BAYER_GBRG16LE)
#define AV_PIX_FMT_BAYER_GRBG16 AV_PIX_FMT_NE(BAYER_GRBG16BE,    BAYER_GRBG16LE)

#define AV_PIX_FMT_YUVA420P9  AV_PIX_FMT_NE(YUVA420P9BE, YUVA420P9LE)
#define AV_PIX_FMT_YUVA422P9  AV_PIX_FMT_NE(YUVA422P9BE, YUVA422P9LE)
#define AV_PIX_FMT_YUVA444P9  AV_PIX_FMT_NE(YUVA444P9BE, YUVA444P9LE)
#define AV_PIX_FMT_YUVA420P10 AV_PIX_FMT_NE(YUVA420P10BE, YUVA420P10LE)
#define AV_PIX_FMT_YUVA422P10 AV_PIX_FMT_NE(YUVA422P10BE, YUVA422P10LE)
#define AV_PIX_FMT_YUVA444P10 AV_PIX_FMT_NE(YUVA444P10BE, YUVA444P10LE)
#define AV_PIX_FMT_YUVA420P16 AV_PIX_FMT_NE(YUVA420P16BE, YUVA420P16LE)
#define AV_PIX_FMT_YUVA422P16 AV_PIX_FMT_NE(YUVA422P16BE, YUVA422P16LE)
#define AV_PIX_FMT_YUVA444P16 AV_PIX_FMT_NE(YUVA444P16BE, YUVA444P16LE)

#define AV_PIX_FMT_XYZ12      AV_PIX_FMT_NE(XYZ12BE, XYZ12LE)
#define AV_PIX_FMT_NV20       AV_PIX_FMT_NE(NV20BE,  NV20LE)

#define PixelFormat AVPixelFormat

#define PIX_FMT_Y400A AV_PIX_FMT_Y400A
#define PIX_FMT_GBR24P AV_PIX_FMT_GBR24P

#define PIX_FMT_NE(be, le) AV_PIX_FMT_NE(be, le)

#define PIX_FMT_RGB32   AV_PIX_FMT_RGB32
#define PIX_FMT_RGB32_1 AV_PIX_FMT_RGB32_1
#define PIX_FMT_BGR32   AV_PIX_FMT_BGR32
#define PIX_FMT_BGR32_1 AV_PIX_FMT_BGR32_1
#define PIX_FMT_0RGB32  AV_PIX_FMT_0RGB32
#define PIX_FMT_0BGR32  AV_PIX_FMT_0BGR32

#define PIX_FMT_GRAY16 AV_PIX_FMT_GRAY16
#define PIX_FMT_RGB48  AV_PIX_FMT_RGB48
#define PIX_FMT_RGB565 AV_PIX_FMT_RGB565
#define PIX_FMT_RGB555 AV_PIX_FMT_RGB555
#define PIX_FMT_RGB444 AV_PIX_FMT_RGB444
#define PIX_FMT_BGR48  AV_PIX_FMT_BGR48
#define PIX_FMT_BGR565 AV_PIX_FMT_BGR565
#define PIX_FMT_BGR555 AV_PIX_FMT_BGR555
#define PIX_FMT_BGR444 AV_PIX_FMT_BGR444

#define PIX_FMT_YUV420P9  AV_PIX_FMT_YUV420P9
#define PIX_FMT_YUV422P9  AV_PIX_FMT_YUV422P9
#define PIX_FMT_YUV444P9  AV_PIX_FMT_YUV444P9
#define PIX_FMT_YUV420P10 AV_PIX_FMT_YUV420P10
#define PIX_FMT_YUV422P10 AV_PIX_FMT_YUV422P10
#define PIX_FMT_YUV444P10 AV_PIX_FMT_YUV444P10
#define PIX_FMT_YUV420P12 AV_PIX_FMT_YUV420P12
#define PIX_FMT_YUV422P12 AV_PIX_FMT_YUV422P12
#define PIX_FMT_YUV444P12 AV_PIX_FMT_YUV444P12
#define PIX_FMT_YUV420P14 AV_PIX_FMT_YUV420P14
#define PIX_FMT_YUV422P14 AV_PIX_FMT_YUV422P14
#define PIX_FMT_YUV444P14 AV_PIX_FMT_YUV444P14
#define PIX_FMT_YUV420P16 AV_PIX_FMT_YUV420P16
#define PIX_FMT_YUV422P16 AV_PIX_FMT_YUV422P16
#define PIX_FMT_YUV444P16 AV_PIX_FMT_YUV444P16

#define PIX_FMT_RGBA64 AV_PIX_FMT_RGBA64
#define PIX_FMT_BGRA64 AV_PIX_FMT_BGRA64
#define PIX_FMT_GBRP9  AV_PIX_FMT_GBRP9
#define PIX_FMT_GBRP10 AV_PIX_FMT_GBRP10
#define PIX_FMT_GBRP12 AV_PIX_FMT_GBRP12
#define PIX_FMT_GBRP14 AV_PIX_FMT_GBRP14
#define PIX_FMT_GBRP16 AV_PIX_FMT_GBRP16

enum AVColorPrimaries {
    AVCOL_PRI_BT709       = 1,
    AVCOL_PRI_UNSPECIFIED = 2,
    AVCOL_PRI_RESERVED    = 3,
    AVCOL_PRI_BT470M      = 4,
    AVCOL_PRI_BT470BG     = 5,
    AVCOL_PRI_SMPTE170M   = 6,
    AVCOL_PRI_SMPTE240M   = 7,
    AVCOL_PRI_FILM        = 8,
    AVCOL_PRI_BT2020      = 9,
    AVCOL_PRI_NB,
};

enum AVColorTransferCharacteristic {
    AVCOL_TRC_BT709        = 1,
    AVCOL_TRC_UNSPECIFIED  = 2,
    AVCOL_TRC_RESERVED     = 3,
    AVCOL_TRC_GAMMA22      = 4,
    AVCOL_TRC_GAMMA28      = 5,
    AVCOL_TRC_SMPTE170M    = 6,
    AVCOL_TRC_SMPTE240M    = 7,
    AVCOL_TRC_LINEAR       = 8,
    AVCOL_TRC_LOG          = 9,
    AVCOL_TRC_LOG_SQRT     = 10,
    AVCOL_TRC_IEC61966_2_4 = 11,
    AVCOL_TRC_BT1361_ECG   = 12,
    AVCOL_TRC_IEC61966_2_1 = 13,
    AVCOL_TRC_BT2020_10    = 14,
    AVCOL_TRC_BT2020_12    = 15,
    AVCOL_TRC_NB,
};

enum AVColorSpace {
    AVCOL_SPC_RGB         = 0,
    AVCOL_SPC_BT709       = 1,
    AVCOL_SPC_UNSPECIFIED = 2,
    AVCOL_SPC_RESERVED    = 3,
    AVCOL_SPC_FCC         = 4,
    AVCOL_SPC_BT470BG     = 5,
    AVCOL_SPC_SMPTE170M   = 6,
    AVCOL_SPC_SMPTE240M   = 7,
    AVCOL_SPC_YCOCG       = 8,
    AVCOL_SPC_BT2020_NCL  = 9,
    AVCOL_SPC_BT2020_CL   = 10,
    AVCOL_SPC_NB,
};
#define AVCOL_SPC_YCGCO AVCOL_SPC_YCOCG

enum AVColorRange {
    AVCOL_RANGE_UNSPECIFIED = 0,
    AVCOL_RANGE_MPEG        = 1,
    AVCOL_RANGE_JPEG        = 2,
    AVCOL_RANGE_NB,
};

enum AVChromaLocation {
    AVCHROMA_LOC_UNSPECIFIED = 0,
    AVCHROMA_LOC_LEFT        = 1,
    AVCHROMA_LOC_CENTER      = 2,
    AVCHROMA_LOC_TOPLEFT     = 3,
    AVCHROMA_LOC_TOP         = 4,
    AVCHROMA_LOC_BOTTOMLEFT  = 5,
    AVCHROMA_LOC_BOTTOM      = 6,
    AVCHROMA_LOC_NB,
};

static inline void* av_x_if_null(const void* p, const void* x)
{
    return (void*)(intptr_t)(p ? p : x);
}

unsigned av_int_list_length_for_size(unsigned elsize, const void* list, uint64_t term) av_pure;

#define av_int_list_length(list, term) \
    av_int_list_length_for_size(sizeof(*(list)), list, term)

FILE* av_fopen_utf8(const char* path, const char* mode);

AVRational av_get_time_base_q(void);

enum AVSampleFormat {
    AV_SAMPLE_FMT_NONE = -1,
    AV_SAMPLE_FMT_U8,
    AV_SAMPLE_FMT_S16,
    AV_SAMPLE_FMT_S32,
    AV_SAMPLE_FMT_FLT,
    AV_SAMPLE_FMT_DBL,

    AV_SAMPLE_FMT_U8P,
    AV_SAMPLE_FMT_S16P,
    AV_SAMPLE_FMT_S32P,
    AV_SAMPLE_FMT_FLTP,
    AV_SAMPLE_FMT_DBLP,

    AV_SAMPLE_FMT_NB
};

const char* av_get_sample_fmt_name(enum AVSampleFormat sample_fmt);

enum AVSampleFormat av_get_sample_fmt(const char* name);

enum AVSampleFormat av_get_alt_sample_fmt(enum AVSampleFormat sample_fmt, int planar);

enum AVSampleFormat av_get_packed_sample_fmt(enum AVSampleFormat sample_fmt);

enum AVSampleFormat av_get_planar_sample_fmt(enum AVSampleFormat sample_fmt);

char* av_get_sample_fmt_string(char* buf, int buf_size, enum AVSampleFormat sample_fmt);

attribute_deprecated
int av_get_bits_per_sample_fmt(enum AVSampleFormat sample_fmt);

int av_get_bytes_per_sample(enum AVSampleFormat sample_fmt);

int av_sample_fmt_is_planar(enum AVSampleFormat sample_fmt);

int av_samples_get_buffer_size(int* linesize, int nb_channels, int nb_samples, enum AVSampleFormat sample_fmt, int align);

int av_samples_fill_arrays(uint8_t** audio_data, int* linesize, const uint8_t* buf, int nb_channels, int nb_samples,
                           enum AVSampleFormat sample_fmt, int align);

int av_samples_alloc(uint8_t** audio_data, int* linesize, int nb_channels, int nb_samples, enum AVSampleFormat sample_fmt, int align);

int av_samples_alloc_array_and_samples(uint8_t*** audio_data, int* linesize, int nb_channels, int nb_samples,
                                       enum AVSampleFormat sample_fmt, int align);

int av_samples_copy(uint8_t** dst, uint8_t* const* src, int dst_offset, int src_offset, int nb_samples, int nb_channels,
                    enum AVSampleFormat sample_fmt);

int av_samples_set_silence(uint8_t** audio_data, int offset, int nb_samples, int nb_channels, enum AVSampleFormat sample_fmt);

#define AVUTIL_BUFFER_H

typedef struct AVBuffer AVBuffer;

typedef struct AVBufferRef {
    AVBuffer* buffer;

    uint8_t* data;

    int size;
} AVBufferRef;

AVBufferRef* av_buffer_alloc(int size);

AVBufferRef* av_buffer_allocz(int size);

#define AV_BUFFER_FLAG_READONLY (1 << 0)

AVBufferRef* av_buffer_create(uint8_t* data, int size, void (*free)(void* opaque, uint8_t* data), void* opaque, int flags);

void av_buffer_default_free(void* opaque, uint8_t* data);

AVBufferRef* av_buffer_ref(AVBufferRef* buf);

void av_buffer_unref(AVBufferRef** buf);

int av_buffer_is_writable(const AVBufferRef* buf);

void* av_buffer_get_opaque(const AVBufferRef* buf);

int av_buffer_get_ref_count(const AVBufferRef* buf);

int av_buffer_make_writable(AVBufferRef** buf);

int av_buffer_realloc(AVBufferRef** buf, int size);

typedef struct AVBufferPool AVBufferPool;

AVBufferPool* av_buffer_pool_init(int size, AVBufferRef* (*alloc)(int size));

void av_buffer_pool_uninit(AVBufferPool** pool);

AVBufferRef* av_buffer_pool_get(AVBufferPool* pool);

#define AVUTIL_CPU_H

#define AV_CPU_FLAG_FORCE    0x80000000

#define AV_CPU_FLAG_MMX          0x0001
#define AV_CPU_FLAG_MMXEXT       0x0002
#define AV_CPU_FLAG_MMX2         0x0002
#define AV_CPU_FLAG_3DNOW        0x0004
#define AV_CPU_FLAG_SSE          0x0008
#define AV_CPU_FLAG_SSE2         0x0010
#define AV_CPU_FLAG_SSE2SLOW 0x40000000

#define AV_CPU_FLAG_3DNOWEXT     0x0020
#define AV_CPU_FLAG_SSE3         0x0040
#define AV_CPU_FLAG_SSE3SLOW 0x20000000

#define AV_CPU_FLAG_SSSE3        0x0080
#define AV_CPU_FLAG_ATOM     0x10000000
#define AV_CPU_FLAG_SSE4         0x0100
#define AV_CPU_FLAG_SSE42        0x0200
#define AV_CPU_FLAG_AVX          0x4000
#define AV_CPU_FLAG_XOP          0x0400
#define AV_CPU_FLAG_FMA4         0x0800

#define AV_CPU_FLAG_CMOV      0x1001000

#define AV_CPU_FLAG_AVX2         0x8000
#define AV_CPU_FLAG_FMA3        0x10000
#define AV_CPU_FLAG_BMI1        0x20000
#define AV_CPU_FLAG_BMI2        0x40000

#define AV_CPU_FLAG_ALTIVEC      0x0001

#define AV_CPU_FLAG_ARMV5TE      (1 << 0)
#define AV_CPU_FLAG_ARMV6        (1 << 1)
#define AV_CPU_FLAG_ARMV6T2      (1 << 2)
#define AV_CPU_FLAG_VFP          (1 << 3)
#define AV_CPU_FLAG_VFPV3        (1 << 4)
#define AV_CPU_FLAG_NEON         (1 << 5)
#define AV_CPU_FLAG_ARMV8        (1 << 6)

int av_get_cpu_flags(void);

void av_force_cpu_flags(int flags);

attribute_deprecated void av_set_cpu_flags_mask(int mask);

attribute_deprecated
int av_parse_cpu_flags(const char* s);

int av_parse_cpu_caps(unsigned* flags, const char* s);

int av_cpu_count(void);

#define AVUTIL_CHANNEL_LAYOUT_H

#define AV_CH_FRONT_LEFT             0x00000001
#define AV_CH_FRONT_RIGHT            0x00000002
#define AV_CH_FRONT_CENTER           0x00000004
#define AV_CH_LOW_FREQUENCY          0x00000008
#define AV_CH_BACK_LEFT              0x00000010
#define AV_CH_BACK_RIGHT             0x00000020
#define AV_CH_FRONT_LEFT_OF_CENTER   0x00000040
#define AV_CH_FRONT_RIGHT_OF_CENTER  0x00000080
#define AV_CH_BACK_CENTER            0x00000100
#define AV_CH_SIDE_LEFT              0x00000200
#define AV_CH_SIDE_RIGHT             0x00000400
#define AV_CH_TOP_CENTER             0x00000800
#define AV_CH_TOP_FRONT_LEFT         0x00001000
#define AV_CH_TOP_FRONT_CENTER       0x00002000
#define AV_CH_TOP_FRONT_RIGHT        0x00004000
#define AV_CH_TOP_BACK_LEFT          0x00008000
#define AV_CH_TOP_BACK_CENTER        0x00010000
#define AV_CH_TOP_BACK_RIGHT         0x00020000
#define AV_CH_STEREO_LEFT            0x20000000
#define AV_CH_STEREO_RIGHT           0x40000000
#define AV_CH_WIDE_LEFT              0x0000000080000000ULL
#define AV_CH_WIDE_RIGHT             0x0000000100000000ULL
#define AV_CH_SURROUND_DIRECT_LEFT   0x0000000200000000ULL
#define AV_CH_SURROUND_DIRECT_RIGHT  0x0000000400000000ULL
#define AV_CH_LOW_FREQUENCY_2        0x0000000800000000ULL

#define AV_CH_LAYOUT_NATIVE          0x8000000000000000ULL

#define AV_CH_LAYOUT_MONO              (AV_CH_FRONT_CENTER)
#define AV_CH_LAYOUT_STEREO            (AV_CH_FRONT_LEFT | AV_CH_FRONT_RIGHT)
#define AV_CH_LAYOUT_2POINT1           (AV_CH_LAYOUT_STEREO | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_2_1               (AV_CH_LAYOUT_STEREO | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_SURROUND          (AV_CH_LAYOUT_STEREO | AV_CH_FRONT_CENTER)
#define AV_CH_LAYOUT_3POINT1           (AV_CH_LAYOUT_SURROUND | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_4POINT0           (AV_CH_LAYOUT_SURROUND | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_4POINT1           (AV_CH_LAYOUT_4POINT0 | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_2_2               (AV_CH_LAYOUT_STEREO | AV_CH_SIDE_LEFT | AV_CH_SIDE_RIGHT)
#define AV_CH_LAYOUT_QUAD              (AV_CH_LAYOUT_STEREO | AV_CH_BACK_LEFT | AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_5POINT0           (AV_CH_LAYOUT_SURROUND | AV_CH_SIDE_LEFT | AV_CH_SIDE_RIGHT)
#define AV_CH_LAYOUT_5POINT1           (AV_CH_LAYOUT_5POINT0 | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_5POINT0_BACK      (AV_CH_LAYOUT_SURROUND | AV_CH_BACK_LEFT | AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_5POINT1_BACK      (AV_CH_LAYOUT_5POINT0_BACK | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_6POINT0           (AV_CH_LAYOUT_5POINT0 | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT0_FRONT     (AV_CH_LAYOUT_2_2 | AV_CH_FRONT_LEFT_OF_CENTER | AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_HEXAGONAL         (AV_CH_LAYOUT_5POINT0_BACK | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1           (AV_CH_LAYOUT_5POINT1 | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1_BACK      (AV_CH_LAYOUT_5POINT1_BACK | AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1_FRONT     (AV_CH_LAYOUT_6POINT0_FRONT | AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_7POINT0           (AV_CH_LAYOUT_5POINT0 | AV_CH_BACK_LEFT | AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_7POINT0_FRONT     (AV_CH_LAYOUT_5POINT0 | AV_CH_FRONT_LEFT_OF_CENTER | AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_7POINT1           (AV_CH_LAYOUT_5POINT1 | AV_CH_BACK_LEFT | AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_7POINT1_WIDE      (AV_CH_LAYOUT_5POINT1 | AV_CH_FRONT_LEFT_OF_CENTER | AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_7POINT1_WIDE_BACK (AV_CH_LAYOUT_5POINT1_BACK | AV_CH_FRONT_LEFT_OF_CENTER | AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_OCTAGONAL         (AV_CH_LAYOUT_5POINT0 | AV_CH_BACK_LEFT | AV_CH_BACK_CENTER | AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_STEREO_DOWNMIX    (AV_CH_STEREO_LEFT | AV_CH_STEREO_RIGHT)

enum AVMatrixEncoding {
    AV_MATRIX_ENCODING_NONE,
    AV_MATRIX_ENCODING_DOLBY,
    AV_MATRIX_ENCODING_DPLII,
    AV_MATRIX_ENCODING_DPLIIX,
    AV_MATRIX_ENCODING_DPLIIZ,
    AV_MATRIX_ENCODING_DOLBYEX,
    AV_MATRIX_ENCODING_DOLBYHEADPHONE,
    AV_MATRIX_ENCODING_NB
};

uint64_t av_get_channel_layout(const char* name);

void av_get_channel_layout_string(char* buf, int buf_size, int nb_channels, uint64_t channel_layout);

struct AVBPrint;

void av_bprint_channel_layout(struct AVBPrint* bp, int nb_channels, uint64_t channel_layout);

int av_get_channel_layout_nb_channels(uint64_t channel_layout);

int64_t av_get_default_channel_layout(int nb_channels);

int av_get_channel_layout_channel_index(uint64_t channel_layout, uint64_t channel);

uint64_t av_channel_layout_extract_channel(uint64_t channel_layout, int index);

const char* av_get_channel_name(uint64_t channel);

const char* av_get_channel_description(uint64_t channel);

int av_get_standard_channel_layout(unsigned index, uint64_t* layout, const char** name);

#define AVUTIL_DICT_H

#define AV_DICT_MATCH_CASE      1
#define AV_DICT_IGNORE_SUFFIX   2

#define AV_DICT_DONT_STRDUP_KEY 4

#define AV_DICT_DONT_STRDUP_VAL 8

#define AV_DICT_DONT_OVERWRITE 16
#define AV_DICT_APPEND         32

typedef struct AVDictionaryEntry {
    char* key;
    char* value;
} AVDictionaryEntry;

typedef struct AVDictionary AVDictionary;

AVDictionaryEntry* av_dict_get(FF_CONST_AVUTIL53 AVDictionary* m, const char* key, const AVDictionaryEntry* prev, int flags);

int av_dict_count(const AVDictionary* m);

int av_dict_set(AVDictionary** pm, const char* key, const char* value, int flags);

int av_dict_parse_string(AVDictionary** pm, const char* str, const char* key_val_sep, const char* pairs_sep, int flags);

void av_dict_copy(AVDictionary** dst, FF_CONST_AVUTIL53 AVDictionary* src, int flags);

void av_dict_free(AVDictionary** m);

#define AVUTIL_FRAME_H

enum AVFrameSideDataType {
    AV_FRAME_DATA_PANSCAN,

    AV_FRAME_DATA_A53_CC,

    AV_FRAME_DATA_STEREO3D,

    AV_FRAME_DATA_MATRIXENCODING,

    AV_FRAME_DATA_DOWNMIX_INFO,

    AV_FRAME_DATA_REPLAYGAIN,

    AV_FRAME_DATA_DISPLAYMATRIX,
};

typedef struct AVFrameSideData {
    enum AVFrameSideDataType type;
    uint8_t* data;
    int size;
    AVDictionary* metadata;
} AVFrameSideData;

typedef struct AVFrame {
#define AV_NUM_DATA_POINTERS 8

    uint8_t* data[AV_NUM_DATA_POINTERS];

    int linesize[AV_NUM_DATA_POINTERS];

    uint8_t** extended_data;

    int width, height;

    int nb_samples;

    int format;

    int key_frame;

    enum AVPictureType pict_type;

    attribute_deprecated
    uint8_t* base[AV_NUM_DATA_POINTERS];

    AVRational sample_aspect_ratio;

    int64_t pts;

    int64_t pkt_pts;

    int64_t pkt_dts;

    int coded_picture_number;

    int display_picture_number;

    int quality;

    attribute_deprecated
    int reference;

    attribute_deprecated
    int8_t* qscale_table;

    attribute_deprecated
    int qstride;

    attribute_deprecated
    int qscale_type;

    attribute_deprecated
    uint8_t* mbskip_table;

    int16_t(*motion_val[2])[2];

    attribute_deprecated
    uint32_t* mb_type;

    attribute_deprecated
    short* dct_coeff;

    attribute_deprecated
    int8_t* ref_index[2];

    void* opaque;

    uint64_t error[AV_NUM_DATA_POINTERS];

    attribute_deprecated
    int type;

    int repeat_pict;

    int interlaced_frame;

    int top_field_first;

    int palette_has_changed;

    attribute_deprecated
    int buffer_hints;

    attribute_deprecated
    struct AVPanScan* pan_scan;

    int64_t reordered_opaque;

    attribute_deprecated void* hwaccel_picture_private;

    attribute_deprecated
    struct AVCodecContext* owner;
    attribute_deprecated
    void* thread_opaque;

    uint8_t motion_subsample_log2;

    int sample_rate;

    uint64_t channel_layout;

    AVBufferRef* buf[AV_NUM_DATA_POINTERS];

    AVBufferRef** extended_buf;

    int nb_extended_buf;

    AVFrameSideData** side_data;
    int nb_side_data;

#define AV_FRAME_FLAG_CORRUPT       (1 << 0)

    int flags;

    enum AVColorRange color_range;

    enum AVColorPrimaries color_primaries;

    enum AVColorTransferCharacteristic color_trc;

    enum AVColorSpace colorspace;

    enum AVChromaLocation chroma_location;

    int64_t best_effort_timestamp;

    int64_t pkt_pos;

    int64_t pkt_duration;

    AVDictionary* metadata;

    int decode_error_flags;
#define FF_DECODE_ERROR_INVALID_BITSTREAM   1
#define FF_DECODE_ERROR_MISSING_REFERENCE   2

    int channels;

    int pkt_size;

    AVBufferRef* qp_table_buf;
} AVFrame;

int64_t av_frame_get_best_effort_timestamp(const AVFrame* frame);
void    av_frame_set_best_effort_timestamp(AVFrame* frame, int64_t val);
int64_t av_frame_get_pkt_duration(const AVFrame* frame);
void    av_frame_set_pkt_duration(AVFrame* frame, int64_t val);
int64_t av_frame_get_pkt_pos(const AVFrame* frame);
void    av_frame_set_pkt_pos(AVFrame* frame, int64_t val);
int64_t av_frame_get_channel_layout(const AVFrame* frame);
void    av_frame_set_channel_layout(AVFrame* frame, int64_t val);
int     av_frame_get_channels(const AVFrame* frame);
void    av_frame_set_channels(AVFrame* frame, int val);
int     av_frame_get_sample_rate(const AVFrame* frame);
void    av_frame_set_sample_rate(AVFrame* frame, int val);
AVDictionary* av_frame_get_metadata(const AVFrame* frame);
void          av_frame_set_metadata(AVFrame* frame, AVDictionary* val);
int     av_frame_get_decode_error_flags(const AVFrame* frame);
void    av_frame_set_decode_error_flags(AVFrame* frame, int val);
int     av_frame_get_pkt_size(const AVFrame* frame);
void    av_frame_set_pkt_size(AVFrame* frame, int val);
AVDictionary** avpriv_frame_get_metadatap(AVFrame* frame);
int8_t* av_frame_get_qp_table(AVFrame* f, int* stride, int* type);
int av_frame_set_qp_table(AVFrame* f, AVBufferRef* buf, int stride, int type);
enum AVColorSpace av_frame_get_colorspace(const AVFrame* frame);
void    av_frame_set_colorspace(AVFrame* frame, enum AVColorSpace val);
enum AVColorRange av_frame_get_color_range(const AVFrame* frame);
void    av_frame_set_color_range(AVFrame* frame, enum AVColorRange val);

const char* av_get_colorspace_name(enum AVColorSpace val);

AVFrame* av_frame_alloc(void);

void av_frame_free(AVFrame** frame);

int av_frame_ref(AVFrame* dst, const AVFrame* src);

AVFrame* av_frame_clone(const AVFrame* src);

void av_frame_unref(AVFrame* frame);

void av_frame_move_ref(AVFrame* dst, AVFrame* src);

int av_frame_get_buffer(AVFrame* frame, int align);

int av_frame_is_writable(AVFrame* frame);

int av_frame_make_writable(AVFrame* frame);

int av_frame_copy(AVFrame* dst, const AVFrame* src);

int av_frame_copy_props(AVFrame* dst, const AVFrame* src);

AVBufferRef* av_frame_get_plane_buffer(AVFrame* frame, int plane);

AVFrameSideData* av_frame_new_side_data(AVFrame* frame, enum AVFrameSideDataType type, int size);

AVFrameSideData* av_frame_get_side_data(const AVFrame* frame, enum AVFrameSideDataType type);

void av_frame_remove_side_data(AVFrame* frame, enum AVFrameSideDataType type);

#define AVCODEC_VERSION_H

#define LIBAVCODEC_VERSION_MAJOR 55
#define LIBAVCODEC_VERSION_MINOR  69
#define LIBAVCODEC_VERSION_MICRO 100

#define LIBAVCODEC_VERSION_INT  AV_VERSION_INT(LIBAVCODEC_VERSION_MAJOR, \
                                               LIBAVCODEC_VERSION_MINOR, \
                                               LIBAVCODEC_VERSION_MICRO)
#define LIBAVCODEC_VERSION      AV_VERSION(LIBAVCODEC_VERSION_MAJOR,    \
                                           LIBAVCODEC_VERSION_MINOR,    \
                                           LIBAVCODEC_VERSION_MICRO)
#define LIBAVCODEC_BUILD        LIBAVCODEC_VERSION_INT

#define LIBAVCODEC_IDENT        "Lavc" AV_STRINGIFY(LIBAVCODEC_VERSION)

#define FF_API_REQUEST_CHANNELS (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_OLD_DECODE_AUDIO (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_OLD_ENCODE_AUDIO (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_OLD_ENCODE_VIDEO (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_CODEC_ID          (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_AUDIO_CONVERT     (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_AVCODEC_RESAMPLE  FF_API_AUDIO_CONVERT
#define FF_API_DEINTERLACE       (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_DESTRUCT_PACKET   (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_GET_BUFFER        (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_MISSING_SAMPLE    (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_LOWRES            (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_CAP_VDPAU         (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_BUFS_VDPAU        (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_VOXWARE           (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_SET_DIMENSIONS    (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_DEBUG_MV          (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_AC_VLC            (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_OLD_MSMPEG4       (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_ASPECT_EXTENDED   (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_THREAD_OPAQUE     (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_CODEC_PKT         (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_ARCH_ALPHA        (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_ERROR_RATE        (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_QSCALE_TYPE       (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_MB_TYPE           (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_MAX_BFRAMES       (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_FAST_MALLOC       (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_NEG_LINESIZES     (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_EMU_EDGE          (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_ARCH_SH4          (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_ARCH_SPARC        (LIBAVCODEC_VERSION_MAJOR < 56)
#define FF_API_INPUT_PRESERVED   (LIBAVCODEC_VERSION_MAJOR < 57)
#define FF_API_NORMALIZE_AQP     (LIBAVCODEC_VERSION_MAJOR < 57)
#define FF_API_GMC               (LIBAVCODEC_VERSION_MAJOR < 57)
#define FF_API_MV0               (LIBAVCODEC_VERSION_MAJOR < 57)
#define FF_API_CODEC_NAME        (LIBAVCODEC_VERSION_MAJOR < 57)

enum AVCodecID {
    AV_CODEC_ID_NONE,

    AV_CODEC_ID_MPEG1VIDEO,
    AV_CODEC_ID_MPEG2VIDEO,
    AV_CODEC_ID_MPEG2VIDEO_XVMC,
    AV_CODEC_ID_H261,
    AV_CODEC_ID_H263,
    AV_CODEC_ID_RV10,
    AV_CODEC_ID_RV20,
    AV_CODEC_ID_MJPEG,
    AV_CODEC_ID_MJPEGB,
    AV_CODEC_ID_LJPEG,
    AV_CODEC_ID_SP5X,
    AV_CODEC_ID_JPEGLS,
    AV_CODEC_ID_MPEG4,
    AV_CODEC_ID_RAWVIDEO,
    AV_CODEC_ID_MSMPEG4V1,
    AV_CODEC_ID_MSMPEG4V2,
    AV_CODEC_ID_MSMPEG4V3,
    AV_CODEC_ID_WMV1,
    AV_CODEC_ID_WMV2,
    AV_CODEC_ID_H263P,
    AV_CODEC_ID_H263I,
    AV_CODEC_ID_FLV1,
    AV_CODEC_ID_SVQ1,
    AV_CODEC_ID_SVQ3,
    AV_CODEC_ID_DVVIDEO,
    AV_CODEC_ID_HUFFYUV,
    AV_CODEC_ID_CYUV,
    AV_CODEC_ID_H264,
    AV_CODEC_ID_INDEO3,
    AV_CODEC_ID_VP3,
    AV_CODEC_ID_THEORA,
    AV_CODEC_ID_ASV1,
    AV_CODEC_ID_ASV2,
    AV_CODEC_ID_FFV1,
    AV_CODEC_ID_4XM,
    AV_CODEC_ID_VCR1,
    AV_CODEC_ID_CLJR,
    AV_CODEC_ID_MDEC,
    AV_CODEC_ID_ROQ,
    AV_CODEC_ID_INTERPLAY_VIDEO,
    AV_CODEC_ID_XAN_WC3,
    AV_CODEC_ID_XAN_WC4,
    AV_CODEC_ID_RPZA,
    AV_CODEC_ID_CINEPAK,
    AV_CODEC_ID_WS_VQA,
    AV_CODEC_ID_MSRLE,
    AV_CODEC_ID_MSVIDEO1,
    AV_CODEC_ID_IDCIN,
    AV_CODEC_ID_8BPS,
    AV_CODEC_ID_SMC,
    AV_CODEC_ID_FLIC,
    AV_CODEC_ID_TRUEMOTION1,
    AV_CODEC_ID_VMDVIDEO,
    AV_CODEC_ID_MSZH,
    AV_CODEC_ID_ZLIB,
    AV_CODEC_ID_QTRLE,
    AV_CODEC_ID_TSCC,
    AV_CODEC_ID_ULTI,
    AV_CODEC_ID_QDRAW,
    AV_CODEC_ID_VIXL,
    AV_CODEC_ID_QPEG,
    AV_CODEC_ID_PNG,
    AV_CODEC_ID_PPM,
    AV_CODEC_ID_PBM,
    AV_CODEC_ID_PGM,
    AV_CODEC_ID_PGMYUV,
    AV_CODEC_ID_PAM,
    AV_CODEC_ID_FFVHUFF,
    AV_CODEC_ID_RV30,
    AV_CODEC_ID_RV40,
    AV_CODEC_ID_VC1,
    AV_CODEC_ID_WMV3,
    AV_CODEC_ID_LOCO,
    AV_CODEC_ID_WNV1,
    AV_CODEC_ID_AASC,
    AV_CODEC_ID_INDEO2,
    AV_CODEC_ID_FRAPS,
    AV_CODEC_ID_TRUEMOTION2,
    AV_CODEC_ID_BMP,
    AV_CODEC_ID_CSCD,
    AV_CODEC_ID_MMVIDEO,
    AV_CODEC_ID_ZMBV,
    AV_CODEC_ID_AVS,
    AV_CODEC_ID_SMACKVIDEO,
    AV_CODEC_ID_NUV,
    AV_CODEC_ID_KMVC,
    AV_CODEC_ID_FLASHSV,
    AV_CODEC_ID_CAVS,
    AV_CODEC_ID_JPEG2000,
    AV_CODEC_ID_VMNC,
    AV_CODEC_ID_VP5,
    AV_CODEC_ID_VP6,
    AV_CODEC_ID_VP6F,
    AV_CODEC_ID_TARGA,
    AV_CODEC_ID_DSICINVIDEO,
    AV_CODEC_ID_TIERTEXSEQVIDEO,
    AV_CODEC_ID_TIFF,
    AV_CODEC_ID_GIF,
    AV_CODEC_ID_DXA,
    AV_CODEC_ID_DNXHD,
    AV_CODEC_ID_THP,
    AV_CODEC_ID_SGI,
    AV_CODEC_ID_C93,
    AV_CODEC_ID_BETHSOFTVID,
    AV_CODEC_ID_PTX,
    AV_CODEC_ID_TXD,
    AV_CODEC_ID_VP6A,
    AV_CODEC_ID_AMV,
    AV_CODEC_ID_VB,
    AV_CODEC_ID_PCX,
    AV_CODEC_ID_SUNRAST,
    AV_CODEC_ID_INDEO4,
    AV_CODEC_ID_INDEO5,
    AV_CODEC_ID_MIMIC,
    AV_CODEC_ID_RL2,
    AV_CODEC_ID_ESCAPE124,
    AV_CODEC_ID_DIRAC,
    AV_CODEC_ID_BFI,
    AV_CODEC_ID_CMV,
    AV_CODEC_ID_MOTIONPIXELS,
    AV_CODEC_ID_TGV,
    AV_CODEC_ID_TGQ,
    AV_CODEC_ID_TQI,
    AV_CODEC_ID_AURA,
    AV_CODEC_ID_AURA2,
    AV_CODEC_ID_V210X,
    AV_CODEC_ID_TMV,
    AV_CODEC_ID_V210,
    AV_CODEC_ID_DPX,
    AV_CODEC_ID_MAD,
    AV_CODEC_ID_FRWU,
    AV_CODEC_ID_FLASHSV2,
    AV_CODEC_ID_CDGRAPHICS,
    AV_CODEC_ID_R210,
    AV_CODEC_ID_ANM,
    AV_CODEC_ID_BINKVIDEO,
    AV_CODEC_ID_IFF_ILBM,
    AV_CODEC_ID_IFF_BYTERUN1,
    AV_CODEC_ID_KGV1,
    AV_CODEC_ID_YOP,
    AV_CODEC_ID_VP8,
    AV_CODEC_ID_PICTOR,
    AV_CODEC_ID_ANSI,
    AV_CODEC_ID_A64_MULTI,
    AV_CODEC_ID_A64_MULTI5,
    AV_CODEC_ID_R10K,
    AV_CODEC_ID_MXPEG,
    AV_CODEC_ID_LAGARITH,
    AV_CODEC_ID_PRORES,
    AV_CODEC_ID_JV,
    AV_CODEC_ID_DFA,
    AV_CODEC_ID_WMV3IMAGE,
    AV_CODEC_ID_VC1IMAGE,
    AV_CODEC_ID_UTVIDEO,
    AV_CODEC_ID_BMV_VIDEO,
    AV_CODEC_ID_VBLE,
    AV_CODEC_ID_DXTORY,
    AV_CODEC_ID_V410,
    AV_CODEC_ID_XWD,
    AV_CODEC_ID_CDXL,
    AV_CODEC_ID_XBM,
    AV_CODEC_ID_ZEROCODEC,
    AV_CODEC_ID_MSS1,
    AV_CODEC_ID_MSA1,
    AV_CODEC_ID_TSCC2,
    AV_CODEC_ID_MTS2,
    AV_CODEC_ID_CLLC,
    AV_CODEC_ID_MSS2,
    AV_CODEC_ID_VP9,
    AV_CODEC_ID_AIC,
    AV_CODEC_ID_ESCAPE130_DEPRECATED,
    AV_CODEC_ID_G2M_DEPRECATED,
    AV_CODEC_ID_WEBP_DEPRECATED,
    AV_CODEC_ID_HNM4_VIDEO,
    AV_CODEC_ID_HEVC_DEPRECATED,
    AV_CODEC_ID_FIC,
    AV_CODEC_ID_ALIAS_PIX,
    AV_CODEC_ID_BRENDER_PIX_DEPRECATED,
    AV_CODEC_ID_PAF_VIDEO_DEPRECATED,
    AV_CODEC_ID_EXR_DEPRECATED,
    AV_CODEC_ID_VP7_DEPRECATED,
    AV_CODEC_ID_SANM_DEPRECATED,
    AV_CODEC_ID_SGIRLE_DEPRECATED,
    AV_CODEC_ID_MVC1_DEPRECATED,
    AV_CODEC_ID_MVC2_DEPRECATED,

    AV_CODEC_ID_BRENDER_PIX= MKBETAG('B', 'P', 'I', 'X'),
    AV_CODEC_ID_Y41P       = MKBETAG('Y', '4', '1', 'P'),
    AV_CODEC_ID_ESCAPE130  = MKBETAG('E', '1', '3', '0'),
    AV_CODEC_ID_EXR        = MKBETAG('0', 'E', 'X', 'R'),
    AV_CODEC_ID_AVRP       = MKBETAG('A', 'V', 'R', 'P'),

    AV_CODEC_ID_012V       = MKBETAG('0', '1', '2', 'V'),
    AV_CODEC_ID_G2M        = MKBETAG(0, 'G', '2', 'M'),
    AV_CODEC_ID_AVUI       = MKBETAG('A', 'V', 'U', 'I'),
    AV_CODEC_ID_AYUV       = MKBETAG('A', 'Y', 'U', 'V'),
    AV_CODEC_ID_TARGA_Y216 = MKBETAG('T', '2', '1', '6'),
    AV_CODEC_ID_V308       = MKBETAG('V', '3', '0', '8'),
    AV_CODEC_ID_V408       = MKBETAG('V', '4', '0', '8'),
    AV_CODEC_ID_YUV4       = MKBETAG('Y', 'U', 'V', '4'),
    AV_CODEC_ID_SANM       = MKBETAG('S', 'A', 'N', 'M'),
    AV_CODEC_ID_PAF_VIDEO  = MKBETAG('P', 'A', 'F', 'V'),
    AV_CODEC_ID_AVRN       = MKBETAG('A', 'V', 'R', 'n'),
    AV_CODEC_ID_CPIA       = MKBETAG('C', 'P', 'I', 'A'),
    AV_CODEC_ID_XFACE      = MKBETAG('X', 'F', 'A', 'C'),
    AV_CODEC_ID_SGIRLE     = MKBETAG('S', 'G', 'I', 'R'),
    AV_CODEC_ID_MVC1       = MKBETAG('M', 'V', 'C', '1'),
    AV_CODEC_ID_MVC2       = MKBETAG('M', 'V', 'C', '2'),
    AV_CODEC_ID_SNOW       = MKBETAG('S', 'N', 'O', 'W'),
    AV_CODEC_ID_WEBP       = MKBETAG('W', 'E', 'B', 'P'),
    AV_CODEC_ID_SMVJPEG    = MKBETAG('S', 'M', 'V', 'J'),
    AV_CODEC_ID_HEVC       = MKBETAG('H', '2', '6', '5'),
#define AV_CODEC_ID_H265 AV_CODEC_ID_HEVC
    AV_CODEC_ID_VP7        = MKBETAG('V', 'P', '7', '0'),

    AV_CODEC_ID_FIRST_AUDIO = 0x10000,
    AV_CODEC_ID_PCM_S16LE = 0x10000,
    AV_CODEC_ID_PCM_S16BE,
    AV_CODEC_ID_PCM_U16LE,
    AV_CODEC_ID_PCM_U16BE,
    AV_CODEC_ID_PCM_S8,
    AV_CODEC_ID_PCM_U8,
    AV_CODEC_ID_PCM_MULAW,
    AV_CODEC_ID_PCM_ALAW,
    AV_CODEC_ID_PCM_S32LE,
    AV_CODEC_ID_PCM_S32BE,
    AV_CODEC_ID_PCM_U32LE,
    AV_CODEC_ID_PCM_U32BE,
    AV_CODEC_ID_PCM_S24LE,
    AV_CODEC_ID_PCM_S24BE,
    AV_CODEC_ID_PCM_U24LE,
    AV_CODEC_ID_PCM_U24BE,
    AV_CODEC_ID_PCM_S24DAUD,
    AV_CODEC_ID_PCM_ZORK,
    AV_CODEC_ID_PCM_S16LE_PLANAR,
    AV_CODEC_ID_PCM_DVD,
    AV_CODEC_ID_PCM_F32BE,
    AV_CODEC_ID_PCM_F32LE,
    AV_CODEC_ID_PCM_F64BE,
    AV_CODEC_ID_PCM_F64LE,
    AV_CODEC_ID_PCM_BLURAY,
    AV_CODEC_ID_PCM_LXF,
    AV_CODEC_ID_S302M,
    AV_CODEC_ID_PCM_S8_PLANAR,
    AV_CODEC_ID_PCM_S24LE_PLANAR_DEPRECATED,
    AV_CODEC_ID_PCM_S32LE_PLANAR_DEPRECATED,
    AV_CODEC_ID_PCM_S24LE_PLANAR = MKBETAG(24, 'P', 'S', 'P'),
    AV_CODEC_ID_PCM_S32LE_PLANAR = MKBETAG(32, 'P', 'S', 'P'),
    AV_CODEC_ID_PCM_S16BE_PLANAR = MKBETAG('P', 'S', 'P', 16),

    AV_CODEC_ID_ADPCM_IMA_QT = 0x11000,
    AV_CODEC_ID_ADPCM_IMA_WAV,
    AV_CODEC_ID_ADPCM_IMA_DK3,
    AV_CODEC_ID_ADPCM_IMA_DK4,
    AV_CODEC_ID_ADPCM_IMA_WS,
    AV_CODEC_ID_ADPCM_IMA_SMJPEG,
    AV_CODEC_ID_ADPCM_MS,
    AV_CODEC_ID_ADPCM_4XM,
    AV_CODEC_ID_ADPCM_XA,
    AV_CODEC_ID_ADPCM_ADX,
    AV_CODEC_ID_ADPCM_EA,
    AV_CODEC_ID_ADPCM_G726,
    AV_CODEC_ID_ADPCM_CT,
    AV_CODEC_ID_ADPCM_SWF,
    AV_CODEC_ID_ADPCM_YAMAHA,
    AV_CODEC_ID_ADPCM_SBPRO_4,
    AV_CODEC_ID_ADPCM_SBPRO_3,
    AV_CODEC_ID_ADPCM_SBPRO_2,
    AV_CODEC_ID_ADPCM_THP,
    AV_CODEC_ID_ADPCM_IMA_AMV,
    AV_CODEC_ID_ADPCM_EA_R1,
    AV_CODEC_ID_ADPCM_EA_R3,
    AV_CODEC_ID_ADPCM_EA_R2,
    AV_CODEC_ID_ADPCM_IMA_EA_SEAD,
    AV_CODEC_ID_ADPCM_IMA_EA_EACS,
    AV_CODEC_ID_ADPCM_EA_XAS,
    AV_CODEC_ID_ADPCM_EA_MAXIS_XA,
    AV_CODEC_ID_ADPCM_IMA_ISS,
    AV_CODEC_ID_ADPCM_G722,
    AV_CODEC_ID_ADPCM_IMA_APC,
    AV_CODEC_ID_ADPCM_VIMA_DEPRECATED,
    AV_CODEC_ID_ADPCM_VIMA = MKBETAG('V', 'I', 'M', 'A'),
    AV_CODEC_ID_VIMA       = MKBETAG('V', 'I', 'M', 'A'),
    AV_CODEC_ID_ADPCM_AFC  = MKBETAG('A', 'F', 'C', ' '),
    AV_CODEC_ID_ADPCM_IMA_OKI = MKBETAG('O', 'K', 'I', ' '),
    AV_CODEC_ID_ADPCM_DTK  = MKBETAG('D', 'T', 'K', ' '),
    AV_CODEC_ID_ADPCM_IMA_RAD = MKBETAG('R', 'A', 'D', ' '),
    AV_CODEC_ID_ADPCM_G726LE = MKBETAG('6', '2', '7', 'G'),

    AV_CODEC_ID_AMR_NB = 0x12000,
    AV_CODEC_ID_AMR_WB,

    AV_CODEC_ID_RA_144 = 0x13000,
    AV_CODEC_ID_RA_288,

    AV_CODEC_ID_ROQ_DPCM = 0x14000,
    AV_CODEC_ID_INTERPLAY_DPCM,
    AV_CODEC_ID_XAN_DPCM,
    AV_CODEC_ID_SOL_DPCM,

    AV_CODEC_ID_MP2 = 0x15000,
    AV_CODEC_ID_MP3,
    AV_CODEC_ID_AAC,
    AV_CODEC_ID_AC3,
    AV_CODEC_ID_DTS,
    AV_CODEC_ID_VORBIS,
    AV_CODEC_ID_DVAUDIO,
    AV_CODEC_ID_WMAV1,
    AV_CODEC_ID_WMAV2,
    AV_CODEC_ID_MACE3,
    AV_CODEC_ID_MACE6,
    AV_CODEC_ID_VMDAUDIO,
    AV_CODEC_ID_FLAC,
    AV_CODEC_ID_MP3ADU,
    AV_CODEC_ID_MP3ON4,
    AV_CODEC_ID_SHORTEN,
    AV_CODEC_ID_ALAC,
    AV_CODEC_ID_WESTWOOD_SND1,
    AV_CODEC_ID_GSM,
    AV_CODEC_ID_QDM2,
    AV_CODEC_ID_COOK,
    AV_CODEC_ID_TRUESPEECH,
    AV_CODEC_ID_TTA,
    AV_CODEC_ID_SMACKAUDIO,
    AV_CODEC_ID_QCELP,
    AV_CODEC_ID_WAVPACK,
    AV_CODEC_ID_DSICINAUDIO,
    AV_CODEC_ID_IMC,
    AV_CODEC_ID_MUSEPACK7,
    AV_CODEC_ID_MLP,
    AV_CODEC_ID_GSM_MS,
    AV_CODEC_ID_ATRAC3,
    AV_CODEC_ID_VOXWARE,
    AV_CODEC_ID_APE,
    AV_CODEC_ID_NELLYMOSER,
    AV_CODEC_ID_MUSEPACK8,
    AV_CODEC_ID_SPEEX,
    AV_CODEC_ID_WMAVOICE,
    AV_CODEC_ID_WMAPRO,
    AV_CODEC_ID_WMALOSSLESS,
    AV_CODEC_ID_ATRAC3P,
    AV_CODEC_ID_EAC3,
    AV_CODEC_ID_SIPR,
    AV_CODEC_ID_MP1,
    AV_CODEC_ID_TWINVQ,
    AV_CODEC_ID_TRUEHD,
    AV_CODEC_ID_MP4ALS,
    AV_CODEC_ID_ATRAC1,
    AV_CODEC_ID_BINKAUDIO_RDFT,
    AV_CODEC_ID_BINKAUDIO_DCT,
    AV_CODEC_ID_AAC_LATM,
    AV_CODEC_ID_QDMC,
    AV_CODEC_ID_CELT,
    AV_CODEC_ID_G723_1,
    AV_CODEC_ID_G729,
    AV_CODEC_ID_8SVX_EXP,
    AV_CODEC_ID_8SVX_FIB,
    AV_CODEC_ID_BMV_AUDIO,
    AV_CODEC_ID_RALF,
    AV_CODEC_ID_IAC,
    AV_CODEC_ID_ILBC,
    AV_CODEC_ID_OPUS_DEPRECATED,
    AV_CODEC_ID_COMFORT_NOISE,
    AV_CODEC_ID_TAK_DEPRECATED,
    AV_CODEC_ID_METASOUND,
    AV_CODEC_ID_PAF_AUDIO_DEPRECATED,
    AV_CODEC_ID_ON2AVC,
    AV_CODEC_ID_FFWAVESYNTH = MKBETAG('F', 'F', 'W', 'S'),
    AV_CODEC_ID_SONIC       = MKBETAG('S', 'O', 'N', 'C'),
    AV_CODEC_ID_SONIC_LS    = MKBETAG('S', 'O', 'N', 'L'),
    AV_CODEC_ID_PAF_AUDIO   = MKBETAG('P', 'A', 'F', 'A'),
    AV_CODEC_ID_OPUS        = MKBETAG('O', 'P', 'U', 'S'),
    AV_CODEC_ID_TAK         = MKBETAG('t', 'B', 'a', 'K'),
    AV_CODEC_ID_EVRC        = MKBETAG('s', 'e', 'v', 'c'),
    AV_CODEC_ID_SMV         = MKBETAG('s', 's', 'm', 'v'),
    AV_CODEC_ID_DSD_LSBF    = MKBETAG('D', 'S', 'D', 'L'),
    AV_CODEC_ID_DSD_MSBF    = MKBETAG('D', 'S', 'D', 'M'),
    AV_CODEC_ID_DSD_LSBF_PLANAR = MKBETAG('D', 'S', 'D', '1'),
    AV_CODEC_ID_DSD_MSBF_PLANAR = MKBETAG('D', 'S', 'D', '8'),

    AV_CODEC_ID_FIRST_SUBTITLE = 0x17000,
    AV_CODEC_ID_DVD_SUBTITLE = 0x17000,
    AV_CODEC_ID_DVB_SUBTITLE,
    AV_CODEC_ID_TEXT,
    AV_CODEC_ID_XSUB,
    AV_CODEC_ID_SSA,
    AV_CODEC_ID_MOV_TEXT,
    AV_CODEC_ID_HDMV_PGS_SUBTITLE,
    AV_CODEC_ID_DVB_TELETEXT,
    AV_CODEC_ID_SRT,
    AV_CODEC_ID_MICRODVD   = MKBETAG('m', 'D', 'V', 'D'),
    AV_CODEC_ID_EIA_608    = MKBETAG('c', '6', '0', '8'),
    AV_CODEC_ID_JACOSUB    = MKBETAG('J', 'S', 'U', 'B'),
    AV_CODEC_ID_SAMI       = MKBETAG('S', 'A', 'M', 'I'),
    AV_CODEC_ID_REALTEXT   = MKBETAG('R', 'T', 'X', 'T'),
    AV_CODEC_ID_SUBVIEWER1 = MKBETAG('S', 'b', 'V', '1'),
    AV_CODEC_ID_SUBVIEWER  = MKBETAG('S', 'u', 'b', 'V'),
    AV_CODEC_ID_SUBRIP     = MKBETAG('S', 'R', 'i', 'p'),
    AV_CODEC_ID_WEBVTT     = MKBETAG('W', 'V', 'T', 'T'),
    AV_CODEC_ID_MPL2       = MKBETAG('M', 'P', 'L', '2'),
    AV_CODEC_ID_VPLAYER    = MKBETAG('V', 'P', 'l', 'r'),
    AV_CODEC_ID_PJS        = MKBETAG('P', 'h', 'J', 'S'),
    AV_CODEC_ID_ASS        = MKBETAG('A', 'S', 'S', ' '),

    AV_CODEC_ID_FIRST_UNKNOWN = 0x18000,
    AV_CODEC_ID_TTF = 0x18000,
    AV_CODEC_ID_BINTEXT    = MKBETAG('B', 'T', 'X', 'T'),
    AV_CODEC_ID_XBIN       = MKBETAG('X', 'B', 'I', 'N'),
    AV_CODEC_ID_IDF        = MKBETAG(0, 'I', 'D', 'F'),
    AV_CODEC_ID_OTF        = MKBETAG(0, 'O', 'T', 'F'),
    AV_CODEC_ID_SMPTE_KLV  = MKBETAG('K', 'L', 'V', 'A'),
    AV_CODEC_ID_DVD_NAV    = MKBETAG('D', 'N', 'A', 'V'),
    AV_CODEC_ID_TIMED_ID3  = MKBETAG('T', 'I', 'D', '3'),
    AV_CODEC_ID_BIN_DATA   = MKBETAG('D', 'A', 'T', 'A'),

    AV_CODEC_ID_PROBE = 0x19000,

    AV_CODEC_ID_MPEG2TS = 0x20000,

    AV_CODEC_ID_MPEG4SYSTEMS = 0x20001,

    AV_CODEC_ID_FFMETADATA = 0x21000,

#define AVCODEC_OLD_CODEC_IDS_H

    CODEC_ID_NONE = AV_CODEC_ID_NONE,

    CODEC_ID_MPEG1VIDEO,
    CODEC_ID_MPEG2VIDEO,
    CODEC_ID_MPEG2VIDEO_XVMC,
    CODEC_ID_H261,
    CODEC_ID_H263,
    CODEC_ID_RV10,
    CODEC_ID_RV20,
    CODEC_ID_MJPEG,
    CODEC_ID_MJPEGB,
    CODEC_ID_LJPEG,
    CODEC_ID_SP5X,
    CODEC_ID_JPEGLS,
    CODEC_ID_MPEG4,
    CODEC_ID_RAWVIDEO,
    CODEC_ID_MSMPEG4V1,
    CODEC_ID_MSMPEG4V2,
    CODEC_ID_MSMPEG4V3,
    CODEC_ID_WMV1,
    CODEC_ID_WMV2,
    CODEC_ID_H263P,
    CODEC_ID_H263I,
    CODEC_ID_FLV1,
    CODEC_ID_SVQ1,
    CODEC_ID_SVQ3,
    CODEC_ID_DVVIDEO,
    CODEC_ID_HUFFYUV,
    CODEC_ID_CYUV,
    CODEC_ID_H264,
    CODEC_ID_INDEO3,
    CODEC_ID_VP3,
    CODEC_ID_THEORA,
    CODEC_ID_ASV1,
    CODEC_ID_ASV2,
    CODEC_ID_FFV1,
    CODEC_ID_4XM,
    CODEC_ID_VCR1,
    CODEC_ID_CLJR,
    CODEC_ID_MDEC,
    CODEC_ID_ROQ,
    CODEC_ID_INTERPLAY_VIDEO,
    CODEC_ID_XAN_WC3,
    CODEC_ID_XAN_WC4,
    CODEC_ID_RPZA,
    CODEC_ID_CINEPAK,
    CODEC_ID_WS_VQA,
    CODEC_ID_MSRLE,
    CODEC_ID_MSVIDEO1,
    CODEC_ID_IDCIN,
    CODEC_ID_8BPS,
    CODEC_ID_SMC,
    CODEC_ID_FLIC,
    CODEC_ID_TRUEMOTION1,
    CODEC_ID_VMDVIDEO,
    CODEC_ID_MSZH,
    CODEC_ID_ZLIB,
    CODEC_ID_QTRLE,
    CODEC_ID_TSCC,
    CODEC_ID_ULTI,
    CODEC_ID_QDRAW,
    CODEC_ID_VIXL,
    CODEC_ID_QPEG,
    CODEC_ID_PNG,
    CODEC_ID_PPM,
    CODEC_ID_PBM,
    CODEC_ID_PGM,
    CODEC_ID_PGMYUV,
    CODEC_ID_PAM,
    CODEC_ID_FFVHUFF,
    CODEC_ID_RV30,
    CODEC_ID_RV40,
    CODEC_ID_VC1,
    CODEC_ID_WMV3,
    CODEC_ID_LOCO,
    CODEC_ID_WNV1,
    CODEC_ID_AASC,
    CODEC_ID_INDEO2,
    CODEC_ID_FRAPS,
    CODEC_ID_TRUEMOTION2,
    CODEC_ID_BMP,
    CODEC_ID_CSCD,
    CODEC_ID_MMVIDEO,
    CODEC_ID_ZMBV,
    CODEC_ID_AVS,
    CODEC_ID_SMACKVIDEO,
    CODEC_ID_NUV,
    CODEC_ID_KMVC,
    CODEC_ID_FLASHSV,
    CODEC_ID_CAVS,
    CODEC_ID_JPEG2000,
    CODEC_ID_VMNC,
    CODEC_ID_VP5,
    CODEC_ID_VP6,
    CODEC_ID_VP6F,
    CODEC_ID_TARGA,
    CODEC_ID_DSICINVIDEO,
    CODEC_ID_TIERTEXSEQVIDEO,
    CODEC_ID_TIFF,
    CODEC_ID_GIF,
    CODEC_ID_DXA,
    CODEC_ID_DNXHD,
    CODEC_ID_THP,
    CODEC_ID_SGI,
    CODEC_ID_C93,
    CODEC_ID_BETHSOFTVID,
    CODEC_ID_PTX,
    CODEC_ID_TXD,
    CODEC_ID_VP6A,
    CODEC_ID_AMV,
    CODEC_ID_VB,
    CODEC_ID_PCX,
    CODEC_ID_SUNRAST,
    CODEC_ID_INDEO4,
    CODEC_ID_INDEO5,
    CODEC_ID_MIMIC,
    CODEC_ID_RL2,
    CODEC_ID_ESCAPE124,
    CODEC_ID_DIRAC,
    CODEC_ID_BFI,
    CODEC_ID_CMV,
    CODEC_ID_MOTIONPIXELS,
    CODEC_ID_TGV,
    CODEC_ID_TGQ,
    CODEC_ID_TQI,
    CODEC_ID_AURA,
    CODEC_ID_AURA2,
    CODEC_ID_V210X,
    CODEC_ID_TMV,
    CODEC_ID_V210,
    CODEC_ID_DPX,
    CODEC_ID_MAD,
    CODEC_ID_FRWU,
    CODEC_ID_FLASHSV2,
    CODEC_ID_CDGRAPHICS,
    CODEC_ID_R210,
    CODEC_ID_ANM,
    CODEC_ID_BINKVIDEO,
    CODEC_ID_IFF_ILBM,
    CODEC_ID_IFF_BYTERUN1,
    CODEC_ID_KGV1,
    CODEC_ID_YOP,
    CODEC_ID_VP8,
    CODEC_ID_PICTOR,
    CODEC_ID_ANSI,
    CODEC_ID_A64_MULTI,
    CODEC_ID_A64_MULTI5,
    CODEC_ID_R10K,
    CODEC_ID_MXPEG,
    CODEC_ID_LAGARITH,
    CODEC_ID_PRORES,
    CODEC_ID_JV,
    CODEC_ID_DFA,
    CODEC_ID_WMV3IMAGE,
    CODEC_ID_VC1IMAGE,
    CODEC_ID_UTVIDEO,
    CODEC_ID_BMV_VIDEO,
    CODEC_ID_VBLE,
    CODEC_ID_DXTORY,
    CODEC_ID_V410,
    CODEC_ID_XWD,
    CODEC_ID_CDXL,
    CODEC_ID_XBM,
    CODEC_ID_ZEROCODEC,
    CODEC_ID_MSS1,
    CODEC_ID_MSA1,
    CODEC_ID_TSCC2,
    CODEC_ID_MTS2,
    CODEC_ID_CLLC,
    CODEC_ID_Y41P       = MKBETAG('Y', '4', '1', 'P'),
    CODEC_ID_ESCAPE130  = MKBETAG('E', '1', '3', '0'),
    CODEC_ID_EXR        = MKBETAG('0', 'E', 'X', 'R'),
    CODEC_ID_AVRP       = MKBETAG('A', 'V', 'R', 'P'),

    CODEC_ID_G2M        = MKBETAG(0, 'G', '2', 'M'),
    CODEC_ID_AVUI       = MKBETAG('A', 'V', 'U', 'I'),
    CODEC_ID_AYUV       = MKBETAG('A', 'Y', 'U', 'V'),
    CODEC_ID_V308       = MKBETAG('V', '3', '0', '8'),
    CODEC_ID_V408       = MKBETAG('V', '4', '0', '8'),
    CODEC_ID_YUV4       = MKBETAG('Y', 'U', 'V', '4'),
    CODEC_ID_SANM       = MKBETAG('S', 'A', 'N', 'M'),
    CODEC_ID_PAF_VIDEO  = MKBETAG('P', 'A', 'F', 'V'),
    CODEC_ID_SNOW       = AV_CODEC_ID_SNOW,

    CODEC_ID_FIRST_AUDIO = 0x10000,
    CODEC_ID_PCM_S16LE = 0x10000,
    CODEC_ID_PCM_S16BE,
    CODEC_ID_PCM_U16LE,
    CODEC_ID_PCM_U16BE,
    CODEC_ID_PCM_S8,
    CODEC_ID_PCM_U8,
    CODEC_ID_PCM_MULAW,
    CODEC_ID_PCM_ALAW,
    CODEC_ID_PCM_S32LE,
    CODEC_ID_PCM_S32BE,
    CODEC_ID_PCM_U32LE,
    CODEC_ID_PCM_U32BE,
    CODEC_ID_PCM_S24LE,
    CODEC_ID_PCM_S24BE,
    CODEC_ID_PCM_U24LE,
    CODEC_ID_PCM_U24BE,
    CODEC_ID_PCM_S24DAUD,
    CODEC_ID_PCM_ZORK,
    CODEC_ID_PCM_S16LE_PLANAR,
    CODEC_ID_PCM_DVD,
    CODEC_ID_PCM_F32BE,
    CODEC_ID_PCM_F32LE,
    CODEC_ID_PCM_F64BE,
    CODEC_ID_PCM_F64LE,
    CODEC_ID_PCM_BLURAY,
    CODEC_ID_PCM_LXF,
    CODEC_ID_S302M,
    CODEC_ID_PCM_S8_PLANAR,

    CODEC_ID_ADPCM_IMA_QT = 0x11000,
    CODEC_ID_ADPCM_IMA_WAV,
    CODEC_ID_ADPCM_IMA_DK3,
    CODEC_ID_ADPCM_IMA_DK4,
    CODEC_ID_ADPCM_IMA_WS,
    CODEC_ID_ADPCM_IMA_SMJPEG,
    CODEC_ID_ADPCM_MS,
    CODEC_ID_ADPCM_4XM,
    CODEC_ID_ADPCM_XA,
    CODEC_ID_ADPCM_ADX,
    CODEC_ID_ADPCM_EA,
    CODEC_ID_ADPCM_G726,
    CODEC_ID_ADPCM_CT,
    CODEC_ID_ADPCM_SWF,
    CODEC_ID_ADPCM_YAMAHA,
    CODEC_ID_ADPCM_SBPRO_4,
    CODEC_ID_ADPCM_SBPRO_3,
    CODEC_ID_ADPCM_SBPRO_2,
    CODEC_ID_ADPCM_THP,
    CODEC_ID_ADPCM_IMA_AMV,
    CODEC_ID_ADPCM_EA_R1,
    CODEC_ID_ADPCM_EA_R3,
    CODEC_ID_ADPCM_EA_R2,
    CODEC_ID_ADPCM_IMA_EA_SEAD,
    CODEC_ID_ADPCM_IMA_EA_EACS,
    CODEC_ID_ADPCM_EA_XAS,
    CODEC_ID_ADPCM_EA_MAXIS_XA,
    CODEC_ID_ADPCM_IMA_ISS,
    CODEC_ID_ADPCM_G722,
    CODEC_ID_ADPCM_IMA_APC,
    CODEC_ID_VIMA       = MKBETAG('V', 'I', 'M', 'A'),

    CODEC_ID_AMR_NB = 0x12000,
    CODEC_ID_AMR_WB,

    CODEC_ID_RA_144 = 0x13000,
    CODEC_ID_RA_288,

    CODEC_ID_ROQ_DPCM = 0x14000,
    CODEC_ID_INTERPLAY_DPCM,
    CODEC_ID_XAN_DPCM,
    CODEC_ID_SOL_DPCM,

    CODEC_ID_MP2 = 0x15000,
    CODEC_ID_MP3,
    CODEC_ID_AAC,
    CODEC_ID_AC3,
    CODEC_ID_DTS,
    CODEC_ID_VORBIS,
    CODEC_ID_DVAUDIO,
    CODEC_ID_WMAV1,
    CODEC_ID_WMAV2,
    CODEC_ID_MACE3,
    CODEC_ID_MACE6,
    CODEC_ID_VMDAUDIO,
    CODEC_ID_FLAC,
    CODEC_ID_MP3ADU,
    CODEC_ID_MP3ON4,
    CODEC_ID_SHORTEN,
    CODEC_ID_ALAC,
    CODEC_ID_WESTWOOD_SND1,
    CODEC_ID_GSM,
    CODEC_ID_QDM2,
    CODEC_ID_COOK,
    CODEC_ID_TRUESPEECH,
    CODEC_ID_TTA,
    CODEC_ID_SMACKAUDIO,
    CODEC_ID_QCELP,
    CODEC_ID_WAVPACK,
    CODEC_ID_DSICINAUDIO,
    CODEC_ID_IMC,
    CODEC_ID_MUSEPACK7,
    CODEC_ID_MLP,
    CODEC_ID_GSM_MS,
    CODEC_ID_ATRAC3,
    CODEC_ID_VOXWARE,
    CODEC_ID_APE,
    CODEC_ID_NELLYMOSER,
    CODEC_ID_MUSEPACK8,
    CODEC_ID_SPEEX,
    CODEC_ID_WMAVOICE,
    CODEC_ID_WMAPRO,
    CODEC_ID_WMALOSSLESS,
    CODEC_ID_ATRAC3P,
    CODEC_ID_EAC3,
    CODEC_ID_SIPR,
    CODEC_ID_MP1,
    CODEC_ID_TWINVQ,
    CODEC_ID_TRUEHD,
    CODEC_ID_MP4ALS,
    CODEC_ID_ATRAC1,
    CODEC_ID_BINKAUDIO_RDFT,
    CODEC_ID_BINKAUDIO_DCT,
    CODEC_ID_AAC_LATM,
    CODEC_ID_QDMC,
    CODEC_ID_CELT,
    CODEC_ID_G723_1,
    CODEC_ID_G729,
    CODEC_ID_8SVX_EXP,
    CODEC_ID_8SVX_FIB,
    CODEC_ID_BMV_AUDIO,
    CODEC_ID_RALF,
    CODEC_ID_IAC,
    CODEC_ID_ILBC,
    CODEC_ID_FFWAVESYNTH = MKBETAG('F', 'F', 'W', 'S'),
    CODEC_ID_SONIC       = MKBETAG('S', 'O', 'N', 'C'),
    CODEC_ID_SONIC_LS    = MKBETAG('S', 'O', 'N', 'L'),
    CODEC_ID_PAF_AUDIO   = MKBETAG('P', 'A', 'F', 'A'),
    CODEC_ID_OPUS        = MKBETAG('O', 'P', 'U', 'S'),

    CODEC_ID_FIRST_SUBTITLE = 0x17000,
    CODEC_ID_DVD_SUBTITLE = 0x17000,
    CODEC_ID_DVB_SUBTITLE,
    CODEC_ID_TEXT,
    CODEC_ID_XSUB,
    CODEC_ID_SSA,
    CODEC_ID_MOV_TEXT,
    CODEC_ID_HDMV_PGS_SUBTITLE,
    CODEC_ID_DVB_TELETEXT,
    CODEC_ID_SRT,
    CODEC_ID_MICRODVD   = MKBETAG('m', 'D', 'V', 'D'),
    CODEC_ID_EIA_608    = MKBETAG('c', '6', '0', '8'),
    CODEC_ID_JACOSUB    = MKBETAG('J', 'S', 'U', 'B'),
    CODEC_ID_SAMI       = MKBETAG('S', 'A', 'M', 'I'),
    CODEC_ID_REALTEXT   = MKBETAG('R', 'T', 'X', 'T'),
    CODEC_ID_SUBVIEWER  = MKBETAG('S', 'u', 'b', 'V'),

    CODEC_ID_FIRST_UNKNOWN = 0x18000,
    CODEC_ID_TTF = 0x18000,
    CODEC_ID_BINTEXT    = MKBETAG('B', 'T', 'X', 'T'),
    CODEC_ID_XBIN       = MKBETAG('X', 'B', 'I', 'N'),
    CODEC_ID_IDF        = MKBETAG(0, 'I', 'D', 'F'),
    CODEC_ID_OTF        = MKBETAG(0, 'O', 'T', 'F'),

    CODEC_ID_PROBE = 0x19000,

    CODEC_ID_MPEG2TS = 0x20000,

    CODEC_ID_MPEG4SYSTEMS = 0x20001,

    CODEC_ID_FFMETADATA = 0x21000,
};

typedef struct AVCodecDescriptor {
    enum AVCodecID id;
    enum AVMediaType type;

    const char* name;

    const char* long_name;

    int props;

    const char* const* mime_types;
} AVCodecDescriptor;

#define AV_CODEC_PROP_INTRA_ONLY    (1 << 0)

#define AV_CODEC_PROP_LOSSY         (1 << 1)

#define AV_CODEC_PROP_LOSSLESS      (1 << 2)

#define AV_CODEC_PROP_BITMAP_SUB    (1 << 16)

#define AV_CODEC_PROP_TEXT_SUB      (1 << 17)

#define FF_INPUT_BUFFER_PADDING_SIZE 32

#define FF_MIN_BUFFER_SIZE 16384

enum Motion_Est_ID {
    ME_ZERO = 1,
    ME_FULL,
    ME_LOG,
    ME_PHODS,
    ME_EPZS,
    ME_X1,
    ME_HEX,
    ME_UMH,
    ME_TESA,
    ME_ITER=50,
};

enum AVDiscard {
    AVDISCARD_NONE    =-16,
    AVDISCARD_DEFAULT =  0,
    AVDISCARD_NONREF  =  8,
    AVDISCARD_BIDIR   = 16,
    AVDISCARD_NONINTRA= 24,
    AVDISCARD_NONKEY  = 32,
    AVDISCARD_ALL     = 48,
};

enum AVAudioServiceType {
    AV_AUDIO_SERVICE_TYPE_MAIN              = 0,
    AV_AUDIO_SERVICE_TYPE_EFFECTS           = 1,
    AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2,
    AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  = 3,
    AV_AUDIO_SERVICE_TYPE_DIALOGUE          = 4,
    AV_AUDIO_SERVICE_TYPE_COMMENTARY        = 5,
    AV_AUDIO_SERVICE_TYPE_EMERGENCY         = 6,
    AV_AUDIO_SERVICE_TYPE_VOICE_OVER        = 7,
    AV_AUDIO_SERVICE_TYPE_KARAOKE           = 8,
    AV_AUDIO_SERVICE_TYPE_NB,
};

typedef struct RcOverride {
    int start_frame;
    int end_frame;
    int qscale;
    float quality_factor;
} RcOverride;

#define FF_MAX_B_FRAMES 16

#define CODEC_FLAG_UNALIGNED 0x0001
#define CODEC_FLAG_QSCALE 0x0002
#define CODEC_FLAG_4MV    0x0004
#define CODEC_FLAG_OUTPUT_CORRUPT 0x0008
#define CODEC_FLAG_QPEL   0x0010

#define CODEC_FLAG_GMC    0x0020

#define CODEC_FLAG_MV0    0x0040

#define CODEC_FLAG_INPUT_PRESERVED 0x0100
#define CODEC_FLAG_PASS1           0x0200
#define CODEC_FLAG_PASS2           0x0400
#define CODEC_FLAG_GRAY            0x2000

#define CODEC_FLAG_EMU_EDGE        0x4000
#define CODEC_FLAG_PSNR            0x8000
#define CODEC_FLAG_TRUNCATED       0x00010000

#define CODEC_FLAG_NORMALIZE_AQP  0x00020000
#define CODEC_FLAG_INTERLACED_DCT 0x00040000
#define CODEC_FLAG_LOW_DELAY      0x00080000
#define CODEC_FLAG_GLOBAL_HEADER  0x00400000
#define CODEC_FLAG_BITEXACT       0x00800000

#define CODEC_FLAG_AC_PRED        0x01000000
#define CODEC_FLAG_LOOP_FILTER    0x00000800
#define CODEC_FLAG_INTERLACED_ME  0x20000000
#define CODEC_FLAG_CLOSED_GOP     0x80000000
#define CODEC_FLAG2_FAST          0x00000001
#define CODEC_FLAG2_NO_OUTPUT     0x00000004
#define CODEC_FLAG2_LOCAL_HEADER  0x00000008
#define CODEC_FLAG2_DROP_FRAME_TIMECODE 0x00002000
#define CODEC_FLAG2_IGNORE_CROP   0x00010000

#define CODEC_FLAG2_CHUNKS        0x00008000
#define CODEC_FLAG2_SHOW_ALL      0x00400000

#define CODEC_CAP_DRAW_HORIZ_BAND 0x0001

#define CODEC_CAP_DR1             0x0002
#define CODEC_CAP_TRUNCATED       0x0008

#define CODEC_CAP_HWACCEL         0x0010

#define CODEC_CAP_DELAY           0x0020

#define CODEC_CAP_SMALL_LAST_FRAME 0x0040

#define CODEC_CAP_HWACCEL_VDPAU    0x0080

#define CODEC_CAP_SUBFRAMES        0x0100

#define CODEC_CAP_EXPERIMENTAL     0x0200

#define CODEC_CAP_CHANNEL_CONF     0x0400

#define CODEC_CAP_NEG_LINESIZES    0x0800

#define CODEC_CAP_FRAME_THREADS    0x1000

#define CODEC_CAP_SLICE_THREADS    0x2000

#define CODEC_CAP_PARAM_CHANGE     0x4000

#define CODEC_CAP_AUTO_THREADS     0x8000

#define CODEC_CAP_VARIABLE_FRAME_SIZE 0x10000

#define CODEC_CAP_INTRA_ONLY       0x40000000

#define CODEC_CAP_LOSSLESS         0x80000000

#define MB_TYPE_INTRA4x4   0x0001
#define MB_TYPE_INTRA16x16 0x0002
#define MB_TYPE_INTRA_PCM  0x0004
#define MB_TYPE_16x16      0x0008
#define MB_TYPE_16x8       0x0010
#define MB_TYPE_8x16       0x0020
#define MB_TYPE_8x8        0x0040
#define MB_TYPE_INTERLACED 0x0080
#define MB_TYPE_DIRECT2    0x0100
#define MB_TYPE_ACPRED     0x0200
#define MB_TYPE_GMC        0x0400
#define MB_TYPE_SKIP       0x0800
#define MB_TYPE_P0L0       0x1000
#define MB_TYPE_P1L0       0x2000
#define MB_TYPE_P0L1       0x4000
#define MB_TYPE_P1L1       0x8000
#define MB_TYPE_L0         (MB_TYPE_P0L0 | MB_TYPE_P1L0)
#define MB_TYPE_L1         (MB_TYPE_P0L1 | MB_TYPE_P1L1)
#define MB_TYPE_L0L1       (MB_TYPE_L0 | MB_TYPE_L1)
#define MB_TYPE_QUANT      0x00010000
#define MB_TYPE_CBP        0x00020000

typedef struct AVPanScan {
    int id;

    int width;
    int height;

    int16_t position[3][2];
}AVPanScan;

#define FF_QSCALE_TYPE_MPEG1 0
#define FF_QSCALE_TYPE_MPEG2 1
#define FF_QSCALE_TYPE_H264  2
#define FF_QSCALE_TYPE_VP56  3

#define FF_BUFFER_TYPE_INTERNAL 1
#define FF_BUFFER_TYPE_USER     2
#define FF_BUFFER_TYPE_SHARED   4
#define FF_BUFFER_TYPE_COPY     8

#define FF_BUFFER_HINTS_VALID    0x01
#define FF_BUFFER_HINTS_READABLE 0x02
#define FF_BUFFER_HINTS_PRESERVE 0x04
#define FF_BUFFER_HINTS_REUSABLE 0x08

#define AV_GET_BUFFER_FLAG_REF (1 << 0)

enum AVPacketSideDataType {
    AV_PKT_DATA_PALETTE,
    AV_PKT_DATA_NEW_EXTRADATA,

    AV_PKT_DATA_PARAM_CHANGE,

    AV_PKT_DATA_H263_MB_INFO,

    AV_PKT_DATA_REPLAYGAIN,

    AV_PKT_DATA_DISPLAYMATRIX,

    AV_PKT_DATA_SKIP_SAMPLES=70,

    AV_PKT_DATA_JP_DUALMONO,

    AV_PKT_DATA_STRINGS_METADATA,

    AV_PKT_DATA_SUBTITLE_POSITION,

    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    AV_PKT_DATA_WEBVTT_SETTINGS,

    AV_PKT_DATA_METADATA_UPDATE,
};

typedef struct AVPacketSideData {
    uint8_t* data;
    int size;
    enum AVPacketSideDataType type;
} AVPacketSideData;

typedef struct AVPacket {
    AVBufferRef* buf;

    int64_t pts;

    int64_t dts;
    uint8_t* data;
    int size;
    int stream_index;

    int flags;

    AVPacketSideData* side_data;
    int side_data_elems;

    int duration;
    attribute_deprecated
    void (* destruct)(struct AVPacket*);
    attribute_deprecated
    void* priv;
    int64_t pos;

    int64_t convergence_duration;
} AVPacket;
#define AV_PKT_FLAG_KEY     0x0001
#define AV_PKT_FLAG_CORRUPT 0x0002

enum AVSideDataParamChangeFlags {
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT  = 0x0001,
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = 0x0002,
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE    = 0x0004,
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS     = 0x0008,
};

struct AVCodecInternal;

enum AVFieldOrder {
    AV_FIELD_UNKNOWN,
        AV_FIELD_PROGRESSIVE,
        AV_FIELD_TT,
        AV_FIELD_BB,
        AV_FIELD_TB,
        AV_FIELD_BT,
};

typedef struct AVCodecContext {
    const AVClass* av_class;
    int log_level_offset;

    enum AVMediaType codec_type;
    const struct AVCodec* codec;

    attribute_deprecated
    char codec_name[32];
    enum AVCodecID codec_id;

    unsigned int codec_tag;

    unsigned int stream_codec_tag;

    void* priv_data;

    struct AVCodecInternal* internal;

    void* opaque;

    int bit_rate;

    int bit_rate_tolerance;

    int global_quality;

    int compression_level;
#define FF_COMPRESSION_DEFAULT -1

    int flags;

    int flags2;

    uint8_t* extradata;
    int extradata_size;

    AVRational time_base;

    int ticks_per_frame;

    int delay;

    int width, height;

    int coded_width, coded_height;

#define FF_ASPECT_EXTENDED 15

    int gop_size;

    enum AVPixelFormat pix_fmt;

    int me_method;

    void (* draw_horiz_band)(struct AVCodecContext* s, const AVFrame* src, int offset[AV_NUM_DATA_POINTERS], int y, int type, int height);

    enum AVPixelFormat (*get_format)(struct AVCodecContext* s, const enum AVPixelFormat* fmt);

    int max_b_frames;

    float b_quant_factor;

    int rc_strategy;
#define FF_RC_STRATEGY_XVID 1

    int b_frame_strategy;

    float b_quant_offset;

    int has_b_frames;

    int mpeg_quant;

    float i_quant_factor;

    float i_quant_offset;

    float lumi_masking;

    float temporal_cplx_masking;

    float spatial_cplx_masking;

    float p_masking;

    float dark_masking;

    int slice_count;

    int prediction_method;
#define FF_PRED_LEFT   0
#define FF_PRED_PLANE  1
#define FF_PRED_MEDIAN 2

    int* slice_offset;

    AVRational sample_aspect_ratio;

    int me_cmp;

    int me_sub_cmp;

    int mb_cmp;

    int ildct_cmp;
#define FF_CMP_SAD    0
#define FF_CMP_SSE    1
#define FF_CMP_SATD   2
#define FF_CMP_DCT    3
#define FF_CMP_PSNR   4
#define FF_CMP_BIT    5
#define FF_CMP_RD     6
#define FF_CMP_ZERO   7
#define FF_CMP_VSAD   8
#define FF_CMP_VSSE   9
#define FF_CMP_NSSE   10
#define FF_CMP_W53    11
#define FF_CMP_W97    12
#define FF_CMP_DCTMAX 13
#define FF_CMP_DCT264 14
#define FF_CMP_CHROMA 256

    int dia_size;

    int last_predictor_count;

    int pre_me;

    int me_pre_cmp;

    int pre_dia_size;

    int me_subpel_quality;

    int dtg_active_format;
#define FF_DTG_AFD_SAME         8
#define FF_DTG_AFD_4_3          9
#define FF_DTG_AFD_16_9         10
#define FF_DTG_AFD_14_9         11
#define FF_DTG_AFD_4_3_SP_14_9  13
#define FF_DTG_AFD_16_9_SP_14_9 14
#define FF_DTG_AFD_SP_4_3       15

    int me_range;

    int intra_quant_bias;
#define FF_DEFAULT_QUANT_BIAS 999999

    int inter_quant_bias;

    int slice_flags;
#define SLICE_FLAG_CODED_ORDER    0x0001
#define SLICE_FLAG_ALLOW_FIELD    0x0002
#define SLICE_FLAG_ALLOW_PLANE    0x0004

    attribute_deprecated int xvmc_acceleration;

    int mb_decision;
#define FF_MB_DECISION_SIMPLE 0
#define FF_MB_DECISION_BITS   1
#define FF_MB_DECISION_RD     2

    uint16_t* intra_matrix;

    uint16_t* inter_matrix;

    int scenechange_threshold;

    int noise_reduction;

    int me_threshold;

    int mb_threshold;

    int intra_dc_precision;

    int skip_top;

    int skip_bottom;

    float border_masking;

    int mb_lmin;

    int mb_lmax;

    int me_penalty_compensation;

    int bidir_refine;

    int brd_scale;

    int keyint_min;

    int refs;

    int chromaoffset;

    int scenechange_factor;

    int mv0_threshold;

    int b_sensitivity;

    enum AVColorPrimaries color_primaries;

    enum AVColorTransferCharacteristic color_trc;

    enum AVColorSpace colorspace;

    enum AVColorRange color_range;

    enum AVChromaLocation chroma_sample_location;

    int slices;

    enum AVFieldOrder field_order;

    int sample_rate;
    int channels;

    enum AVSampleFormat sample_fmt;

    int frame_size;

    int frame_number;

    int block_align;

    int cutoff;

    attribute_deprecated int request_channels;

    uint64_t channel_layout;

    uint64_t request_channel_layout;

    enum AVAudioServiceType audio_service_type;

    enum AVSampleFormat request_sample_fmt;

    attribute_deprecated
    int (* get_buffer)(struct AVCodecContext* c, AVFrame* pic);

    attribute_deprecated
    void (* release_buffer)(struct AVCodecContext* c, AVFrame* pic);

    attribute_deprecated
    int (* reget_buffer)(struct AVCodecContext* c, AVFrame* pic);

    int (* get_buffer2)(struct AVCodecContext* s, AVFrame* frame, int flags);

    int refcounted_frames;

    float qcompress;
    float qblur;

    int qmin;

    int qmax;

    int max_qdiff;

    float rc_qsquish;

    float rc_qmod_amp;
    int rc_qmod_freq;

    int rc_buffer_size;

    int rc_override_count;
    RcOverride* rc_override;

    const char* rc_eq;

    int rc_max_rate;

    int rc_min_rate;

    float rc_buffer_aggressivity;

    float rc_initial_cplx;

    float rc_max_available_vbv_use;

    float rc_min_vbv_overflow_use;

    int rc_initial_buffer_occupancy;

#define FF_CODER_TYPE_VLC       0
#define FF_CODER_TYPE_AC        1
#define FF_CODER_TYPE_RAW       2
#define FF_CODER_TYPE_RLE       3
#define FF_CODER_TYPE_DEFLATE   4

    int coder_type;

    int context_model;

    int lmin;

    int lmax;

    int frame_skip_threshold;

    int frame_skip_factor;

    int frame_skip_exp;

    int frame_skip_cmp;

    int trellis;

    int min_prediction_order;

    int max_prediction_order;

    int64_t timecode_frame_start;

    void (* rtp_callback)(struct AVCodecContext* avctx, void* data, int size, int mb_nb);

    int rtp_payload_size;

    int mv_bits;
    int header_bits;
    int i_tex_bits;
    int p_tex_bits;
    int i_count;
    int p_count;
    int skip_count;
    int misc_bits;

    int frame_bits;

    char* stats_out;

    char* stats_in;

    int workaround_bugs;
#define FF_BUG_AUTODETECT       1
#define FF_BUG_OLD_MSMPEG4      2
#define FF_BUG_XVID_ILACE       4
#define FF_BUG_UMP4             8
#define FF_BUG_NO_PADDING       16
#define FF_BUG_AMV              32
#define FF_BUG_AC_VLC           0
#define FF_BUG_QPEL_CHROMA      64
#define FF_BUG_STD_QPEL         128
#define FF_BUG_QPEL_CHROMA2     256
#define FF_BUG_DIRECT_BLOCKSIZE 512
#define FF_BUG_EDGE             1024
#define FF_BUG_HPEL_CHROMA      2048
#define FF_BUG_DC_CLIP          4096
#define FF_BUG_MS               8192
#define FF_BUG_TRUNCATED       16384

    int strict_std_compliance;
#define FF_COMPLIANCE_VERY_STRICT   2
#define FF_COMPLIANCE_STRICT        1
#define FF_COMPLIANCE_NORMAL        0
#define FF_COMPLIANCE_UNOFFICIAL   -1
#define FF_COMPLIANCE_EXPERIMENTAL -2

    int error_concealment;
#define FF_EC_GUESS_MVS   1
#define FF_EC_DEBLOCK     2
#define FF_EC_FAVOR_INTER 256

    int debug;
#define FF_DEBUG_PICT_INFO   1
#define FF_DEBUG_RC          2
#define FF_DEBUG_BITSTREAM   4
#define FF_DEBUG_MB_TYPE     8
#define FF_DEBUG_QP          16

#define FF_DEBUG_MV          32
#define FF_DEBUG_DCT_COEFF   0x00000040
#define FF_DEBUG_SKIP        0x00000080
#define FF_DEBUG_STARTCODE   0x00000100
#define FF_DEBUG_PTS         0x00000200
#define FF_DEBUG_ER          0x00000400
#define FF_DEBUG_MMCO        0x00000800
#define FF_DEBUG_BUGS        0x00001000
#define FF_DEBUG_VIS_QP      0x00002000
#define FF_DEBUG_VIS_MB_TYPE 0x00004000
#define FF_DEBUG_BUFFERS     0x00008000
#define FF_DEBUG_THREADS     0x00010000
#define FF_DEBUG_NOMC        0x01000000

    int debug_mv;
#define FF_DEBUG_VIS_MV_P_FOR  0x00000001
#define FF_DEBUG_VIS_MV_B_FOR  0x00000002
#define FF_DEBUG_VIS_MV_B_BACK 0x00000004

    int err_recognition;

#define AV_EF_CRCCHECK  (1 << 0)
#define AV_EF_BITSTREAM (1 << 1)
#define AV_EF_BUFFER    (1 << 2)
#define AV_EF_EXPLODE   (1 << 3)

#define AV_EF_IGNORE_ERR (1 << 15)
#define AV_EF_CAREFUL    (1 << 16)
#define AV_EF_COMPLIANT  (1 << 17)
#define AV_EF_AGGRESSIVE (1 << 18)

    int64_t reordered_opaque;

    struct AVHWAccel* hwaccel;

    void* hwaccel_context;

    uint64_t error[AV_NUM_DATA_POINTERS];

    int dct_algo;
#define FF_DCT_AUTO    0
#define FF_DCT_FASTINT 1
#define FF_DCT_INT     2
#define FF_DCT_MMX     3
#define FF_DCT_ALTIVEC 5
#define FF_DCT_FAAN    6

    int idct_algo;
#define FF_IDCT_AUTO          0
#define FF_IDCT_INT           1
#define FF_IDCT_SIMPLE        2
#define FF_IDCT_SIMPLEMMX     3
#define FF_IDCT_ARM           7
#define FF_IDCT_ALTIVEC       8
#define FF_IDCT_SH4           9
#define FF_IDCT_SIMPLEARM     10
#define FF_IDCT_IPP           13
#define FF_IDCT_XVIDMMX       14
#define FF_IDCT_SIMPLEARMV5TE 16
#define FF_IDCT_SIMPLEARMV6   17
#define FF_IDCT_SIMPLEVIS     18
#define FF_IDCT_FAAN          20
#define FF_IDCT_SIMPLENEON    22
#define FF_IDCT_SIMPLEALPHA   23
#define FF_IDCT_SIMPLEAUTO    128

    int bits_per_coded_sample;

    int bits_per_raw_sample;

    int lowres;

    AVFrame* coded_frame;

    int thread_count;

    int thread_type;
#define FF_THREAD_FRAME   1
#define FF_THREAD_SLICE   2

    int active_thread_type;

    int thread_safe_callbacks;

    int (* execute)(struct AVCodecContext* c, int (*func)(struct AVCodecContext* c2, void* arg), void* arg2, int* ret, int count, int size);

    int (* execute2)(struct AVCodecContext* c, int (*func)(struct AVCodecContext* c2, void* arg, int jobnr, int threadnr), void* arg2,
                     int* ret, int count);

    attribute_deprecated
    void* thread_opaque;

    int nsse_weight;

    int profile;
#define FF_PROFILE_UNKNOWN -99
#define FF_PROFILE_RESERVED -100

#define FF_PROFILE_AAC_MAIN 0
#define FF_PROFILE_AAC_LOW  1
#define FF_PROFILE_AAC_SSR  2
#define FF_PROFILE_AAC_LTP  3
#define FF_PROFILE_AAC_HE   4
#define FF_PROFILE_AAC_HE_V2 28
#define FF_PROFILE_AAC_LD   22
#define FF_PROFILE_AAC_ELD  38
#define FF_PROFILE_MPEG2_AAC_LOW 128
#define FF_PROFILE_MPEG2_AAC_HE  131

#define FF_PROFILE_DTS         20
#define FF_PROFILE_DTS_ES      30
#define FF_PROFILE_DTS_96_24   40
#define FF_PROFILE_DTS_HD_HRA  50
#define FF_PROFILE_DTS_HD_MA   60

#define FF_PROFILE_MPEG2_422    0
#define FF_PROFILE_MPEG2_HIGH   1
#define FF_PROFILE_MPEG2_SS     2
#define FF_PROFILE_MPEG2_SNR_SCALABLE  3
#define FF_PROFILE_MPEG2_MAIN   4
#define FF_PROFILE_MPEG2_SIMPLE 5

#define FF_PROFILE_H264_CONSTRAINED  (1 << 9)
#define FF_PROFILE_H264_INTRA        (1 << 11)

#define FF_PROFILE_H264_BASELINE             66
#define FF_PROFILE_H264_CONSTRAINED_BASELINE (66 | FF_PROFILE_H264_CONSTRAINED)
#define FF_PROFILE_H264_MAIN                 77
#define FF_PROFILE_H264_EXTENDED             88
#define FF_PROFILE_H264_HIGH                 100
#define FF_PROFILE_H264_HIGH_10              110
#define FF_PROFILE_H264_HIGH_10_INTRA        (110 | FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_HIGH_422             122
#define FF_PROFILE_H264_HIGH_422_INTRA       (122 | FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_HIGH_444             144
#define FF_PROFILE_H264_HIGH_444_PREDICTIVE  244
#define FF_PROFILE_H264_HIGH_444_INTRA       (244 | FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_CAVLC_444            44

#define FF_PROFILE_VC1_SIMPLE   0
#define FF_PROFILE_VC1_MAIN     1
#define FF_PROFILE_VC1_COMPLEX  2
#define FF_PROFILE_VC1_ADVANCED 3

#define FF_PROFILE_MPEG4_SIMPLE                     0
#define FF_PROFILE_MPEG4_SIMPLE_SCALABLE            1
#define FF_PROFILE_MPEG4_CORE                       2
#define FF_PROFILE_MPEG4_MAIN                       3
#define FF_PROFILE_MPEG4_N_BIT                      4
#define FF_PROFILE_MPEG4_SCALABLE_TEXTURE           5
#define FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION      6
#define FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE     7
#define FF_PROFILE_MPEG4_HYBRID                     8
#define FF_PROFILE_MPEG4_ADVANCED_REAL_TIME         9
#define FF_PROFILE_MPEG4_CORE_SCALABLE             10
#define FF_PROFILE_MPEG4_ADVANCED_CODING           11
#define FF_PROFILE_MPEG4_ADVANCED_CORE             12
#define FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE 13
#define FF_PROFILE_MPEG4_SIMPLE_STUDIO             14
#define FF_PROFILE_MPEG4_ADVANCED_SIMPLE           15

#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0   0
#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1   1
#define FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION  2
#define FF_PROFILE_JPEG2000_DCINEMA_2K              3
#define FF_PROFILE_JPEG2000_DCINEMA_4K              4

#define FF_PROFILE_HEVC_MAIN                        1
#define FF_PROFILE_HEVC_MAIN_10                     2
#define FF_PROFILE_HEVC_MAIN_STILL_PICTURE          3
#define FF_PROFILE_HEVC_REXT                        4

    int level;
#define FF_LEVEL_UNKNOWN -99

    enum AVDiscard skip_loop_filter;

    enum AVDiscard skip_idct;

    enum AVDiscard skip_frame;

    uint8_t* subtitle_header;
    int subtitle_header_size;

    attribute_deprecated
    int error_rate;

    attribute_deprecated
    AVPacket* pkt;

    uint64_t vbv_delay;

    int side_data_only_packets;

    AVRational pkt_timebase;

    const AVCodecDescriptor* codec_descriptor;

    int64_t pts_correction_num_faulty_pts;
    int64_t pts_correction_num_faulty_dts;
    int64_t pts_correction_last_pts;
    int64_t pts_correction_last_dts;

    char* sub_charenc;

    int sub_charenc_mode;
#define FF_SUB_CHARENC_MODE_DO_NOTHING  -1
#define FF_SUB_CHARENC_MODE_AUTOMATIC    0
#define FF_SUB_CHARENC_MODE_PRE_DECODER  1

    int skip_alpha;

    int seek_preroll;

    uint16_t* chroma_intra_matrix;
} AVCodecContext;

AVRational av_codec_get_pkt_timebase(const AVCodecContext* avctx);
void       av_codec_set_pkt_timebase(AVCodecContext* avctx, AVRational val);

const AVCodecDescriptor* av_codec_get_codec_descriptor(const AVCodecContext* avctx);
void                     av_codec_set_codec_descriptor(AVCodecContext* avctx, const AVCodecDescriptor* desc);

int  av_codec_get_lowres(const AVCodecContext* avctx);
void av_codec_set_lowres(AVCodecContext* avctx, int val);

int  av_codec_get_seek_preroll(const AVCodecContext* avctx);
void av_codec_set_seek_preroll(AVCodecContext* avctx, int val);

uint16_t* av_codec_get_chroma_intra_matrix(const AVCodecContext* avctx);
void av_codec_set_chroma_intra_matrix(AVCodecContext* avctx, uint16_t* val);

typedef struct AVProfile {
    int profile;
    const char* name;
} AVProfile;

typedef struct AVCodecDefault AVCodecDefault;

struct AVSubtitle;

typedef struct AVCodec {
    const char* name;

    const char* long_name;
    enum AVMediaType type;
    enum AVCodecID id;

    int capabilities;
    const AVRational* supported_framerates;
    const enum AVPixelFormat* pix_fmts;
    const int* supported_samplerates;
    const enum AVSampleFormat* sample_fmts;
    const uint64_t* channel_layouts;
    uint8_t max_lowres;
    const AVClass* priv_class;
    const AVProfile* profiles;

    int priv_data_size;
    struct AVCodec* next;

    int (* init_thread_copy)(AVCodecContext*);

    int (* update_thread_context)(AVCodecContext* dst, const AVCodecContext* src);

    const AVCodecDefault* defaults;

    void (* init_static_data)(struct AVCodec* codec);

    int (* init)(AVCodecContext*);
    int (* encode_sub)(AVCodecContext*, uint8_t* buf, int buf_size, const struct AVSubtitle* sub);

    int (* encode2)(AVCodecContext* avctx, AVPacket* avpkt, const AVFrame* frame, int* got_packet_ptr);
    int (* decode)(AVCodecContext*, void* outdata, int* outdata_size, AVPacket* avpkt);
    int (* close)(AVCodecContext*);

    void (* flush)(AVCodecContext*);
} AVCodec;

int av_codec_get_max_lowres(const AVCodec* codec);

struct MpegEncContext;

typedef struct AVHWAccel {
    const char* name;

    enum AVMediaType type;

    enum AVCodecID id;

    enum AVPixelFormat pix_fmt;

    int capabilities;

    struct AVHWAccel* next;

    int (* alloc_frame)(AVCodecContext* avctx, AVFrame* frame);

    int (* start_frame)(AVCodecContext* avctx, const uint8_t* buf, uint32_t buf_size);

    int (* decode_slice)(AVCodecContext* avctx, const uint8_t* buf, uint32_t buf_size);

    int (* end_frame)(AVCodecContext* avctx);

    int frame_priv_data_size;

    void (* decode_mb)(struct MpegEncContext* s);

    int (* init)(AVCodecContext* avctx);

    int (* uninit)(AVCodecContext* avctx);

    int priv_data_size;
} AVHWAccel;

typedef struct AVPicture {
    uint8_t* data[AV_NUM_DATA_POINTERS];
    int linesize[AV_NUM_DATA_POINTERS];
} AVPicture;

enum AVSubtitleType {
    SUBTITLE_NONE,

    SUBTITLE_BITMAP,

    SUBTITLE_TEXT,

    SUBTITLE_ASS,
};

#define AV_SUBTITLE_FLAG_FORCED 0x00000001

typedef struct AVSubtitleRect {
    int x;
    int y;
    int w;
    int h;
    int nb_colors;

    AVPicture pict;
    enum AVSubtitleType type;

    char* text;

    char* ass;

    int flags;
} AVSubtitleRect;

typedef struct AVSubtitle {
    uint16_t format;
    uint32_t start_display_time;
    uint32_t end_display_time;
    unsigned num_rects;
    AVSubtitleRect** rects;
    int64_t pts;
} AVSubtitle;

AVCodec* av_codec_next(const AVCodec* c);

unsigned avcodec_version(void);

const char* avcodec_configuration(void);

const char* avcodec_license(void);

void avcodec_register(AVCodec* codec);

void avcodec_register_all(void);

AVCodecContext* avcodec_alloc_context3(const AVCodec* codec);

void avcodec_free_context(AVCodecContext** avctx);

int avcodec_get_context_defaults3(AVCodecContext* s, const AVCodec* codec);

const AVClass* avcodec_get_class(void);

const AVClass* avcodec_get_frame_class(void);

const AVClass* avcodec_get_subtitle_rect_class(void);

int avcodec_copy_context(AVCodecContext* dest, const AVCodecContext* src);

attribute_deprecated
AVFrame* avcodec_alloc_frame(void);

attribute_deprecated
void avcodec_get_frame_defaults(AVFrame* frame);

attribute_deprecated
void avcodec_free_frame(AVFrame** frame);

int avcodec_open2(AVCodecContext* avctx, const AVCodec* codec, AVDictionary** options);

int avcodec_close(AVCodecContext* avctx);

void avsubtitle_free(AVSubtitle* sub);

attribute_deprecated
void av_destruct_packet(AVPacket* pkt);

void av_init_packet(AVPacket* pkt);

int av_new_packet(AVPacket* pkt, int size);

void av_shrink_packet(AVPacket* pkt, int size);

int av_grow_packet(AVPacket* pkt, int grow_by);

int av_packet_from_data(AVPacket* pkt, uint8_t* data, int size);

int av_dup_packet(AVPacket* pkt);

int av_copy_packet(AVPacket* dst, const AVPacket* src);

int av_copy_packet_side_data(AVPacket* dst, const AVPacket* src);

void av_free_packet(AVPacket* pkt);

uint8_t* av_packet_new_side_data(AVPacket* pkt, enum AVPacketSideDataType type, int size);

int av_packet_shrink_side_data(AVPacket* pkt, enum AVPacketSideDataType type, int size);

uint8_t* av_packet_get_side_data(AVPacket* pkt, enum AVPacketSideDataType type, int* size);

int av_packet_merge_side_data(AVPacket* pkt);

int av_packet_split_side_data(AVPacket* pkt);

uint8_t* av_packet_pack_dictionary(AVDictionary* dict, int* size);

int av_packet_unpack_dictionary(const uint8_t* data, int size, AVDictionary** dict);

void av_packet_free_side_data(AVPacket* pkt);

int av_packet_ref(AVPacket* dst, const AVPacket* src);

void av_packet_unref(AVPacket* pkt);

void av_packet_move_ref(AVPacket* dst, AVPacket* src);

int av_packet_copy_props(AVPacket* dst, const AVPacket* src);

void av_packet_rescale_ts(AVPacket* pkt, AVRational tb_src, AVRational tb_dst);

AVCodec* avcodec_find_decoder(enum AVCodecID id);

AVCodec* avcodec_find_decoder_by_name(const char* name);

attribute_deprecated int avcodec_default_get_buffer(AVCodecContext* s, AVFrame* pic);
attribute_deprecated void avcodec_default_release_buffer(AVCodecContext* s, AVFrame* pic);
attribute_deprecated int avcodec_default_reget_buffer(AVCodecContext* s, AVFrame* pic);

int avcodec_default_get_buffer2(AVCodecContext* s, AVFrame* frame, int flags);

attribute_deprecated
unsigned avcodec_get_edge_width(void);

void avcodec_align_dimensions(AVCodecContext* s, int* width, int* height);

void avcodec_align_dimensions2(AVCodecContext* s, int* width, int* height, int linesize_align[AV_NUM_DATA_POINTERS]);

int avcodec_enum_to_chroma_pos(int* xpos, int* ypos, enum AVChromaLocation pos);

enum AVChromaLocation avcodec_chroma_pos_to_enum(int xpos, int ypos);

attribute_deprecated int avcodec_decode_audio3(AVCodecContext* avctx, int16_t* samples, int* frame_size_ptr, AVPacket* avpkt);

int avcodec_decode_audio4(AVCodecContext* avctx, AVFrame* frame, int* got_frame_ptr, const AVPacket* avpkt);

int avcodec_decode_video2(AVCodecContext* avctx, AVFrame* picture, int* got_picture_ptr, const AVPacket* avpkt);

int avcodec_decode_subtitle2(AVCodecContext* avctx, AVSubtitle* sub, int* got_sub_ptr, AVPacket* avpkt);

enum AVPictureStructure {
    AV_PICTURE_STRUCTURE_UNKNOWN,
    AV_PICTURE_STRUCTURE_TOP_FIELD,
    AV_PICTURE_STRUCTURE_BOTTOM_FIELD,
    AV_PICTURE_STRUCTURE_FRAME,
};

typedef struct AVCodecParserContext {
    void* priv_data;
    struct AVCodecParser* parser;
    int64_t frame_offset;
    int64_t cur_offset;

    int64_t next_frame_offset;

    int pict_type;

    int repeat_pict;
    int64_t pts;
    int64_t dts;

    int64_t last_pts;
    int64_t last_dts;
    int fetch_timestamp;

#define AV_PARSER_PTS_NB 4
    int cur_frame_start_index;
    int64_t cur_frame_offset[AV_PARSER_PTS_NB];
    int64_t cur_frame_pts[AV_PARSER_PTS_NB];
    int64_t cur_frame_dts[AV_PARSER_PTS_NB];

    int flags;
#define PARSER_FLAG_COMPLETE_FRAMES           0x0001
#define PARSER_FLAG_ONCE                      0x0002

#define PARSER_FLAG_FETCHED_OFFSET            0x0004
#define PARSER_FLAG_USE_CODEC_TS              0x1000

    int64_t offset;
    int64_t cur_frame_end[AV_PARSER_PTS_NB];

    int key_frame;

    int64_t convergence_duration;

    int dts_sync_point;

    int dts_ref_dts_delta;

    int pts_dts_delta;

    int64_t cur_frame_pos[AV_PARSER_PTS_NB];

    int64_t pos;

    int64_t last_pos;

    int duration;

    enum AVFieldOrder field_order;

    enum AVPictureStructure picture_structure;

    int output_picture_number;
} AVCodecParserContext;

typedef struct AVCodecParser {
    int codec_ids[5];
    int priv_data_size;
    int (* parser_init)(AVCodecParserContext* s);
    int (* parser_parse)(AVCodecParserContext* s, AVCodecContext* avctx, const uint8_t** poutbuf, int* poutbuf_size, const uint8_t* buf,
                         int buf_size);
    void (* parser_close)(AVCodecParserContext* s);
    int (* split)(AVCodecContext* avctx, const uint8_t* buf, int buf_size);
    struct AVCodecParser* next;
} AVCodecParser;

AVCodecParser* av_parser_next(AVCodecParser* c);

void av_register_codec_parser(AVCodecParser* parser);
AVCodecParserContext* av_parser_init(int codec_id);

int av_parser_parse2(AVCodecParserContext* s, AVCodecContext* avctx, uint8_t** poutbuf, int* poutbuf_size, const uint8_t* buf, int buf_size,
                     int64_t pts, int64_t dts, int64_t pos);

int av_parser_change(AVCodecParserContext* s, AVCodecContext* avctx, uint8_t** poutbuf, int* poutbuf_size, const uint8_t* buf, int buf_size,
                     int keyframe);
void av_parser_close(AVCodecParserContext* s);

AVCodec* avcodec_find_encoder(enum AVCodecID id);

AVCodec* avcodec_find_encoder_by_name(const char* name);

int attribute_deprecated avcodec_encode_audio(AVCodecContext* avctx, uint8_t* buf, int buf_size, const short* samples);

int avcodec_encode_audio2(AVCodecContext* avctx, AVPacket* avpkt, const AVFrame* frame, int* got_packet_ptr);

attribute_deprecated
int avcodec_encode_video(AVCodecContext* avctx, uint8_t* buf, int buf_size, const AVFrame* pict);

int avcodec_encode_video2(AVCodecContext* avctx, AVPacket* avpkt, const AVFrame* frame, int* got_packet_ptr);

int avcodec_encode_subtitle(AVCodecContext* avctx, uint8_t* buf, int buf_size, const AVSubtitle* sub);

struct ReSampleContext;
struct AVResampleContext;

typedef struct ReSampleContext ReSampleContext;

attribute_deprecated
ReSampleContext* av_audio_resample_init(int output_channels, int input_channels, int output_rate, int input_rate,
                                        enum AVSampleFormat sample_fmt_out, enum AVSampleFormat sample_fmt_in, int filter_length,
                                        int log2_phase_count, int linear, double cutoff);

attribute_deprecated
int audio_resample(ReSampleContext* s, short* output, short* input, int nb_samples);

attribute_deprecated
void audio_resample_close(ReSampleContext* s);

attribute_deprecated
struct AVResampleContext* av_resample_init(int out_rate, int in_rate, int filter_length, int log2_phase_count, int linear, double cutoff);

attribute_deprecated
int av_resample(struct AVResampleContext* c, short* dst, short* src, int* consumed, int src_size, int dst_size, int update_ctx);

attribute_deprecated
void av_resample_compensate(struct AVResampleContext* c, int sample_delta, int compensation_distance);
attribute_deprecated
void av_resample_close(struct AVResampleContext* c);

int avpicture_alloc(AVPicture* picture, enum AVPixelFormat pix_fmt, int width, int height);

void avpicture_free(AVPicture* picture);

int avpicture_fill(AVPicture* picture, const uint8_t* ptr, enum AVPixelFormat pix_fmt, int width, int height);

int avpicture_layout(const AVPicture* src, enum AVPixelFormat pix_fmt, int width, int height, unsigned char* dest, int dest_size);

int avpicture_get_size(enum AVPixelFormat pix_fmt, int width, int height);

attribute_deprecated
int avpicture_deinterlace(AVPicture* dst, const AVPicture* src, enum AVPixelFormat pix_fmt, int width, int height);

void av_picture_copy(AVPicture* dst, const AVPicture* src, enum AVPixelFormat pix_fmt, int width, int height);

int av_picture_crop(AVPicture* dst, const AVPicture* src, enum AVPixelFormat pix_fmt, int top_band, int left_band);

int av_picture_pad(AVPicture* dst, const AVPicture* src, int height, int width, enum AVPixelFormat pix_fmt, int padtop, int padbottom,
                   int padleft, int padright, int* color);

void avcodec_get_chroma_sub_sample(enum AVPixelFormat pix_fmt, int* h_shift, int* v_shift);

unsigned int avcodec_pix_fmt_to_codec_tag(enum AVPixelFormat pix_fmt);

int avcodec_get_pix_fmt_loss(enum AVPixelFormat dst_pix_fmt, enum AVPixelFormat src_pix_fmt, int has_alpha);

enum AVPixelFormat avcodec_find_best_pix_fmt_of_list(const enum AVPixelFormat* pix_fmt_list, enum AVPixelFormat src_pix_fmt, int has_alpha,
                                                     int* loss_ptr);

enum AVPixelFormat avcodec_find_best_pix_fmt_of_2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
                                                  enum AVPixelFormat src_pix_fmt, int has_alpha, int* loss_ptr);

attribute_deprecated
enum AVPixelFormat avcodec_find_best_pix_fmt2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
                                              enum AVPixelFormat src_pix_fmt, int has_alpha, int* loss_ptr);

enum AVPixelFormat avcodec_default_get_format(struct AVCodecContext* s, const enum AVPixelFormat* fmt);

attribute_deprecated
void avcodec_set_dimensions(AVCodecContext* s, int width, int height);

size_t av_get_codec_tag_string(char* buf, size_t buf_size, unsigned int codec_tag);

void avcodec_string(char* buf, int buf_size, AVCodecContext* enc, int encode);

const char* av_get_profile_name(const AVCodec* codec, int profile);

int avcodec_default_execute(AVCodecContext* c, int (*func)(AVCodecContext* c2, void* arg2), void* arg, int* ret, int count, int size);
int avcodec_default_execute2(AVCodecContext* c, int (*func)(AVCodecContext* c2, void* arg2, int, int), void* arg, int* ret, int count);

int avcodec_fill_audio_frame(AVFrame* frame, int nb_channels, enum AVSampleFormat sample_fmt, const uint8_t* buf, int buf_size, int align);

void avcodec_flush_buffers(AVCodecContext* avctx);

int av_get_bits_per_sample(enum AVCodecID codec_id);

enum AVCodecID av_get_pcm_codec(enum AVSampleFormat fmt, int be);

int av_get_exact_bits_per_sample(enum AVCodecID codec_id);

int av_get_audio_frame_duration(AVCodecContext* avctx, int frame_bytes);

typedef struct AVBitStreamFilterContext {
    void* priv_data;
    struct AVBitStreamFilter* filter;
    AVCodecParserContext* parser;
    struct AVBitStreamFilterContext* next;
} AVBitStreamFilterContext;

typedef struct AVBitStreamFilter {
    const char* name;
    int priv_data_size;
    int (* filter)(AVBitStreamFilterContext* bsfc, AVCodecContext* avctx, const char* args, uint8_t** poutbuf, int* poutbuf_size,
                   const uint8_t* buf, int buf_size, int keyframe);
    void (* close)(AVBitStreamFilterContext* bsfc);
    struct AVBitStreamFilter* next;
} AVBitStreamFilter;

void av_register_bitstream_filter(AVBitStreamFilter* bsf);

AVBitStreamFilterContext* av_bitstream_filter_init(const char* name);

int av_bitstream_filter_filter(AVBitStreamFilterContext* bsfc, AVCodecContext* avctx, const char* args, uint8_t** poutbuf,
                               int* poutbuf_size, const uint8_t* buf, int buf_size, int keyframe);

void av_bitstream_filter_close(AVBitStreamFilterContext* bsf);

AVBitStreamFilter* av_bitstream_filter_next(AVBitStreamFilter* f);

void av_fast_padded_malloc(void* ptr, unsigned int* size, size_t min_size);

void av_fast_padded_mallocz(void* ptr, unsigned int* size, size_t min_size);

unsigned int av_xiphlacing(unsigned char* s, unsigned int v);

attribute_deprecated
void av_log_missing_feature(void* avc, const char* feature, int want_sample);

attribute_deprecated
void av_log_ask_for_sample(void* avc, const char* msg, ...) av_printf_format(2, 3);

void av_register_hwaccel(AVHWAccel* hwaccel);

AVHWAccel* av_hwaccel_next(AVHWAccel* hwaccel);

enum AVLockOp {
    AV_LOCK_CREATE,
    AV_LOCK_OBTAIN,
    AV_LOCK_RELEASE,
    AV_LOCK_DESTROY,
};

int av_lockmgr_register(int (*cb)(void** mutex, enum AVLockOp op));

enum AVMediaType avcodec_get_type(enum AVCodecID codec_id);

const char* avcodec_get_name(enum AVCodecID id);

int avcodec_is_open(AVCodecContext* s);

int av_codec_is_encoder(const AVCodec* codec);

int av_codec_is_decoder(const AVCodec* codec);

const AVCodecDescriptor* avcodec_descriptor_get(enum AVCodecID id);

const AVCodecDescriptor* avcodec_descriptor_next(const AVCodecDescriptor* prev);

const AVCodecDescriptor* avcodec_descriptor_get_by_name(const char* name);

#define AVFORMAT_AVFORMAT_H

#define AVFORMAT_AVIO_H

#define AVFORMAT_VERSION_H

#define LIBAVFORMAT_VERSION_MAJOR 55
#define LIBAVFORMAT_VERSION_MINOR 48
#define LIBAVFORMAT_VERSION_MICRO 100

#define LIBAVFORMAT_VERSION_INT AV_VERSION_INT(LIBAVFORMAT_VERSION_MAJOR, \
                                               LIBAVFORMAT_VERSION_MINOR, \
                                               LIBAVFORMAT_VERSION_MICRO)
#define LIBAVFORMAT_VERSION     AV_VERSION(LIBAVFORMAT_VERSION_MAJOR,   \
                                           LIBAVFORMAT_VERSION_MINOR,   \
                                           LIBAVFORMAT_VERSION_MICRO)
#define LIBAVFORMAT_BUILD       LIBAVFORMAT_VERSION_INT

#define LIBAVFORMAT_IDENT       "Lavf" AV_STRINGIFY(LIBAVFORMAT_VERSION)

#define FF_API_REFERENCE_DTS            (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_LAVF_BITEXACT            (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_LAVF_FRAC                (LIBAVFORMAT_VERSION_MAJOR < 57)
#define FF_API_LAVF_CODEC_TB            (LIBAVFORMAT_VERSION_MAJOR < 57)

#define FF_API_ALLOC_OUTPUT_CONTEXT    (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_FORMAT_PARAMETERS       (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_NEW_STREAM              (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_SET_PTS_INFO            (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_CLOSE_INPUT_FILE        (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_READ_PACKET             (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_ASS_SSA                 (LIBAVFORMAT_VERSION_MAJOR < 56)
#define FF_API_R_FRAME_RATE            1

#define AVIO_SEEKABLE_NORMAL 0x0001

typedef struct AVIOInterruptCB {
    int (* callback)(void*);
    void* opaque;
} AVIOInterruptCB;

typedef struct AVIOContext {
    const AVClass* av_class;
    unsigned char* buffer;
    int buffer_size;
    unsigned char* buf_ptr;
    unsigned char* buf_end;

    void* opaque;

    int (* read_packet)(void* opaque, uint8_t* buf, int buf_size);
    int (* write_packet)(void* opaque, uint8_t* buf, int buf_size);
    int64_t (* seek)(void* opaque, int64_t offset, int whence);
    int64_t pos;
    int must_flush;
    int eof_reached;
    int write_flag;
    int max_packet_size;
    unsigned long checksum;
    unsigned char* checksum_ptr;
    unsigned long (* update_checksum)(unsigned long checksum, const uint8_t* buf, unsigned int size);
    int error;

    int (* read_pause)(void* opaque, int pause);

    int64_t (* read_seek)(void* opaque, int stream_index, int64_t timestamp, int flags);

    int seekable;

    int64_t maxsize;

    int direct;

    int64_t bytes_read;

    int seek_count;

    int writeout_count;

    int orig_buffer_size;
} AVIOContext;

const char* avio_find_protocol_name(const char* url);

int avio_check(const char* url, int flags);

AVIOContext* avio_alloc_context(
    unsigned char* buffer, int buffer_size, int write_flag, void* opaque, int (*read_packet)(void* opaque, uint8_t* buf,
                                                                                             int buf_size), int (*write_packet)(
        void* opaque, uint8_t* buf, int buf_size), int64_t (*seek)(void* opaque, int64_t offset, int whence));

void avio_w8(AVIOContext* s, int b);
void avio_write(AVIOContext* s, const unsigned char* buf, int size);
void avio_wl64(AVIOContext* s, uint64_t val);
void avio_wb64(AVIOContext* s, uint64_t val);
void avio_wl32(AVIOContext* s, unsigned int val);
void avio_wb32(AVIOContext* s, unsigned int val);
void avio_wl24(AVIOContext* s, unsigned int val);
void avio_wb24(AVIOContext* s, unsigned int val);
void avio_wl16(AVIOContext* s, unsigned int val);
void avio_wb16(AVIOContext* s, unsigned int val);

int avio_put_str(AVIOContext* s, const char* str);

int avio_put_str16le(AVIOContext* s, const char* str);

#define AVSEEK_SIZE 0x10000

#define AVSEEK_FORCE 0x20000

int64_t avio_seek(AVIOContext* s, int64_t offset, int whence);

int64_t avio_skip(AVIOContext* s, int64_t offset);

static av_always_inline int64_t avio_tell(AVIOContext* s)
{
    return avio_seek(s, 0, SEEK_CUR);
}

int64_t avio_size(AVIOContext* s);

int url_feof(AVIOContext* s);

int avio_printf(AVIOContext* s, const char* fmt, ...) av_printf_format(2, 3);

void avio_flush(AVIOContext* s);

int avio_read(AVIOContext* s, unsigned char* buf, int size);

int          avio_r8(AVIOContext* s);
unsigned int avio_rl16(AVIOContext* s);
unsigned int avio_rl24(AVIOContext* s);
unsigned int avio_rl32(AVIOContext* s);
uint64_t     avio_rl64(AVIOContext* s);
unsigned int avio_rb16(AVIOContext* s);
unsigned int avio_rb24(AVIOContext* s);
unsigned int avio_rb32(AVIOContext* s);
uint64_t     avio_rb64(AVIOContext* s);

int avio_get_str(AVIOContext* pb, int maxlen, char* buf, int buflen);

int avio_get_str16le(AVIOContext* pb, int maxlen, char* buf, int buflen);
int avio_get_str16be(AVIOContext* pb, int maxlen, char* buf, int buflen);

#define AVIO_FLAG_READ  1
#define AVIO_FLAG_WRITE 2
#define AVIO_FLAG_READ_WRITE (AVIO_FLAG_READ | AVIO_FLAG_WRITE)

#define AVIO_FLAG_NONBLOCK 8

#define AVIO_FLAG_DIRECT 0x8000

int avio_open(AVIOContext** s, const char* url, int flags);

int avio_open2(AVIOContext** s, const char* url, int flags, const AVIOInterruptCB* int_cb, AVDictionary** options);

int avio_close(AVIOContext* s);

int avio_closep(AVIOContext** s);

int avio_open_dyn_buf(AVIOContext** s);

int avio_close_dyn_buf(AVIOContext* s, uint8_t** pbuffer);

const char* avio_enum_protocols(void** opaque, int output);

int     avio_pause(AVIOContext* h, int pause);

int64_t avio_seek_time(AVIOContext* h, int stream_index, int64_t timestamp, int flags);

struct AVFormatContext;

struct AVDeviceInfoList;
struct AVDeviceCapabilitiesQuery;

int av_get_packet(AVIOContext* s, AVPacket* pkt, int size);

int av_append_packet(AVIOContext* s, AVPacket* pkt, int size);

typedef struct AVFrac {
    int64_t val, num, den;
} AVFrac;

struct AVCodecTag;

typedef struct AVProbeData {
    const char* filename;
    unsigned char* buf;
    int buf_size;
} AVProbeData;

#define AVPROBE_SCORE_RETRY (AVPROBE_SCORE_MAX / 4)
#define AVPROBE_SCORE_STREAM_RETRY (AVPROBE_SCORE_MAX / 4 - 1)

#define AVPROBE_SCORE_EXTENSION  50
#define AVPROBE_SCORE_MAX       100

#define AVPROBE_PADDING_SIZE 32

#define AVFMT_NOFILE        0x0001
#define AVFMT_NEEDNUMBER    0x0002
#define AVFMT_SHOW_IDS      0x0008
#define AVFMT_RAWPICTURE    0x0020

#define AVFMT_GLOBALHEADER  0x0040
#define AVFMT_NOTIMESTAMPS  0x0080
#define AVFMT_GENERIC_INDEX 0x0100
#define AVFMT_TS_DISCONT    0x0200
#define AVFMT_VARIABLE_FPS  0x0400
#define AVFMT_NODIMENSIONS  0x0800
#define AVFMT_NOSTREAMS     0x1000
#define AVFMT_NOBINSEARCH   0x2000
#define AVFMT_NOGENSEARCH   0x4000
#define AVFMT_NO_BYTE_SEEK  0x8000
#define AVFMT_ALLOW_FLUSH  0x10000
#define AVFMT_TS_NONSTRICT 0x20000

#define AVFMT_TS_NEGATIVE  0x40000

#define AVFMT_SEEK_TO_PTS   0x4000000

typedef struct AVOutputFormat {
    const char* name;

    const char* long_name;
    const char* mime_type;
    const char* extensions;

    enum AVCodecID audio_codec;
    enum AVCodecID video_codec;
    enum AVCodecID subtitle_codec;

    int flags;

    const struct AVCodecTag* const* codec_tag;

    const AVClass* priv_class;

    struct AVOutputFormat* next;

    int priv_data_size;

    int (* write_header)(struct AVFormatContext*);

    int (* write_packet)(struct AVFormatContext*, AVPacket* pkt);
    int (* write_trailer)(struct AVFormatContext*);

    int (* interleave_packet)(struct AVFormatContext*, AVPacket* out, AVPacket* in, int flush);

    int (* query_codec)(enum AVCodecID id, int std_compliance);

    void (* get_output_timestamp)(struct AVFormatContext* s, int stream, int64_t* dts, int64_t* wall);

    int (* control_message)(struct AVFormatContext* s, int type, void* data, size_t data_size);

    int (* write_uncoded_frame)(struct AVFormatContext*, int stream_index, AVFrame** frame, unsigned flags);

    int (* get_device_list)(struct AVFormatContext* s, struct AVDeviceInfoList* device_list);

    int (* create_device_capabilities)(struct AVFormatContext* s, struct AVDeviceCapabilitiesQuery* caps);

    int (* free_device_capabilities)(struct AVFormatContext* s, struct AVDeviceCapabilitiesQuery* caps);
} AVOutputFormat;

typedef struct AVInputFormat {
    const char* name;

    const char* long_name;

    int flags;

    const char* extensions;

    const struct AVCodecTag* const* codec_tag;

    const AVClass* priv_class;

    struct AVInputFormat* next;

    int raw_codec_id;

    int priv_data_size;

    int (* read_probe)(AVProbeData*);

    int (* read_header)(struct AVFormatContext*);

    int (* read_packet)(struct AVFormatContext*, AVPacket* pkt);

    int (* read_close)(struct AVFormatContext*);

    int (* read_seek)(struct AVFormatContext*, int stream_index, int64_t timestamp, int flags);

    int64_t (* read_timestamp)(struct AVFormatContext* s, int stream_index, int64_t* pos, int64_t pos_limit);

    int (* read_play)(struct AVFormatContext*);

    int (* read_pause)(struct AVFormatContext*);

    int (* read_seek2)(struct AVFormatContext* s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);

    int (* get_device_list)(struct AVFormatContext* s, struct AVDeviceInfoList* device_list);

    int (* create_device_capabilities)(struct AVFormatContext* s, struct AVDeviceCapabilitiesQuery* caps);

    int (* free_device_capabilities)(struct AVFormatContext* s, struct AVDeviceCapabilitiesQuery* caps);
} AVInputFormat;

enum AVStreamParseType {
    AVSTREAM_PARSE_NONE,
    AVSTREAM_PARSE_FULL,
    AVSTREAM_PARSE_HEADERS,
    AVSTREAM_PARSE_TIMESTAMPS,
    AVSTREAM_PARSE_FULL_ONCE,
    AVSTREAM_PARSE_FULL_RAW=MKTAG(0, 'R', 'A', 'W'),
};

typedef struct AVIndexEntry {
    int64_t pos;
    int64_t timestamp;

#define AVINDEX_KEYFRAME 0x0001
    int flags : 2;
    int size : 30;
    int min_distance;
} AVIndexEntry;

#define AV_DISPOSITION_DEFAULT   0x0001
#define AV_DISPOSITION_DUB       0x0002
#define AV_DISPOSITION_ORIGINAL  0x0004
#define AV_DISPOSITION_COMMENT   0x0008
#define AV_DISPOSITION_LYRICS    0x0010
#define AV_DISPOSITION_KARAOKE   0x0020

#define AV_DISPOSITION_FORCED    0x0040
#define AV_DISPOSITION_HEARING_IMPAIRED  0x0080
#define AV_DISPOSITION_VISUAL_IMPAIRED   0x0100
#define AV_DISPOSITION_CLEAN_EFFECTS     0x0200

#define AV_DISPOSITION_ATTACHED_PIC      0x0400

#define AV_DISPOSITION_CAPTIONS     0x10000
#define AV_DISPOSITION_DESCRIPTIONS 0x20000
#define AV_DISPOSITION_METADATA     0x40000

#define AV_PTS_WRAP_IGNORE      0
#define AV_PTS_WRAP_ADD_OFFSET  1
#define AV_PTS_WRAP_SUB_OFFSET  -1

typedef struct AVStream {
    int index;

    int id;

    AVCodecContext* codec;
    void* priv_data;

    attribute_deprecated
    struct AVFrac pts;

    AVRational time_base;

    int64_t start_time;

    int64_t duration;

    int64_t nb_frames;

    int disposition;

    enum AVDiscard discard;

    AVRational sample_aspect_ratio;

    AVDictionary* metadata;

    AVRational avg_frame_rate;

    AVPacket attached_pic;

    AVPacketSideData* side_data;

    int nb_side_data;

#define MAX_STD_TIMEBASES (60 * 12 + 6)
    struct {
        int64_t last_dts;
        int64_t duration_gcd;
        int duration_count;
        int64_t rfps_duration_sum;
        double (*duration_error)[2][MAX_STD_TIMEBASES];
        int64_t codec_info_duration;
        int64_t codec_info_duration_fields;

        int found_decoder;

        int64_t last_duration;

        int64_t fps_first_dts;
        int fps_first_dts_idx;
        int64_t fps_last_dts;
        int fps_last_dts_idx;
    }* info;

    int pts_wrap_bits;

    int64_t do_not_use;

    int64_t first_dts;
    int64_t cur_dts;
    int64_t last_IP_pts;
    int last_IP_duration;

#define MAX_PROBE_PACKETS 2500
    int probe_packets;

    int codec_info_nb_frames;

    enum AVStreamParseType need_parsing;
    struct AVCodecParserContext* parser;

    struct AVPacketList* last_in_packet_buffer;
    AVProbeData probe_data;
#define MAX_REORDER_DELAY 16
    int64_t pts_buffer[MAX_REORDER_DELAY + 1];

    AVIndexEntry* index_entries;

    int nb_index_entries;
    unsigned int index_entries_allocated_size;

    AVRational r_frame_rate;

    int stream_identifier;

    int64_t interleaver_chunk_size;
    int64_t interleaver_chunk_duration;

    int request_probe;

    int skip_to_keyframe;

    int skip_samples;

    int nb_decoded_frames;

    int64_t mux_ts_offset;

    int64_t pts_wrap_reference;

    int pts_wrap_behavior;

    int update_initial_durations_done;

    int64_t pts_reorder_error[MAX_REORDER_DELAY + 1];
    uint8_t pts_reorder_error_count[MAX_REORDER_DELAY + 1];

    int64_t last_dts_for_order_check;
    uint8_t dts_ordered;
    uint8_t dts_misordered;

    int inject_global_side_data;
} AVStream;

AVRational av_stream_get_r_frame_rate(const AVStream* s);
void       av_stream_set_r_frame_rate(AVStream* s, AVRational r);
struct AVCodecParserContext* av_stream_get_parser(const AVStream* s);

int64_t    av_stream_get_end_pts(const AVStream* st);

#define AV_PROGRAM_RUNNING 1

typedef struct AVProgram {
    int id;
    int flags;
    enum AVDiscard discard;
    unsigned int* stream_index;
    unsigned int nb_stream_indexes;
    AVDictionary* metadata;

    int program_num;
    int pmt_pid;
    int pcr_pid;

    int64_t start_time;
    int64_t end_time;

    int64_t pts_wrap_reference;
    int pts_wrap_behavior;
} AVProgram;

#define AVFMTCTX_NOHEADER      0x0001

typedef struct AVChapter {
    int id;
    AVRational time_base;
    int64_t start, end;
    AVDictionary* metadata;
} AVChapter;

typedef int (* av_format_control_message)(struct AVFormatContext* s, int type, void* data, size_t data_size);

enum AVDurationEstimationMethod {
    AVFMT_DURATION_FROM_PTS,
    AVFMT_DURATION_FROM_STREAM,
    AVFMT_DURATION_FROM_BITRATE
};

typedef struct AVFormatInternal AVFormatInternal;

typedef struct AVFormatContext {
    const AVClass* av_class;

    struct AVInputFormat* iformat;

    struct AVOutputFormat* oformat;

    void* priv_data;

    AVIOContext* pb;

    int ctx_flags;

    unsigned int nb_streams;

    AVStream** streams;

    char filename[1024];

    int64_t start_time;

    int64_t duration;

    int bit_rate;

    unsigned int packet_size;
    int max_delay;

    int flags;
#define AVFMT_FLAG_GENPTS       0x0001
#define AVFMT_FLAG_IGNIDX       0x0002
#define AVFMT_FLAG_NONBLOCK     0x0004
#define AVFMT_FLAG_IGNDTS       0x0008
#define AVFMT_FLAG_NOFILLIN     0x0010
#define AVFMT_FLAG_NOPARSE      0x0020
#define AVFMT_FLAG_NOBUFFER     0x0040
#define AVFMT_FLAG_CUSTOM_IO    0x0080
#define AVFMT_FLAG_DISCARD_CORRUPT  0x0100
#define AVFMT_FLAG_FLUSH_PACKETS    0x0200

#define AVFMT_FLAG_BITEXACT         0x0400
#define AVFMT_FLAG_MP4A_LATM    0x8000
#define AVFMT_FLAG_SORT_DTS    0x10000
#define AVFMT_FLAG_PRIV_OPT    0x20000
#define AVFMT_FLAG_KEEP_SIDE_DATA 0x40000

    unsigned int probesize;

    attribute_deprecated
    int max_analyze_duration;

    const uint8_t* key;
    int keylen;

    unsigned int nb_programs;
    AVProgram** programs;

    enum AVCodecID video_codec_id;

    enum AVCodecID audio_codec_id;

    enum AVCodecID subtitle_codec_id;

    unsigned int max_index_size;

    unsigned int max_picture_buffer;

    unsigned int nb_chapters;
    AVChapter** chapters;

    AVDictionary* metadata;

    int64_t start_time_realtime;

    int fps_probe_size;

    int error_recognition;

    AVIOInterruptCB interrupt_callback;

    int debug;
#define FF_FDEBUG_TS        0x0001

    int64_t max_interleave_delta;

    int strict_std_compliance;

    int ts_id;

    int audio_preload;

    int max_chunk_duration;

    int max_chunk_size;

    int use_wallclock_as_timestamps;

    int avoid_negative_ts;

    int avio_flags;

    enum AVDurationEstimationMethod duration_estimation_method;

    int64_t skip_initial_bytes;

    unsigned int correct_ts_overflow;

    int seek2any;

    int flush_packets;

    int probe_score;

    int format_probesize;

    struct AVPacketList* packet_buffer;
    struct AVPacketList* packet_buffer_end;

    int64_t data_offset;

    struct AVPacketList* raw_packet_buffer;
    struct AVPacketList* raw_packet_buffer_end;

    struct AVPacketList* parse_queue;
    struct AVPacketList* parse_queue_end;

#define RAW_PACKET_BUFFER_SIZE 2500000
    int raw_packet_buffer_remaining_size;

    int64_t offset;

    AVRational offset_timebase;

    AVFormatInternal* internal;

    int io_repositioned;

    AVCodec* video_codec;

    AVCodec* audio_codec;

    AVCodec* subtitle_codec;

    int metadata_header_padding;

    void* opaque;

    av_format_control_message control_message_cb;

    int64_t output_ts_offset;

    int64_t max_analyze_duration2;
} AVFormatContext;

int av_format_get_probe_score(const AVFormatContext* s);
AVCodec* av_format_get_video_codec(const AVFormatContext* s);
void      av_format_set_video_codec(AVFormatContext* s, AVCodec* c);
AVCodec* av_format_get_audio_codec(const AVFormatContext* s);
void      av_format_set_audio_codec(AVFormatContext* s, AVCodec* c);
AVCodec* av_format_get_subtitle_codec(const AVFormatContext* s);
void      av_format_set_subtitle_codec(AVFormatContext* s, AVCodec* c);
int       av_format_get_metadata_header_padding(const AVFormatContext* s);
void      av_format_set_metadata_header_padding(AVFormatContext* s, int c);
void* av_format_get_opaque(const AVFormatContext* s);
void      av_format_set_opaque(AVFormatContext* s, void* opaque);
av_format_control_message av_format_get_control_message_cb(const AVFormatContext* s);
void      av_format_set_control_message_cb(AVFormatContext* s, av_format_control_message callback);

void av_format_inject_global_side_data(AVFormatContext* s);

enum AVDurationEstimationMethod av_fmt_ctx_get_duration_estimation_method(const AVFormatContext* ctx);

typedef struct AVPacketList {
    AVPacket pkt;
    struct AVPacketList* next;
} AVPacketList;

unsigned avformat_version(void);

const char* avformat_configuration(void);

const char* avformat_license(void);

void av_register_all(void);

void av_register_input_format(AVInputFormat* format);
void av_register_output_format(AVOutputFormat* format);

int avformat_network_init(void);

int avformat_network_deinit(void);

AVInputFormat* av_iformat_next(AVInputFormat* f);

AVOutputFormat* av_oformat_next(AVOutputFormat* f);

AVFormatContext* avformat_alloc_context(void);

void avformat_free_context(AVFormatContext* s);

const AVClass* avformat_get_class(void);

AVStream* avformat_new_stream(AVFormatContext* s, const AVCodec* c);

uint8_t* av_stream_get_side_data(AVStream* stream, enum AVPacketSideDataType type, int* size);

AVProgram* av_new_program(AVFormatContext* s, int id);

attribute_deprecated
AVFormatContext* avformat_alloc_output_context(const char* format, AVOutputFormat* oformat, const char* filename);

int avformat_alloc_output_context2(AVFormatContext** ctx, AVOutputFormat* oformat, const char* format_name, const char* filename);

AVInputFormat* av_find_input_format(const char* short_name);

AVInputFormat* av_probe_input_format(AVProbeData* pd, int is_opened);

AVInputFormat* av_probe_input_format2(AVProbeData* pd, int is_opened, int* score_max);

AVInputFormat* av_probe_input_format3(AVProbeData* pd, int is_opened, int* score_ret);

int av_probe_input_buffer2(AVIOContext* pb, AVInputFormat** fmt, const char* filename, void* logctx, unsigned int offset,
                           unsigned int max_probe_size);

int av_probe_input_buffer(AVIOContext* pb, AVInputFormat** fmt, const char* filename, void* logctx, unsigned int offset,
                          unsigned int max_probe_size);

int avformat_open_input(AVFormatContext** ps, const char* filename, AVInputFormat* fmt, AVDictionary** options);

attribute_deprecated
int av_demuxer_open(AVFormatContext* ic);

attribute_deprecated
int av_find_stream_info(AVFormatContext* ic);

int avformat_find_stream_info(AVFormatContext* ic, AVDictionary** options);

AVProgram* av_find_program_from_stream(AVFormatContext* ic, AVProgram* last, int s);

int av_find_best_stream(AVFormatContext* ic, enum AVMediaType type, int wanted_stream_nb, int related_stream, AVCodec** decoder_ret,
                        int flags);

attribute_deprecated
int av_read_packet(AVFormatContext* s, AVPacket* pkt);

int av_read_frame(AVFormatContext* s, AVPacket* pkt);

int av_seek_frame(AVFormatContext* s, int stream_index, int64_t timestamp, int flags);

int avformat_seek_file(AVFormatContext* s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);

int av_read_play(AVFormatContext* s);

int av_read_pause(AVFormatContext* s);

attribute_deprecated
void av_close_input_file(AVFormatContext* s);

void avformat_close_input(AVFormatContext** s);

attribute_deprecated
AVStream* av_new_stream(AVFormatContext* s, int id);

attribute_deprecated
void av_set_pts_info(AVStream* s, int pts_wrap_bits, unsigned int pts_num, unsigned int pts_den);

#define AVSEEK_FLAG_BACKWARD 1
#define AVSEEK_FLAG_BYTE     2
#define AVSEEK_FLAG_ANY      4
#define AVSEEK_FLAG_FRAME    8

int avformat_write_header(AVFormatContext* s, AVDictionary** options);

int av_write_frame(AVFormatContext* s, AVPacket* pkt);

int av_interleaved_write_frame(AVFormatContext* s, AVPacket* pkt);

int av_write_uncoded_frame(AVFormatContext* s, int stream_index, AVFrame* frame);

int av_interleaved_write_uncoded_frame(AVFormatContext* s, int stream_index, AVFrame* frame);

int av_write_uncoded_frame_query(AVFormatContext* s, int stream_index);

int av_write_trailer(AVFormatContext* s);

AVOutputFormat* av_guess_format(const char* short_name, const char* filename, const char* mime_type);

enum AVCodecID av_guess_codec(AVOutputFormat* fmt, const char* short_name, const char* filename, const char* mime_type,
                              enum AVMediaType type);

int av_get_output_timestamp(struct AVFormatContext* s, int stream, int64_t* dts, int64_t* wall);

void av_hex_dump(FILE* f, const uint8_t* buf, int size);

void av_hex_dump_log(void* avcl, int level, const uint8_t* buf, int size);

void av_pkt_dump2(FILE* f, const AVPacket* pkt, int dump_payload, const AVStream* st);

void av_pkt_dump_log2(void* avcl, int level, const AVPacket* pkt, int dump_payload, const AVStream* st);

enum AVCodecID av_codec_get_id(const struct AVCodecTag* const* tags, unsigned int tag);

unsigned int av_codec_get_tag(const struct AVCodecTag* const* tags, enum AVCodecID id);

int av_codec_get_tag2(const struct AVCodecTag* const* tags, enum AVCodecID id, unsigned int* tag);

int av_find_default_stream_index(AVFormatContext* s);

int av_index_search_timestamp(AVStream* st, int64_t timestamp, int flags);

int av_add_index_entry(AVStream* st, int64_t pos, int64_t timestamp, int size, int distance, int flags);

void av_url_split(char* proto,         int proto_size, char* authorization, int authorization_size, char* hostname,      int hostname_size,
                  int* port_ptr, char* path,          int path_size, const char* url);

void av_dump_format(AVFormatContext* ic, int index, const char* url, int is_output);

int av_get_frame_filename(char* buf, int buf_size, const char* path, int number);

int av_filename_number_test(const char* filename);

int av_sdp_create(AVFormatContext* ac[], int n_files, char* buf, int size);

int av_match_ext(const char* filename, const char* extensions);

int avformat_query_codec(AVOutputFormat* ofmt, enum AVCodecID codec_id, int std_compliance);

const struct AVCodecTag* avformat_get_riff_video_tags(void);

const struct AVCodecTag* avformat_get_riff_audio_tags(void);

const struct AVCodecTag* avformat_get_mov_video_tags(void);

const struct AVCodecTag* avformat_get_mov_audio_tags(void);

AVRational av_guess_sample_aspect_ratio(AVFormatContext* format, AVStream* stream, AVFrame* frame);

AVRational av_guess_frame_rate(AVFormatContext* ctx, AVStream* stream, AVFrame* frame);

int avformat_match_stream_specifier(AVFormatContext* s, AVStream* st, const char* spec);

int avformat_queue_attached_pictures(AVFormatContext* s);

#define AVUTIL_FIFO_H

typedef struct AVFifoBuffer {
    uint8_t* buffer;
    uint8_t* rptr, * wptr, * end;
    uint32_t rndx, wndx;
} AVFifoBuffer;

AVFifoBuffer* av_fifo_alloc(unsigned int size);

AVFifoBuffer* av_fifo_alloc_array(size_t nmemb, size_t size);

void av_fifo_free(AVFifoBuffer* f);

void av_fifo_freep(AVFifoBuffer** f);

void av_fifo_reset(AVFifoBuffer* f);

int av_fifo_size(FF_CONST_AVUTIL53 AVFifoBuffer* f);

int av_fifo_space(FF_CONST_AVUTIL53 AVFifoBuffer* f);

int av_fifo_generic_read(AVFifoBuffer* f, void* dest, int buf_size, void (*func)(void*, void*, int));

int av_fifo_generic_write(AVFifoBuffer* f, void* src, int size, int (*func)(void*, void*, int));

int av_fifo_realloc2(AVFifoBuffer* f, unsigned int size);

int av_fifo_grow(AVFifoBuffer* f, unsigned int additional_space);

void av_fifo_drain(AVFifoBuffer* f, int size);

static inline uint8_t* av_fifo_peek2(const AVFifoBuffer* f, int offs)
{
    uint8_t* ptr = f->rptr + offs;
    if (ptr >= f->end) {
        ptr = f->buffer + (ptr - f->end);
    } else if (ptr < f->buffer) {
        ptr = f->end - (f->buffer - ptr);
    }
    return ptr;
}
