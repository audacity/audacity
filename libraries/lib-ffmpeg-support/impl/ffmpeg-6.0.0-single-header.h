// This header was generated from the FFMPEG headers
#pragma once

#include <string.h>
#include <math.h>
#include <time.h>
#include <stddef.h>
#include <limits.h>
#include <errno.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>

#define AVCODEC_AVCODEC_H

#define AVUTIL_SAMPLEFMT_H

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
    AV_SAMPLE_FMT_S64,         
    AV_SAMPLE_FMT_S64P,        

    AV_SAMPLE_FMT_NB           
};

const char *av_get_sample_fmt_name(enum AVSampleFormat sample_fmt);

enum AVSampleFormat av_get_sample_fmt(const char *name);

enum AVSampleFormat av_get_alt_sample_fmt(enum AVSampleFormat sample_fmt, int planar);

enum AVSampleFormat av_get_packed_sample_fmt(enum AVSampleFormat sample_fmt);

enum AVSampleFormat av_get_planar_sample_fmt(enum AVSampleFormat sample_fmt);

char *av_get_sample_fmt_string(char *buf, int buf_size, enum AVSampleFormat sample_fmt);

int av_get_bytes_per_sample(enum AVSampleFormat sample_fmt);

int av_sample_fmt_is_planar(enum AVSampleFormat sample_fmt);

int av_samples_get_buffer_size(int *linesize, int nb_channels, int nb_samples,
                               enum AVSampleFormat sample_fmt, int align);

int av_samples_fill_arrays(uint8_t **audio_data, int *linesize,
                           const uint8_t *buf,
                           int nb_channels, int nb_samples,
                           enum AVSampleFormat sample_fmt, int align);

int av_samples_alloc(uint8_t **audio_data, int *linesize, int nb_channels,
                     int nb_samples, enum AVSampleFormat sample_fmt, int align);

int av_samples_alloc_array_and_samples(uint8_t ***audio_data, int *linesize, int nb_channels,
                                       int nb_samples, enum AVSampleFormat sample_fmt, int align);

int av_samples_copy(uint8_t **dst, uint8_t * const *src, int dst_offset,
                    int src_offset, int nb_samples, int nb_channels,
                    enum AVSampleFormat sample_fmt);

int av_samples_set_silence(uint8_t **audio_data, int offset, int nb_samples,
                           int nb_channels, enum AVSampleFormat sample_fmt);

#define AVUTIL_ATTRIBUTES_H

#    define AV_GCC_VERSION_AT_LEAST(x,y) 0
#    define AV_GCC_VERSION_AT_MOST(x,y)  0

#    define AV_HAS_BUILTIN(x) 0

#    define av_always_inline inline

#    define av_extern_inline inline

#    define av_warn_unused_result

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

#define AVUTIL_AVUTIL_H

unsigned avutil_version(void);

const char *av_version_info(void);

const char *avutil_configuration(void);

const char *avutil_license(void);

enum AVMediaType {
    AVMEDIA_TYPE_UNKNOWN = -1,  
    AVMEDIA_TYPE_VIDEO,
    AVMEDIA_TYPE_AUDIO,
    AVMEDIA_TYPE_DATA,          
    AVMEDIA_TYPE_SUBTITLE,
    AVMEDIA_TYPE_ATTACHMENT,    
    AVMEDIA_TYPE_NB
};

const char *av_get_media_type_string(enum AVMediaType media_type);

#define FF_LAMBDA_SHIFT 7
#define FF_LAMBDA_SCALE (1<<FF_LAMBDA_SHIFT)
#define FF_QP2LAMBDA 118 
#define FF_LAMBDA_MAX (256*128-1)

#define FF_QUALITY_SCALE FF_LAMBDA_SCALE 

#define AV_NOPTS_VALUE          ((int64_t)UINT64_C(0x8000000000000000))

#define AV_TIME_BASE            1000000

#define AV_TIME_BASE_Q          (AVRational){1, AV_TIME_BASE}

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

#define AVUTIL_MACROS_H

#   define AV_NE(be, le) (le)

#define FFDIFFSIGN(x,y) (((x)>(y)) - ((x)<(y)))

#define FFMAX(a,b) ((a) > (b) ? (a) : (b))
#define FFMAX3(a,b,c) FFMAX(FFMAX(a,b),c)
#define FFMIN(a,b) ((a) > (b) ? (b) : (a))
#define FFMIN3(a,b,c) FFMIN(FFMIN(a,b),c)

#define FFSWAP(type,a,b) do{type SWAP_tmp= b; b= a; a= SWAP_tmp;}while(0)
#define FF_ARRAY_ELEMS(a) (sizeof(a) / sizeof((a)[0]))

#define MKTAG(a,b,c,d)   ((a) | ((b) << 8) | ((c) << 16) | ((unsigned)(d) << 24))
#define MKBETAG(a,b,c,d) ((d) | ((c) << 8) | ((b) << 16) | ((unsigned)(a) << 24))

#define AV_STRINGIFY(s)         AV_TOSTRING(s)
#define AV_TOSTRING(s) #s

#define AV_GLUE(a, b) a ## b
#define AV_JOIN(a, b) AV_GLUE(a, b)

#define AV_PRAGMA(s) _Pragma(#s)

#define FFALIGN(x, a) (((x)+(a)-1)&~((a)-1))

#define RSHIFT(a,b) ((a) > 0 ? ((a) + ((1<<(b))>>1))>>(b) : ((a) + ((1<<(b))>>1)-1)>>(b))

#define ROUNDED_DIV(a,b) (((a)>=0 ? (a) + ((b)>>1) : (a) - ((b)>>1))/(b))

#define AV_CEIL_RSHIFT(a,b) (!av_builtin_constant_p(b) ? -((-(a)) >> (b)) \
                                                       : ((a) + (1<<(b)) - 1) >> (b))

#define FF_CEIL_RSHIFT AV_CEIL_RSHIFT

#define FFUDIV(a,b) (((a)>0 ?(a):(a)-(b)+1) / (b))
#define FFUMOD(a,b) ((a)-(b)*FFUDIV(a,b))

#define FFABS(a) ((a) >= 0 ? (a) : (-(a)))
#define FFSIGN(a) ((a) > 0 ? 1 : -1)

#define FFNABS(a) ((a) <= 0 ? (a) : (-(a)))

#define FFABSU(a) ((a) <= 0 ? -(unsigned)(a) : (unsigned)(a))
#define FFABS64U(a) ((a) <= 0 ? -(uint64_t)(a) : (uint64_t)(a))

#   define av_ceil_log2     av_ceil_log2_c
#   define av_clip          av_clip_c
#   define av_clip64        av_clip64_c
#   define av_clip_uint8    av_clip_uint8_c
#   define av_clip_int8     av_clip_int8_c
#   define av_clip_uint16   av_clip_uint16_c
#   define av_clip_int16    av_clip_int16_c
#   define av_clipl_int32   av_clipl_int32_c
#   define av_clip_intp2    av_clip_intp2_c
#   define av_clip_uintp2   av_clip_uintp2_c
#   define av_mod_uintp2    av_mod_uintp2_c
#   define av_sat_add32     av_sat_add32_c
#   define av_sat_dadd32    av_sat_dadd32_c
#   define av_sat_sub32     av_sat_sub32_c
#   define av_sat_dsub32    av_sat_dsub32_c
#   define av_sat_add64     av_sat_add64_c
#   define av_sat_sub64     av_sat_sub64_c
#   define av_clipf         av_clipf_c
#   define av_clipd         av_clipd_c
#   define av_popcount      av_popcount_c
#   define av_popcount64    av_popcount64_c
#   define av_parity        av_parity_c

av_const int av_log2(unsigned v);

av_const int av_log2_16bit(unsigned v);

static av_always_inline av_const int av_clip_c(int a, int amin, int amax)
{
    if      (a < amin) return amin;
    else if (a > amax) return amax;
    else               return a;
}

static av_always_inline av_const int64_t av_clip64_c(int64_t a, int64_t amin, int64_t amax)
{
    if      (a < amin) return amin;
    else if (a > amax) return amax;
    else               return a;
}

static av_always_inline av_const uint8_t av_clip_uint8_c(int a)
{
    if (a&(~0xFF)) return (~a)>>31;
    else           return a;
}

static av_always_inline av_const int8_t av_clip_int8_c(int a)
{
    if ((a+0x80U) & ~0xFF) return (a>>31) ^ 0x7F;
    else                  return a;
}

static av_always_inline av_const uint16_t av_clip_uint16_c(int a)
{
    if (a&(~0xFFFF)) return (~a)>>31;
    else             return a;
}

static av_always_inline av_const int16_t av_clip_int16_c(int a)
{
    if ((a+0x8000U) & ~0xFFFF) return (a>>31) ^ 0x7FFF;
    else                      return a;
}

static av_always_inline av_const int32_t av_clipl_int32_c(int64_t a)
{
    if ((a+0x80000000u) & ~UINT64_C(0xFFFFFFFF)) return (int32_t)((a>>63) ^ 0x7FFFFFFF);
    else                                         return (int32_t)a;
}

static av_always_inline av_const int av_clip_intp2_c(int a, int p)
{
    if (((unsigned)a + (1 << p)) & ~((2 << p) - 1))
        return (a >> 31) ^ ((1 << p) - 1);
    else
        return a;
}

static av_always_inline av_const unsigned av_clip_uintp2_c(int a, int p)
{
    if (a & ~((1<<p) - 1)) return (~a) >> 31 & ((1<<p) - 1);
    else                   return  a;
}

static av_always_inline av_const unsigned av_mod_uintp2_c(unsigned a, unsigned p)
{
    return a & ((1U << p) - 1);
}

static av_always_inline int av_sat_add32_c(int a, int b)
{
    return av_clipl_int32((int64_t)a + b);
}

static av_always_inline int av_sat_dadd32_c(int a, int b)
{
    return av_sat_add32(a, av_sat_add32(b, b));
}

static av_always_inline int av_sat_sub32_c(int a, int b)
{
    return av_clipl_int32((int64_t)a - b);
}

static av_always_inline int av_sat_dsub32_c(int a, int b)
{
    return av_sat_sub32(a, av_sat_add32(b, b));
}

static av_always_inline int64_t av_sat_add64_c(int64_t a, int64_t b) {
    int64_t s = a+(uint64_t)b;
    if ((int64_t)(a^b | ~s^b) >= 0)
        return INT64_MAX ^ (b >> 63);
    return s;
}

static av_always_inline int64_t av_sat_sub64_c(int64_t a, int64_t b) {
    if (b <= 0 && a >= INT64_MAX + b)
        return INT64_MAX;
    if (b >= 0 && a <= INT64_MIN + b)
        return INT64_MIN;
    return a - b;
}

static av_always_inline av_const float av_clipf_c(float a, float amin, float amax)
{
    return FFMIN(FFMAX(a, amin), amax);
}

static av_always_inline av_const double av_clipd_c(double a, double amin, double amax)
{
    return FFMIN(FFMAX(a, amin), amax);
}

static av_always_inline av_const int av_ceil_log2_c(int x)
{
    return av_log2((x - 1U) << 1);
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

static av_always_inline av_const int av_parity_c(uint32_t v)
{
    return av_popcount(v) & 1;
}

#define GET_UTF8(val, GET_BYTE, ERROR)\
    val= (GET_BYTE);\
    {\
        uint32_t top = (val & 128) >> 1;\
        if ((val & 0xc0) == 0x80 || val >= 0xFE)\
            {ERROR}\
        while (val & top) {\
            unsigned int tmp = (GET_BYTE) - 128;\
            if(tmp>>6)\
                {ERROR}\
            val= (val<<6) + tmp;\
            top <<= 5;\
        }\
        val &= (top << 1) - 1;\
    }

#define GET_UTF16(val, GET_16BIT, ERROR)\
    val = (GET_16BIT);\
    {\
        unsigned int hi = val - 0xD800;\
        if (hi < 0x800) {\
            val = (GET_16BIT) - 0xDC00;\
            if (val > 0x3FFU || hi > 0x3FFU)\
                {ERROR}\
            val += (hi<<10) + 0x10000;\
        }\
    }\

#define PUT_UTF8(val, tmp, PUT_BYTE)\
    {\
        int bytes, shift;\
        uint32_t in = val;\
        if (in < 0x80) {\
            tmp = in;\
            PUT_BYTE\
        } else {\
            bytes = (av_log2(in) + 4) / 5;\
            shift = (bytes - 1) * 6;\
            tmp = (256 - (256 >> bytes)) | (in >> shift);\
            PUT_BYTE\
            while (shift >= 6) {\
                shift -= 6;\
                tmp = 0x80 | ((in >> shift) & 0x3f);\
                PUT_BYTE\
            }\
        }\
    }

#define PUT_UTF16(val, tmp, PUT_16BIT)\
    {\
        uint32_t in = val;\
        if (in < 0x10000) {\
            tmp = in;\
            PUT_16BIT\
        } else {\
            tmp = 0xD800 | ((in - 0x10000) >> 10);\
            PUT_16BIT\
            tmp = 0xDC00 | ((in - 0x10000) & 0x3FF);\
            PUT_16BIT\
        }\
    }\

#define AVUTIL_MEM_H

#define AVUTIL_VERSION_H

#define AV_VERSION_INT(a, b, c) ((a)<<16 | (b)<<8 | (c))
#define AV_VERSION_DOT(a, b, c) a ##.## b ##.## c
#define AV_VERSION(a, b, c) AV_VERSION_DOT(a, b, c)

#define AV_VERSION_MAJOR(a) ((a) >> 16)
#define AV_VERSION_MINOR(a) (((a) & 0x00FF00) >> 8)
#define AV_VERSION_MICRO(a) ((a) & 0xFF)

#define LIBAVUTIL_VERSION_MAJOR  58
#define LIBAVUTIL_VERSION_MINOR   2
#define LIBAVUTIL_VERSION_MICRO 100

#define LIBAVUTIL_VERSION_INT   AV_VERSION_INT(LIBAVUTIL_VERSION_MAJOR, \
                                               LIBAVUTIL_VERSION_MINOR, \
                                               LIBAVUTIL_VERSION_MICRO)
#define LIBAVUTIL_VERSION       AV_VERSION(LIBAVUTIL_VERSION_MAJOR,     \
                                           LIBAVUTIL_VERSION_MINOR,     \
                                           LIBAVUTIL_VERSION_MICRO)
#define LIBAVUTIL_BUILD         LIBAVUTIL_VERSION_INT

#define LIBAVUTIL_IDENT         "Lavu" AV_STRINGIFY(LIBAVUTIL_VERSION)

#define FF_API_FIFO_PEEK2               (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_FIFO_OLD_API             (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_XVMC                     (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_OLD_CHANNEL_LAYOUT       (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_AV_FOPEN_UTF8            (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_PKT_DURATION             (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_REORDERED_OPAQUE         (LIBAVUTIL_VERSION_MAJOR < 59)
#define FF_API_FRAME_PICTURE_NUMBER     (LIBAVUTIL_VERSION_MAJOR < 59)

    #define av_malloc_attrib

    #define av_alloc_size(...)

void *av_malloc(size_t size) av_malloc_attrib av_alloc_size(1);

void *av_mallocz(size_t size) av_malloc_attrib av_alloc_size(1);

av_alloc_size(1, 2) void *av_malloc_array(size_t nmemb, size_t size);

void *av_calloc(size_t nmemb, size_t size) av_malloc_attrib av_alloc_size(1, 2);

void *av_realloc(void *ptr, size_t size) av_alloc_size(2);

av_warn_unused_result
int av_reallocp(void *ptr, size_t size);

void *av_realloc_f(void *ptr, size_t nelem, size_t elsize);

av_alloc_size(2, 3) void *av_realloc_array(void *ptr, size_t nmemb, size_t size);

int av_reallocp_array(void *ptr, size_t nmemb, size_t size);

void *av_fast_realloc(void *ptr, unsigned int *size, size_t min_size);

void av_fast_malloc(void *ptr, unsigned int *size, size_t min_size);

void av_fast_mallocz(void *ptr, unsigned int *size, size_t min_size);

void av_free(void *ptr);

void av_freep(void *ptr);

char *av_strdup(const char *s) av_malloc_attrib;

char *av_strndup(const char *s, size_t len) av_malloc_attrib;

void *av_memdup(const void *p, size_t size);

void av_memcpy_backptr(uint8_t *dst, int back, int cnt);

void av_dynarray_add(void *tab_ptr, int *nb_ptr, void *elem);

av_warn_unused_result
int av_dynarray_add_nofree(void *tab_ptr, int *nb_ptr, void *elem);

void *av_dynarray2_add(void **tab_ptr, int *nb_ptr, size_t elem_size,
                       const uint8_t *elem_data);

int av_size_mult(size_t a, size_t b, size_t *r);

void av_max_alloc(size_t max);

#define AVUTIL_ERROR_H

#define AVERROR(e) (e)
#define AVUNERROR(e) (e)

#define FFERRTAG(a, b, c, d) (-(int)MKTAG(a, b, c, d))

#define AVERROR_BSF_NOT_FOUND      FFERRTAG(0xF8,'B','S','F') 
#define AVERROR_BUG                FFERRTAG( 'B','U','G','!') 
#define AVERROR_BUFFER_TOO_SMALL   FFERRTAG( 'B','U','F','S') 
#define AVERROR_DECODER_NOT_FOUND  FFERRTAG(0xF8,'D','E','C') 
#define AVERROR_DEMUXER_NOT_FOUND  FFERRTAG(0xF8,'D','E','M') 
#define AVERROR_ENCODER_NOT_FOUND  FFERRTAG(0xF8,'E','N','C') 
#define AVERROR_EOF                FFERRTAG( 'E','O','F',' ') 
#define AVERROR_EXIT               FFERRTAG( 'E','X','I','T') 
#define AVERROR_EXTERNAL           FFERRTAG( 'E','X','T',' ') 
#define AVERROR_FILTER_NOT_FOUND   FFERRTAG(0xF8,'F','I','L') 
#define AVERROR_INVALIDDATA        FFERRTAG( 'I','N','D','A') 
#define AVERROR_MUXER_NOT_FOUND    FFERRTAG(0xF8,'M','U','X') 
#define AVERROR_OPTION_NOT_FOUND   FFERRTAG(0xF8,'O','P','T') 
#define AVERROR_PATCHWELCOME       FFERRTAG( 'P','A','W','E') 
#define AVERROR_PROTOCOL_NOT_FOUND FFERRTAG(0xF8,'P','R','O') 

#define AVERROR_STREAM_NOT_FOUND   FFERRTAG(0xF8,'S','T','R') 

#define AVERROR_BUG2               FFERRTAG( 'B','U','G',' ')
#define AVERROR_UNKNOWN            FFERRTAG( 'U','N','K','N') 
#define AVERROR_EXPERIMENTAL       (-0x2bb2afa8) 
#define AVERROR_INPUT_CHANGED      (-0x636e6701) 
#define AVERROR_OUTPUT_CHANGED     (-0x636e6702) 

#define AVERROR_HTTP_BAD_REQUEST   FFERRTAG(0xF8,'4','0','0')
#define AVERROR_HTTP_UNAUTHORIZED  FFERRTAG(0xF8,'4','0','1')
#define AVERROR_HTTP_FORBIDDEN     FFERRTAG(0xF8,'4','0','3')
#define AVERROR_HTTP_NOT_FOUND     FFERRTAG(0xF8,'4','0','4')
#define AVERROR_HTTP_OTHER_4XX     FFERRTAG(0xF8,'4','X','X')
#define AVERROR_HTTP_SERVER_ERROR  FFERRTAG(0xF8,'5','X','X')

#define AV_ERROR_MAX_STRING_SIZE 64

int av_strerror(int errnum, char *errbuf, size_t errbuf_size);

static inline char *av_make_error_string(char *errbuf, size_t errbuf_size, int errnum)
{
    av_strerror(errnum, errbuf, errbuf_size);
    return errbuf;
}

#define av_err2str(errnum) \
    av_make_error_string((char[AV_ERROR_MAX_STRING_SIZE]){0}, AV_ERROR_MAX_STRING_SIZE, errnum)

#define AVUTIL_RATIONAL_H

typedef struct AVRational{
    int num; 
    int den; 
} AVRational;

static inline AVRational av_make_q(int num, int den)
{
    AVRational r = { num, den };
    return r;
}

static inline int av_cmp_q(AVRational a, AVRational b){
    const int64_t tmp= a.num * (int64_t)b.den - b.num * (int64_t)a.den;

    if(tmp) return (int)((tmp ^ a.den ^ b.den)>>63)|1;
    else if(b.den && a.den) return 0;
    else if(a.num && b.num) return (a.num>>31) - (b.num>>31);
    else                    return INT_MIN;
}

static inline double av_q2d(AVRational a){
    return a.num / (double) a.den;
}

int av_reduce(int *dst_num, int *dst_den, int64_t num, int64_t den, int64_t max);

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

uint32_t av_q2intfloat(AVRational q);

AVRational av_gcd_q(AVRational a, AVRational b, int max_den, AVRational def);

#define AVUTIL_MATHEMATICS_H

#define AVUTIL_INTFLOAT_H

union av_intfloat32 {
    uint32_t i;
    float    f;
};

union av_intfloat64 {
    uint64_t i;
    double   f;
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

int64_t av_rescale_rnd(int64_t a, int64_t b, int64_t c, enum AVRounding rnd) av_const;

int64_t av_rescale_q(int64_t a, AVRational bq, AVRational cq) av_const;

int64_t av_rescale_q_rnd(int64_t a, AVRational bq, AVRational cq,
                         enum AVRounding rnd) av_const;

int av_compare_ts(int64_t ts_a, AVRational tb_a, int64_t ts_b, AVRational tb_b);

int64_t av_compare_mod(uint64_t a, uint64_t b, uint64_t mod);

int64_t av_rescale_delta(AVRational in_tb, int64_t in_ts,  AVRational fs_tb, int duration, int64_t *last, AVRational out_tb);

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
    AV_CLASS_CATEGORY_NB  
}AVClassCategory;

#define AV_IS_INPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_INPUT))

#define AV_IS_OUTPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_OUTPUT))

struct AVOptionRanges;

typedef struct AVClass {
    
    const char* class_name;

    const char* (*item_name)(void* ctx);

    const struct AVOption *option;

    int version;

    int log_level_offset_offset;

    int parent_log_context_offset;

    AVClassCategory category;

    AVClassCategory (*get_category)(void* ctx);

    int (*query_ranges)(struct AVOptionRanges **, void *obj, const char *key, int flags);

    void* (*child_next)(void *obj, void *prev);

    const struct AVClass* (*child_class_iterate)(void **iter);
} AVClass;

#define AV_LOG_QUIET    -8

#define AV_LOG_PANIC     0

#define AV_LOG_FATAL     8

#define AV_LOG_ERROR    16

#define AV_LOG_WARNING  24

#define AV_LOG_INFO     32

#define AV_LOG_VERBOSE  40

#define AV_LOG_DEBUG    48

#define AV_LOG_TRACE    56

#define AV_LOG_MAX_OFFSET (AV_LOG_TRACE - AV_LOG_QUIET)

#define AV_LOG_C(x) ((x) << 8)

void av_log(void *avcl, int level, const char *fmt, ...) av_printf_format(3, 4);

void av_log_once(void* avcl, int initial_level, int subsequent_level, int *state, const char *fmt, ...) av_printf_format(5, 6);

void av_vlog(void *avcl, int level, const char *fmt, va_list vl);

int av_log_get_level(void);

void av_log_set_level(int level);

void av_log_set_callback(void (*callback)(void*, int, const char*, va_list));

void av_log_default_callback(void *avcl, int level, const char *fmt,
                             va_list vl);

const char* av_default_item_name(void* ctx);
AVClassCategory av_default_get_category(void *ptr);

void av_log_format_line(void *ptr, int level, const char *fmt, va_list vl,
                        char *line, int line_size, int *print_prefix);

int av_log_format_line2(void *ptr, int level, const char *fmt, va_list vl,
                        char *line, int line_size, int *print_prefix);

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

    AV_PIX_FMT_VAAPI,

    AV_PIX_FMT_YUV420P16LE,  
    AV_PIX_FMT_YUV420P16BE,  
    AV_PIX_FMT_YUV422P16LE,  
    AV_PIX_FMT_YUV422P16BE,  
    AV_PIX_FMT_YUV444P16LE,  
    AV_PIX_FMT_YUV444P16BE,  
    AV_PIX_FMT_DXVA2_VLD,    

    AV_PIX_FMT_RGB444LE,  
    AV_PIX_FMT_RGB444BE,  
    AV_PIX_FMT_BGR444LE,  
    AV_PIX_FMT_BGR444BE,  
    AV_PIX_FMT_YA8,       

    AV_PIX_FMT_Y400A = AV_PIX_FMT_YA8, 
    AV_PIX_FMT_GRAY8A= AV_PIX_FMT_YA8, 

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
    AV_PIX_FMT_GBRP,      
    AV_PIX_FMT_GBR24P = AV_PIX_FMT_GBRP, 
    AV_PIX_FMT_GBRP9BE,   
    AV_PIX_FMT_GBRP9LE,   
    AV_PIX_FMT_GBRP10BE,  
    AV_PIX_FMT_GBRP10LE,  
    AV_PIX_FMT_GBRP16BE,  
    AV_PIX_FMT_GBRP16LE,  
    AV_PIX_FMT_YUVA422P,  
    AV_PIX_FMT_YUVA444P,  
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

    AV_PIX_FMT_RGBA64BE,     
    AV_PIX_FMT_RGBA64LE,     
    AV_PIX_FMT_BGRA64BE,     
    AV_PIX_FMT_BGRA64LE,     

    AV_PIX_FMT_YVYU422,   

    AV_PIX_FMT_YA16BE,       
    AV_PIX_FMT_YA16LE,       

    AV_PIX_FMT_GBRAP,        
    AV_PIX_FMT_GBRAP16BE,    
    AV_PIX_FMT_GBRAP16LE,    
    
    AV_PIX_FMT_QSV,
    
    AV_PIX_FMT_MMAL,

    AV_PIX_FMT_D3D11VA_VLD,  

    AV_PIX_FMT_CUDA,

    AV_PIX_FMT_0RGB,        
    AV_PIX_FMT_RGB0,        
    AV_PIX_FMT_0BGR,        
    AV_PIX_FMT_BGR0,        

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

    AV_PIX_FMT_XVMC,

    AV_PIX_FMT_YUV440P10LE, 
    AV_PIX_FMT_YUV440P10BE, 
    AV_PIX_FMT_YUV440P12LE, 
    AV_PIX_FMT_YUV440P12BE, 
    AV_PIX_FMT_AYUV64LE,    
    AV_PIX_FMT_AYUV64BE,    

    AV_PIX_FMT_VIDEOTOOLBOX, 

    AV_PIX_FMT_P010LE, 
    AV_PIX_FMT_P010BE, 

    AV_PIX_FMT_GBRAP12BE,  
    AV_PIX_FMT_GBRAP12LE,  

    AV_PIX_FMT_GBRAP10BE,  
    AV_PIX_FMT_GBRAP10LE,  

    AV_PIX_FMT_MEDIACODEC, 

    AV_PIX_FMT_GRAY12BE,   
    AV_PIX_FMT_GRAY12LE,   
    AV_PIX_FMT_GRAY10BE,   
    AV_PIX_FMT_GRAY10LE,   

    AV_PIX_FMT_P016LE, 
    AV_PIX_FMT_P016BE, 

    AV_PIX_FMT_D3D11,

    AV_PIX_FMT_GRAY9BE,   
    AV_PIX_FMT_GRAY9LE,   

    AV_PIX_FMT_GBRPF32BE,  
    AV_PIX_FMT_GBRPF32LE,  
    AV_PIX_FMT_GBRAPF32BE, 
    AV_PIX_FMT_GBRAPF32LE, 

    AV_PIX_FMT_DRM_PRIME,
    
    AV_PIX_FMT_OPENCL,

    AV_PIX_FMT_GRAY14BE,   
    AV_PIX_FMT_GRAY14LE,   

    AV_PIX_FMT_GRAYF32BE,  
    AV_PIX_FMT_GRAYF32LE,  

    AV_PIX_FMT_YUVA422P12BE, 
    AV_PIX_FMT_YUVA422P12LE, 
    AV_PIX_FMT_YUVA444P12BE, 
    AV_PIX_FMT_YUVA444P12LE, 

    AV_PIX_FMT_NV24,      
    AV_PIX_FMT_NV42,      

    AV_PIX_FMT_VULKAN,

    AV_PIX_FMT_Y210BE,    
    AV_PIX_FMT_Y210LE,    

    AV_PIX_FMT_X2RGB10LE, 
    AV_PIX_FMT_X2RGB10BE, 
    AV_PIX_FMT_X2BGR10LE, 
    AV_PIX_FMT_X2BGR10BE, 

    AV_PIX_FMT_P210BE,      
    AV_PIX_FMT_P210LE,      

    AV_PIX_FMT_P410BE,      
    AV_PIX_FMT_P410LE,      

    AV_PIX_FMT_P216BE,      
    AV_PIX_FMT_P216LE,      

    AV_PIX_FMT_P416BE,      
    AV_PIX_FMT_P416LE,      

    AV_PIX_FMT_VUYA,        

    AV_PIX_FMT_RGBAF16BE,   
    AV_PIX_FMT_RGBAF16LE,   

    AV_PIX_FMT_VUYX,        

    AV_PIX_FMT_P012LE,      
    AV_PIX_FMT_P012BE,      

    AV_PIX_FMT_Y212BE,      
    AV_PIX_FMT_Y212LE,      

    AV_PIX_FMT_XV30BE,      
    AV_PIX_FMT_XV30LE,      

    AV_PIX_FMT_XV36BE,      
    AV_PIX_FMT_XV36LE,      

    AV_PIX_FMT_RGBF32BE,    
    AV_PIX_FMT_RGBF32LE,    

    AV_PIX_FMT_RGBAF32BE,   
    AV_PIX_FMT_RGBAF32LE,   

    AV_PIX_FMT_NB         
};

#   define AV_PIX_FMT_NE(be, le) AV_PIX_FMT_##le

#define AV_PIX_FMT_RGB32   AV_PIX_FMT_NE(ARGB, BGRA)
#define AV_PIX_FMT_RGB32_1 AV_PIX_FMT_NE(RGBA, ABGR)
#define AV_PIX_FMT_BGR32   AV_PIX_FMT_NE(ABGR, RGBA)
#define AV_PIX_FMT_BGR32_1 AV_PIX_FMT_NE(BGRA, ARGB)
#define AV_PIX_FMT_0RGB32  AV_PIX_FMT_NE(0RGB, BGR0)
#define AV_PIX_FMT_0BGR32  AV_PIX_FMT_NE(0BGR, RGB0)

#define AV_PIX_FMT_GRAY9  AV_PIX_FMT_NE(GRAY9BE,  GRAY9LE)
#define AV_PIX_FMT_GRAY10 AV_PIX_FMT_NE(GRAY10BE, GRAY10LE)
#define AV_PIX_FMT_GRAY12 AV_PIX_FMT_NE(GRAY12BE, GRAY12LE)
#define AV_PIX_FMT_GRAY14 AV_PIX_FMT_NE(GRAY14BE, GRAY14LE)
#define AV_PIX_FMT_GRAY16 AV_PIX_FMT_NE(GRAY16BE, GRAY16LE)
#define AV_PIX_FMT_YA16   AV_PIX_FMT_NE(YA16BE,   YA16LE)
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

#define AV_PIX_FMT_YUV420P9  AV_PIX_FMT_NE(YUV420P9BE , YUV420P9LE)
#define AV_PIX_FMT_YUV422P9  AV_PIX_FMT_NE(YUV422P9BE , YUV422P9LE)
#define AV_PIX_FMT_YUV444P9  AV_PIX_FMT_NE(YUV444P9BE , YUV444P9LE)
#define AV_PIX_FMT_YUV420P10 AV_PIX_FMT_NE(YUV420P10BE, YUV420P10LE)
#define AV_PIX_FMT_YUV422P10 AV_PIX_FMT_NE(YUV422P10BE, YUV422P10LE)
#define AV_PIX_FMT_YUV440P10 AV_PIX_FMT_NE(YUV440P10BE, YUV440P10LE)
#define AV_PIX_FMT_YUV444P10 AV_PIX_FMT_NE(YUV444P10BE, YUV444P10LE)
#define AV_PIX_FMT_YUV420P12 AV_PIX_FMT_NE(YUV420P12BE, YUV420P12LE)
#define AV_PIX_FMT_YUV422P12 AV_PIX_FMT_NE(YUV422P12BE, YUV422P12LE)
#define AV_PIX_FMT_YUV440P12 AV_PIX_FMT_NE(YUV440P12BE, YUV440P12LE)
#define AV_PIX_FMT_YUV444P12 AV_PIX_FMT_NE(YUV444P12BE, YUV444P12LE)
#define AV_PIX_FMT_YUV420P14 AV_PIX_FMT_NE(YUV420P14BE, YUV420P14LE)
#define AV_PIX_FMT_YUV422P14 AV_PIX_FMT_NE(YUV422P14BE, YUV422P14LE)
#define AV_PIX_FMT_YUV444P14 AV_PIX_FMT_NE(YUV444P14BE, YUV444P14LE)
#define AV_PIX_FMT_YUV420P16 AV_PIX_FMT_NE(YUV420P16BE, YUV420P16LE)
#define AV_PIX_FMT_YUV422P16 AV_PIX_FMT_NE(YUV422P16BE, YUV422P16LE)
#define AV_PIX_FMT_YUV444P16 AV_PIX_FMT_NE(YUV444P16BE, YUV444P16LE)

#define AV_PIX_FMT_GBRP9     AV_PIX_FMT_NE(GBRP9BE ,    GBRP9LE)
#define AV_PIX_FMT_GBRP10    AV_PIX_FMT_NE(GBRP10BE,    GBRP10LE)
#define AV_PIX_FMT_GBRP12    AV_PIX_FMT_NE(GBRP12BE,    GBRP12LE)
#define AV_PIX_FMT_GBRP14    AV_PIX_FMT_NE(GBRP14BE,    GBRP14LE)
#define AV_PIX_FMT_GBRP16    AV_PIX_FMT_NE(GBRP16BE,    GBRP16LE)
#define AV_PIX_FMT_GBRAP10   AV_PIX_FMT_NE(GBRAP10BE,   GBRAP10LE)
#define AV_PIX_FMT_GBRAP12   AV_PIX_FMT_NE(GBRAP12BE,   GBRAP12LE)
#define AV_PIX_FMT_GBRAP16   AV_PIX_FMT_NE(GBRAP16BE,   GBRAP16LE)

#define AV_PIX_FMT_BAYER_BGGR16 AV_PIX_FMT_NE(BAYER_BGGR16BE,    BAYER_BGGR16LE)
#define AV_PIX_FMT_BAYER_RGGB16 AV_PIX_FMT_NE(BAYER_RGGB16BE,    BAYER_RGGB16LE)
#define AV_PIX_FMT_BAYER_GBRG16 AV_PIX_FMT_NE(BAYER_GBRG16BE,    BAYER_GBRG16LE)
#define AV_PIX_FMT_BAYER_GRBG16 AV_PIX_FMT_NE(BAYER_GRBG16BE,    BAYER_GRBG16LE)

#define AV_PIX_FMT_GBRPF32    AV_PIX_FMT_NE(GBRPF32BE,  GBRPF32LE)
#define AV_PIX_FMT_GBRAPF32   AV_PIX_FMT_NE(GBRAPF32BE, GBRAPF32LE)

#define AV_PIX_FMT_GRAYF32    AV_PIX_FMT_NE(GRAYF32BE, GRAYF32LE)

#define AV_PIX_FMT_YUVA420P9  AV_PIX_FMT_NE(YUVA420P9BE , YUVA420P9LE)
#define AV_PIX_FMT_YUVA422P9  AV_PIX_FMT_NE(YUVA422P9BE , YUVA422P9LE)
#define AV_PIX_FMT_YUVA444P9  AV_PIX_FMT_NE(YUVA444P9BE , YUVA444P9LE)
#define AV_PIX_FMT_YUVA420P10 AV_PIX_FMT_NE(YUVA420P10BE, YUVA420P10LE)
#define AV_PIX_FMT_YUVA422P10 AV_PIX_FMT_NE(YUVA422P10BE, YUVA422P10LE)
#define AV_PIX_FMT_YUVA444P10 AV_PIX_FMT_NE(YUVA444P10BE, YUVA444P10LE)
#define AV_PIX_FMT_YUVA422P12 AV_PIX_FMT_NE(YUVA422P12BE, YUVA422P12LE)
#define AV_PIX_FMT_YUVA444P12 AV_PIX_FMT_NE(YUVA444P12BE, YUVA444P12LE)
#define AV_PIX_FMT_YUVA420P16 AV_PIX_FMT_NE(YUVA420P16BE, YUVA420P16LE)
#define AV_PIX_FMT_YUVA422P16 AV_PIX_FMT_NE(YUVA422P16BE, YUVA422P16LE)
#define AV_PIX_FMT_YUVA444P16 AV_PIX_FMT_NE(YUVA444P16BE, YUVA444P16LE)

#define AV_PIX_FMT_XYZ12      AV_PIX_FMT_NE(XYZ12BE, XYZ12LE)
#define AV_PIX_FMT_NV20       AV_PIX_FMT_NE(NV20BE,  NV20LE)
#define AV_PIX_FMT_AYUV64     AV_PIX_FMT_NE(AYUV64BE, AYUV64LE)
#define AV_PIX_FMT_P010       AV_PIX_FMT_NE(P010BE,  P010LE)
#define AV_PIX_FMT_P012       AV_PIX_FMT_NE(P012BE,  P012LE)
#define AV_PIX_FMT_P016       AV_PIX_FMT_NE(P016BE,  P016LE)

#define AV_PIX_FMT_Y210       AV_PIX_FMT_NE(Y210BE,  Y210LE)
#define AV_PIX_FMT_Y212       AV_PIX_FMT_NE(Y212BE,  Y212LE)
#define AV_PIX_FMT_XV30       AV_PIX_FMT_NE(XV30BE,  XV30LE)
#define AV_PIX_FMT_XV36       AV_PIX_FMT_NE(XV36BE,  XV36LE)
#define AV_PIX_FMT_X2RGB10    AV_PIX_FMT_NE(X2RGB10BE, X2RGB10LE)
#define AV_PIX_FMT_X2BGR10    AV_PIX_FMT_NE(X2BGR10BE, X2BGR10LE)

#define AV_PIX_FMT_P210       AV_PIX_FMT_NE(P210BE, P210LE)
#define AV_PIX_FMT_P410       AV_PIX_FMT_NE(P410BE, P410LE)
#define AV_PIX_FMT_P216       AV_PIX_FMT_NE(P216BE, P216LE)
#define AV_PIX_FMT_P416       AV_PIX_FMT_NE(P416BE, P416LE)

#define AV_PIX_FMT_RGBAF16    AV_PIX_FMT_NE(RGBAF16BE, RGBAF16LE)

#define AV_PIX_FMT_RGBF32     AV_PIX_FMT_NE(RGBF32BE, RGBF32LE)
#define AV_PIX_FMT_RGBAF32    AV_PIX_FMT_NE(RGBAF32BE, RGBAF32LE)

enum AVColorPrimaries {
    AVCOL_PRI_RESERVED0   = 0,
    AVCOL_PRI_BT709       = 1,  
    AVCOL_PRI_UNSPECIFIED = 2,
    AVCOL_PRI_RESERVED    = 3,
    AVCOL_PRI_BT470M      = 4,  

    AVCOL_PRI_BT470BG     = 5,  
    AVCOL_PRI_SMPTE170M   = 6,  
    AVCOL_PRI_SMPTE240M   = 7,  
    AVCOL_PRI_FILM        = 8,  
    AVCOL_PRI_BT2020      = 9,  
    AVCOL_PRI_SMPTE428    = 10, 
    AVCOL_PRI_SMPTEST428_1 = AVCOL_PRI_SMPTE428,
    AVCOL_PRI_SMPTE431    = 11, 
    AVCOL_PRI_SMPTE432    = 12, 
    AVCOL_PRI_EBU3213     = 22, 
    AVCOL_PRI_JEDEC_P22   = AVCOL_PRI_EBU3213,
    AVCOL_PRI_NB                
};

enum AVColorTransferCharacteristic {
    AVCOL_TRC_RESERVED0    = 0,
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
    AVCOL_TRC_SMPTE2084    = 16, 
    AVCOL_TRC_SMPTEST2084  = AVCOL_TRC_SMPTE2084,
    AVCOL_TRC_SMPTE428     = 17, 
    AVCOL_TRC_SMPTEST428_1 = AVCOL_TRC_SMPTE428,
    AVCOL_TRC_ARIB_STD_B67 = 18, 
    AVCOL_TRC_NB                 
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
    AVCOL_SPC_YCGCO       = 8,  
    AVCOL_SPC_YCOCG       = AVCOL_SPC_YCGCO,
    AVCOL_SPC_BT2020_NCL  = 9,  
    AVCOL_SPC_BT2020_CL   = 10, 
    AVCOL_SPC_SMPTE2085   = 11, 
    AVCOL_SPC_CHROMA_DERIVED_NCL = 12, 
    AVCOL_SPC_CHROMA_DERIVED_CL = 13, 
    AVCOL_SPC_ICTCP       = 14, 
    AVCOL_SPC_NB                
};

enum AVColorRange {
    AVCOL_RANGE_UNSPECIFIED = 0,

    AVCOL_RANGE_MPEG        = 1,

    AVCOL_RANGE_JPEG        = 2,
    AVCOL_RANGE_NB               
};

enum AVChromaLocation {
    AVCHROMA_LOC_UNSPECIFIED = 0,
    AVCHROMA_LOC_LEFT        = 1, 
    AVCHROMA_LOC_CENTER      = 2, 
    AVCHROMA_LOC_TOPLEFT     = 3, 
    AVCHROMA_LOC_TOP         = 4,
    AVCHROMA_LOC_BOTTOMLEFT  = 5,
    AVCHROMA_LOC_BOTTOM      = 6,
    AVCHROMA_LOC_NB               
};

static inline void *av_x_if_null(const void *p, const void *x)
{
    return (void *)(intptr_t)(p ? p : x);
}

unsigned av_int_list_length_for_size(unsigned elsize,
                                     const void *list, uint64_t term) av_pure;

#define av_int_list_length(list, term) \
    av_int_list_length_for_size(sizeof(*(list)), list, term)

attribute_deprecated
FILE *av_fopen_utf8(const char *path, const char *mode);

AVRational av_get_time_base_q(void);

#define AV_FOURCC_MAX_STRING_SIZE 32

#define av_fourcc2str(fourcc) av_fourcc_make_string((char[AV_FOURCC_MAX_STRING_SIZE]){0}, fourcc)

char *av_fourcc_make_string(char *buf, uint32_t fourcc);

#define AVUTIL_BUFFER_H

typedef struct AVBuffer AVBuffer;

typedef struct AVBufferRef {
    AVBuffer *buffer;

    uint8_t *data;
    
    size_t   size;
} AVBufferRef;

AVBufferRef *av_buffer_alloc(size_t size);

AVBufferRef *av_buffer_allocz(size_t size);

#define AV_BUFFER_FLAG_READONLY (1 << 0)

AVBufferRef *av_buffer_create(uint8_t *data, size_t size,
                              void (*free)(void *opaque, uint8_t *data),
                              void *opaque, int flags);

void av_buffer_default_free(void *opaque, uint8_t *data);

AVBufferRef *av_buffer_ref(const AVBufferRef *buf);

void av_buffer_unref(AVBufferRef **buf);

int av_buffer_is_writable(const AVBufferRef *buf);

void *av_buffer_get_opaque(const AVBufferRef *buf);

int av_buffer_get_ref_count(const AVBufferRef *buf);

int av_buffer_make_writable(AVBufferRef **buf);

int av_buffer_realloc(AVBufferRef **buf, size_t size);

int av_buffer_replace(AVBufferRef **dst, const AVBufferRef *src);

typedef struct AVBufferPool AVBufferPool;

AVBufferPool *av_buffer_pool_init(size_t size, AVBufferRef* (*alloc)(size_t size));

AVBufferPool *av_buffer_pool_init2(size_t size, void *opaque,
                                   AVBufferRef* (*alloc)(void *opaque, size_t size),
                                   void (*pool_free)(void *opaque));

void av_buffer_pool_uninit(AVBufferPool **pool);

AVBufferRef *av_buffer_pool_get(AVBufferPool *pool);

void *av_buffer_pool_buffer_get_opaque(const AVBufferRef *ref);

#define AVUTIL_DICT_H

#define AV_DICT_MATCH_CASE      1   
#define AV_DICT_IGNORE_SUFFIX   2   

#define AV_DICT_DONT_STRDUP_KEY 4   

#define AV_DICT_DONT_STRDUP_VAL 8   

#define AV_DICT_DONT_OVERWRITE 16   
#define AV_DICT_APPEND         32   

#define AV_DICT_MULTIKEY       64   

typedef struct AVDictionaryEntry {
    char *key;
    char *value;
} AVDictionaryEntry;

typedef struct AVDictionary AVDictionary;

AVDictionaryEntry *av_dict_get(const AVDictionary *m, const char *key,
                               const AVDictionaryEntry *prev, int flags);

const AVDictionaryEntry *av_dict_iterate(const AVDictionary *m,
                                         const AVDictionaryEntry *prev);

int av_dict_count(const AVDictionary *m);

int av_dict_set(AVDictionary **pm, const char *key, const char *value, int flags);

int av_dict_set_int(AVDictionary **pm, const char *key, int64_t value, int flags);

int av_dict_parse_string(AVDictionary **pm, const char *str,
                         const char *key_val_sep, const char *pairs_sep,
                         int flags);

int av_dict_copy(AVDictionary **dst, const AVDictionary *src, int flags);

void av_dict_free(AVDictionary **m);

int av_dict_get_string(const AVDictionary *m, char **buffer,
                       const char key_val_sep, const char pairs_sep);

#define AVUTIL_FRAME_H

#define AVUTIL_CHANNEL_LAYOUT_H

enum AVChannel {
    
    AV_CHAN_NONE = -1,
    AV_CHAN_FRONT_LEFT,
    AV_CHAN_FRONT_RIGHT,
    AV_CHAN_FRONT_CENTER,
    AV_CHAN_LOW_FREQUENCY,
    AV_CHAN_BACK_LEFT,
    AV_CHAN_BACK_RIGHT,
    AV_CHAN_FRONT_LEFT_OF_CENTER,
    AV_CHAN_FRONT_RIGHT_OF_CENTER,
    AV_CHAN_BACK_CENTER,
    AV_CHAN_SIDE_LEFT,
    AV_CHAN_SIDE_RIGHT,
    AV_CHAN_TOP_CENTER,
    AV_CHAN_TOP_FRONT_LEFT,
    AV_CHAN_TOP_FRONT_CENTER,
    AV_CHAN_TOP_FRONT_RIGHT,
    AV_CHAN_TOP_BACK_LEFT,
    AV_CHAN_TOP_BACK_CENTER,
    AV_CHAN_TOP_BACK_RIGHT,
    
    AV_CHAN_STEREO_LEFT = 29,
    
    AV_CHAN_STEREO_RIGHT,
    AV_CHAN_WIDE_LEFT,
    AV_CHAN_WIDE_RIGHT,
    AV_CHAN_SURROUND_DIRECT_LEFT,
    AV_CHAN_SURROUND_DIRECT_RIGHT,
    AV_CHAN_LOW_FREQUENCY_2,
    AV_CHAN_TOP_SIDE_LEFT,
    AV_CHAN_TOP_SIDE_RIGHT,
    AV_CHAN_BOTTOM_FRONT_CENTER,
    AV_CHAN_BOTTOM_FRONT_LEFT,
    AV_CHAN_BOTTOM_FRONT_RIGHT,

    AV_CHAN_UNUSED = 0x200,

    AV_CHAN_UNKNOWN = 0x300,

    AV_CHAN_AMBISONIC_BASE = 0x400,
    
    AV_CHAN_AMBISONIC_END  = 0x7ff,
};

enum AVChannelOrder {
    
    AV_CHANNEL_ORDER_UNSPEC,
    
    AV_CHANNEL_ORDER_NATIVE,
    
    AV_CHANNEL_ORDER_CUSTOM,
    
    AV_CHANNEL_ORDER_AMBISONIC,
};

#define AV_CH_FRONT_LEFT             (1ULL << AV_CHAN_FRONT_LEFT           )
#define AV_CH_FRONT_RIGHT            (1ULL << AV_CHAN_FRONT_RIGHT          )
#define AV_CH_FRONT_CENTER           (1ULL << AV_CHAN_FRONT_CENTER         )
#define AV_CH_LOW_FREQUENCY          (1ULL << AV_CHAN_LOW_FREQUENCY        )
#define AV_CH_BACK_LEFT              (1ULL << AV_CHAN_BACK_LEFT            )
#define AV_CH_BACK_RIGHT             (1ULL << AV_CHAN_BACK_RIGHT           )
#define AV_CH_FRONT_LEFT_OF_CENTER   (1ULL << AV_CHAN_FRONT_LEFT_OF_CENTER )
#define AV_CH_FRONT_RIGHT_OF_CENTER  (1ULL << AV_CHAN_FRONT_RIGHT_OF_CENTER)
#define AV_CH_BACK_CENTER            (1ULL << AV_CHAN_BACK_CENTER          )
#define AV_CH_SIDE_LEFT              (1ULL << AV_CHAN_SIDE_LEFT            )
#define AV_CH_SIDE_RIGHT             (1ULL << AV_CHAN_SIDE_RIGHT           )
#define AV_CH_TOP_CENTER             (1ULL << AV_CHAN_TOP_CENTER           )
#define AV_CH_TOP_FRONT_LEFT         (1ULL << AV_CHAN_TOP_FRONT_LEFT       )
#define AV_CH_TOP_FRONT_CENTER       (1ULL << AV_CHAN_TOP_FRONT_CENTER     )
#define AV_CH_TOP_FRONT_RIGHT        (1ULL << AV_CHAN_TOP_FRONT_RIGHT      )
#define AV_CH_TOP_BACK_LEFT          (1ULL << AV_CHAN_TOP_BACK_LEFT        )
#define AV_CH_TOP_BACK_CENTER        (1ULL << AV_CHAN_TOP_BACK_CENTER      )
#define AV_CH_TOP_BACK_RIGHT         (1ULL << AV_CHAN_TOP_BACK_RIGHT       )
#define AV_CH_STEREO_LEFT            (1ULL << AV_CHAN_STEREO_LEFT          )
#define AV_CH_STEREO_RIGHT           (1ULL << AV_CHAN_STEREO_RIGHT         )
#define AV_CH_WIDE_LEFT              (1ULL << AV_CHAN_WIDE_LEFT            )
#define AV_CH_WIDE_RIGHT             (1ULL << AV_CHAN_WIDE_RIGHT           )
#define AV_CH_SURROUND_DIRECT_LEFT   (1ULL << AV_CHAN_SURROUND_DIRECT_LEFT )
#define AV_CH_SURROUND_DIRECT_RIGHT  (1ULL << AV_CHAN_SURROUND_DIRECT_RIGHT)
#define AV_CH_LOW_FREQUENCY_2        (1ULL << AV_CHAN_LOW_FREQUENCY_2      )
#define AV_CH_TOP_SIDE_LEFT          (1ULL << AV_CHAN_TOP_SIDE_LEFT        )
#define AV_CH_TOP_SIDE_RIGHT         (1ULL << AV_CHAN_TOP_SIDE_RIGHT       )
#define AV_CH_BOTTOM_FRONT_CENTER    (1ULL << AV_CHAN_BOTTOM_FRONT_CENTER  )
#define AV_CH_BOTTOM_FRONT_LEFT      (1ULL << AV_CHAN_BOTTOM_FRONT_LEFT    )
#define AV_CH_BOTTOM_FRONT_RIGHT     (1ULL << AV_CHAN_BOTTOM_FRONT_RIGHT   )

#define AV_CH_LAYOUT_NATIVE          0x8000000000000000ULL

#define AV_CH_LAYOUT_MONO              (AV_CH_FRONT_CENTER)
#define AV_CH_LAYOUT_STEREO            (AV_CH_FRONT_LEFT|AV_CH_FRONT_RIGHT)
#define AV_CH_LAYOUT_2POINT1           (AV_CH_LAYOUT_STEREO|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_2_1               (AV_CH_LAYOUT_STEREO|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_SURROUND          (AV_CH_LAYOUT_STEREO|AV_CH_FRONT_CENTER)
#define AV_CH_LAYOUT_3POINT1           (AV_CH_LAYOUT_SURROUND|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_4POINT0           (AV_CH_LAYOUT_SURROUND|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_4POINT1           (AV_CH_LAYOUT_4POINT0|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_2_2               (AV_CH_LAYOUT_STEREO|AV_CH_SIDE_LEFT|AV_CH_SIDE_RIGHT)
#define AV_CH_LAYOUT_QUAD              (AV_CH_LAYOUT_STEREO|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_5POINT0           (AV_CH_LAYOUT_SURROUND|AV_CH_SIDE_LEFT|AV_CH_SIDE_RIGHT)
#define AV_CH_LAYOUT_5POINT1           (AV_CH_LAYOUT_5POINT0|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_5POINT0_BACK      (AV_CH_LAYOUT_SURROUND|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_5POINT1_BACK      (AV_CH_LAYOUT_5POINT0_BACK|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_6POINT0           (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT0_FRONT     (AV_CH_LAYOUT_2_2|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_HEXAGONAL         (AV_CH_LAYOUT_5POINT0_BACK|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1           (AV_CH_LAYOUT_5POINT1|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1_BACK      (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_BACK_CENTER)
#define AV_CH_LAYOUT_6POINT1_FRONT     (AV_CH_LAYOUT_6POINT0_FRONT|AV_CH_LOW_FREQUENCY)
#define AV_CH_LAYOUT_7POINT0           (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_7POINT0_FRONT     (AV_CH_LAYOUT_5POINT0|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_7POINT1           (AV_CH_LAYOUT_5POINT1|AV_CH_BACK_LEFT|AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_7POINT1_WIDE      (AV_CH_LAYOUT_5POINT1|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_7POINT1_WIDE_BACK (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER)
#define AV_CH_LAYOUT_7POINT1_TOP_BACK  (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_TOP_FRONT_LEFT|AV_CH_TOP_FRONT_RIGHT)
#define AV_CH_LAYOUT_OCTAGONAL         (AV_CH_LAYOUT_5POINT0|AV_CH_BACK_LEFT|AV_CH_BACK_CENTER|AV_CH_BACK_RIGHT)
#define AV_CH_LAYOUT_CUBE              (AV_CH_LAYOUT_QUAD|AV_CH_TOP_FRONT_LEFT|AV_CH_TOP_FRONT_RIGHT|AV_CH_TOP_BACK_LEFT|AV_CH_TOP_BACK_RIGHT)
#define AV_CH_LAYOUT_HEXADECAGONAL     (AV_CH_LAYOUT_OCTAGONAL|AV_CH_WIDE_LEFT|AV_CH_WIDE_RIGHT|AV_CH_TOP_BACK_LEFT|AV_CH_TOP_BACK_RIGHT|AV_CH_TOP_BACK_CENTER|AV_CH_TOP_FRONT_CENTER|AV_CH_TOP_FRONT_LEFT|AV_CH_TOP_FRONT_RIGHT)
#define AV_CH_LAYOUT_STEREO_DOWNMIX    (AV_CH_STEREO_LEFT|AV_CH_STEREO_RIGHT)
#define AV_CH_LAYOUT_22POINT2          (AV_CH_LAYOUT_5POINT1_BACK|AV_CH_FRONT_LEFT_OF_CENTER|AV_CH_FRONT_RIGHT_OF_CENTER|AV_CH_BACK_CENTER|AV_CH_LOW_FREQUENCY_2|AV_CH_SIDE_LEFT|AV_CH_SIDE_RIGHT|AV_CH_TOP_FRONT_LEFT|AV_CH_TOP_FRONT_RIGHT|AV_CH_TOP_FRONT_CENTER|AV_CH_TOP_CENTER|AV_CH_TOP_BACK_LEFT|AV_CH_TOP_BACK_RIGHT|AV_CH_TOP_SIDE_LEFT|AV_CH_TOP_SIDE_RIGHT|AV_CH_TOP_BACK_CENTER|AV_CH_BOTTOM_FRONT_CENTER|AV_CH_BOTTOM_FRONT_LEFT|AV_CH_BOTTOM_FRONT_RIGHT)

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

typedef struct AVChannelCustom {
    enum AVChannel id;
    char name[16];
    void *opaque;
} AVChannelCustom;

typedef struct AVChannelLayout {
    
    enum AVChannelOrder order;

    int nb_channels;

    union {
        
        uint64_t mask;
        
        AVChannelCustom *map;
    } u;

    void *opaque;
} AVChannelLayout;

#define AV_CHANNEL_LAYOUT_MASK(nb, m) \
    { .order = AV_CHANNEL_ORDER_NATIVE, .nb_channels = (nb), .u = { .mask = (m) }}

#define AV_CHANNEL_LAYOUT_MONO              AV_CHANNEL_LAYOUT_MASK(1,  AV_CH_LAYOUT_MONO)
#define AV_CHANNEL_LAYOUT_STEREO            AV_CHANNEL_LAYOUT_MASK(2,  AV_CH_LAYOUT_STEREO)
#define AV_CHANNEL_LAYOUT_2POINT1           AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_2POINT1)
#define AV_CHANNEL_LAYOUT_2_1               AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_2_1)
#define AV_CHANNEL_LAYOUT_SURROUND          AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_SURROUND)
#define AV_CHANNEL_LAYOUT_3POINT1           AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_3POINT1)
#define AV_CHANNEL_LAYOUT_4POINT0           AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_4POINT0)
#define AV_CHANNEL_LAYOUT_4POINT1           AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_4POINT1)
#define AV_CHANNEL_LAYOUT_2_2               AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_2_2)
#define AV_CHANNEL_LAYOUT_QUAD              AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_QUAD)
#define AV_CHANNEL_LAYOUT_5POINT0           AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_5POINT0)
#define AV_CHANNEL_LAYOUT_5POINT1           AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_5POINT1)
#define AV_CHANNEL_LAYOUT_5POINT0_BACK      AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_5POINT0_BACK)
#define AV_CHANNEL_LAYOUT_5POINT1_BACK      AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_5POINT1_BACK)
#define AV_CHANNEL_LAYOUT_6POINT0           AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_6POINT0)
#define AV_CHANNEL_LAYOUT_6POINT0_FRONT     AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_6POINT0_FRONT)
#define AV_CHANNEL_LAYOUT_HEXAGONAL         AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_HEXAGONAL)
#define AV_CHANNEL_LAYOUT_6POINT1           AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1)
#define AV_CHANNEL_LAYOUT_6POINT1_BACK      AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1_BACK)
#define AV_CHANNEL_LAYOUT_6POINT1_FRONT     AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1_FRONT)
#define AV_CHANNEL_LAYOUT_7POINT0           AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_7POINT0)
#define AV_CHANNEL_LAYOUT_7POINT0_FRONT     AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_7POINT0_FRONT)
#define AV_CHANNEL_LAYOUT_7POINT1           AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1)
#define AV_CHANNEL_LAYOUT_7POINT1_WIDE      AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1_WIDE)
#define AV_CHANNEL_LAYOUT_7POINT1_WIDE_BACK AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1_WIDE_BACK)
#define AV_CHANNEL_LAYOUT_7POINT1_TOP_BACK  AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1_TOP_BACK)
#define AV_CHANNEL_LAYOUT_OCTAGONAL         AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_OCTAGONAL)
#define AV_CHANNEL_LAYOUT_CUBE              AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_CUBE)
#define AV_CHANNEL_LAYOUT_HEXADECAGONAL     AV_CHANNEL_LAYOUT_MASK(16, AV_CH_LAYOUT_HEXADECAGONAL)
#define AV_CHANNEL_LAYOUT_STEREO_DOWNMIX    AV_CHANNEL_LAYOUT_MASK(2,  AV_CH_LAYOUT_STEREO_DOWNMIX)
#define AV_CHANNEL_LAYOUT_22POINT2          AV_CHANNEL_LAYOUT_MASK(24, AV_CH_LAYOUT_22POINT2)
#define AV_CHANNEL_LAYOUT_AMBISONIC_FIRST_ORDER \
    { .order = AV_CHANNEL_ORDER_AMBISONIC, .nb_channels = 4, .u = { .mask = 0 }}

struct AVBPrint;

attribute_deprecated
uint64_t av_get_channel_layout(const char *name);

attribute_deprecated
int av_get_extended_channel_layout(const char *name, uint64_t* channel_layout, int* nb_channels);

attribute_deprecated
void av_get_channel_layout_string(char *buf, int buf_size, int nb_channels, uint64_t channel_layout);

attribute_deprecated
void av_bprint_channel_layout(struct AVBPrint *bp, int nb_channels, uint64_t channel_layout);

attribute_deprecated
int av_get_channel_layout_nb_channels(uint64_t channel_layout);

attribute_deprecated
int64_t av_get_default_channel_layout(int nb_channels);

attribute_deprecated
int av_get_channel_layout_channel_index(uint64_t channel_layout,
                                        uint64_t channel);

attribute_deprecated
uint64_t av_channel_layout_extract_channel(uint64_t channel_layout, int index);

attribute_deprecated
const char *av_get_channel_name(uint64_t channel);

attribute_deprecated
const char *av_get_channel_description(uint64_t channel);

attribute_deprecated
int av_get_standard_channel_layout(unsigned index, uint64_t *layout,
                                   const char **name);

int av_channel_name(char *buf, size_t buf_size, enum AVChannel channel);

void av_channel_name_bprint(struct AVBPrint *bp, enum AVChannel channel_id);

int av_channel_description(char *buf, size_t buf_size, enum AVChannel channel);

void av_channel_description_bprint(struct AVBPrint *bp, enum AVChannel channel_id);

enum AVChannel av_channel_from_string(const char *name);

int av_channel_layout_from_mask(AVChannelLayout *channel_layout, uint64_t mask);

int av_channel_layout_from_string(AVChannelLayout *channel_layout,
                                  const char *str);

void av_channel_layout_default(AVChannelLayout *ch_layout, int nb_channels);

const AVChannelLayout *av_channel_layout_standard(void **opaque);

void av_channel_layout_uninit(AVChannelLayout *channel_layout);

int av_channel_layout_copy(AVChannelLayout *dst, const AVChannelLayout *src);

int av_channel_layout_describe(const AVChannelLayout *channel_layout,
                               char *buf, size_t buf_size);

int av_channel_layout_describe_bprint(const AVChannelLayout *channel_layout,
                                      struct AVBPrint *bp);

enum AVChannel
av_channel_layout_channel_from_index(const AVChannelLayout *channel_layout, unsigned int idx);

int av_channel_layout_index_from_channel(const AVChannelLayout *channel_layout,
                                         enum AVChannel channel);

int av_channel_layout_index_from_string(const AVChannelLayout *channel_layout,
                                        const char *name);

enum AVChannel
av_channel_layout_channel_from_string(const AVChannelLayout *channel_layout,
                                      const char *name);

uint64_t av_channel_layout_subset(const AVChannelLayout *channel_layout,
                                  uint64_t mask);

int av_channel_layout_check(const AVChannelLayout *channel_layout);

int av_channel_layout_compare(const AVChannelLayout *chl, const AVChannelLayout *chl1);

enum AVFrameSideDataType {
    
    AV_FRAME_DATA_PANSCAN,
    
    AV_FRAME_DATA_A53_CC,
    
    AV_FRAME_DATA_STEREO3D,
    
    AV_FRAME_DATA_MATRIXENCODING,
    
    AV_FRAME_DATA_DOWNMIX_INFO,
    
    AV_FRAME_DATA_REPLAYGAIN,
    
    AV_FRAME_DATA_DISPLAYMATRIX,
    
    AV_FRAME_DATA_AFD,
    
    AV_FRAME_DATA_MOTION_VECTORS,
    
    AV_FRAME_DATA_SKIP_SAMPLES,
    
    AV_FRAME_DATA_AUDIO_SERVICE_TYPE,
    
    AV_FRAME_DATA_MASTERING_DISPLAY_METADATA,
    
    AV_FRAME_DATA_GOP_TIMECODE,

    AV_FRAME_DATA_SPHERICAL,

    AV_FRAME_DATA_CONTENT_LIGHT_LEVEL,

    AV_FRAME_DATA_ICC_PROFILE,

    AV_FRAME_DATA_S12M_TIMECODE,

    AV_FRAME_DATA_DYNAMIC_HDR_PLUS,

    AV_FRAME_DATA_REGIONS_OF_INTEREST,

    AV_FRAME_DATA_VIDEO_ENC_PARAMS,

    AV_FRAME_DATA_SEI_UNREGISTERED,

    AV_FRAME_DATA_FILM_GRAIN_PARAMS,

    AV_FRAME_DATA_DETECTION_BBOXES,

    AV_FRAME_DATA_DOVI_RPU_BUFFER,

    AV_FRAME_DATA_DOVI_METADATA,

    AV_FRAME_DATA_DYNAMIC_HDR_VIVID,

    AV_FRAME_DATA_AMBIENT_VIEWING_ENVIRONMENT,
};

enum AVActiveFormatDescription {
    AV_AFD_SAME         = 8,
    AV_AFD_4_3          = 9,
    AV_AFD_16_9         = 10,
    AV_AFD_14_9         = 11,
    AV_AFD_4_3_SP_14_9  = 13,
    AV_AFD_16_9_SP_14_9 = 14,
    AV_AFD_SP_4_3       = 15,
};

typedef struct AVFrameSideData {
    enum AVFrameSideDataType type;
    uint8_t *data;
    size_t   size;
    AVDictionary *metadata;
    AVBufferRef *buf;
} AVFrameSideData;

typedef struct AVRegionOfInterest {
    
    uint32_t self_size;
    
    int top;
    int bottom;
    int left;
    int right;
    
    AVRational qoffset;
} AVRegionOfInterest;

typedef struct AVFrame {
#define AV_NUM_DATA_POINTERS 8
    
    uint8_t *data[AV_NUM_DATA_POINTERS];

    int linesize[AV_NUM_DATA_POINTERS];

    uint8_t **extended_data;

    int width, height;
    
    int nb_samples;

    int format;

    int key_frame;

    enum AVPictureType pict_type;

    AVRational sample_aspect_ratio;

    int64_t pts;

    int64_t pkt_dts;

    AVRational time_base;

    attribute_deprecated
    int coded_picture_number;
    
    attribute_deprecated
    int display_picture_number;

    int quality;

    void *opaque;

    int repeat_pict;

    int interlaced_frame;

    int top_field_first;

    int palette_has_changed;

    attribute_deprecated
    int64_t reordered_opaque;

    int sample_rate;

    attribute_deprecated
    uint64_t channel_layout;

    AVBufferRef *buf[AV_NUM_DATA_POINTERS];

    AVBufferRef **extended_buf;
    
    int        nb_extended_buf;

    AVFrameSideData **side_data;
    int            nb_side_data;

#define AV_FRAME_FLAG_CORRUPT       (1 << 0)

#define AV_FRAME_FLAG_DISCARD   (1 << 2)

    int flags;

    enum AVColorRange color_range;

    enum AVColorPrimaries color_primaries;

    enum AVColorTransferCharacteristic color_trc;

    enum AVColorSpace colorspace;

    enum AVChromaLocation chroma_location;

    int64_t best_effort_timestamp;

    int64_t pkt_pos;

    attribute_deprecated
    int64_t pkt_duration;

    AVDictionary *metadata;

    int decode_error_flags;
#define FF_DECODE_ERROR_INVALID_BITSTREAM   1
#define FF_DECODE_ERROR_MISSING_REFERENCE   2
#define FF_DECODE_ERROR_CONCEALMENT_ACTIVE  4
#define FF_DECODE_ERROR_DECODE_SLICES       8

    attribute_deprecated
    int channels;

    int pkt_size;

    AVBufferRef *hw_frames_ctx;

    AVBufferRef *opaque_ref;

    size_t crop_top;
    size_t crop_bottom;
    size_t crop_left;
    size_t crop_right;
    
    AVBufferRef *private_ref;

    AVChannelLayout ch_layout;

    int64_t duration;
} AVFrame;

AVFrame *av_frame_alloc(void);

void av_frame_free(AVFrame **frame);

int av_frame_ref(AVFrame *dst, const AVFrame *src);

AVFrame *av_frame_clone(const AVFrame *src);

void av_frame_unref(AVFrame *frame);

void av_frame_move_ref(AVFrame *dst, AVFrame *src);

int av_frame_get_buffer(AVFrame *frame, int align);

int av_frame_is_writable(AVFrame *frame);

int av_frame_make_writable(AVFrame *frame);

int av_frame_copy(AVFrame *dst, const AVFrame *src);

int av_frame_copy_props(AVFrame *dst, const AVFrame *src);

AVBufferRef *av_frame_get_plane_buffer(AVFrame *frame, int plane);

AVFrameSideData *av_frame_new_side_data(AVFrame *frame,
                                        enum AVFrameSideDataType type,
                                        size_t size);

AVFrameSideData *av_frame_new_side_data_from_buf(AVFrame *frame,
                                                 enum AVFrameSideDataType type,
                                                 AVBufferRef *buf);

AVFrameSideData *av_frame_get_side_data(const AVFrame *frame,
                                        enum AVFrameSideDataType type);

void av_frame_remove_side_data(AVFrame *frame, enum AVFrameSideDataType type);

enum {
    
    AV_FRAME_CROP_UNALIGNED     = 1 << 0,
};

int av_frame_apply_cropping(AVFrame *frame, int flags);

const char *av_frame_side_data_name(enum AVFrameSideDataType type);

#define AVCODEC_CODEC_H

#define AVUTIL_HWCONTEXT_H

enum AVHWDeviceType {
    AV_HWDEVICE_TYPE_NONE,
    AV_HWDEVICE_TYPE_VDPAU,
    AV_HWDEVICE_TYPE_CUDA,
    AV_HWDEVICE_TYPE_VAAPI,
    AV_HWDEVICE_TYPE_DXVA2,
    AV_HWDEVICE_TYPE_QSV,
    AV_HWDEVICE_TYPE_VIDEOTOOLBOX,
    AV_HWDEVICE_TYPE_D3D11VA,
    AV_HWDEVICE_TYPE_DRM,
    AV_HWDEVICE_TYPE_OPENCL,
    AV_HWDEVICE_TYPE_MEDIACODEC,
    AV_HWDEVICE_TYPE_VULKAN,
};

typedef struct AVHWDeviceInternal AVHWDeviceInternal;

typedef struct AVHWDeviceContext {
    
    const AVClass *av_class;

    AVHWDeviceInternal *internal;

    enum AVHWDeviceType type;

    void *hwctx;

    void (*free)(struct AVHWDeviceContext *ctx);

    void *user_opaque;
} AVHWDeviceContext;

typedef struct AVHWFramesInternal AVHWFramesInternal;

typedef struct AVHWFramesContext {
    
    const AVClass *av_class;

    AVHWFramesInternal *internal;

    AVBufferRef *device_ref;

    AVHWDeviceContext *device_ctx;

    void *hwctx;

    void (*free)(struct AVHWFramesContext *ctx);

    void *user_opaque;

    AVBufferPool *pool;

    int initial_pool_size;

    enum AVPixelFormat format;

    enum AVPixelFormat sw_format;

    int width, height;
} AVHWFramesContext;

enum AVHWDeviceType av_hwdevice_find_type_by_name(const char *name);

const char *av_hwdevice_get_type_name(enum AVHWDeviceType type);

enum AVHWDeviceType av_hwdevice_iterate_types(enum AVHWDeviceType prev);

AVBufferRef *av_hwdevice_ctx_alloc(enum AVHWDeviceType type);

int av_hwdevice_ctx_init(AVBufferRef *ref);

int av_hwdevice_ctx_create(AVBufferRef **device_ctx, enum AVHWDeviceType type,
                           const char *device, AVDictionary *opts, int flags);

int av_hwdevice_ctx_create_derived(AVBufferRef **dst_ctx,
                                   enum AVHWDeviceType type,
                                   AVBufferRef *src_ctx, int flags);

int av_hwdevice_ctx_create_derived_opts(AVBufferRef **dst_ctx,
                                        enum AVHWDeviceType type,
                                        AVBufferRef *src_ctx,
                                        AVDictionary *options, int flags);

AVBufferRef *av_hwframe_ctx_alloc(AVBufferRef *device_ctx);

int av_hwframe_ctx_init(AVBufferRef *ref);

int av_hwframe_get_buffer(AVBufferRef *hwframe_ctx, AVFrame *frame, int flags);

int av_hwframe_transfer_data(AVFrame *dst, const AVFrame *src, int flags);

enum AVHWFrameTransferDirection {
    
    AV_HWFRAME_TRANSFER_DIRECTION_FROM,

    AV_HWFRAME_TRANSFER_DIRECTION_TO,
};

int av_hwframe_transfer_get_formats(AVBufferRef *hwframe_ctx,
                                    enum AVHWFrameTransferDirection dir,
                                    enum AVPixelFormat **formats, int flags);

typedef struct AVHWFramesConstraints {
    
    enum AVPixelFormat *valid_hw_formats;

    enum AVPixelFormat *valid_sw_formats;

    int min_width;
    int min_height;

    int max_width;
    int max_height;
} AVHWFramesConstraints;

void *av_hwdevice_hwconfig_alloc(AVBufferRef *device_ctx);

AVHWFramesConstraints *av_hwdevice_get_hwframe_constraints(AVBufferRef *ref,
                                                           const void *hwconfig);

void av_hwframe_constraints_free(AVHWFramesConstraints **constraints);

enum {
    
    AV_HWFRAME_MAP_READ      = 1 << 0,
    
    AV_HWFRAME_MAP_WRITE     = 1 << 1,
    
    AV_HWFRAME_MAP_OVERWRITE = 1 << 2,
    
    AV_HWFRAME_MAP_DIRECT    = 1 << 3,
};

int av_hwframe_map(AVFrame *dst, const AVFrame *src, int flags);

int av_hwframe_ctx_create_derived(AVBufferRef **derived_frame_ctx,
                                  enum AVPixelFormat format,
                                  AVBufferRef *derived_device_ctx,
                                  AVBufferRef *source_frame_ctx,
                                  int flags);

#define AVCODEC_CODEC_ID_H

#define AVCODEC_VERSION_MAJOR_H

#define LIBAVCODEC_VERSION_MAJOR  60

#define FF_API_INIT_PACKET         (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_IDCT_NONE           (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_SVTAV1_OPTS         (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_AYUV_CODECID        (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_VT_OUTPUT_CALLBACK  (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_AVCODEC_CHROMA_POS  (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_VT_HWACCEL_CONTEXT  (LIBAVCODEC_VERSION_MAJOR < 61)
#define FF_API_AVCTX_FRAME_NUMBER  (LIBAVCODEC_VERSION_MAJOR < 61)

#define FF_CODEC_CRYSTAL_HD        (LIBAVCODEC_VERSION_MAJOR < 61)

enum AVCodecID {
    AV_CODEC_ID_NONE,

    AV_CODEC_ID_MPEG1VIDEO,
    AV_CODEC_ID_MPEG2VIDEO, 
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
#define AV_CODEC_ID_IFF_BYTERUN1 AV_CODEC_ID_IFF_ILBM
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
    AV_CODEC_ID_ESCAPE130,
    AV_CODEC_ID_G2M,
    AV_CODEC_ID_WEBP,
    AV_CODEC_ID_HNM4_VIDEO,
    AV_CODEC_ID_HEVC,
#define AV_CODEC_ID_H265 AV_CODEC_ID_HEVC
    AV_CODEC_ID_FIC,
    AV_CODEC_ID_ALIAS_PIX,
    AV_CODEC_ID_BRENDER_PIX,
    AV_CODEC_ID_PAF_VIDEO,
    AV_CODEC_ID_EXR,
    AV_CODEC_ID_VP7,
    AV_CODEC_ID_SANM,
    AV_CODEC_ID_SGIRLE,
    AV_CODEC_ID_MVC1,
    AV_CODEC_ID_MVC2,
    AV_CODEC_ID_HQX,
    AV_CODEC_ID_TDSC,
    AV_CODEC_ID_HQ_HQA,
    AV_CODEC_ID_HAP,
    AV_CODEC_ID_DDS,
    AV_CODEC_ID_DXV,
    AV_CODEC_ID_SCREENPRESSO,
    AV_CODEC_ID_RSCC,
    AV_CODEC_ID_AVS2,
    AV_CODEC_ID_PGX,
    AV_CODEC_ID_AVS3,
    AV_CODEC_ID_MSP2,
    AV_CODEC_ID_VVC,
#define AV_CODEC_ID_H266 AV_CODEC_ID_VVC
    AV_CODEC_ID_Y41P,
    AV_CODEC_ID_AVRP,
    AV_CODEC_ID_012V,
    AV_CODEC_ID_AVUI,
    AV_CODEC_ID_AYUV,
    AV_CODEC_ID_TARGA_Y216,
    AV_CODEC_ID_V308,
    AV_CODEC_ID_V408,
    AV_CODEC_ID_YUV4,
    AV_CODEC_ID_AVRN,
    AV_CODEC_ID_CPIA,
    AV_CODEC_ID_XFACE,
    AV_CODEC_ID_SNOW,
    AV_CODEC_ID_SMVJPEG,
    AV_CODEC_ID_APNG,
    AV_CODEC_ID_DAALA,
    AV_CODEC_ID_CFHD,
    AV_CODEC_ID_TRUEMOTION2RT,
    AV_CODEC_ID_M101,
    AV_CODEC_ID_MAGICYUV,
    AV_CODEC_ID_SHEERVIDEO,
    AV_CODEC_ID_YLC,
    AV_CODEC_ID_PSD,
    AV_CODEC_ID_PIXLET,
    AV_CODEC_ID_SPEEDHQ,
    AV_CODEC_ID_FMVC,
    AV_CODEC_ID_SCPR,
    AV_CODEC_ID_CLEARVIDEO,
    AV_CODEC_ID_XPM,
    AV_CODEC_ID_AV1,
    AV_CODEC_ID_BITPACKED,
    AV_CODEC_ID_MSCC,
    AV_CODEC_ID_SRGC,
    AV_CODEC_ID_SVG,
    AV_CODEC_ID_GDV,
    AV_CODEC_ID_FITS,
    AV_CODEC_ID_IMM4,
    AV_CODEC_ID_PROSUMER,
    AV_CODEC_ID_MWSC,
    AV_CODEC_ID_WCMV,
    AV_CODEC_ID_RASC,
    AV_CODEC_ID_HYMT,
    AV_CODEC_ID_ARBC,
    AV_CODEC_ID_AGM,
    AV_CODEC_ID_LSCR,
    AV_CODEC_ID_VP4,
    AV_CODEC_ID_IMM5,
    AV_CODEC_ID_MVDV,
    AV_CODEC_ID_MVHA,
    AV_CODEC_ID_CDTOONS,
    AV_CODEC_ID_MV30,
    AV_CODEC_ID_NOTCHLC,
    AV_CODEC_ID_PFM,
    AV_CODEC_ID_MOBICLIP,
    AV_CODEC_ID_PHOTOCD,
    AV_CODEC_ID_IPU,
    AV_CODEC_ID_ARGO,
    AV_CODEC_ID_CRI,
    AV_CODEC_ID_SIMBIOSIS_IMX,
    AV_CODEC_ID_SGA_VIDEO,
    AV_CODEC_ID_GEM,
    AV_CODEC_ID_VBN,
    AV_CODEC_ID_JPEGXL,
    AV_CODEC_ID_QOI,
    AV_CODEC_ID_PHM,
    AV_CODEC_ID_RADIANCE_HDR,
    AV_CODEC_ID_WBMP,
    AV_CODEC_ID_MEDIA100,
    AV_CODEC_ID_VQC,

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
    AV_CODEC_ID_PCM_S24LE_PLANAR,
    AV_CODEC_ID_PCM_S32LE_PLANAR,
    AV_CODEC_ID_PCM_S16BE_PLANAR,
    AV_CODEC_ID_PCM_S64LE,
    AV_CODEC_ID_PCM_S64BE,
    AV_CODEC_ID_PCM_F16LE,
    AV_CODEC_ID_PCM_F24LE,
    AV_CODEC_ID_PCM_VIDC,
    AV_CODEC_ID_PCM_SGA,

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
    AV_CODEC_ID_ADPCM_VIMA,
    AV_CODEC_ID_ADPCM_AFC,
    AV_CODEC_ID_ADPCM_IMA_OKI,
    AV_CODEC_ID_ADPCM_DTK,
    AV_CODEC_ID_ADPCM_IMA_RAD,
    AV_CODEC_ID_ADPCM_G726LE,
    AV_CODEC_ID_ADPCM_THP_LE,
    AV_CODEC_ID_ADPCM_PSX,
    AV_CODEC_ID_ADPCM_AICA,
    AV_CODEC_ID_ADPCM_IMA_DAT4,
    AV_CODEC_ID_ADPCM_MTAF,
    AV_CODEC_ID_ADPCM_AGM,
    AV_CODEC_ID_ADPCM_ARGO,
    AV_CODEC_ID_ADPCM_IMA_SSI,
    AV_CODEC_ID_ADPCM_ZORK,
    AV_CODEC_ID_ADPCM_IMA_APM,
    AV_CODEC_ID_ADPCM_IMA_ALP,
    AV_CODEC_ID_ADPCM_IMA_MTF,
    AV_CODEC_ID_ADPCM_IMA_CUNNING,
    AV_CODEC_ID_ADPCM_IMA_MOFLEX,
    AV_CODEC_ID_ADPCM_IMA_ACORN,
    AV_CODEC_ID_ADPCM_XMD,

    AV_CODEC_ID_AMR_NB = 0x12000,
    AV_CODEC_ID_AMR_WB,

    AV_CODEC_ID_RA_144 = 0x13000,
    AV_CODEC_ID_RA_288,

    AV_CODEC_ID_ROQ_DPCM = 0x14000,
    AV_CODEC_ID_INTERPLAY_DPCM,
    AV_CODEC_ID_XAN_DPCM,
    AV_CODEC_ID_SOL_DPCM,
    AV_CODEC_ID_SDX2_DPCM,
    AV_CODEC_ID_GREMLIN_DPCM,
    AV_CODEC_ID_DERF_DPCM,
    AV_CODEC_ID_WADY_DPCM,
    AV_CODEC_ID_CBD2_DPCM,

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
    AV_CODEC_ID_OPUS,
    AV_CODEC_ID_COMFORT_NOISE,
    AV_CODEC_ID_TAK,
    AV_CODEC_ID_METASOUND,
    AV_CODEC_ID_PAF_AUDIO,
    AV_CODEC_ID_ON2AVC,
    AV_CODEC_ID_DSS_SP,
    AV_CODEC_ID_CODEC2,
    AV_CODEC_ID_FFWAVESYNTH,
    AV_CODEC_ID_SONIC,
    AV_CODEC_ID_SONIC_LS,
    AV_CODEC_ID_EVRC,
    AV_CODEC_ID_SMV,
    AV_CODEC_ID_DSD_LSBF,
    AV_CODEC_ID_DSD_MSBF,
    AV_CODEC_ID_DSD_LSBF_PLANAR,
    AV_CODEC_ID_DSD_MSBF_PLANAR,
    AV_CODEC_ID_4GV,
    AV_CODEC_ID_INTERPLAY_ACM,
    AV_CODEC_ID_XMA1,
    AV_CODEC_ID_XMA2,
    AV_CODEC_ID_DST,
    AV_CODEC_ID_ATRAC3AL,
    AV_CODEC_ID_ATRAC3PAL,
    AV_CODEC_ID_DOLBY_E,
    AV_CODEC_ID_APTX,
    AV_CODEC_ID_APTX_HD,
    AV_CODEC_ID_SBC,
    AV_CODEC_ID_ATRAC9,
    AV_CODEC_ID_HCOM,
    AV_CODEC_ID_ACELP_KELVIN,
    AV_CODEC_ID_MPEGH_3D_AUDIO,
    AV_CODEC_ID_SIREN,
    AV_CODEC_ID_HCA,
    AV_CODEC_ID_FASTAUDIO,
    AV_CODEC_ID_MSNSIREN,
    AV_CODEC_ID_DFPWM,
    AV_CODEC_ID_BONK,
    AV_CODEC_ID_MISC4,
    AV_CODEC_ID_APAC,
    AV_CODEC_ID_FTR,
    AV_CODEC_ID_WAVARC,
    AV_CODEC_ID_RKA,

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
    AV_CODEC_ID_MICRODVD,
    AV_CODEC_ID_EIA_608,
    AV_CODEC_ID_JACOSUB,
    AV_CODEC_ID_SAMI,
    AV_CODEC_ID_REALTEXT,
    AV_CODEC_ID_STL,
    AV_CODEC_ID_SUBVIEWER1,
    AV_CODEC_ID_SUBVIEWER,
    AV_CODEC_ID_SUBRIP,
    AV_CODEC_ID_WEBVTT,
    AV_CODEC_ID_MPL2,
    AV_CODEC_ID_VPLAYER,
    AV_CODEC_ID_PJS,
    AV_CODEC_ID_ASS,
    AV_CODEC_ID_HDMV_TEXT_SUBTITLE,
    AV_CODEC_ID_TTML,
    AV_CODEC_ID_ARIB_CAPTION,

    AV_CODEC_ID_FIRST_UNKNOWN = 0x18000,           
    AV_CODEC_ID_TTF = 0x18000,

    AV_CODEC_ID_SCTE_35, 
    AV_CODEC_ID_EPG,
    AV_CODEC_ID_BINTEXT,
    AV_CODEC_ID_XBIN,
    AV_CODEC_ID_IDF,
    AV_CODEC_ID_OTF,
    AV_CODEC_ID_SMPTE_KLV,
    AV_CODEC_ID_DVD_NAV,
    AV_CODEC_ID_TIMED_ID3,
    AV_CODEC_ID_BIN_DATA,

    AV_CODEC_ID_PROBE = 0x19000, 

    AV_CODEC_ID_MPEG2TS = 0x20000, 

    AV_CODEC_ID_MPEG4SYSTEMS = 0x20001, 

    AV_CODEC_ID_FFMETADATA = 0x21000,   
    AV_CODEC_ID_WRAPPED_AVFRAME = 0x21001, 
    
    AV_CODEC_ID_VNULL,
    
    AV_CODEC_ID_ANULL,
};

enum AVMediaType avcodec_get_type(enum AVCodecID codec_id);

const char *avcodec_get_name(enum AVCodecID id);

int av_get_bits_per_sample(enum AVCodecID codec_id);

int av_get_exact_bits_per_sample(enum AVCodecID codec_id);

const char *avcodec_profile_name(enum AVCodecID codec_id, int profile);

enum AVCodecID av_get_pcm_codec(enum AVSampleFormat fmt, int be);

#define AV_CODEC_CAP_DRAW_HORIZ_BAND     (1 <<  0)

#define AV_CODEC_CAP_DR1                 (1 <<  1)

#define AV_CODEC_CAP_DELAY               (1 <<  5)

#define AV_CODEC_CAP_SMALL_LAST_FRAME    (1 <<  6)

#define AV_CODEC_CAP_SUBFRAMES           (1 <<  8)

#define AV_CODEC_CAP_EXPERIMENTAL        (1 <<  9)

#define AV_CODEC_CAP_CHANNEL_CONF        (1 << 10)

#define AV_CODEC_CAP_FRAME_THREADS       (1 << 12)

#define AV_CODEC_CAP_SLICE_THREADS       (1 << 13)

#define AV_CODEC_CAP_PARAM_CHANGE        (1 << 14)

#define AV_CODEC_CAP_OTHER_THREADS       (1 << 15)

#define AV_CODEC_CAP_VARIABLE_FRAME_SIZE (1 << 16)

#define AV_CODEC_CAP_AVOID_PROBING       (1 << 17)

#define AV_CODEC_CAP_HARDWARE            (1 << 18)

#define AV_CODEC_CAP_HYBRID              (1 << 19)

#define AV_CODEC_CAP_ENCODER_REORDERED_OPAQUE (1 << 20)

#define AV_CODEC_CAP_ENCODER_FLUSH   (1 << 21)

#define AV_CODEC_CAP_ENCODER_RECON_FRAME (1 << 22)

typedef struct AVProfile {
    int profile;
    const char *name; 
} AVProfile;

typedef struct AVCodec {
    
    const char *name;
    
    const char *long_name;
    enum AVMediaType type;
    enum AVCodecID id;
    
    int capabilities;
    uint8_t max_lowres;                     
    const AVRational *supported_framerates; 
    const enum AVPixelFormat *pix_fmts;     
    const int *supported_samplerates;       
    const enum AVSampleFormat *sample_fmts; 
    
    attribute_deprecated
    const uint64_t *channel_layouts;         
    const AVClass *priv_class;              
    const AVProfile *profiles;              

    const char *wrapper_name;

    const AVChannelLayout *ch_layouts;
} AVCodec;

const AVCodec *av_codec_iterate(void **opaque);

const AVCodec *avcodec_find_decoder(enum AVCodecID id);

const AVCodec *avcodec_find_decoder_by_name(const char *name);

const AVCodec *avcodec_find_encoder(enum AVCodecID id);

const AVCodec *avcodec_find_encoder_by_name(const char *name);

int av_codec_is_encoder(const AVCodec *codec);

int av_codec_is_decoder(const AVCodec *codec);

const char *av_get_profile_name(const AVCodec *codec, int profile);

enum {
    
    AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX = 0x01,
    
    AV_CODEC_HW_CONFIG_METHOD_HW_FRAMES_CTX = 0x02,
    
    AV_CODEC_HW_CONFIG_METHOD_INTERNAL      = 0x04,
    
    AV_CODEC_HW_CONFIG_METHOD_AD_HOC        = 0x08,
};

typedef struct AVCodecHWConfig {
    
    enum AVPixelFormat pix_fmt;
    
    int methods;
    
    enum AVHWDeviceType device_type;
} AVCodecHWConfig;

const AVCodecHWConfig *avcodec_get_hw_config(const AVCodec *codec, int index);

#define AVCODEC_CODEC_DESC_H

typedef struct AVCodecDescriptor {
    enum AVCodecID     id;
    enum AVMediaType type;
    
    const char      *name;
    
    const char *long_name;
    
    int             props;
    
    const char *const *mime_types;
    
    const struct AVProfile *profiles;
} AVCodecDescriptor;

#define AV_CODEC_PROP_INTRA_ONLY    (1 << 0)

#define AV_CODEC_PROP_LOSSY         (1 << 1)

#define AV_CODEC_PROP_LOSSLESS      (1 << 2)

#define AV_CODEC_PROP_REORDER       (1 << 3)

#define AV_CODEC_PROP_BITMAP_SUB    (1 << 16)

#define AV_CODEC_PROP_TEXT_SUB      (1 << 17)

const AVCodecDescriptor *avcodec_descriptor_get(enum AVCodecID id);

const AVCodecDescriptor *avcodec_descriptor_next(const AVCodecDescriptor *prev);

const AVCodecDescriptor *avcodec_descriptor_get_by_name(const char *name);

#define AVCODEC_CODEC_PAR_H

enum AVFieldOrder {
    AV_FIELD_UNKNOWN,
    AV_FIELD_PROGRESSIVE,
    AV_FIELD_TT,          
    AV_FIELD_BB,          
    AV_FIELD_TB,          
    AV_FIELD_BT,          
};

typedef struct AVCodecParameters {
    
    enum AVMediaType codec_type;
    
    enum AVCodecID   codec_id;
    
    uint32_t         codec_tag;

    uint8_t *extradata;
    
    int      extradata_size;

    int format;

    int64_t bit_rate;

    int bits_per_coded_sample;

    int bits_per_raw_sample;

    int profile;
    int level;

    int width;
    int height;

    AVRational sample_aspect_ratio;

    enum AVFieldOrder                  field_order;

    enum AVColorRange                  color_range;
    enum AVColorPrimaries              color_primaries;
    enum AVColorTransferCharacteristic color_trc;
    enum AVColorSpace                  color_space;
    enum AVChromaLocation              chroma_location;

    int video_delay;

    attribute_deprecated
    uint64_t channel_layout;
    
    attribute_deprecated
    int      channels;
    
    int      sample_rate;
    
    int      block_align;
    
    int      frame_size;

    int initial_padding;
    
    int trailing_padding;
    
    int seek_preroll;

    AVChannelLayout ch_layout;
} AVCodecParameters;

AVCodecParameters *avcodec_parameters_alloc(void);

void avcodec_parameters_free(AVCodecParameters **par);

int avcodec_parameters_copy(AVCodecParameters *dst, const AVCodecParameters *src);

int av_get_audio_frame_duration2(AVCodecParameters *par, int frame_bytes);

#define AVCODEC_DEFS_H

#define AV_INPUT_BUFFER_PADDING_SIZE 64

#define AV_EF_CRCCHECK       (1<<0)
#define AV_EF_BITSTREAM      (1<<1)   
#define AV_EF_BUFFER         (1<<2)   
#define AV_EF_EXPLODE        (1<<3)   

#define AV_EF_IGNORE_ERR     (1<<15)  
#define AV_EF_CAREFUL        (1<<16)  
#define AV_EF_COMPLIANT      (1<<17)  
#define AV_EF_AGGRESSIVE     (1<<18)  

#define FF_COMPLIANCE_VERY_STRICT   2 
#define FF_COMPLIANCE_STRICT        1 
#define FF_COMPLIANCE_NORMAL        0
#define FF_COMPLIANCE_UNOFFICIAL   -1 
#define FF_COMPLIANCE_EXPERIMENTAL -2 

enum AVDiscard{
    
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
    AV_AUDIO_SERVICE_TYPE_NB                   , 
};

typedef struct AVPanScan {
    
    int id;

    int width;
    int height;

    int16_t position[3][2];
} AVPanScan;

typedef struct AVCPBProperties {
    
    int64_t max_bitrate;
    
    int64_t min_bitrate;
    
    int64_t avg_bitrate;

    int64_t buffer_size;

    uint64_t vbv_delay;
} AVCPBProperties;

AVCPBProperties *av_cpb_properties_alloc(size_t *size);

typedef struct AVProducerReferenceTime {
    
    int64_t wallclock;
    int flags;
} AVProducerReferenceTime;

unsigned int av_xiphlacing(unsigned char *s, unsigned int v);

#define AVCODEC_PACKET_H

enum AVPacketSideDataType {
    
    AV_PKT_DATA_PALETTE,

    AV_PKT_DATA_NEW_EXTRADATA,

    AV_PKT_DATA_PARAM_CHANGE,

    AV_PKT_DATA_H263_MB_INFO,

    AV_PKT_DATA_REPLAYGAIN,

    AV_PKT_DATA_DISPLAYMATRIX,

    AV_PKT_DATA_STEREO3D,

    AV_PKT_DATA_AUDIO_SERVICE_TYPE,

    AV_PKT_DATA_QUALITY_STATS,

    AV_PKT_DATA_FALLBACK_TRACK,

    AV_PKT_DATA_CPB_PROPERTIES,

    AV_PKT_DATA_SKIP_SAMPLES,

    AV_PKT_DATA_JP_DUALMONO,

    AV_PKT_DATA_STRINGS_METADATA,

    AV_PKT_DATA_SUBTITLE_POSITION,

    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    AV_PKT_DATA_WEBVTT_SETTINGS,

    AV_PKT_DATA_METADATA_UPDATE,

    AV_PKT_DATA_MPEGTS_STREAM_ID,

    AV_PKT_DATA_MASTERING_DISPLAY_METADATA,

    AV_PKT_DATA_SPHERICAL,

    AV_PKT_DATA_CONTENT_LIGHT_LEVEL,

    AV_PKT_DATA_A53_CC,

    AV_PKT_DATA_ENCRYPTION_INIT_INFO,

    AV_PKT_DATA_ENCRYPTION_INFO,

    AV_PKT_DATA_AFD,

    AV_PKT_DATA_PRFT,

    AV_PKT_DATA_ICC_PROFILE,

    AV_PKT_DATA_DOVI_CONF,

    AV_PKT_DATA_S12M_TIMECODE,

    AV_PKT_DATA_DYNAMIC_HDR10_PLUS,

    AV_PKT_DATA_NB
};

#define AV_PKT_DATA_QUALITY_FACTOR AV_PKT_DATA_QUALITY_STATS 

typedef struct AVPacketSideData {
    uint8_t *data;
    size_t   size;
    enum AVPacketSideDataType type;
} AVPacketSideData;

typedef struct AVPacket {
    
    AVBufferRef *buf;
    
    int64_t pts;
    
    int64_t dts;
    uint8_t *data;
    int   size;
    int   stream_index;
    
    int   flags;
    
    AVPacketSideData *side_data;
    int side_data_elems;

    int64_t duration;

    int64_t pos;                            

    void *opaque;

    AVBufferRef *opaque_ref;

    AVRational time_base;
} AVPacket;

attribute_deprecated
typedef struct AVPacketList {
    AVPacket pkt;
    struct AVPacketList *next;
} AVPacketList;

#define AV_PKT_FLAG_KEY     0x0001 
#define AV_PKT_FLAG_CORRUPT 0x0002 

#define AV_PKT_FLAG_DISCARD   0x0004

#define AV_PKT_FLAG_TRUSTED   0x0008

#define AV_PKT_FLAG_DISPOSABLE 0x0010

enum AVSideDataParamChangeFlags {
    
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT  = 0x0001,
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = 0x0002,
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE    = 0x0004,
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS     = 0x0008,
};

AVPacket *av_packet_alloc(void);

AVPacket *av_packet_clone(const AVPacket *src);

void av_packet_free(AVPacket **pkt);

attribute_deprecated
void av_init_packet(AVPacket *pkt);

int av_new_packet(AVPacket *pkt, int size);

void av_shrink_packet(AVPacket *pkt, int size);

int av_grow_packet(AVPacket *pkt, int grow_by);

int av_packet_from_data(AVPacket *pkt, uint8_t *data, int size);

uint8_t* av_packet_new_side_data(AVPacket *pkt, enum AVPacketSideDataType type,
                                 size_t size);

int av_packet_add_side_data(AVPacket *pkt, enum AVPacketSideDataType type,
                            uint8_t *data, size_t size);

int av_packet_shrink_side_data(AVPacket *pkt, enum AVPacketSideDataType type,
                               size_t size);

uint8_t* av_packet_get_side_data(const AVPacket *pkt, enum AVPacketSideDataType type,
                                 size_t *size);

const char *av_packet_side_data_name(enum AVPacketSideDataType type);

uint8_t *av_packet_pack_dictionary(AVDictionary *dict, size_t *size);

int av_packet_unpack_dictionary(const uint8_t *data, size_t size,
                                AVDictionary **dict);

void av_packet_free_side_data(AVPacket *pkt);

int av_packet_ref(AVPacket *dst, const AVPacket *src);

void av_packet_unref(AVPacket *pkt);

void av_packet_move_ref(AVPacket *dst, AVPacket *src);

int av_packet_copy_props(AVPacket *dst, const AVPacket *src);

int av_packet_make_refcounted(AVPacket *pkt);

int av_packet_make_writable(AVPacket *pkt);

void av_packet_rescale_ts(AVPacket *pkt, AVRational tb_src, AVRational tb_dst);

#define AVCODEC_VERSION_H

#define LIBAVCODEC_VERSION_MINOR   3
#define LIBAVCODEC_VERSION_MICRO 100

#define LIBAVCODEC_VERSION_INT  AV_VERSION_INT(LIBAVCODEC_VERSION_MAJOR, \
                                               LIBAVCODEC_VERSION_MINOR, \
                                               LIBAVCODEC_VERSION_MICRO)
#define LIBAVCODEC_VERSION      AV_VERSION(LIBAVCODEC_VERSION_MAJOR,    \
                                           LIBAVCODEC_VERSION_MINOR,    \
                                           LIBAVCODEC_VERSION_MICRO)
#define LIBAVCODEC_BUILD        LIBAVCODEC_VERSION_INT

#define LIBAVCODEC_IDENT        "Lavc" AV_STRINGIFY(LIBAVCODEC_VERSION)

#define AV_INPUT_BUFFER_MIN_SIZE 16384

typedef struct RcOverride{
    int start_frame;
    int end_frame;
    int qscale; 
    float quality_factor;
} RcOverride;

#define AV_CODEC_FLAG_UNALIGNED       (1 <<  0)

#define AV_CODEC_FLAG_QSCALE          (1 <<  1)

#define AV_CODEC_FLAG_4MV             (1 <<  2)

#define AV_CODEC_FLAG_OUTPUT_CORRUPT  (1 <<  3)

#define AV_CODEC_FLAG_QPEL            (1 <<  4)

#define AV_CODEC_FLAG_DROPCHANGED     (1 <<  5)

#define AV_CODEC_FLAG_RECON_FRAME     (1 <<  6)

#define AV_CODEC_FLAG_COPY_OPAQUE     (1 <<  7)

#define AV_CODEC_FLAG_FRAME_DURATION  (1 <<  8)

#define AV_CODEC_FLAG_PASS1           (1 <<  9)

#define AV_CODEC_FLAG_PASS2           (1 << 10)

#define AV_CODEC_FLAG_LOOP_FILTER     (1 << 11)

#define AV_CODEC_FLAG_GRAY            (1 << 13)

#define AV_CODEC_FLAG_PSNR            (1 << 15)

#define AV_CODEC_FLAG_INTERLACED_DCT  (1 << 18)

#define AV_CODEC_FLAG_LOW_DELAY       (1 << 19)

#define AV_CODEC_FLAG_GLOBAL_HEADER   (1 << 22)

#define AV_CODEC_FLAG_BITEXACT        (1 << 23)

#define AV_CODEC_FLAG_AC_PRED         (1 << 24)

#define AV_CODEC_FLAG_INTERLACED_ME   (1 << 29)
#define AV_CODEC_FLAG_CLOSED_GOP      (1U << 31)

#define AV_CODEC_FLAG2_FAST           (1 <<  0)

#define AV_CODEC_FLAG2_NO_OUTPUT      (1 <<  2)

#define AV_CODEC_FLAG2_LOCAL_HEADER   (1 <<  3)

#define AV_CODEC_FLAG2_CHUNKS         (1 << 15)

#define AV_CODEC_FLAG2_IGNORE_CROP    (1 << 16)

#define AV_CODEC_FLAG2_SHOW_ALL       (1 << 22)

#define AV_CODEC_FLAG2_EXPORT_MVS     (1 << 28)

#define AV_CODEC_FLAG2_SKIP_MANUAL    (1 << 29)

#define AV_CODEC_FLAG2_RO_FLUSH_NOOP  (1 << 30)

#define AV_CODEC_FLAG2_ICC_PROFILES   (1U << 31)

#define AV_CODEC_EXPORT_DATA_MVS         (1 << 0)

#define AV_CODEC_EXPORT_DATA_PRFT        (1 << 1)

#define AV_CODEC_EXPORT_DATA_VIDEO_ENC_PARAMS (1 << 2)

#define AV_CODEC_EXPORT_DATA_FILM_GRAIN (1 << 3)

#define AV_GET_BUFFER_FLAG_REF (1 << 0)

#define AV_GET_ENCODE_BUFFER_FLAG_REF (1 << 0)

struct AVCodecInternal;

typedef struct AVCodecContext {
    
    const AVClass *av_class;
    int log_level_offset;

    enum AVMediaType codec_type; 
    const struct AVCodec  *codec;
    enum AVCodecID     codec_id; 

    unsigned int codec_tag;

    void *priv_data;

    struct AVCodecInternal *internal;

    void *opaque;

    int64_t bit_rate;

    int bit_rate_tolerance;

    int global_quality;

    int compression_level;
#define FF_COMPRESSION_DEFAULT -1

    int flags;

    int flags2;

    uint8_t *extradata;
    int extradata_size;

    AVRational time_base;

    int ticks_per_frame;

    int delay;

    int width, height;

    int coded_width, coded_height;

    int gop_size;

    enum AVPixelFormat pix_fmt;

    void (*draw_horiz_band)(struct AVCodecContext *s,
                            const AVFrame *src, int offset[AV_NUM_DATA_POINTERS],
                            int y, int type, int height);

    enum AVPixelFormat (*get_format)(struct AVCodecContext *s, const enum AVPixelFormat * fmt);

    int max_b_frames;

    float b_quant_factor;

    float b_quant_offset;

    int has_b_frames;

    float i_quant_factor;

    float i_quant_offset;

    float lumi_masking;

    float temporal_cplx_masking;

    float spatial_cplx_masking;

    float p_masking;

    float dark_masking;

    int slice_count;

    int *slice_offset;

    AVRational sample_aspect_ratio;

    int me_cmp;
    
    int me_sub_cmp;
    
    int mb_cmp;
    
    int ildct_cmp;
#define FF_CMP_SAD          0
#define FF_CMP_SSE          1
#define FF_CMP_SATD         2
#define FF_CMP_DCT          3
#define FF_CMP_PSNR         4
#define FF_CMP_BIT          5
#define FF_CMP_RD           6
#define FF_CMP_ZERO         7
#define FF_CMP_VSAD         8
#define FF_CMP_VSSE         9
#define FF_CMP_NSSE         10
#define FF_CMP_W53          11
#define FF_CMP_W97          12
#define FF_CMP_DCTMAX       13
#define FF_CMP_DCT264       14
#define FF_CMP_MEDIAN_SAD   15
#define FF_CMP_CHROMA       256

    int dia_size;

    int last_predictor_count;

    int me_pre_cmp;

    int pre_dia_size;

    int me_subpel_quality;

    int me_range;

    int slice_flags;
#define SLICE_FLAG_CODED_ORDER    0x0001 
#define SLICE_FLAG_ALLOW_FIELD    0x0002 
#define SLICE_FLAG_ALLOW_PLANE    0x0004 

    int mb_decision;
#define FF_MB_DECISION_SIMPLE 0        
#define FF_MB_DECISION_BITS   1        
#define FF_MB_DECISION_RD     2        

    uint16_t *intra_matrix;

    uint16_t *inter_matrix;

    int intra_dc_precision;

    int skip_top;

    int skip_bottom;

    int mb_lmin;

    int mb_lmax;

    int bidir_refine;

    int keyint_min;

    int refs;

    int mv0_threshold;

    enum AVColorPrimaries color_primaries;

    enum AVColorTransferCharacteristic color_trc;

    enum AVColorSpace colorspace;

    enum AVColorRange color_range;

    enum AVChromaLocation chroma_sample_location;

    int slices;

    enum AVFieldOrder field_order;

    int sample_rate; 

    attribute_deprecated
    int channels;

    enum AVSampleFormat sample_fmt;  

    int frame_size;

    attribute_deprecated
    int frame_number;

    int block_align;

    int cutoff;

    attribute_deprecated
    uint64_t channel_layout;

    attribute_deprecated
    uint64_t request_channel_layout;

    enum AVAudioServiceType audio_service_type;

    enum AVSampleFormat request_sample_fmt;

    int (*get_buffer2)(struct AVCodecContext *s, AVFrame *frame, int flags);

    float qcompress;  
    float qblur;      

    int qmin;

    int qmax;

    int max_qdiff;

    int rc_buffer_size;

    int rc_override_count;
    RcOverride *rc_override;

    int64_t rc_max_rate;

    int64_t rc_min_rate;

    float rc_max_available_vbv_use;

    float rc_min_vbv_overflow_use;

    int rc_initial_buffer_occupancy;

    int trellis;

    char *stats_out;

    char *stats_in;

    int workaround_bugs;
#define FF_BUG_AUTODETECT       1  
#define FF_BUG_XVID_ILACE       4
#define FF_BUG_UMP4             8
#define FF_BUG_NO_PADDING       16
#define FF_BUG_AMV              32
#define FF_BUG_QPEL_CHROMA      64
#define FF_BUG_STD_QPEL         128
#define FF_BUG_QPEL_CHROMA2     256
#define FF_BUG_DIRECT_BLOCKSIZE 512
#define FF_BUG_EDGE             1024
#define FF_BUG_HPEL_CHROMA      2048
#define FF_BUG_DC_CLIP          4096
#define FF_BUG_MS               8192 
#define FF_BUG_TRUNCATED       16384
#define FF_BUG_IEDGE           32768

    int strict_std_compliance;

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
#define FF_DEBUG_DCT_COEFF   0x00000040
#define FF_DEBUG_SKIP        0x00000080
#define FF_DEBUG_STARTCODE   0x00000100
#define FF_DEBUG_ER          0x00000400
#define FF_DEBUG_MMCO        0x00000800
#define FF_DEBUG_BUGS        0x00001000
#define FF_DEBUG_BUFFERS     0x00008000
#define FF_DEBUG_THREADS     0x00010000
#define FF_DEBUG_GREEN_MD    0x00800000
#define FF_DEBUG_NOMC        0x01000000

    int err_recognition;

    attribute_deprecated
    int64_t reordered_opaque;

    const struct AVHWAccel *hwaccel;

    void *hwaccel_context;

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
#define FF_IDCT_SIMPLEARM     10
#define FF_IDCT_XVID          14
#define FF_IDCT_SIMPLEARMV5TE 16
#define FF_IDCT_SIMPLEARMV6   17
#define FF_IDCT_FAAN          20
#define FF_IDCT_SIMPLENEON    22

#define FF_IDCT_NONE          24
#define FF_IDCT_SIMPLEAUTO    128

     int bits_per_coded_sample;

    int bits_per_raw_sample;

     int lowres;

    int thread_count;

    int thread_type;
#define FF_THREAD_FRAME   1 
#define FF_THREAD_SLICE   2 

    int active_thread_type;

    int (*execute)(struct AVCodecContext *c, int (*func)(struct AVCodecContext *c2, void *arg), void *arg2, int *ret, int count, int size);

    int (*execute2)(struct AVCodecContext *c, int (*func)(struct AVCodecContext *c2, void *arg, int jobnr, int threadnr), void *arg2, int *ret, int count);

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

#define FF_PROFILE_DNXHD         0
#define FF_PROFILE_DNXHR_LB      1
#define FF_PROFILE_DNXHR_SQ      2
#define FF_PROFILE_DNXHR_HQ      3
#define FF_PROFILE_DNXHR_HQX     4
#define FF_PROFILE_DNXHR_444     5

#define FF_PROFILE_DTS         20
#define FF_PROFILE_DTS_ES      30
#define FF_PROFILE_DTS_96_24   40
#define FF_PROFILE_DTS_HD_HRA  50
#define FF_PROFILE_DTS_HD_MA   60
#define FF_PROFILE_DTS_EXPRESS 70

#define FF_PROFILE_MPEG2_422    0
#define FF_PROFILE_MPEG2_HIGH   1
#define FF_PROFILE_MPEG2_SS     2
#define FF_PROFILE_MPEG2_SNR_SCALABLE  3
#define FF_PROFILE_MPEG2_MAIN   4
#define FF_PROFILE_MPEG2_SIMPLE 5

#define FF_PROFILE_H264_CONSTRAINED  (1<<9)  
#define FF_PROFILE_H264_INTRA        (1<<11) 

#define FF_PROFILE_H264_BASELINE             66
#define FF_PROFILE_H264_CONSTRAINED_BASELINE (66|FF_PROFILE_H264_CONSTRAINED)
#define FF_PROFILE_H264_MAIN                 77
#define FF_PROFILE_H264_EXTENDED             88
#define FF_PROFILE_H264_HIGH                 100
#define FF_PROFILE_H264_HIGH_10              110
#define FF_PROFILE_H264_HIGH_10_INTRA        (110|FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_MULTIVIEW_HIGH       118
#define FF_PROFILE_H264_HIGH_422             122
#define FF_PROFILE_H264_HIGH_422_INTRA       (122|FF_PROFILE_H264_INTRA)
#define FF_PROFILE_H264_STEREO_HIGH          128
#define FF_PROFILE_H264_HIGH_444             144
#define FF_PROFILE_H264_HIGH_444_PREDICTIVE  244
#define FF_PROFILE_H264_HIGH_444_INTRA       (244|FF_PROFILE_H264_INTRA)
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

#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0   1
#define FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1   2
#define FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION  32768
#define FF_PROFILE_JPEG2000_DCINEMA_2K              3
#define FF_PROFILE_JPEG2000_DCINEMA_4K              4

#define FF_PROFILE_VP9_0                            0
#define FF_PROFILE_VP9_1                            1
#define FF_PROFILE_VP9_2                            2
#define FF_PROFILE_VP9_3                            3

#define FF_PROFILE_HEVC_MAIN                        1
#define FF_PROFILE_HEVC_MAIN_10                     2
#define FF_PROFILE_HEVC_MAIN_STILL_PICTURE          3
#define FF_PROFILE_HEVC_REXT                        4

#define FF_PROFILE_VVC_MAIN_10                      1
#define FF_PROFILE_VVC_MAIN_10_444                 33

#define FF_PROFILE_AV1_MAIN                         0
#define FF_PROFILE_AV1_HIGH                         1
#define FF_PROFILE_AV1_PROFESSIONAL                 2

#define FF_PROFILE_MJPEG_HUFFMAN_BASELINE_DCT            0xc0
#define FF_PROFILE_MJPEG_HUFFMAN_EXTENDED_SEQUENTIAL_DCT 0xc1
#define FF_PROFILE_MJPEG_HUFFMAN_PROGRESSIVE_DCT         0xc2
#define FF_PROFILE_MJPEG_HUFFMAN_LOSSLESS                0xc3
#define FF_PROFILE_MJPEG_JPEG_LS                         0xf7

#define FF_PROFILE_SBC_MSBC                         1

#define FF_PROFILE_PRORES_PROXY     0
#define FF_PROFILE_PRORES_LT        1
#define FF_PROFILE_PRORES_STANDARD  2
#define FF_PROFILE_PRORES_HQ        3
#define FF_PROFILE_PRORES_4444      4
#define FF_PROFILE_PRORES_XQ        5

#define FF_PROFILE_ARIB_PROFILE_A 0
#define FF_PROFILE_ARIB_PROFILE_C 1

#define FF_PROFILE_KLVA_SYNC 0
#define FF_PROFILE_KLVA_ASYNC 1

     int level;
#define FF_LEVEL_UNKNOWN -99

    enum AVDiscard skip_loop_filter;

    enum AVDiscard skip_idct;

    enum AVDiscard skip_frame;

    uint8_t *subtitle_header;
    int subtitle_header_size;

    int initial_padding;

    AVRational framerate;

    enum AVPixelFormat sw_pix_fmt;

    AVRational pkt_timebase;

    const AVCodecDescriptor *codec_descriptor;

    int64_t pts_correction_num_faulty_pts; 
    int64_t pts_correction_num_faulty_dts; 
    int64_t pts_correction_last_pts;       
    int64_t pts_correction_last_dts;       

    char *sub_charenc;

    int sub_charenc_mode;
#define FF_SUB_CHARENC_MODE_DO_NOTHING  -1  
#define FF_SUB_CHARENC_MODE_AUTOMATIC    0  
#define FF_SUB_CHARENC_MODE_PRE_DECODER  1  
#define FF_SUB_CHARENC_MODE_IGNORE       2  

    int skip_alpha;

    int seek_preroll;

    uint16_t *chroma_intra_matrix;

    uint8_t *dump_separator;

    char *codec_whitelist;

    unsigned properties;
#define FF_CODEC_PROPERTY_LOSSLESS        0x00000001
#define FF_CODEC_PROPERTY_CLOSED_CAPTIONS 0x00000002
#define FF_CODEC_PROPERTY_FILM_GRAIN      0x00000004

    AVPacketSideData *coded_side_data;
    int            nb_coded_side_data;

    AVBufferRef *hw_frames_ctx;

    int trailing_padding;

    int64_t max_pixels;

    AVBufferRef *hw_device_ctx;

    int hwaccel_flags;

    int apply_cropping;

    int extra_hw_frames;

    int discard_damaged_percentage;

    int64_t max_samples;

    int export_side_data;

    int (*get_encode_buffer)(struct AVCodecContext *s, AVPacket *pkt, int flags);

    AVChannelLayout ch_layout;

    int64_t frame_num;
} AVCodecContext;

typedef struct AVHWAccel {
    
    const char *name;

    enum AVMediaType type;

    enum AVCodecID id;

    enum AVPixelFormat pix_fmt;

    int capabilities;

    int (*alloc_frame)(AVCodecContext *avctx, AVFrame *frame);

    int (*start_frame)(AVCodecContext *avctx, const uint8_t *buf, uint32_t buf_size);

    int (*decode_params)(AVCodecContext *avctx, int type, const uint8_t *buf, uint32_t buf_size);

    int (*decode_slice)(AVCodecContext *avctx, const uint8_t *buf, uint32_t buf_size);

    int (*end_frame)(AVCodecContext *avctx);

    int frame_priv_data_size;

    int (*init)(AVCodecContext *avctx);

    int (*uninit)(AVCodecContext *avctx);

    int priv_data_size;

    int caps_internal;

    int (*frame_params)(AVCodecContext *avctx, AVBufferRef *hw_frames_ctx);
} AVHWAccel;

#define AV_HWACCEL_CODEC_CAP_EXPERIMENTAL 0x0200

#define AV_HWACCEL_FLAG_IGNORE_LEVEL (1 << 0)

#define AV_HWACCEL_FLAG_ALLOW_HIGH_DEPTH (1 << 1)

#define AV_HWACCEL_FLAG_ALLOW_PROFILE_MISMATCH (1 << 2)

#define AV_HWACCEL_FLAG_UNSAFE_OUTPUT (1 << 3)

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

    uint8_t *data[4];
    int linesize[4];

    enum AVSubtitleType type;

    char *text;                     

    char *ass;

    int flags;
} AVSubtitleRect;

typedef struct AVSubtitle {
    uint16_t format; 
    uint32_t start_display_time; 
    uint32_t end_display_time; 
    unsigned num_rects;
    AVSubtitleRect **rects;
    int64_t pts;    
} AVSubtitle;

unsigned avcodec_version(void);

const char *avcodec_configuration(void);

const char *avcodec_license(void);

AVCodecContext *avcodec_alloc_context3(const AVCodec *codec);

void avcodec_free_context(AVCodecContext **avctx);

const AVClass *avcodec_get_class(void);

const AVClass *avcodec_get_subtitle_rect_class(void);

int avcodec_parameters_from_context(AVCodecParameters *par,
                                    const AVCodecContext *codec);

int avcodec_parameters_to_context(AVCodecContext *codec,
                                  const AVCodecParameters *par);

int avcodec_open2(AVCodecContext *avctx, const AVCodec *codec, AVDictionary **options);

int avcodec_close(AVCodecContext *avctx);

void avsubtitle_free(AVSubtitle *sub);

int avcodec_default_get_buffer2(AVCodecContext *s, AVFrame *frame, int flags);

int avcodec_default_get_encode_buffer(AVCodecContext *s, AVPacket *pkt, int flags);

void avcodec_align_dimensions(AVCodecContext *s, int *width, int *height);

void avcodec_align_dimensions2(AVCodecContext *s, int *width, int *height,
                               int linesize_align[AV_NUM_DATA_POINTERS]);

 attribute_deprecated
int avcodec_enum_to_chroma_pos(int *xpos, int *ypos, enum AVChromaLocation pos);

 attribute_deprecated
enum AVChromaLocation avcodec_chroma_pos_to_enum(int xpos, int ypos);

int avcodec_decode_subtitle2(AVCodecContext *avctx, AVSubtitle *sub,
                             int *got_sub_ptr, const AVPacket *avpkt);

int avcodec_send_packet(AVCodecContext *avctx, const AVPacket *avpkt);

int avcodec_receive_frame(AVCodecContext *avctx, AVFrame *frame);

int avcodec_send_frame(AVCodecContext *avctx, const AVFrame *frame);

int avcodec_receive_packet(AVCodecContext *avctx, AVPacket *avpkt);

int avcodec_get_hw_frames_parameters(AVCodecContext *avctx,
                                     AVBufferRef *device_ref,
                                     enum AVPixelFormat hw_pix_fmt,
                                     AVBufferRef **out_frames_ref);

enum AVPictureStructure {
    AV_PICTURE_STRUCTURE_UNKNOWN,      
    AV_PICTURE_STRUCTURE_TOP_FIELD,    
    AV_PICTURE_STRUCTURE_BOTTOM_FIELD, 
    AV_PICTURE_STRUCTURE_FRAME,        
};

typedef struct AVCodecParserContext {
    void *priv_data;
    const struct AVCodecParser *parser;
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

    int width;
    int height;

    int coded_width;
    int coded_height;

    int format;
} AVCodecParserContext;

typedef struct AVCodecParser {
    int codec_ids[7]; 
    int priv_data_size;
    int (*parser_init)(AVCodecParserContext *s);
    
    int (*parser_parse)(AVCodecParserContext *s,
                        AVCodecContext *avctx,
                        const uint8_t **poutbuf, int *poutbuf_size,
                        const uint8_t *buf, int buf_size);
    void (*parser_close)(AVCodecParserContext *s);
    int (*split)(AVCodecContext *avctx, const uint8_t *buf, int buf_size);
} AVCodecParser;

const AVCodecParser *av_parser_iterate(void **opaque);

AVCodecParserContext *av_parser_init(int codec_id);

int av_parser_parse2(AVCodecParserContext *s,
                     AVCodecContext *avctx,
                     uint8_t **poutbuf, int *poutbuf_size,
                     const uint8_t *buf, int buf_size,
                     int64_t pts, int64_t dts,
                     int64_t pos);

void av_parser_close(AVCodecParserContext *s);

int avcodec_encode_subtitle(AVCodecContext *avctx, uint8_t *buf, int buf_size,
                            const AVSubtitle *sub);

unsigned int avcodec_pix_fmt_to_codec_tag(enum AVPixelFormat pix_fmt);

enum AVPixelFormat avcodec_find_best_pix_fmt_of_list(const enum AVPixelFormat *pix_fmt_list,
                                            enum AVPixelFormat src_pix_fmt,
                                            int has_alpha, int *loss_ptr);

enum AVPixelFormat avcodec_default_get_format(struct AVCodecContext *s, const enum AVPixelFormat * fmt);

void avcodec_string(char *buf, int buf_size, AVCodecContext *enc, int encode);

int avcodec_default_execute(AVCodecContext *c, int (*func)(AVCodecContext *c2, void *arg2),void *arg, int *ret, int count, int size);
int avcodec_default_execute2(AVCodecContext *c, int (*func)(AVCodecContext *c2, void *arg2, int, int),void *arg, int *ret, int count);

int avcodec_fill_audio_frame(AVFrame *frame, int nb_channels,
                             enum AVSampleFormat sample_fmt, const uint8_t *buf,
                             int buf_size, int align);

void avcodec_flush_buffers(AVCodecContext *avctx);

int av_get_audio_frame_duration(AVCodecContext *avctx, int frame_bytes);

void av_fast_padded_malloc(void *ptr, unsigned int *size, size_t min_size);

void av_fast_padded_mallocz(void *ptr, unsigned int *size, size_t min_size);

int avcodec_is_open(AVCodecContext *s);

#define AVFORMAT_AVFORMAT_H

#define AVFORMAT_AVIO_H

#define AVFORMAT_VERSION_MAJOR_H

#define LIBAVFORMAT_VERSION_MAJOR  60

#define FF_API_COMPUTE_PKT_FIELDS2      (LIBAVFORMAT_VERSION_MAJOR < 61)
#define FF_API_GET_END_PTS              (LIBAVFORMAT_VERSION_MAJOR < 61)
#define FF_API_AVIODIRCONTEXT           (LIBAVFORMAT_VERSION_MAJOR < 61)
#define FF_API_AVFORMAT_IO_CLOSE        (LIBAVFORMAT_VERSION_MAJOR < 61)

#define FF_API_R_FRAME_RATE            1

#define AVIO_SEEKABLE_NORMAL (1 << 0)

#define AVIO_SEEKABLE_TIME   (1 << 1)

typedef struct AVIOInterruptCB {
    int (*callback)(void*);
    void *opaque;
} AVIOInterruptCB;

enum AVIODirEntryType {
    AVIO_ENTRY_UNKNOWN,
    AVIO_ENTRY_BLOCK_DEVICE,
    AVIO_ENTRY_CHARACTER_DEVICE,
    AVIO_ENTRY_DIRECTORY,
    AVIO_ENTRY_NAMED_PIPE,
    AVIO_ENTRY_SYMBOLIC_LINK,
    AVIO_ENTRY_SOCKET,
    AVIO_ENTRY_FILE,
    AVIO_ENTRY_SERVER,
    AVIO_ENTRY_SHARE,
    AVIO_ENTRY_WORKGROUP,
};

typedef struct AVIODirEntry {
    char *name;                           
    int type;                             
    int utf8;                             

    int64_t size;                         
    int64_t modification_timestamp;       

    int64_t access_timestamp;             

    int64_t status_change_timestamp;      

    int64_t user_id;                      
    int64_t group_id;                     
    int64_t filemode;                     
} AVIODirEntry;

typedef struct AVIODirContext {
    struct URLContext *url_context;
} AVIODirContext;

enum AVIODataMarkerType {
    
    AVIO_DATA_MARKER_HEADER,
    
    AVIO_DATA_MARKER_SYNC_POINT,
    
    AVIO_DATA_MARKER_BOUNDARY_POINT,
    
    AVIO_DATA_MARKER_UNKNOWN,
    
    AVIO_DATA_MARKER_TRAILER,
    
    AVIO_DATA_MARKER_FLUSH_POINT,
};

typedef struct AVIOContext {
    
    const AVClass *av_class;

    unsigned char *buffer;  
    int buffer_size;        
    unsigned char *buf_ptr; 
    unsigned char *buf_end; 

    void *opaque;           

    int (*read_packet)(void *opaque, uint8_t *buf, int buf_size);
    int (*write_packet)(void *opaque, uint8_t *buf, int buf_size);
    int64_t (*seek)(void *opaque, int64_t offset, int whence);
    int64_t pos;            
    int eof_reached;        
    int error;              
    int write_flag;         
    int max_packet_size;
    int min_packet_size;    

    unsigned long checksum;
    unsigned char *checksum_ptr;
    unsigned long (*update_checksum)(unsigned long checksum, const uint8_t *buf, unsigned int size);
    
    int (*read_pause)(void *opaque, int pause);
    
    int64_t (*read_seek)(void *opaque, int stream_index,
                         int64_t timestamp, int flags);
    
    int seekable;

    int direct;

    const char *protocol_whitelist;

    const char *protocol_blacklist;

    int (*write_data_type)(void *opaque, uint8_t *buf, int buf_size,
                           enum AVIODataMarkerType type, int64_t time);
    
    int ignore_boundary_point;

    unsigned char *buf_ptr_max;

    int64_t bytes_read;

    int64_t bytes_written;
} AVIOContext;

const char *avio_find_protocol_name(const char *url);

int avio_check(const char *url, int flags);

int avio_open_dir(AVIODirContext **s, const char *url, AVDictionary **options);

int avio_read_dir(AVIODirContext *s, AVIODirEntry **next);

int avio_close_dir(AVIODirContext **s);

void avio_free_directory_entry(AVIODirEntry **entry);

AVIOContext *avio_alloc_context(
                  unsigned char *buffer,
                  int buffer_size,
                  int write_flag,
                  void *opaque,
                  int (*read_packet)(void *opaque, uint8_t *buf, int buf_size),
                  int (*write_packet)(void *opaque, uint8_t *buf, int buf_size),
                  int64_t (*seek)(void *opaque, int64_t offset, int whence));

void avio_context_free(AVIOContext **s);

void avio_w8(AVIOContext *s, int b);
void avio_write(AVIOContext *s, const unsigned char *buf, int size);
void avio_wl64(AVIOContext *s, uint64_t val);
void avio_wb64(AVIOContext *s, uint64_t val);
void avio_wl32(AVIOContext *s, unsigned int val);
void avio_wb32(AVIOContext *s, unsigned int val);
void avio_wl24(AVIOContext *s, unsigned int val);
void avio_wb24(AVIOContext *s, unsigned int val);
void avio_wl16(AVIOContext *s, unsigned int val);
void avio_wb16(AVIOContext *s, unsigned int val);

int avio_put_str(AVIOContext *s, const char *str);

int avio_put_str16le(AVIOContext *s, const char *str);

int avio_put_str16be(AVIOContext *s, const char *str);

void avio_write_marker(AVIOContext *s, int64_t time, enum AVIODataMarkerType type);

#define AVSEEK_SIZE 0x10000

#define AVSEEK_FORCE 0x20000

int64_t avio_seek(AVIOContext *s, int64_t offset, int whence);

int64_t avio_skip(AVIOContext *s, int64_t offset);

static av_always_inline int64_t avio_tell(AVIOContext *s)
{
    return avio_seek(s, 0, SEEK_CUR);
}

int64_t avio_size(AVIOContext *s);

int avio_feof(AVIOContext *s);

int avio_vprintf(AVIOContext *s, const char *fmt, va_list ap);

int avio_printf(AVIOContext *s, const char *fmt, ...) av_printf_format(2, 3);

void avio_print_string_array(AVIOContext *s, const char *strings[]);

#define avio_print(s, ...) \
    avio_print_string_array(s, (const char*[]){__VA_ARGS__, NULL})

void avio_flush(AVIOContext *s);

int avio_read(AVIOContext *s, unsigned char *buf, int size);

int avio_read_partial(AVIOContext *s, unsigned char *buf, int size);

int          avio_r8  (AVIOContext *s);
unsigned int avio_rl16(AVIOContext *s);
unsigned int avio_rl24(AVIOContext *s);
unsigned int avio_rl32(AVIOContext *s);
uint64_t     avio_rl64(AVIOContext *s);
unsigned int avio_rb16(AVIOContext *s);
unsigned int avio_rb24(AVIOContext *s);
unsigned int avio_rb32(AVIOContext *s);
uint64_t     avio_rb64(AVIOContext *s);

int avio_get_str(AVIOContext *pb, int maxlen, char *buf, int buflen);

int avio_get_str16le(AVIOContext *pb, int maxlen, char *buf, int buflen);
int avio_get_str16be(AVIOContext *pb, int maxlen, char *buf, int buflen);

#define AVIO_FLAG_READ  1                                      
#define AVIO_FLAG_WRITE 2                                      
#define AVIO_FLAG_READ_WRITE (AVIO_FLAG_READ|AVIO_FLAG_WRITE)  

#define AVIO_FLAG_NONBLOCK 8

#define AVIO_FLAG_DIRECT 0x8000

int avio_open(AVIOContext **s, const char *url, int flags);

int avio_open2(AVIOContext **s, const char *url, int flags,
               const AVIOInterruptCB *int_cb, AVDictionary **options);

int avio_close(AVIOContext *s);

int avio_closep(AVIOContext **s);

int avio_open_dyn_buf(AVIOContext **s);

int avio_get_dyn_buf(AVIOContext *s, uint8_t **pbuffer);

int avio_close_dyn_buf(AVIOContext *s, uint8_t **pbuffer);

const char *avio_enum_protocols(void **opaque, int output);

const AVClass *avio_protocol_get_class(const char *name);

int     avio_pause(AVIOContext *h, int pause);

int64_t avio_seek_time(AVIOContext *h, int stream_index,
                       int64_t timestamp, int flags);

struct AVBPrint;

int avio_read_to_bprint(AVIOContext *h, struct AVBPrint *pb, size_t max_size);

int avio_accept(AVIOContext *s, AVIOContext **c);

int avio_handshake(AVIOContext *c);

#define AVFORMAT_VERSION_H

#define LIBAVFORMAT_VERSION_MINOR   3
#define LIBAVFORMAT_VERSION_MICRO 100

#define LIBAVFORMAT_VERSION_INT AV_VERSION_INT(LIBAVFORMAT_VERSION_MAJOR, \
                                               LIBAVFORMAT_VERSION_MINOR, \
                                               LIBAVFORMAT_VERSION_MICRO)
#define LIBAVFORMAT_VERSION     AV_VERSION(LIBAVFORMAT_VERSION_MAJOR,   \
                                           LIBAVFORMAT_VERSION_MINOR,   \
                                           LIBAVFORMAT_VERSION_MICRO)
#define LIBAVFORMAT_BUILD       LIBAVFORMAT_VERSION_INT

#define LIBAVFORMAT_IDENT       "Lavf" AV_STRINGIFY(LIBAVFORMAT_VERSION)

struct AVFormatContext;

struct AVDeviceInfoList;

int av_get_packet(AVIOContext *s, AVPacket *pkt, int size);

int av_append_packet(AVIOContext *s, AVPacket *pkt, int size);

struct AVCodecTag;

typedef struct AVProbeData {
    const char *filename;
    unsigned char *buf; 
    int buf_size;       
    const char *mime_type; 
} AVProbeData;

#define AVPROBE_SCORE_RETRY (AVPROBE_SCORE_MAX/4)
#define AVPROBE_SCORE_STREAM_RETRY (AVPROBE_SCORE_MAX/4-1)

#define AVPROBE_SCORE_EXTENSION  50 
#define AVPROBE_SCORE_MIME       75 
#define AVPROBE_SCORE_MAX       100 

#define AVPROBE_PADDING_SIZE 32             

#define AVFMT_NOFILE        0x0001
#define AVFMT_NEEDNUMBER    0x0002 

#define AVFMT_EXPERIMENTAL  0x0004
#define AVFMT_SHOW_IDS      0x0008 
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
    const char *name;
    
    const char *long_name;
    const char *mime_type;
    const char *extensions; 
    
    enum AVCodecID audio_codec;    
    enum AVCodecID video_codec;    
    enum AVCodecID subtitle_codec; 
    
    int flags;

    const struct AVCodecTag * const *codec_tag;

    const AVClass *priv_class; 
} AVOutputFormat;

typedef struct AVInputFormat {
    
    const char *name;

    const char *long_name;

    int flags;

    const char *extensions;

    const struct AVCodecTag * const *codec_tag;

    const AVClass *priv_class; 

    const char *mime_type;

    int raw_codec_id;

    int priv_data_size;

    int flags_internal;

    int (*read_probe)(const AVProbeData *);

    int (*read_header)(struct AVFormatContext *);

    int (*read_packet)(struct AVFormatContext *, AVPacket *pkt);

    int (*read_close)(struct AVFormatContext *);

    int (*read_seek)(struct AVFormatContext *,
                     int stream_index, int64_t timestamp, int flags);

    int64_t (*read_timestamp)(struct AVFormatContext *s, int stream_index,
                              int64_t *pos, int64_t pos_limit);

    int (*read_play)(struct AVFormatContext *);

    int (*read_pause)(struct AVFormatContext *);

    int (*read_seek2)(struct AVFormatContext *s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);

    int (*get_device_list)(struct AVFormatContext *s, struct AVDeviceInfoList *device_list);

} AVInputFormat;

enum AVStreamParseType {
    AVSTREAM_PARSE_NONE,
    AVSTREAM_PARSE_FULL,       
    AVSTREAM_PARSE_HEADERS,    
    AVSTREAM_PARSE_TIMESTAMPS, 
    AVSTREAM_PARSE_FULL_ONCE,  
    AVSTREAM_PARSE_FULL_RAW,   

};

typedef struct AVIndexEntry {
    int64_t pos;
    int64_t timestamp;        

#define AVINDEX_KEYFRAME 0x0001
#define AVINDEX_DISCARD_FRAME  0x0002    

    int flags:2;
    int size:30; 
    int min_distance;         
} AVIndexEntry;

#define AV_DISPOSITION_DEFAULT              (1 << 0)

#define AV_DISPOSITION_DUB                  (1 << 1)

#define AV_DISPOSITION_ORIGINAL             (1 << 2)

#define AV_DISPOSITION_COMMENT              (1 << 3)

#define AV_DISPOSITION_LYRICS               (1 << 4)

#define AV_DISPOSITION_KARAOKE              (1 << 5)

#define AV_DISPOSITION_FORCED               (1 << 6)

#define AV_DISPOSITION_HEARING_IMPAIRED     (1 << 7)

#define AV_DISPOSITION_VISUAL_IMPAIRED      (1 << 8)

#define AV_DISPOSITION_CLEAN_EFFECTS        (1 << 9)

#define AV_DISPOSITION_ATTACHED_PIC         (1 << 10)

#define AV_DISPOSITION_TIMED_THUMBNAILS     (1 << 11)

#define AV_DISPOSITION_NON_DIEGETIC         (1 << 12)

#define AV_DISPOSITION_CAPTIONS             (1 << 16)

#define AV_DISPOSITION_DESCRIPTIONS         (1 << 17)

#define AV_DISPOSITION_METADATA             (1 << 18)

#define AV_DISPOSITION_DEPENDENT            (1 << 19)

#define AV_DISPOSITION_STILL_IMAGE          (1 << 20)

int av_disposition_from_string(const char *disp);

const char *av_disposition_to_string(int disposition);

#define AV_PTS_WRAP_IGNORE      0   
#define AV_PTS_WRAP_ADD_OFFSET  1   
#define AV_PTS_WRAP_SUB_OFFSET  -1  

typedef struct AVStream {
    
    const AVClass *av_class;

    int index;    
    
    int id;

    AVCodecParameters *codecpar;

    void *priv_data;

    AVRational time_base;

    int64_t start_time;

    int64_t duration;

    int64_t nb_frames;                 

    int disposition;

    enum AVDiscard discard; 

    AVRational sample_aspect_ratio;

    AVDictionary *metadata;

    AVRational avg_frame_rate;

    AVPacket attached_pic;

    AVPacketSideData *side_data;
    
    int            nb_side_data;

    int event_flags;

#define AVSTREAM_EVENT_FLAG_METADATA_UPDATED 0x0001

#define AVSTREAM_EVENT_FLAG_NEW_PACKETS (1 << 1)

    AVRational r_frame_rate;

    int pts_wrap_bits;
} AVStream;

struct AVCodecParserContext *av_stream_get_parser(const AVStream *s);

attribute_deprecated
int64_t    av_stream_get_end_pts(const AVStream *st);

#define AV_PROGRAM_RUNNING 1

typedef struct AVProgram {
    int            id;
    int            flags;
    enum AVDiscard discard;        
    unsigned int   *stream_index;
    unsigned int   nb_stream_indexes;
    AVDictionary *metadata;

    int program_num;
    int pmt_pid;
    int pcr_pid;
    int pmt_version;

    int64_t start_time;
    int64_t end_time;

    int64_t pts_wrap_reference;    
    int pts_wrap_behavior;         
} AVProgram;

#define AVFMTCTX_NOHEADER      0x0001 

#define AVFMTCTX_UNSEEKABLE    0x0002 

typedef struct AVChapter {
    int64_t id;             
    AVRational time_base;   
    int64_t start, end;     
    AVDictionary *metadata;
} AVChapter;

typedef int (*av_format_control_message)(struct AVFormatContext *s, int type,
                                         void *data, size_t data_size);

typedef int (*AVOpenCallback)(struct AVFormatContext *s, AVIOContext **pb, const char *url, int flags,
                              const AVIOInterruptCB *int_cb, AVDictionary **options);

enum AVDurationEstimationMethod {
    AVFMT_DURATION_FROM_PTS,    
    AVFMT_DURATION_FROM_STREAM, 
    AVFMT_DURATION_FROM_BITRATE 
};

typedef struct AVFormatContext {
    
    const AVClass *av_class;

    const struct AVInputFormat *iformat;

    const struct AVOutputFormat *oformat;

    void *priv_data;

    AVIOContext *pb;

    int ctx_flags;

    unsigned int nb_streams;
    
    AVStream **streams;

    char *url;

    int64_t start_time;

    int64_t duration;

    int64_t bit_rate;

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
#define AVFMT_FLAG_SORT_DTS    0x10000 
#define AVFMT_FLAG_FAST_SEEK   0x80000 
#define AVFMT_FLAG_SHORTEST   0x100000 
#define AVFMT_FLAG_AUTO_BSF   0x200000 

    int64_t probesize;

    int64_t max_analyze_duration;

    const uint8_t *key;
    int keylen;

    unsigned int nb_programs;
    AVProgram **programs;

    enum AVCodecID video_codec_id;

    enum AVCodecID audio_codec_id;

    enum AVCodecID subtitle_codec_id;

    unsigned int max_index_size;

    unsigned int max_picture_buffer;

    unsigned int nb_chapters;
    AVChapter **chapters;

    AVDictionary *metadata;

    int64_t start_time_realtime;

    int fps_probe_size;

    int error_recognition;

    AVIOInterruptCB interrupt_callback;

    int debug;
#define FF_FDEBUG_TS        0x0001

    int64_t max_interleave_delta;

    int strict_std_compliance;

    int event_flags;

#define AVFMT_EVENT_FLAG_METADATA_UPDATED 0x0001

    int max_ts_probe;

    int avoid_negative_ts;
#define AVFMT_AVOID_NEG_TS_AUTO             -1 
#define AVFMT_AVOID_NEG_TS_DISABLED          0 
#define AVFMT_AVOID_NEG_TS_MAKE_NON_NEGATIVE 1 
#define AVFMT_AVOID_NEG_TS_MAKE_ZERO         2 

    int ts_id;

    int audio_preload;

    int max_chunk_duration;

    int max_chunk_size;

    int use_wallclock_as_timestamps;

    int avio_flags;

    enum AVDurationEstimationMethod duration_estimation_method;

    int64_t skip_initial_bytes;

    unsigned int correct_ts_overflow;

    int seek2any;

    int flush_packets;

    int probe_score;

    int format_probesize;

    char *codec_whitelist;

    char *format_whitelist;

    int io_repositioned;

    const AVCodec *video_codec;

    const AVCodec *audio_codec;

    const AVCodec *subtitle_codec;

    const AVCodec *data_codec;

    int metadata_header_padding;

    void *opaque;

    av_format_control_message control_message_cb;

    int64_t output_ts_offset;

    uint8_t *dump_separator;

    enum AVCodecID data_codec_id;

    char *protocol_whitelist;

    int (*io_open)(struct AVFormatContext *s, AVIOContext **pb, const char *url,
                   int flags, AVDictionary **options);

    attribute_deprecated
    void (*io_close)(struct AVFormatContext *s, AVIOContext *pb);

    char *protocol_blacklist;

    int max_streams;

    int skip_estimate_duration_from_pts;

    int max_probe_packets;

    int (*io_close2)(struct AVFormatContext *s, AVIOContext *pb);
} AVFormatContext;

void av_format_inject_global_side_data(AVFormatContext *s);

enum AVDurationEstimationMethod av_fmt_ctx_get_duration_estimation_method(const AVFormatContext* ctx);

unsigned avformat_version(void);

const char *avformat_configuration(void);

const char *avformat_license(void);

int avformat_network_init(void);

int avformat_network_deinit(void);

const AVOutputFormat *av_muxer_iterate(void **opaque);

const AVInputFormat *av_demuxer_iterate(void **opaque);

AVFormatContext *avformat_alloc_context(void);

void avformat_free_context(AVFormatContext *s);

const AVClass *avformat_get_class(void);

const AVClass *av_stream_get_class(void);

AVStream *avformat_new_stream(AVFormatContext *s, const AVCodec *c);

int av_stream_add_side_data(AVStream *st, enum AVPacketSideDataType type,
                            uint8_t *data, size_t size);

uint8_t *av_stream_new_side_data(AVStream *stream,
                                 enum AVPacketSideDataType type, size_t size);

uint8_t *av_stream_get_side_data(const AVStream *stream,
                                 enum AVPacketSideDataType type, size_t *size);

AVProgram *av_new_program(AVFormatContext *s, int id);

int avformat_alloc_output_context2(AVFormatContext **ctx, const AVOutputFormat *oformat,
                                   const char *format_name, const char *filename);

const AVInputFormat *av_find_input_format(const char *short_name);

const AVInputFormat *av_probe_input_format(const AVProbeData *pd, int is_opened);

const AVInputFormat *av_probe_input_format2(const AVProbeData *pd,
                                            int is_opened, int *score_max);

const AVInputFormat *av_probe_input_format3(const AVProbeData *pd,
                                            int is_opened, int *score_ret);

int av_probe_input_buffer2(AVIOContext *pb, const AVInputFormat **fmt,
                           const char *url, void *logctx,
                           unsigned int offset, unsigned int max_probe_size);

int av_probe_input_buffer(AVIOContext *pb, const AVInputFormat **fmt,
                          const char *url, void *logctx,
                          unsigned int offset, unsigned int max_probe_size);

int avformat_open_input(AVFormatContext **ps, const char *url,
                        const AVInputFormat *fmt, AVDictionary **options);

int avformat_find_stream_info(AVFormatContext *ic, AVDictionary **options);

AVProgram *av_find_program_from_stream(AVFormatContext *ic, AVProgram *last, int s);

void av_program_add_stream_index(AVFormatContext *ac, int progid, unsigned int idx);

int av_find_best_stream(AVFormatContext *ic,
                        enum AVMediaType type,
                        int wanted_stream_nb,
                        int related_stream,
                        const AVCodec **decoder_ret,
                        int flags);

int av_read_frame(AVFormatContext *s, AVPacket *pkt);

int av_seek_frame(AVFormatContext *s, int stream_index, int64_t timestamp,
                  int flags);

int avformat_seek_file(AVFormatContext *s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);

int avformat_flush(AVFormatContext *s);

int av_read_play(AVFormatContext *s);

int av_read_pause(AVFormatContext *s);

void avformat_close_input(AVFormatContext **s);

#define AVSEEK_FLAG_BACKWARD 1 
#define AVSEEK_FLAG_BYTE     2 
#define AVSEEK_FLAG_ANY      4 
#define AVSEEK_FLAG_FRAME    8 

#define AVSTREAM_INIT_IN_WRITE_HEADER 0 
#define AVSTREAM_INIT_IN_INIT_OUTPUT  1 

av_warn_unused_result
int avformat_write_header(AVFormatContext *s, AVDictionary **options);

av_warn_unused_result
int avformat_init_output(AVFormatContext *s, AVDictionary **options);

int av_write_frame(AVFormatContext *s, AVPacket *pkt);

int av_interleaved_write_frame(AVFormatContext *s, AVPacket *pkt);

int av_write_uncoded_frame(AVFormatContext *s, int stream_index,
                           AVFrame *frame);

int av_interleaved_write_uncoded_frame(AVFormatContext *s, int stream_index,
                                       AVFrame *frame);

int av_write_uncoded_frame_query(AVFormatContext *s, int stream_index);

int av_write_trailer(AVFormatContext *s);

const AVOutputFormat *av_guess_format(const char *short_name,
                                      const char *filename,
                                      const char *mime_type);

enum AVCodecID av_guess_codec(const AVOutputFormat *fmt, const char *short_name,
                              const char *filename, const char *mime_type,
                              enum AVMediaType type);

int av_get_output_timestamp(struct AVFormatContext *s, int stream,
                            int64_t *dts, int64_t *wall);

void av_hex_dump(FILE *f, const uint8_t *buf, int size);

void av_hex_dump_log(void *avcl, int level, const uint8_t *buf, int size);

void av_pkt_dump2(FILE *f, const AVPacket *pkt, int dump_payload, const AVStream *st);

void av_pkt_dump_log2(void *avcl, int level, const AVPacket *pkt, int dump_payload,
                      const AVStream *st);

enum AVCodecID av_codec_get_id(const struct AVCodecTag * const *tags, unsigned int tag);

unsigned int av_codec_get_tag(const struct AVCodecTag * const *tags, enum AVCodecID id);

int av_codec_get_tag2(const struct AVCodecTag * const *tags, enum AVCodecID id,
                      unsigned int *tag);

int av_find_default_stream_index(AVFormatContext *s);

int av_index_search_timestamp(AVStream *st, int64_t timestamp, int flags);

int avformat_index_get_entries_count(const AVStream *st);

const AVIndexEntry *avformat_index_get_entry(AVStream *st, int idx);

const AVIndexEntry *avformat_index_get_entry_from_timestamp(AVStream *st,
                                                            int64_t wanted_timestamp,
                                                            int flags);

int av_add_index_entry(AVStream *st, int64_t pos, int64_t timestamp,
                       int size, int distance, int flags);

void av_url_split(char *proto,         int proto_size,
                  char *authorization, int authorization_size,
                  char *hostname,      int hostname_size,
                  int *port_ptr,
                  char *path,          int path_size,
                  const char *url);

void av_dump_format(AVFormatContext *ic,
                    int index,
                    const char *url,
                    int is_output);

#define AV_FRAME_FILENAME_FLAGS_MULTIPLE 1 

int av_get_frame_filename2(char *buf, int buf_size,
                          const char *path, int number, int flags);

int av_get_frame_filename(char *buf, int buf_size,
                          const char *path, int number);

int av_filename_number_test(const char *filename);

int av_sdp_create(AVFormatContext *ac[], int n_files, char *buf, int size);

int av_match_ext(const char *filename, const char *extensions);

int avformat_query_codec(const AVOutputFormat *ofmt, enum AVCodecID codec_id,
                         int std_compliance);

const struct AVCodecTag *avformat_get_riff_video_tags(void);

const struct AVCodecTag *avformat_get_riff_audio_tags(void);

const struct AVCodecTag *avformat_get_mov_video_tags(void);

const struct AVCodecTag *avformat_get_mov_audio_tags(void);

AVRational av_guess_sample_aspect_ratio(AVFormatContext *format, AVStream *stream, AVFrame *frame);

AVRational av_guess_frame_rate(AVFormatContext *ctx, AVStream *stream, AVFrame *frame);

int avformat_match_stream_specifier(AVFormatContext *s, AVStream *st,
                                    const char *spec);

int avformat_queue_attached_pictures(AVFormatContext *s);

enum AVTimebaseSource {
    AVFMT_TBCF_AUTO = -1,
    AVFMT_TBCF_DECODER,
    AVFMT_TBCF_DEMUXER,
    AVFMT_TBCF_R_FRAMERATE,
};

int avformat_transfer_internal_stream_timing_info(const AVOutputFormat *ofmt,
                                                  AVStream *ost, const AVStream *ist,
                                                  enum AVTimebaseSource copy_tb);

AVRational av_stream_get_codec_timebase(const AVStream *st);

#define AVUTIL_FIFO_H

typedef struct AVFifo AVFifo;

typedef int AVFifoCB(void *opaque, void *buf, size_t *nb_elems);

#define AV_FIFO_FLAG_AUTO_GROW      (1 << 0)

AVFifo *av_fifo_alloc2(size_t elems, size_t elem_size,
                       unsigned int flags);

size_t av_fifo_elem_size(const AVFifo *f);

void av_fifo_auto_grow_limit(AVFifo *f, size_t max_elems);

size_t av_fifo_can_read(const AVFifo *f);

size_t av_fifo_can_write(const AVFifo *f);

int av_fifo_grow2(AVFifo *f, size_t inc);

int av_fifo_write(AVFifo *f, const void *buf, size_t nb_elems);

int av_fifo_write_from_cb(AVFifo *f, AVFifoCB read_cb,
                          void *opaque, size_t *nb_elems);

int av_fifo_read(AVFifo *f, void *buf, size_t nb_elems);

int av_fifo_read_to_cb(AVFifo *f, AVFifoCB write_cb,
                       void *opaque, size_t *nb_elems);

int av_fifo_peek(AVFifo *f, void *buf, size_t nb_elems, size_t offset);

int av_fifo_peek_to_cb(AVFifo *f, AVFifoCB write_cb, void *opaque,
                       size_t *nb_elems, size_t offset);

void av_fifo_drain2(AVFifo *f, size_t size);

void av_fifo_reset2(AVFifo *f);

void av_fifo_freep2(AVFifo **f);

typedef struct AVFifoBuffer {
    uint8_t *buffer;
    uint8_t *rptr, *wptr, *end;
    uint32_t rndx, wndx;
} AVFifoBuffer;

attribute_deprecated
AVFifoBuffer *av_fifo_alloc(unsigned int size);

attribute_deprecated
AVFifoBuffer *av_fifo_alloc_array(size_t nmemb, size_t size);

attribute_deprecated
void av_fifo_free(AVFifoBuffer *f);

attribute_deprecated
void av_fifo_freep(AVFifoBuffer **f);

attribute_deprecated
void av_fifo_reset(AVFifoBuffer *f);

attribute_deprecated
int av_fifo_size(const AVFifoBuffer *f);

attribute_deprecated
int av_fifo_space(const AVFifoBuffer *f);

attribute_deprecated
int av_fifo_generic_peek_at(AVFifoBuffer *f, void *dest, int offset, int buf_size, void (*func)(void*, void*, int));

attribute_deprecated
int av_fifo_generic_peek(AVFifoBuffer *f, void *dest, int buf_size, void (*func)(void*, void*, int));

attribute_deprecated
int av_fifo_generic_read(AVFifoBuffer *f, void *dest, int buf_size, void (*func)(void*, void*, int));

attribute_deprecated
int av_fifo_generic_write(AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int));

attribute_deprecated
int av_fifo_realloc2(AVFifoBuffer *f, unsigned int size);

attribute_deprecated
int av_fifo_grow(AVFifoBuffer *f, unsigned int additional_space);

attribute_deprecated
void av_fifo_drain(AVFifoBuffer *f, int size);

attribute_deprecated
static inline uint8_t *av_fifo_peek2(const AVFifoBuffer *f, int offs)
{
    uint8_t *ptr = f->rptr + offs;
    if (ptr >= f->end)
        ptr = f->buffer + (ptr - f->end);
    else if (ptr < f->buffer)
        ptr = f->end - (f->buffer - ptr);
    return ptr;
}

