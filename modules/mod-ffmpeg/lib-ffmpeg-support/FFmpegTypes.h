/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegTypes.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <errno.h>

#define AUDACITY_MKTAG(a, b, c, d) ((a) | ((b) << 8) | ((c) << 16) | ((unsigned)(d) << 24))
#define AUDACITY_FFERRTAG(a, b, c, d) (-(int)AUDACITY_MKTAG(a, b, c, d))

#if EDOM > 0
#   define AUDACITY_AVERROR(e) \
      (-(e)) ///< Returns a negative error code from a POSIX error code, to
             ///< return from library functions.
#   define AUDACITY_AVUNERROR(e) \
      (-(e)) ///< Returns a POSIX error code from a library function error
             ///< return value.
#else
/* Some platforms have E* and errno already negated. */
#   define AUDACITY_AVERROR(e) (e)
#   define AUDACITY_AVUNERROR(e) (e)
#endif

#define AUDACITY_AVERROR_EOF AUDACITY_FFERRTAG('E', 'O', 'F', ' ') 

#define AUDACITY_AVFMT_NOFILE 0x0001
/*
#define AVFMT_NEEDNUMBER 0x0002
#define AVFMT_SHOW_IDS 0x0008
#define AVFMT_RAWPICTURE 0x0020
 */
#define AUDACITY_AVFMT_GLOBALHEADER 0x0040
/*
#define AVFMT_NOTIMESTAMPS 0x0080
#define AVFMT_GENERIC_INDEX 0x0100
#define AVFMT_TS_DISCONT 0x0200
#define AVFMT_VARIABLE_FPS 0x0400
#define AVFMT_NODIMENSIONS 0x0800
#define AVFMT_NOSTREAMS 0x1000
#define AVFMT_NOBINSEARCH 0x2000
#define AVFMT_NOGENSEARCH 0x4000
#define AVFMT_NO_BYTE_SEEK 0x8000
#define AVFMT_ALLOW_FLUSH 0x10000
#define AVFMT_TS_NONSTRICT 0x20000
#define AVFMT_TS_NEGATIVE 0x40000
#define AVFMT_SEEK_TO_PTS 0x4000000

#define AVFMT_FLAG_GENPTS 0x0001
#define AVFMT_FLAG_IGNIDX 0x0002
#define AVFMT_FLAG_NONBLOCK 0x0004
#define AVFMT_FLAG_IGNDTS 0x0008
#define AVFMT_FLAG_NOFILLIN 0x0010
#define AVFMT_FLAG_NOPARSE 0x0020
#define AVFMT_FLAG_NOBUFFER 0x0040
#define AVFMT_FLAG_CUSTOM_IO 0x0080
#define AVFMT_FLAG_DISCARD_CORRUPT 0x0100
#define AVFMT_FLAG_FLUSH_PACKETS 0x0200

#define AVFMT_FLAG_BITEXACT 0x0400
#define AVFMT_FLAG_MP4A_LATM 0x8000
#define AVFMT_FLAG_SORT_DTS 0x10000
#define AVFMT_FLAG_PRIV_OPT 0x20000
#define AVFMT_FLAG_KEEP_SIDE_DATA 0x40000
*/

#define AUDACITY_AV_NOPTS_VALUE ((int64_t)UINT64_C(0x8000000000000000))

#ifndef AV_VERSION_INT
#define AV_VERSION_INT(a, b, c) (a << 16 | b << 8 | c)
#endif

#define AUDACITY_AV_TIME_BASE (1000 * 1000)

#define AUDACITY_AV_CODEC_FLAG_QSCALE (1 << 1)

#define AUDACITY_AV_CODEC_CAP_SMALL_LAST_FRAME    (1 <<  6)


//#define FF_LAMBDA_SHIFT 7
//#define FF_LAMBDA_SCALE (1 << FF_LAMBDA_SHIFT)
#define AUDACITY_FF_QP2LAMBDA 118
//#define FF_LAMBDA_MAX (256 * 128 - 1)

/*
#define FF_COMPLIANCE_VERY_STRICT 2
#define FF_COMPLIANCE_STRICT 1
#define FF_COMPLIANCE_NORMAL 0
#define FF_COMPLIANCE_UNOFFICIAL -1
 */
#define AUDACITY_FF_COMPLIANCE_EXPERIMENTAL -2

/*
#define FF_PROFILE_AAC_MAIN 0
 */
#define AUDACITY_FF_PROFILE_AAC_LOW 1
/*
#define FF_PROFILE_AAC_SSR 2
#define FF_PROFILE_AAC_LTP 3
#define FF_PROFILE_AAC_HE 4
#define FF_PROFILE_AAC_HE_V2 28
#define FF_PROFILE_AAC_LD 22
#define FF_PROFILE_AAC_ELD 38
#define FF_PROFILE_MPEG2_AAC_LOW 128
#define FF_PROFILE_MPEG2_AAC_HE 131
 */

#define AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER (1 << 22)

typedef struct AVDictionary AVDictionary;
typedef struct AVFifoBuffer AVFifoBuffer;
typedef struct AVFrame AVFrame;
typedef struct AVFormatContext AVFormatContext;
typedef struct AVPacket AVPacket;
typedef struct AVOutputFormat AVOutputFormat;
typedef struct AVStream AVStream;
typedef struct AVCodec AVCodec;
typedef struct AVInputFormat AVInputFormat;
typedef struct AVIOContext AVIOContext;
typedef struct AVCodecContext AVCodecContext;

using AVCodecIDFwd = int;
using AVMediaTypeFwd = int;
using AVPixelFormatFwd = int;
using AVSampleFormatFwd = int;
using AVDiscardFwd = int;

// Stable structures and emums

struct AudacityAVRational {
   int num { 0 }; ///< numerator
   int den { 0 }; ///< denominator
};

//! Simply an overlay of AVDictionaryEntry, but we avoid including that type
struct AudacityAVDictionaryEntry {
   char* key { nullptr };
   char* value { nullptr };
};

enum AudacityAVSampleFormat {
    AUDACITY_AV_SAMPLE_FMT_NONE = -1,
    AUDACITY_AV_SAMPLE_FMT_U8,          ///< unsigned 8 bits
    AUDACITY_AV_SAMPLE_FMT_S16,         ///< signed 16 bits
    AUDACITY_AV_SAMPLE_FMT_S32,         ///< signed 32 bits
    AUDACITY_AV_SAMPLE_FMT_FLT,         ///< float
    AUDACITY_AV_SAMPLE_FMT_DBL,         ///< double

    AUDACITY_AV_SAMPLE_FMT_U8P,         ///< unsigned 8 bits, planar
    AUDACITY_AV_SAMPLE_FMT_S16P,        ///< signed 16 bits, planar
    AUDACITY_AV_SAMPLE_FMT_S32P,        ///< signed 32 bits, planar
    AUDACITY_AV_SAMPLE_FMT_FLTP,        ///< float, planar
    AUDACITY_AV_SAMPLE_FMT_DBLP,        ///< double, planar
    AUDACITY_AV_SAMPLE_FMT_S64,
    AUDACITY_AV_SAMPLE_FMT_S64P,

    AUDACITY_AV_SAMPLE_FMT_NB           ///< Number of sample formats. DO NOT USE if linking dynamically
};

struct FFMPegVersion final
{
   unsigned Major { 0 };
   unsigned Minor { 0 };
   unsigned Micro { 0 };

   unsigned GetIntVersion() const noexcept
   {
      return AV_VERSION_INT(Major, Minor, Micro);
   }
};

typedef struct AVBuffer AVBuffer;

struct AudacityAVBufferRef
{
   AVBuffer* buffer;

   uint8_t* data;

   int size;
};
