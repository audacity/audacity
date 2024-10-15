/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecFunctions.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>

#include "FFmpegTypes.h"

typedef struct AVCodecParameters AVCodecParameters;

struct FFMPEG_SUPPORT_API AVCodecFunctions
{
   FFMPegVersion      AVCodecVersion;

   void               (*av_packet_ref) (AVPacket* dst, const AVPacket* src) = nullptr;
   void               (*av_packet_unref) (AVPacket* pkt) = nullptr;
   void               (*av_init_packet) (AVPacket *pkt) = nullptr;
   AVCodec*           (*avcodec_find_encoder) (AVCodecIDFwd id) = nullptr;
   AVCodec*           (*avcodec_find_encoder_by_name) (const char *name) = nullptr;
   AVCodec*           (*avcodec_find_decoder) (AVCodecIDFwd id) = nullptr;
   const char*        (*avcodec_get_name) (AVCodecIDFwd id) = nullptr;
   int                (*avcodec_open2) (AVCodecContext *avctx, const AVCodec *codec, AVDictionary **options) = nullptr;
   int                (*avcodec_is_open) (AVCodecContext *avctx) = nullptr;
   int                (*avcodec_close) (AVCodecContext *avctx) = nullptr;
   AVCodecContext*    (*avcodec_alloc_context3) (const AVCodec* codec) = nullptr;
   int                (*av_codec_is_encoder) (const AVCodec *codec) = nullptr;
   int                (*avcodec_fill_audio_frame) (AVFrame *frame, int nb_channels, AVSampleFormatFwd sample_fmt, const uint8_t *buf, int buf_size, int align) = nullptr;

   // The following functions are not present in all library versions:
   AVPacket*          (*av_packet_alloc) () = nullptr;
   void               (*av_packet_free)(AVPacket** pkt) = nullptr;
   void               (*avcodec_free_context) (AVCodecContext** avctx) = nullptr;
   int                (*avcodec_parameters_to_context)(AVCodecContext* codec, const AVCodecParameters* par) = nullptr;
   int                (*avcodec_parameters_from_context)(AVCodecParameters *par, const AVCodecContext *codec)= nullptr;
   int                (*avcodec_decode_audio4) (AVCodecContext *avctx, AVFrame *frame, int *got_output, const AVPacket *avpkt) = nullptr;
   int                (*avcodec_encode_audio2) (AVCodecContext *avctx, AVPacket *pkt, const AVFrame *frame, int *got_output) = nullptr;
   void               (*avcodec_register_all)(void) = nullptr;
   AVCodec*           (*av_codec_next)(const AVCodec* c) = nullptr;
   const AVCodec*     (*av_codec_iterate)(void** opaque) = nullptr;

   // New API for decoding and encoding. Audacity will preffer it when available.
   int                (*avcodec_send_packet)(AVCodecContext* avctx, const AVPacket* avpkt) = nullptr;
   int                (*avcodec_receive_frame)(AVCodecContext* avctx, AVFrame* frame) = nullptr;
   int                (*avcodec_send_frame)(AVCodecContext* avctx, const AVFrame* frame) = nullptr;
   int                (*avcodec_receive_packet)(AVCodecContext* avctx, AVPacket* avpkt) = nullptr;
};
