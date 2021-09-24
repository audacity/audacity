/**********************************************************************

  Audacity: A Digital Audio Editor

  AVUtilFunctions.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdarg>
#include <cstdint>

#include "FFmpegTypes.h"

struct FFMPEG_SUPPORT_API AVUtilFunctions
{
   FFMPegVersion      AVUtilVersion;

   void*              (*av_malloc) (size_t size) = nullptr;
   void               (*av_free) (void *ptr) = nullptr;
   void               (*av_dict_free) (AVDictionary **m) = nullptr;
   AudacityAVDictionaryEntry*
                      (*av_dict_get) (const AVDictionary *m, const char *key, const AudacityAVDictionaryEntry *prev, int flags) = nullptr;
   int                (*av_dict_set) (AVDictionary **pm, const char *key, const char *value, int flags) = nullptr;
   void               (*av_dict_copy) (AVDictionary** dst, const AVDictionary* src, int flags) = nullptr;
   int                (*av_get_bytes_per_sample) (AVSampleFormatFwd sample_fmt) = nullptr;
   void               (*av_log_set_callback) (void (*cb)(void*, int, const char*, va_list)) = nullptr;
   void               (*av_log_default_callback) (void* ptr, int level, const char* fmt, va_list vl) = nullptr;
   AVFifoBuffer*      (*av_fifo_alloc) (unsigned int size) = nullptr;
   int                (*av_fifo_generic_read) (AVFifoBuffer *f, void *buf, int buf_size, void (*func)(void*, void*, int)) = nullptr;
   int                (*av_fifo_realloc2) (AVFifoBuffer *f, unsigned int size) = nullptr;
   void               (*av_fifo_free) (AVFifoBuffer *f) = nullptr;
   int                (*av_fifo_size) (const AVFifoBuffer *f) = nullptr;
   int                (*av_fifo_generic_write) (AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int)) = nullptr;
   int64_t            (*av_rescale_q) (int64_t a, AudacityAVRational bq, AudacityAVRational cq) = nullptr;
   AVFrame*           (*av_frame_alloc) (void) = nullptr;
   void               (*av_frame_free) (AVFrame **frame) = nullptr;
   int                (*av_samples_get_buffer_size) (int *linesize, int nb_channels, int nb_samples, AVSampleFormatFwd sample_fmt, int align) = nullptr;
   int64_t            (*av_get_default_channel_layout) (int nb_channels) = nullptr;
   int                (*av_strerror) (int errnum, char *errbuf, size_t errbuf_size) = nullptr;
   int                (*av_get_channel_layout_nb_channels)(uint64_t channel_layout) = nullptr;
};
