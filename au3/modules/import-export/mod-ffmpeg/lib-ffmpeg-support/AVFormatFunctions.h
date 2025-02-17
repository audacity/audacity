/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatFunctions.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>

#include "FFmpegTypes.h"

struct FFMPEG_SUPPORT_API AVFormatFunctions
{
    FFMPegVersion AVFormatVersion;

    int (*avformat_find_stream_info)(AVFormatContext* ic, AVDictionary** options) = nullptr;
    int (*av_read_frame)(AVFormatContext* s, AVPacket* pkt) = nullptr;
    int (*av_seek_frame)(AVFormatContext* s, int stream_index, int64_t timestamp, int flags) = nullptr;
    void (*avformat_close_input)(AVFormatContext** s) = nullptr;
    int (*avformat_write_header)(AVFormatContext* s, AVDictionary** options) = nullptr;
    int (*av_interleaved_write_frame)(AVFormatContext* s, AVPacket* pkt) = nullptr;
    AVOutputFormat*(*av_oformat_next)(const AVOutputFormat* f) = nullptr;
    AVStream*(*avformat_new_stream)(AVFormatContext* s, const AVCodec* c) = nullptr;
    AVFormatContext*(*avformat_alloc_context)(void) = nullptr;
    int (*av_write_trailer)(AVFormatContext* s) = nullptr;
    unsigned int (*av_codec_get_tag)(const struct AVCodecTag* const* tags, AVCodecIDFwd id) = nullptr;
    int (*avformat_open_input)(AVFormatContext** ic_ptr, const char* filename, const AVInputFormat* fmt, AVDictionary** options) = nullptr;
    int64_t (*avio_size)(AVIOContext* s) = nullptr;
    AVIOContext*(*avio_alloc_context)(unsigned char* buffer, int buffer_size, int write_flag, void* opaque,
                                      int (*read_packet)(void* opaque, uint8_t* buf, int buf_size),
                                      int (*write_packet)(void* opaque, const uint8_t* buf, int buf_size),
                                      int64_t (*seek)(void* opaque, int64_t offset, int whence)) = nullptr;
    AVOutputFormat*(*av_guess_format)(const char* short_name, const char* filename, const char* mime_type) = nullptr;
    void (*avformat_free_context)(AVFormatContext* s) = nullptr;

    // The following functions are not present in all library versions:
    void (*av_register_all)(void) = nullptr;
    void (*avio_context_free)(AVIOContext** s) = nullptr;
    const AVOutputFormat*(*av_muxer_iterate)(void** opaque);
};
