/**********************************************************************

  Audacity: A Digital Audio Editor

  AVStreamWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <memory>

#include "../FFmpegTypes.h"

struct FFmpegFunctions;
class AVDictionaryWrapper;
typedef struct AVStream AVStream;
typedef struct AVCodecContext AVCodecContext;

class AVCodecContextWrapper;

class FFMPEG_SUPPORT_API AVStreamWrapper
{
public:
    AVStreamWrapper(const AVStreamWrapper&) = delete;
    AVStreamWrapper& operator=(AVStreamWrapper&) = delete;

    AVStreamWrapper(AVStreamWrapper&&) = delete;
    AVStreamWrapper& operator=(AVStreamWrapper&&) = delete;

    AVStreamWrapper(const FFmpegFunctions& ffmpeg, AVStream* wrapped) noexcept;

    AVStream* GetWrappedValue() noexcept;
    const AVStream* GetWrappedValue() const noexcept;

    virtual ~AVStreamWrapper() = default;

    virtual int GetIndex() const noexcept = 0;

    virtual int GetId() const noexcept = 0;
    virtual void SetId(int id) noexcept = 0;

    virtual AudacityAVRational GetTimeBase() const noexcept = 0;
    virtual void SetTimeBase(AudacityAVRational time_base) noexcept = 0;

    virtual int64_t GetStartTime() const noexcept = 0;
    virtual void SetStartTime(int64_t start_time) noexcept = 0;

    virtual int64_t GetDuration() const noexcept = 0;
    virtual void SetDuration(int64_t duration) noexcept = 0;

    virtual int64_t GetFramesCount() const noexcept = 0;
    virtual void SetFramesCount(int64_t nb_frames) noexcept = 0;

    virtual int GetDisposition() const noexcept = 0;
    virtual void SetDisposition(int disposition) noexcept = 0;

    virtual AVSampleFormatFwd GetDiscard() const noexcept = 0;
    virtual void SetDiscard(AVDiscardFwd discard) noexcept = 0;

    virtual AudacityAVRational GetSampleAspectRatio() const noexcept = 0;
    virtual void SetSampleAspectRatio(AudacityAVRational sample_aspect_ratio) noexcept = 0;

    virtual AVDictionaryWrapper GetMetadata() const noexcept = 0;
    virtual void SetMetadata(AVDictionaryWrapper metadata) noexcept = 0;

    virtual bool IsAudio() const noexcept = 0;

    virtual AVCodecIDFwd GetAVCodecID() const noexcept = 0;

    virtual std::unique_ptr<AVCodecContextWrapper> GetAVCodecContext() const noexcept = 0;

    virtual int SetParametersFromContext(AVCodecContextWrapper& context) noexcept = 0;

protected:
    const FFmpegFunctions& mFFmpeg;
    AVStream* mAVStream { nullptr };
};
