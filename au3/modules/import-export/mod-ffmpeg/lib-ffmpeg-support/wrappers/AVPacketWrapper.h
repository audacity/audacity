/**********************************************************************

  Audacity: A Digital Audio Editor

  AVPacketWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include "../FFmpegTypes.h"

#include <memory>

struct FFmpegFunctions;
typedef struct AVPacket AVPacket;

class FFMPEG_SUPPORT_API AVPacketWrapper
{
public:
    AVPacketWrapper(const AVPacketWrapper&) = delete;
    AVPacketWrapper& operator=(AVPacketWrapper&) = delete;

    AVPacketWrapper(AVPacketWrapper&&) = delete;
    AVPacketWrapper& operator=(AVPacketWrapper&&) = delete;

    explicit AVPacketWrapper(const FFmpegFunctions& ffmpeg) noexcept;

    AVPacket* GetWrappedValue() noexcept;
    const AVPacket* GetWrappedValue() const noexcept;

    virtual ~AVPacketWrapper();

    virtual AudacityAVBufferRef* GetBuf() const noexcept = 0;

    virtual int64_t GetPresentationTimestamp() const noexcept = 0;
    virtual int64_t GetDecompressionTimestamp() const noexcept = 0;

    virtual int GetDuration() const noexcept = 0;

    virtual void RescalePresentationTimestamp(AudacityAVRational bq, AudacityAVRational cq) noexcept = 0;
    virtual void RescaleDecompressionTimestamp(AudacityAVRational bq, AudacityAVRational cq) noexcept = 0;
    virtual void RescaleDuration(AudacityAVRational bq, AudacityAVRational cq) noexcept = 0;

    virtual uint8_t* GetData() const noexcept = 0;
    virtual int GetSize() const noexcept = 0;

    virtual bool OffsetPacket(size_t offset) noexcept = 0;
    virtual void ResetData() noexcept = 0;
    virtual void ResetTimestamps() noexcept = 0;

    virtual int GetStreamIndex() const noexcept = 0;
    virtual void SetStreamIndex(int index) noexcept = 0;

    virtual int GetFlags() const noexcept = 0;
    virtual int64_t GetPos() const noexcept = 0;
    virtual int64_t GetConvergenceDuration() const noexcept = 0;

    //! @post return value is not null
    virtual std::unique_ptr<AVPacketWrapper> Clone() const noexcept = 0;

protected:
    const FFmpegFunctions& mFFmpeg;
    AVPacket* mAVPacket { nullptr };

    bool mUseAVFree { true };
};
