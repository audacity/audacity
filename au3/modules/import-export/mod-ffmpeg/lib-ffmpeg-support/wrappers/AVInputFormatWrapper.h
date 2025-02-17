/**********************************************************************

  Audacity: A Digital Audio Editor

  AVInputFormatWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

struct FFmpegFunctions;
typedef struct AVInputFormat AVInputFormat;

class FFMPEG_SUPPORT_API AVInputFormatWrapper
{
public:
    AVInputFormatWrapper(const AVInputFormatWrapper&) = delete;
    AVInputFormatWrapper& operator=(AVInputFormatWrapper&) = delete;

    AVInputFormatWrapper(AVInputFormatWrapper&&) = delete;
    AVInputFormatWrapper& operator=(AVInputFormatWrapper&&) = delete;

    explicit AVInputFormatWrapper(AVInputFormat* wrapped) noexcept;

    AVInputFormat* GetWrappedValue() noexcept;
    const AVInputFormat* GetWrappedValue() const noexcept;

    virtual ~AVInputFormatWrapper() = default;

    virtual const char* GetName() const noexcept = 0;
    virtual const char* GetLongName() const noexcept = 0;
    virtual int GetFlags() const noexcept = 0;
    virtual const char* GetExtensions() const noexcept = 0;
    virtual const struct AVCodecTag* const* GetCodecTag() const noexcept = 0;
protected:
    AVInputFormat* mAVInputFormat { nullptr };
};
