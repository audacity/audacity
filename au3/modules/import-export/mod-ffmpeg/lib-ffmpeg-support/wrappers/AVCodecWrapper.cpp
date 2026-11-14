/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVCodecWrapper.h"

#include "../FFmpegFunctions.h"

AVCodecWrapper::AVCodecWrapper(const AVCodec* wrapped) noexcept
    : mAVCodec(wrapped)
{
}

const AVCodec* AVCodecWrapper::GetWrappedValue() const noexcept
{
    return mAVCodec;
}
