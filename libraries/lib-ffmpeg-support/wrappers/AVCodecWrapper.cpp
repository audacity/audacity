/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVCodecWrapper.h"

#include "FFmpegFunctions.h"

AVCodecWrapper::AVCodecWrapper(AVCodec* wrapped) noexcept
    : mAVCodec(wrapped)
{
}

AVCodec* AVCodecWrapper::GetWrappedValue() noexcept
{
   return mAVCodec;
}

const AVCodec* AVCodecWrapper::GetWrappedValue() const noexcept
{
   return mAVCodec;
}
