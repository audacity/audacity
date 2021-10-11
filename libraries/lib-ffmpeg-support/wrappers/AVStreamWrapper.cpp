/**********************************************************************

  Audacity: A Digital Audio Editor

  AVStreamWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVStreamWrapper.h"

#include "FFmpegFunctions.h"

AVStreamWrapper::AVStreamWrapper(
   const FFmpegFunctions& ffmpeg, AVStream* wrapped) noexcept
    : mFFmpeg(ffmpeg)
    , mAVStream(wrapped)
{
}

AVStream* AVStreamWrapper::GetWrappedValue() noexcept
{
   return mAVStream;
}

const AVStream* AVStreamWrapper::GetWrappedValue() const noexcept
{
   return mAVStream;
}
