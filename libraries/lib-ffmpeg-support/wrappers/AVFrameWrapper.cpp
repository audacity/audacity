/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFrameWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVFrameWrapper.h"

#include "FFmpegFunctions.h"

AVFrameWrapper::AVFrameWrapper(const FFmpegFunctions& ffmpeg) noexcept
    : mFFmpeg(ffmpeg)
{
   mAVFrame = mFFmpeg.av_frame_alloc();
}

AVFrame* AVFrameWrapper::GetWrappedValue() noexcept
{
   return mAVFrame;
}

const AVFrame* AVFrameWrapper::GetWrappedValue() const noexcept
{
   return mAVFrame;
}

AVFrameWrapper::~AVFrameWrapper()
{
   if (mAVFrame != nullptr)
      mFFmpeg.av_frame_free(&mAVFrame);
}
