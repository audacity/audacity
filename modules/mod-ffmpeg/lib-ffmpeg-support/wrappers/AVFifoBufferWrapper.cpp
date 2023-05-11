/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFifoBufferWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVFifoBufferWrapper.h"

#include "FFmpegFunctions.h"


AVFifoBufferWrapper::AVFifoBufferWrapper(
   const FFmpegFunctions& ffmpeg, int size) noexcept
    : mFFmpeg(ffmpeg)
{
   mAVFifoBuffer = mFFmpeg.av_fifo_alloc(size);
}

AVFifoBuffer* AVFifoBufferWrapper::GetWrappedValue() noexcept
{
   return mAVFifoBuffer;
}

const AVFifoBuffer* AVFifoBufferWrapper::GetWrappedValue() const noexcept
{
   return mAVFifoBuffer;
}

AVFifoBufferWrapper::~AVFifoBufferWrapper()
{
   mFFmpeg.av_fifo_free(mAVFifoBuffer);
}
