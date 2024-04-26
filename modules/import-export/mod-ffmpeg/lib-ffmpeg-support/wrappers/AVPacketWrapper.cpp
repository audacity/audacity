/**********************************************************************

  Audacity: A Digital Audio Editor

  AVPacketWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVPacketWrapper.h"

#include "FFmpegFunctions.h"

AVPacketWrapper::AVPacketWrapper(const FFmpegFunctions& ffmpeg) noexcept
    : mFFmpeg(ffmpeg)
{
}

AVPacket* AVPacketWrapper::GetWrappedValue() noexcept
{
   return mAVPacket;
}

const AVPacket* AVPacketWrapper::GetWrappedValue() const noexcept
{
   return mAVPacket;
}

AVPacketWrapper::~AVPacketWrapper()
{
   if (mAVPacket != nullptr)
   {
      if (!mUseAVFree)
      {
         mFFmpeg.av_packet_free(&mAVPacket);
      }
      else
      {
         mFFmpeg.av_packet_unref(mAVPacket);
         mFFmpeg.av_free(mAVPacket);
      }
   }
}
