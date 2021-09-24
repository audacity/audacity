/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecContextWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVCodecContextWrapper.h"

#include <cstring>

#include "FFmpegFunctions.h"
#include "AVCodecWrapper.h"

AVCodecContextWrapper::AVCodecContextWrapper(
   const FFmpegFunctions& ffmpeg, std::unique_ptr<AVCodecWrapper> codec) noexcept
    : mFFmpeg(ffmpeg)
    , mAVCodec(std::move(codec))
    , mIsOwned(true)
{
   mAVCodecContext =
      mFFmpeg.avcodec_alloc_context3(mAVCodec->GetWrappedValue());
}

AVCodecContextWrapper::AVCodecContextWrapper(
   const FFmpegFunctions& ffmpeg, AVCodecContext* wrapped) noexcept
    : mFFmpeg(ffmpeg)
    , mAVCodecContext(wrapped)
    , mIsOwned(false)
{

}

AVCodecContext* AVCodecContextWrapper::GetWrappedValue() noexcept
{
    return mAVCodecContext;
}

const AVCodecContext* AVCodecContextWrapper::GetWrappedValue() const noexcept
{
   return mAVCodecContext;
}

AVCodecContextWrapper::~AVCodecContextWrapper()
{
   if (mIsOwned && mAVCodecContext != nullptr)
   {
      // avcodec_free_context, complementary to avcodec_alloc_context3, is
      // not necessarily loaded
      if (mFFmpeg.avcodec_free_context != nullptr)
      {
         mFFmpeg.avcodec_free_context(&mAVCodecContext);
      }
      else
      {
         // Its not clear how to avoid the leak here, but let's close
         // the codec at least
         if (mFFmpeg.avcodec_is_open(mAVCodecContext))
            mFFmpeg.avcodec_close(mAVCodecContext);
      }
   }
}

std::vector<uint8_t>
AVCodecContextWrapper::DecodeAudioPacket(const AVPacketWrapper* packet)
{
   auto frame = mFFmpeg.CreateAVFrameWrapper();
   std::vector<uint8_t> data;

   if (mFFmpeg.avcodec_decode_audio4)
   {
      std::unique_ptr<AVPacketWrapper> packetCopy =
         packet ? packet->Clone() : mFFmpeg.CreateAVPacketWrapper();

      /*
       "Flushing is done by calling this function [avcodec_decode_audio4]
       with packets with avpkt->data set to NULL and avpkt->size set to 0
       until it stops returning samples."
       (That implies, not necessarily just one loop pass to flush)
       */
      bool flushing = packet
         ? (packetCopy->GetSize() == 0 && packetCopy->GetData() == nullptr)
         : true;
      if (!flushing && packetCopy->GetData() == nullptr)
         return {};

      const int channels = GetChannels();

      int bytesDecoded = 0;
      do
      {
         int gotFrame;
         // Deprecated?  https://ffmpeg.org/doxygen/3.3/group__lavc__decoding.html#gaaa1fbe477c04455cdc7a994090100db4
         bytesDecoded = mFFmpeg.avcodec_decode_audio4(
            mAVCodecContext, frame->GetWrappedValue(), &gotFrame,
            packetCopy->GetWrappedValue());

         if (bytesDecoded < 0)
            return data; // Packet decoding has failed

         if (gotFrame == 0)
            /*
             "Note that this field being set to zero does not mean that an
             error has occurred. For decoders with AV_CODEC_CAP_DELAY set, no
             given decode call is guaranteed to produce a frame."
             */
            // (Let's assume this doesn't happen when flushing)
            continue;

         const auto sampleSize =
            static_cast<size_t>(mFFmpeg.av_get_bytes_per_sample(
               static_cast<AVSampleFormatFwd>(frame->GetFormat())));

         const auto samplesCount = frame->GetSamplesCount();
         const auto frameSize = channels * sampleSize * samplesCount;

         auto oldSize = data.size();
         data.resize(oldSize + frameSize);
         auto pData = &data[oldSize];

         if (frame->GetData(1) != nullptr)
         {
            // We return interleaved buffer
            for (int channel = 0; channel < channels; channel++)
            {
               for (int sample = 0; sample < samplesCount; sample++)
               {
                  const uint8_t* channelData =
                     frame->GetExtendedData(channel) + sampleSize * sample;

                  uint8_t* output =
                     pData + sampleSize * (channels * sample + channel);

                  std::copy(channelData, channelData + sampleSize, output);
               }
            }
         }
         else
         {
            uint8_t* frameData = frame->GetData(0);
            std::copy(frameData, frameData + frameSize, pData);
         }

         packetCopy->OffsetPacket(bytesDecoded);
      }
      while ( flushing ? bytesDecoded > 0 : packetCopy->GetSize() > 0 );
   }

   return data;
}

namespace
{
unsigned int MakeTag(char a, char b, char c, char d) noexcept
{
   return
      (static_cast<unsigned>(a) << 0) | (static_cast<unsigned>(b) << 8) |
      (static_cast<unsigned>(c) << 16) | (static_cast<unsigned>(d) << 24);
}
}

void AVCodecContextWrapper::SetCodecTagFourCC(const char* fourCC) noexcept
{
   if (fourCC == nullptr || std::strlen(fourCC) != 4)
      return;

   SetCodecTag(MakeTag(fourCC[0], fourCC[1], fourCC[2], fourCC[3]));
}
