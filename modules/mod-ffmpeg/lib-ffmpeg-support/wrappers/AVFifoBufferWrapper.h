/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFifoBufferWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

struct FFmpegFunctions;
typedef struct AVFifoBuffer AVFifoBuffer;

class FFMPEG_SUPPORT_API AVFifoBufferWrapper
{
public:
   AVFifoBufferWrapper(const AVFifoBufferWrapper&) = delete;
   AVFifoBufferWrapper& operator=(AVFifoBufferWrapper&) = delete;

   AVFifoBufferWrapper(AVFifoBufferWrapper&&) = delete;
   AVFifoBufferWrapper& operator=(AVFifoBufferWrapper&&) = delete;

   AVFifoBufferWrapper(
      const FFmpegFunctions& ffmpeg, int size) noexcept;

   AVFifoBuffer* GetWrappedValue() noexcept;
   const AVFifoBuffer* GetWrappedValue() const noexcept;

   virtual ~AVFifoBufferWrapper();

protected:
   const FFmpegFunctions& mFFmpeg;
   AVFifoBuffer* mAVFifoBuffer { nullptr };
};
