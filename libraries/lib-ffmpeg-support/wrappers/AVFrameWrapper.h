/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFrameWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>

#include "FFmpegTypes.h"

struct FFmpegFunctions;
class AVDictionaryWrapper;
typedef struct AVFrame AVFrame;

#define FF_DECODE_ERROR_INVALID_BITSTREAM 1
#define FF_DECODE_ERROR_MISSING_REFERENCE 2

class FFMPEG_SUPPORT_API AVFrameWrapper
{
public:
   AVFrameWrapper(const AVFrameWrapper&) = delete;
   AVFrameWrapper& operator=(AVFrameWrapper&) = delete;

   AVFrameWrapper(AVFrameWrapper&&) = delete;
   AVFrameWrapper& operator=(AVFrameWrapper&&) = delete;

   explicit AVFrameWrapper(const FFmpegFunctions& ffmpeg) noexcept;

   AVFrame* GetWrappedValue() noexcept;
   const AVFrame* GetWrappedValue() const noexcept;

   virtual ~AVFrameWrapper();

   virtual int GetNumDataPointers() const noexcept = 0;
   virtual uint8_t* GetData(int index) const noexcept = 0;
   virtual int GetLineSize(int index) const noexcept = 0;
   virtual uint64_t GetError(int index) const noexcept = 0;
   virtual uint8_t* GetExtendedData(int index) const noexcept = 0;

   virtual int GetWidth() const noexcept = 0;
   virtual int GetHeight() const noexcept = 0;

   virtual int GetSamplesCount() const noexcept = 0;
   virtual void SetSamplesCount(int count) noexcept = 0;

   virtual AVSampleFormatFwd GetFormat() const noexcept = 0;
   virtual void SetFormat(AVSampleFormatFwd format) noexcept = 0;

   virtual int GetKeyFrame() const noexcept = 0;

   virtual AudacityAVRational GetSampleAspectRatio() const noexcept = 0;
   virtual int64_t GetPresentationTimestamp() const noexcept = 0;
   virtual int64_t GetPacketPresentationTimestamp() const noexcept = 0;
   virtual int64_t GetPacketDecompressionTimestamp() const noexcept = 0;
   virtual int GetCodedPictureNumber() const noexcept = 0;
   virtual int GetDisplayPictureNumber() const noexcept = 0;
   virtual int GetQuality() const noexcept = 0;

   virtual void* GetOpaque() const noexcept = 0;
   virtual void SetOpaque(void *opaque) noexcept = 0;

   virtual int GetRepeatPict() const noexcept = 0;
   virtual int GetInterlacedFrame() const noexcept = 0;
   virtual int GetTopFieldFirst() const noexcept = 0;
   virtual int GetPaletteHasChanged() const noexcept = 0;
   virtual int64_t GetReorderedOpaque() const noexcept = 0;
   virtual int GetSampleRate() const noexcept = 0;

   virtual uint64_t GetChannelLayout() const noexcept = 0;
   virtual void SetChannelLayout(uint64_t layout) noexcept = 0;

   virtual int GetSideDataCount() const noexcept = 0;
   virtual int GetFlags() const noexcept = 0;
   virtual int64_t GetBestEffortTimestamp() const noexcept = 0;
   virtual int64_t GetPacketPos() const noexcept = 0;
   virtual int64_t GetPacketDuration() const noexcept = 0;
   virtual AVDictionaryWrapper GetMetadata() const noexcept = 0;
   virtual int GetDecodeErrorFlags() const noexcept = 0;
   virtual int GetChannels() const noexcept = 0;
   virtual int GetPacketSize() const noexcept = 0;
protected:
   const FFmpegFunctions& mFFmpeg;
   AVFrame* mAVFrame { nullptr };
};
