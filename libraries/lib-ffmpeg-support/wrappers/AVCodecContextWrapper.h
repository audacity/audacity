/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecContextWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <memory>
#include <vector>
#include <cstdint>

#include "FFmpegTypes.h"

#include "SampleFormat.h"

struct FFmpegFunctions;
typedef struct AVCodecContext AVCodecContext;

class AVCodecWrapper;
class AVDictionaryWrapper;
class AVFrameWrapper;
class AVPacketWrapper;

class FFMPEG_SUPPORT_API AVCodecContextWrapper
{
public:
   AVCodecContextWrapper(const AVCodecContextWrapper&) = delete;
   AVCodecContextWrapper& operator=(AVCodecContextWrapper&) = delete;

   AVCodecContextWrapper(AVCodecContextWrapper&&) = delete;
   AVCodecContextWrapper& operator=(AVCodecContextWrapper&&) = delete;

   AVCodecContextWrapper(const FFmpegFunctions& ffmpeg, std::unique_ptr<AVCodecWrapper> codec) noexcept;
   AVCodecContextWrapper(const FFmpegFunctions& ffmpeg, AVCodecContext* wrapped) noexcept;

   AVCodecContext* GetWrappedValue() noexcept;
   const AVCodecContext* GetWrappedValue() const noexcept;

   virtual ~AVCodecContextWrapper();

   std::vector<uint8_t> DecodeAudioPacket(const AVPacketWrapper* packet);

   virtual sampleFormat GetPreferredAudacitySampleFormat() const noexcept = 0;

   virtual std::vector<int16_t> DecodeAudioPacketInt16(const AVPacketWrapper* packet) = 0;
   virtual std::vector<float> DecodeAudioPacketFloat(const AVPacketWrapper* packet) = 0;

   virtual int GetBitRate() const noexcept = 0;
   virtual void SetBitRate(int value) noexcept = 0;

   virtual uint64_t GetChannelLayout() const noexcept = 0;
   virtual void SetChannelLayout(uint64_t value) noexcept = 0;

   virtual int GetChannels() const noexcept = 0;
   virtual void SetChannels(int value) noexcept = 0;

   virtual const AVCodecWrapper* GetCodec() const noexcept = 0;

   virtual AVCodecIDFwd GetCodecId() const noexcept = 0;

   void SetCodecTagFourCC(const char* fourCC) noexcept;

   virtual void SetCodecTag(unsigned int tag) noexcept = 0;
   virtual unsigned int GetCodecTag() const noexcept = 0;

   virtual AVMediaTypeFwd GetCodecType() const noexcept = 0;

   virtual int GetCompressionLevel() const noexcept = 0;
   virtual void SetCompressionLevel(int value) noexcept = 0;

   virtual int GetCutoff() const noexcept = 0;
   virtual void SetCutoff(int value) noexcept = 0;

   virtual int GetFlags() const noexcept = 0;
   virtual void SetFlags(int value) noexcept = 0;

   virtual int GetFlags2() const noexcept = 0;
   virtual void SetFlags2(int value) noexcept = 0;

   virtual int GetFrameNumber() const noexcept = 0;
   virtual void SetFrameNumber(int value) noexcept = 0;

   virtual int GetFrameSize() const noexcept = 0;
   virtual void SetFrameSize(int value) noexcept = 0;

   virtual int GetGlobalQuality() const noexcept = 0;
   virtual void SetGlobalQuality(int value) noexcept = 0;

   virtual int GetProfile() const noexcept = 0;
   virtual void SetProfile(int value) noexcept = 0;

   virtual AVSampleFormatFwd GetSampleFmt() const noexcept = 0;
   virtual void SetSampleFmt(AVSampleFormatFwd value) noexcept = 0;

   virtual int GetSampleRate() const noexcept = 0;
   virtual void SetSampleRate(int value) noexcept = 0;

   virtual int GetStrictStdCompliance() const noexcept = 0;
   virtual void SetStrictStdCompliance(int value) noexcept = 0;

   virtual struct AudacityAVRational GetTimeBase() const noexcept = 0;
   virtual void SetTimeBase(struct AudacityAVRational value) noexcept = 0;

   /*!
    @param options   A dictionary filled with AVCodecContext and
    codec-private options. On return this object will be filled with
    options that were not found.

    @return zero if success, negative if error
    */
   virtual int Open(
      const AVCodecWrapper *codec, AVDictionaryWrapper *options = nullptr) = 0;

protected:
   const FFmpegFunctions& mFFmpeg;
   AVCodecContext* mAVCodecContext { nullptr };
   // May be created on demand to satisfy GetCodec():
   mutable std::unique_ptr<AVCodecWrapper> mAVCodec;

   bool mIsOwned { false };
};
