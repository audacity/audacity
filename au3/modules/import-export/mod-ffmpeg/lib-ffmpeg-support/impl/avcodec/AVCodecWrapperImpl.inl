/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/
class AVCodecWrapperImpl : public AVCodecWrapper
{
public:
   explicit
   AVCodecWrapperImpl(const FFmpegFunctions& ffmpeg, const AVCodec* wrapped) noexcept
      : AVCodecWrapper(wrapped)
      , mFFmpeg(ffmpeg)
   {
   }

   const char* GetName() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->name;

      return {};
   }

   const char* GetLongName() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->long_name;

      return {};
   }

   AVMediaTypeFwd GetType() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->type;

      return {};
   }

   AVCodecIDFwd GetId() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->id;

      return {};
   }

   int GetCapabilities() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->capabilities;

      return {};
   }

   const AVRational* GetSupportedFramerates() const noexcept override
   {
      return AVCodecCapabilities::GetSupportedFramerates(mFFmpeg, mAVCodec);
   }

   const AVPixelFormatFwd* GetPixFmts() const noexcept override
   {
      return AVCodecCapabilities::GetPixFmts(mFFmpeg, mAVCodec);
   }

   const int* GetSupportedSamplerates() const noexcept override
   {
      return AVCodecCapabilities::GetSupportedSamplerates(mFFmpeg, mAVCodec);
   }

   const AVSampleFormatFwd* GetSampleFmts() const noexcept override
   {
      return AVCodecCapabilities::GetSampleFmts(mFFmpeg, mAVCodec);
   }

   uint8_t GetMaxLowres() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->max_lowres;

      return {};
   }

   bool IsAudio() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->type == AVMEDIA_TYPE_AUDIO;

      return {};
   }

private:
   const FFmpegFunctions& mFFmpeg;
};

std::unique_ptr<AVCodecWrapper>CreateAVCodecWrapper(
   const FFmpegFunctions& ffmpeg, const AVCodec* obj)
{
   return std::make_unique<AVCodecWrapperImpl>(ffmpeg, obj);
}
