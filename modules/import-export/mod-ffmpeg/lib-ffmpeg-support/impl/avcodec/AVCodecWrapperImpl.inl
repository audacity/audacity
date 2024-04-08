/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/
class AVCodecWrapperImpl : public AVCodecWrapper
{
public:
   explicit
   AVCodecWrapperImpl(const AVCodec* wrapped) noexcept
      : AVCodecWrapper(wrapped)
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
      if (mAVCodec != nullptr)
         return mAVCodec->supported_framerates;

      return {};
   }

   const AVPixelFormatFwd* GetPixFmts() const noexcept override
   {
      static_assert(sizeof(AVPixelFormat) == sizeof(AVPixelFormatFwd));
      if (mAVCodec != nullptr)
         return reinterpret_cast<const AVPixelFormatFwd*>(mAVCodec->pix_fmts);

      return {};
   }

   const int* GetSupportedSamplerates() const noexcept override
   {
      if (mAVCodec != nullptr)
         return mAVCodec->supported_samplerates;

      return {};
   }

   const AVSampleFormatFwd* GetSampleFmts() const noexcept override
   {
      static_assert(sizeof(AVSampleFormat) == sizeof(AVSampleFormatFwd));

      if (mAVCodec != nullptr)
         return reinterpret_cast<const AVSampleFormatFwd*>(mAVCodec->sample_fmts);

      return {};
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
};

std::unique_ptr<AVCodecWrapper>CreateAVCodecWrapper(const AVCodec* obj)
{
   return std::make_unique<AVCodecWrapperImpl>(obj);
}
