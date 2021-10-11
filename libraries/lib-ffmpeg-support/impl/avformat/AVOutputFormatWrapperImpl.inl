/**********************************************************************

  Audacity: A Digital Audio Editor

  AVOutputFormatWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

class AVOutputFormatWrapperImpl : public AVOutputFormatWrapper
{
public:
   explicit AVOutputFormatWrapperImpl(AVOutputFormat* wrapped)
      : AVOutputFormatWrapper(wrapped)
   {
   }

   const char* GetName() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->name;

      return {};
   }

   const char* GetLongName() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->long_name;

      return {};
   }

   const char* GetMimeType() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->mime_type;

      return {};
   }

   const char* GetExtensions() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->extensions;

      return {};
   }

   AVCodecIDFwd GetAudioCodec() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->audio_codec;

      return {};
   }

   int GetFlags() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->flags;

      return {};
   }

   const struct AVCodecTag* const* GetCodecTag() const noexcept override
   {
      if (mAVOutputFormat != nullptr)
         return mAVOutputFormat->codec_tag;

      return {};
   }
};

std::unique_ptr<AVOutputFormatWrapper> CreateAVOutputFormatWrapper (AVOutputFormat* wrapped)
{
   return std::make_unique<AVOutputFormatWrapperImpl>(wrapped);
}
