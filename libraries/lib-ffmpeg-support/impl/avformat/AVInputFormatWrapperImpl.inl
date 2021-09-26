/**********************************************************************

  Audacity: A Digital Audio Editor

  AVInputFormatWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

class AVInputFormatWrapperImpl : public AVInputFormatWrapper
{
public:
   AVInputFormatWrapperImpl(AVInputFormat* wrapped)
      : AVInputFormatWrapper(wrapped)
   {}

   const char* GetName() const noexcept override
   {
      if (mAVInputFormat != nullptr)
         return mAVInputFormat->name;

      return {};
   }

   const char* GetLongName() const noexcept override
   {
      if (mAVInputFormat != nullptr)
         return mAVInputFormat->long_name;

      return {};
   }

   int GetFlags() const noexcept override
   {
      if (mAVInputFormat != nullptr)
         return mAVInputFormat->flags;

      return {};
   }

   const char* GetExtensions() const noexcept override
   {
      if (mAVInputFormat != nullptr)
         return mAVInputFormat->extensions;

      return {};
   }

   const struct AVCodecTag* const* GetCodecTag() const noexcept override
   {
      if (mAVInputFormat != nullptr)
         return mAVInputFormat->codec_tag;

      return {};
   }
};

std::unique_ptr<AVInputFormatWrapper> CreateAVInputFormatWrapper (AVInputFormat* wrapped)
{
   return std::make_unique<AVInputFormatWrapperImpl>(wrapped);
}
