/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecCapabilitiesLegacy.inl

  Dmitry Vedenko

**********************************************************************/
class AVCodecCapabilities
{
public:
   static const AVRational* GetSupportedFramerates(
      const FFmpegFunctions&, const AVCodec* codec) noexcept
   {
      return codec != nullptr ? codec->supported_framerates : nullptr;
   }

   static const AVPixelFormatFwd* GetPixFmts(
      const FFmpegFunctions&, const AVCodec* codec) noexcept
   {
      static_assert(sizeof(AVPixelFormat) == sizeof(AVPixelFormatFwd));
      return codec != nullptr
         ? reinterpret_cast<const AVPixelFormatFwd*>(codec->pix_fmts)
         : nullptr;
   }

   static const int* GetSupportedSamplerates(
      const FFmpegFunctions&, const AVCodec* codec) noexcept
   {
      return codec != nullptr ? codec->supported_samplerates : nullptr;
   }

   static const AVSampleFormatFwd* GetSampleFmts(
      const FFmpegFunctions&, const AVCodec* codec) noexcept
   {
      static_assert(sizeof(AVSampleFormat) == sizeof(AVSampleFormatFwd));
      return codec != nullptr
         ? reinterpret_cast<const AVSampleFormatFwd*>(codec->sample_fmts)
         : nullptr;
   }
};
