/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecCapabilitiesQuery.inl

  Dmitry Vedenko

**********************************************************************/
class AVCodecCapabilities
{
public:
   static const AVRational* GetSupportedFramerates(
      const FFmpegFunctions& ffmpeg, const AVCodec* codec) noexcept
   {
      return GetSupportedConfig<AVRational>(
         ffmpeg, codec, AV_CODEC_CONFIG_FRAME_RATE);
   }

   static const AVPixelFormatFwd* GetPixFmts(
      const FFmpegFunctions& ffmpeg, const AVCodec* codec) noexcept
   {
      static_assert(sizeof(AVPixelFormat) == sizeof(AVPixelFormatFwd));
      return GetSupportedConfig<AVPixelFormatFwd>(
         ffmpeg, codec, AV_CODEC_CONFIG_PIX_FORMAT);
   }

   static const int* GetSupportedSamplerates(
      const FFmpegFunctions& ffmpeg, const AVCodec* codec) noexcept
   {
      return GetSupportedConfig<int>(
         ffmpeg, codec, AV_CODEC_CONFIG_SAMPLE_RATE);
   }

   static const AVSampleFormatFwd* GetSampleFmts(
      const FFmpegFunctions& ffmpeg, const AVCodec* codec) noexcept
   {
      static_assert(sizeof(AVSampleFormat) == sizeof(AVSampleFormatFwd));
      return GetSupportedConfig<AVSampleFormatFwd>(
         ffmpeg, codec, AV_CODEC_CONFIG_SAMPLE_FORMAT);
   }

private:
   template<typename T>
   static const T* GetSupportedConfig(
      const FFmpegFunctions& ffmpeg, const AVCodec* codec,
      AVCodecConfigFwd config) noexcept
   {
      const void* values = nullptr;
      if (codec != nullptr
          && ffmpeg.avcodec_get_supported_config != nullptr
          && ffmpeg.avcodec_get_supported_config(
             nullptr, codec, config, 0, &values, nullptr) >= 0)
         return static_cast<const T*>(values);

      return nullptr;
   }
};
