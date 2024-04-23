/**********************************************************************

  Audacity: A Digital Audio Editor

  AVCodecContextWrapper.inl

  Dmitry Vedenko

**********************************************************************/

template<typename ResultType>
struct Converters
{};

template <> struct Converters<float>
{
   static float Convert(uint8_t value)
   {
      return static_cast<float>((value - 0x80) / static_cast<double>(1u << 7));
   }

   static float Convert(int16_t value)
   {
      return static_cast<float>(value / static_cast<double>(1u << 15));
   }

   static float Convert(int32_t value)
   {
      return static_cast<float>(value / static_cast<double>(1u << 31));
   }

   static float Convert(int64_t value)
   {
      return static_cast<float>(value / static_cast<double>(1ull << 63));
   }

   static float Convert(float value)
   {
      return value;
   }

   static float Convert(double value)
   {
      return static_cast<float>(value);
   }
};

template <>
struct Converters<int16_t>
{
   static int16_t Convert(uint8_t value)
   {
      return (value - 0x80) << 8;
   }

   static int16_t Convert(int16_t value)
   {
      return value;
   }

   static int16_t Convert(int32_t value)
   {
      return Convert(Converters<float>::Convert(value));
   }

   static int16_t Convert(int64_t value)
   {
      return Convert(Converters<float>::Convert(value));
   }

   static int16_t Convert(float value)
   {
      long intValue = lrintf(value * (1 << 15));

      intValue = std::clamp<long>(
         intValue,
         std::numeric_limits<int16_t>::min(),
         std::numeric_limits<int16_t>::max()
      );

      return static_cast<int16_t>(intValue);
   }

   static int16_t Convert(double value)
   {
      long intValue = lrint(value * (1 << 15));

      intValue = std::clamp<long>(
         intValue, std::numeric_limits<int16_t>::min(),
         std::numeric_limits<int16_t>::max());

      return static_cast<int16_t>(intValue);
   }
};

template<typename OutputType, typename InputType>
std::vector<OutputType> Convert(const void* rawData, const size_t dataSize)
{
   const size_t samplesCount = dataSize / sizeof(InputType);

   std::vector<OutputType> output;
   output.reserve(samplesCount);

   const InputType* currentSample = static_cast<const InputType*>(rawData);

   for (int sample = 0; sample < samplesCount; ++sample)
   {
      output.push_back(Converters<OutputType>::Convert(*currentSample));
      ++currentSample;
   }

   return output;
}

class AVCodecContextWrapperImpl : public AVCodecContextWrapper
{
public:
   AVCodecContextWrapperImpl(const FFmpegFunctions& ffmpeg, std::unique_ptr<AVCodecWrapper> codec) noexcept
      : AVCodecContextWrapper(ffmpeg, std::move(codec))
   {
   }

   AVCodecContextWrapperImpl(const FFmpegFunctions& ffmpeg, AVCodecContext* wrapped)
      : AVCodecContextWrapper(ffmpeg, wrapped)
   {
      if(mAVCodecContext == nullptr)
         return;

      if (mFFmpeg.av_codec_is_encoder(mAVCodecContext->codec))
         mAVCodec = mFFmpeg.CreateEncoder(mAVCodecContext->codec_id);
      else
         mAVCodec = mFFmpeg.CreateDecoder(mAVCodecContext->codec_id);
   }

   int GetBitRate() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         // May truncate int64_t to int.  But who uses such high rates, really?
         return mAVCodecContext->bit_rate;

      return {};
   }

   void SetBitRate(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->bit_rate = value;
   }

   const AVChannelLayoutWrapper* GetChannelLayout() const noexcept override
   {
      return GetChannelLayoutSafe();
   }

   void SetChannelLayout(const AVChannelLayoutWrapper* layout) noexcept override
   {
      if (mAVCodecContext == nullptr || layout == nullptr)
         return;

      mChannelLayoutWrapper = layout->Clone();
// Clone never returns nullptr
#if HAS_AV_CHANNEL_LAYOUT
      mAVCodecContext->ch_layout = *layout->GetChannelLayout();
#else
      mAVCodecContext->channel_layout = layout->GetLegacyChannelLayout();
      mAVCodecContext->channels       = layout->GetChannelsCount();
#endif
   }

   int GetChannels() const noexcept override
   {
      if (auto layout = GetChannelLayoutSafe(); layout != nullptr)
         return layout->GetChannelsCount();

      return {};
   }

   const AVCodecWrapper* GetCodec() const noexcept override
   {
      if (!mAVCodec && mAVCodecContext && mAVCodecContext->codec)
      {
         if (mFFmpeg.av_codec_is_encoder(mAVCodecContext->codec))
            mAVCodec = mFFmpeg.CreateEncoder(mAVCodecContext->codec_id);
         else
            mAVCodec = mFFmpeg.CreateDecoder(mAVCodecContext->codec_id);
      }

      return mAVCodec.get();
   }

   AVCodecIDFwd GetCodecId() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->codec_id;

      return {};
   }

   void SetCodecTag(unsigned int tag) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->codec_tag = tag;
   }

   unsigned int GetCodecTag() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->codec_tag;

      return {};
   }

   AVMediaTypeFwd GetCodecType() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->codec_type;

      return {};
   }

   int GetCompressionLevel() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->compression_level;

      return {};
   }

   void SetCompressionLevel(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->compression_level = value;
   }

   int GetCutoff() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->cutoff;

      return {};
   }

   void SetCutoff(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->cutoff = value;
   }

   int GetFlags() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->flags;

      return {};
   }

   void SetFlags(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->flags = value;
   }

   int GetFlags2() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->flags2;

      return {};
   }

   void SetFlags2(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->flags2 = value;
   }

   int GetFrameNumber() const noexcept override
   {
#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(61, 0, 0)
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->frame_number;
#else
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->frame_num;
#endif

      return {};
   }

   int GetFrameSize() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->frame_size;

      return {};
   }

   void SetFrameSize(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->frame_size = value;
   }

   int GetGlobalQuality() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->global_quality;

      return {};
   }

   void SetGlobalQuality(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->global_quality = value;
   }

   int GetProfile() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->profile;

      return {};
   }

   void SetProfile(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->profile = value;
   }

   AVSampleFormatFwd GetSampleFmt() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->sample_fmt;

      return {};
   }

   void SetSampleFmt(AVSampleFormatFwd value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->sample_fmt = static_cast<AVSampleFormat>(value);
   }

   int GetSampleRate() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->sample_rate;

      return {};
   }

   void SetSampleRate(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->sample_rate = value;
   }

   int GetStrictStdCompliance() const noexcept override
   {
      if (mAVCodecContext != nullptr)
         return mAVCodecContext->strict_std_compliance;

      return {};
   }

   void SetStrictStdCompliance(int value) noexcept override
   {
      if (mAVCodecContext != nullptr)
         mAVCodecContext->strict_std_compliance = value;
   }

   struct AudacityAVRational GetTimeBase() const noexcept override
   {
      if (mAVCodecContext != nullptr)
      {
         return { mAVCodecContext->time_base.num,
                  mAVCodecContext->time_base.den };
      }

      return {};
   }

   void SetTimeBase(struct AudacityAVRational value) noexcept override
   {
      if (mAVCodecContext != nullptr)
      {
         mAVCodecContext->time_base.num = value.num;
         mAVCodecContext->time_base.den = value.den;
      };
   }

   sampleFormat GetPreferredAudacitySampleFormat() const noexcept override
   {
      if (mAVCodecContext == nullptr)
         return int16Sample;

      switch (mAVCodecContext->sample_fmt)
      {
      case AV_SAMPLE_FMT_U8:
      case AV_SAMPLE_FMT_U8P:
      case AV_SAMPLE_FMT_S16:
      case AV_SAMPLE_FMT_S16P:
         return int16Sample;
      default:
         return floatSample;
      }
      return floatSample;
   }

   std::vector<int16_t> DecodeAudioPacketInt16(const AVPacketWrapper* packet) override
   {
      if (mAVCodecContext == nullptr)
         return {};

      const auto rawData = DecodeAudioPacket(packet);

      switch (mAVCodecContext->sample_fmt)
      {
      case AV_SAMPLE_FMT_U8:
      case AV_SAMPLE_FMT_U8P:
         return Convert<int16_t, uint8_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_S16:
      case AV_SAMPLE_FMT_S16P:
         return Convert<int16_t, int16_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_S32:
      case AV_SAMPLE_FMT_S32P:
         return Convert<int16_t, int32_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_FLT:
      case AV_SAMPLE_FMT_FLTP:
         return Convert<int16_t, float>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_DBL:
      case AV_SAMPLE_FMT_DBLP:
         return Convert<int16_t, double>(rawData.data(), rawData.size());
#if LIBAVFORMAT_VERSION_MAJOR >= 58
      case AV_SAMPLE_FMT_S64:
      case AV_SAMPLE_FMT_S64P:
         return Convert<int16_t, int64_t>(rawData.data(), rawData.size());
#endif
      default:
         return {};
      }
   }

   std::vector<float> DecodeAudioPacketFloat(const AVPacketWrapper* packet) override
   {
      if (mAVCodecContext == nullptr)
         return {};

      const auto rawData = DecodeAudioPacket(packet);

      switch (mAVCodecContext->sample_fmt)
      {
      case AV_SAMPLE_FMT_U8:
      case AV_SAMPLE_FMT_U8P:
         return Convert<float, uint8_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_S16:
      case AV_SAMPLE_FMT_S16P:
         return Convert<float, int16_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_S32:
      case AV_SAMPLE_FMT_S32P:
         return Convert<float, int32_t>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_FLT:
      case AV_SAMPLE_FMT_FLTP:
         return Convert<float, float>(rawData.data(), rawData.size());
      case AV_SAMPLE_FMT_DBL:
      case AV_SAMPLE_FMT_DBLP:
         return Convert<float, double>(rawData.data(), rawData.size());
#if LIBAVFORMAT_VERSION_MAJOR >= 58
      case AV_SAMPLE_FMT_S64:
      case AV_SAMPLE_FMT_S64P:
         return Convert<float, int64_t>(rawData.data(), rawData.size());
#endif
      default:
         return {};
      }
   }

   int Open( const AVCodecWrapper *codec, AVDictionaryWrapper *options )
   override
   {
      if (mAVCodecContext == nullptr)
         return {};

      AVDictionary *dictionary = options ? options->Release() : nullptr;

      int result = mFFmpeg.avcodec_open2(mAVCodecContext,
         codec ? codec->GetWrappedValue() : nullptr,
         dictionary ? &dictionary : nullptr);

      if (options)
         *options = AVDictionaryWrapper{ mFFmpeg, dictionary };

      return result;
   }
   const AVChannelLayoutWrapper* GetChannelLayoutSafe() const noexcept
   {
      if (mAVCodecContext == nullptr)
         return nullptr;

      if (mChannelLayoutWrapper == nullptr)
      {
#if HAS_AV_CHANNEL_LAYOUT
         mChannelLayoutWrapper =
            mFFmpeg.CreateAVChannelLayout(&mAVCodecContext->ch_layout);
#else
         mChannelLayoutWrapper = mFFmpeg.CreateLegacyChannelLayout(
            mAVCodecContext->channel_layout, mAVCodecContext->channels);
#endif
      }

      return mChannelLayoutWrapper.get();
   }

   mutable std::unique_ptr<AVChannelLayoutWrapper> mChannelLayoutWrapper;
};

std::unique_ptr<AVCodecContextWrapper> CreateAVCodecContextWrapperFromCodec(
   const FFmpegFunctions& fns, std::unique_ptr<AVCodecWrapper> codec)
{
   return std::make_unique<AVCodecContextWrapperImpl>(fns, std::move(codec));
}

std::unique_ptr<AVCodecContextWrapper> CreateAVCodecContextWrapper(
   const FFmpegFunctions& fns, AVCodecContext* context)
{
   return std::make_unique<AVCodecContextWrapperImpl>(fns, context);
}
