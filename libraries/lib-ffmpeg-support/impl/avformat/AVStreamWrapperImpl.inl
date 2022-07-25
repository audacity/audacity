/**********************************************************************

  Audacity: A Digital Audio Editor

  AVStreamWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

class AVStreamWrapperImpl : public AVStreamWrapper
{
public:
   AVStreamWrapperImpl(
      const FFmpegFunctions& ffmpeg, AVStream* wrapped, bool forEncoding)
       : AVStreamWrapper(ffmpeg, wrapped)
       , mForEncoding(forEncoding)
   {
   }

   int GetIndex() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->index;

      return {};
   }

   int GetId() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->id;

      return {};
   }

   void SetId(int id) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->id = id;
   }

   AudacityAVRational GetTimeBase() const noexcept override
   {
      if (mAVStream != nullptr)
         return { mAVStream->time_base.num, mAVStream->time_base.den };

      return {};
   }

   void SetTimeBase(AudacityAVRational time_base) noexcept override
   {
      if (mAVStream == nullptr)
         return;

      mAVStream->time_base.num = time_base.num;
      mAVStream->time_base.den = time_base.den;
   }

   int64_t GetStartTime() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->start_time;

      return {};
   }

   void SetStartTime(int64_t start_time) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->start_time = start_time;
   }

   int64_t GetDuration() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->duration;

      return {};
   }

   void SetDuration(int64_t duration) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->duration = duration;
   }

   int64_t GetFramesCount() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->nb_frames;

      return {};
   }

   void SetFramesCount(int64_t nb_frames) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->nb_frames = nb_frames;
   }

   int GetDisposition() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->disposition;

      return {};
   }

   void SetDisposition(int disposition) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->disposition = disposition;
   }

   AVSampleFormatFwd GetDiscard() const noexcept override
   {
      if (mAVStream != nullptr)
         return mAVStream->discard;

      return {};
   }

   void SetDiscard(AVDiscardFwd discard) noexcept override
   {
      if (mAVStream != nullptr)
         mAVStream->discard = static_cast<AVDiscard>(discard);
   }

   AudacityAVRational GetSampleAspectRatio() const noexcept override
   {
      if (mAVStream != nullptr)
         return { mAVStream->sample_aspect_ratio.num,
                  mAVStream->sample_aspect_ratio.den };

      return {};
   }

   void SetSampleAspectRatio(
      AudacityAVRational sample_aspect_ratio) noexcept override
   {
      if (mAVStream == nullptr)
         return;

      mAVStream->sample_aspect_ratio.num = sample_aspect_ratio.num;
      mAVStream->sample_aspect_ratio.den = sample_aspect_ratio.den;
   }

   AVDictionaryWrapper GetMetadata() const noexcept override
   {
      if (mAVStream != nullptr)
         return AVDictionaryWrapper(mFFmpeg, mAVStream->metadata);

      return AVDictionaryWrapper(mFFmpeg);
   }

   void SetMetadata(AVDictionaryWrapper metadata) noexcept override
   {
      if (mAVStream != nullptr) {
         // Destroy any previous metadata
         if (mAVStream->metadata != nullptr)
            mFFmpeg.av_dict_free(&mAVStream->metadata);

         // Does this leak or will the stream destroy the dictionary too?
         // Not stated here: https://ffmpeg.org/doxygen/trunk/structAVStream.html
         // But see: https://ffmpeg.org/doxygen/trunk/libavformat_2utils_8c_source.html#l00635
         // And where that function is called: https://ffmpeg.org/doxygen/trunk/libavformat_2utils_8c.html#af8a54ade3869125802dab7fdcd8688b3
         // So all is well IF this stream has an AVFormatContext owner
         // But note that AVStreamWrapper itself does not free resources
         // in its destructor!
         mAVStream->metadata = metadata.Release();
      }
   }

   bool IsAudio() const noexcept override
   {
      if (mAVStream != nullptr)
#if LIBAVFORMAT_VERSION_MAJOR <= 58
         return mAVStream->codec->codec_type == AVMEDIA_TYPE_AUDIO;
#else
         return mAVStream->codecpar->codec_type == AVMEDIA_TYPE_AUDIO;
#endif

      return {};
   }

   AVCodecIDFwd GetAVCodecID() const noexcept override
   {
      if (mAVStream != nullptr)
#if LIBAVFORMAT_VERSION_MAJOR <= 58
         return mAVStream->codec->codec_id;
#else
         return mAVStream->codecpar->codec_id;
#endif

      return AV_CODEC_ID_NONE;
   }

   std::unique_ptr<AVCodecContextWrapper>
   GetAVCodecContext() const noexcept override
   {
      if (mAVStream == nullptr)
         return {};
      
#if LIBAVFORMAT_VERSION_MAJOR <= 58
      return mFFmpeg.CreateAVCodecContextWrapper(mAVStream->codec);
#else
      // No memory management is involved here!
      auto avcodec = mForEncoding ?
                        mFFmpeg.avcodec_find_encoder(mAVStream->codecpar->codec_id) :
                        mFFmpeg.avcodec_find_decoder(mAVStream->codecpar->codec_id);

      AVCodecContext* codecContext = mFFmpeg.avcodec_alloc_context3(avcodec);

      if (codecContext == nullptr)
         return {};

      auto context = mFFmpeg.CreateAVCodecContextWrapper(codecContext);

      if (!mForEncoding)
      {
         auto ret = mFFmpeg.avcodec_parameters_to_context(
            codecContext, mAVStream->codecpar);

         if (ret < 0)
            return {};
      }
      
      return context;
#endif
   }

   int SetParametersFromContext(AVCodecContextWrapper& context) noexcept
   {
#if LIBAVFORMAT_VERSION_MAJOR <= 58
      return 0;
#else
      return mFFmpeg.avcodec_parameters_from_context(
         mAVStream->codecpar, context.GetWrappedValue());
#endif
   }

private:
   const bool mForEncoding;
};

std::unique_ptr<AVStreamWrapper>
CreateAVStreamWrapper(const FFmpegFunctions& ffmpeg, AVStream* wrapped, bool forEncoding)
{
   return std::make_unique<AVStreamWrapperImpl>(
      ffmpeg, wrapped, forEncoding);
}
