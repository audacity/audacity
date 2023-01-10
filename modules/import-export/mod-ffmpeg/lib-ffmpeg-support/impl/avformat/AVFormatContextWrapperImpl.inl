/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatContextWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

//! Some checks for correct overlaying.
// There is no AVDictionaryWrapper.inl, so put it here
static_assert(
   sizeof(AudacityAVDictionaryEntry) == sizeof(AVDictionaryEntry) &&
   offsetof(AudacityAVDictionaryEntry, key) == offsetof(AVDictionaryEntry, key)
      &&
   offsetof(AudacityAVDictionaryEntry, value) == offsetof(AVDictionaryEntry,  value),
   "AudacityAVDictionaryEntry does not safely overlay AVDictionaryEntry"
);

//! More sanity checks of macro constants
static_assert(
   AV_DICT_MATCH_CASE == DICT_MATCH_CASE
   && AV_DICT_IGNORE_SUFFIX == DICT_IGNORE_SUFFIX
   && AV_NOPTS_VALUE == AUDACITY_AV_NOPTS_VALUE
   && AV_TIME_BASE == AUDACITY_AV_TIME_BASE
   && FF_QP2LAMBDA == AUDACITY_FF_QP2LAMBDA
   && FF_PROFILE_AAC_LOW == AUDACITY_FF_PROFILE_AAC_LOW
   && AVFMT_NOFILE == AUDACITY_AVFMT_NOFILE
   && AVFMT_GLOBALHEADER == AUDACITY_AVFMT_GLOBALHEADER
   && FF_COMPLIANCE_EXPERIMENTAL == AUDACITY_FF_COMPLIANCE_EXPERIMENTAL
   && AV_SAMPLE_FMT_U8 == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_U8)
   && AV_SAMPLE_FMT_U8P == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_U8P)
   && AV_SAMPLE_FMT_U8P == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_U8P)
   && AV_SAMPLE_FMT_S16P == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_S16P)
   && AV_SAMPLE_FMT_S32 == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_S32)
   && AV_SAMPLE_FMT_S32P == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_S32P)
   && AV_SAMPLE_FMT_FLT == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_FLT)
   && AV_SAMPLE_FMT_FLTP == static_cast<int>(AUDACITY_AV_SAMPLE_FMT_FLTP)
,
   "FFmpeg constants don't match"
);

class AVFormatContextWrapperImpl : public AVFormatContextWrapper
{
public:
   AVFormatContextWrapperImpl(const FFmpegFunctions& ffmpeg)
      : AVFormatContextWrapper(ffmpeg)
   {
      mAVFormatContext = mFFmpeg.avformat_alloc_context();
   }

   AVInputFormat* GetIFormat() const noexcept override
   {
      if (mAVFormatContext != nullptr)
#if LIBAVFORMAT_VERSION_MAJOR <= 58
         return mAVFormatContext->iformat;
#else
         return const_cast<AVInputFormat*>(mAVFormatContext->iformat);
#endif

      return {};
   }

   AVOutputFormat* GetOFormat() const noexcept override
   {
      if (mAVFormatContext != nullptr)
#if LIBAVFORMAT_VERSION_MAJOR <= 58
         return mAVFormatContext->oformat;
#else
         return const_cast<AVOutputFormat*>(mAVFormatContext->oformat);
#endif

      return {};
   }

   void SetOutputFormat(std::unique_ptr<AVOutputFormatWrapper> oformat) noexcept override
   {
      if (mAVFormatContext != nullptr)
      {
         mAVFormatContext->oformat = const_cast<AVOutputFormat*>(oformat->GetWrappedValue());
         mOutputFormat = std::move(oformat);
      }
   }

   AVIOContextWrapper* GetAVIOContext() const noexcept override
   {
      return mAVIOContext.get();
   }

   void SetAVIOContext(std::unique_ptr<AVIOContextWrapper> pb) noexcept override
   {
      if (mAVFormatContext != nullptr)
      {
         mAVIOContext = std::move(pb);
         mAVFormatContext->pb = mAVIOContext->GetWrappedValue();
      }
   }

   int GetCtxFlags() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->ctx_flags;

      return {};
   }

   unsigned int GetStreamsCount() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->nb_streams;

      return {};
   }

   const AVFormatContextWrapper::StreamsList& GetStreams() const noexcept override
   {
      return mStreams;
   }

   const char* GetFilename() const noexcept override
   {
      if (mAVFormatContext != nullptr)
#if LIBAVFORMAT_VERSION_MAJOR <= 58
         return mAVFormatContext->filename;
#else
         return mAVFormatContext->url;
#endif

      return {};
   }

   void SetFilename(const char* filename) noexcept override
   {
      if (mAVFormatContext == nullptr)
         return;
#if LIBAVFORMAT_VERSION_MAJOR <= 58
      const size_t len =
         std::min(sizeof(mAVFormatContext->filename) - 1, std::strlen(filename));

      std::copy(filename, filename + len, mAVFormatContext->filename);

      mAVFormatContext->filename[len] = '\0';
#else
      mAVFormatContext->url = mFFmpeg.av_strdup(filename);
#endif
   }

   int64_t GetStartTime() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->start_time;

      return {};
   }

   int64_t GetDuration() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->duration;

      return {};
   }

   int GetBitRate() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         // May truncate int64_t to int.  But who uses such high rates, really?
         return mAVFormatContext->bit_rate;

      return {};
   }

   void SetBitRate(int bit_rate) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->bit_rate = bit_rate;
   }

   unsigned int GetPacketSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->packet_size;

      return {};
   }

   void SetPacketSize(unsigned int packet_size) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->packet_size = packet_size;
   }

   int GetMaxDelay() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_delay;

      return {};
   }

   void SetMaxDelay(int max_delay) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_delay = max_delay;
   }

   int GetFlags() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->flags;

      return {};
   }

   void SetFlags(int flags) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->flags = flags;
   }

   unsigned int GetProbeSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->probesize;

      return {};
   }

   void SetProbeSize(unsigned int probesize) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->probesize = probesize;
   }

   int GetMaxAnalyzeDuration() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_analyze_duration;

      return {};
   }

   void SetMaxAnalyzeDuration(int max_analyze_duration) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_analyze_duration = max_analyze_duration;
   }

   AVCodecIDFwd GetAudioCodecId() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->audio_codec_id;

      return {};
   }

   void SetAudioCodecId(AVCodecIDFwd audio_codec_id) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->audio_codec_id = static_cast<AVCodecID>(audio_codec_id);
   }

   unsigned int GetMaxIndexSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_index_size;

      return {};
   }

   void SetMaxIndexSize(unsigned int max_index_size) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_index_size = max_index_size;
   }

   AVDictionaryWrapper GetMetadata() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return AVDictionaryWrapper(mFFmpeg, mAVFormatContext->metadata);

      return AVDictionaryWrapper(mFFmpeg);
   }

   void SetMetadata(AVDictionaryWrapper metadata) noexcept override
   {
      if (mAVFormatContext == nullptr)
         return;

      if (mAVFormatContext->metadata != nullptr)
         mFFmpeg.av_dict_free(&mAVFormatContext->metadata);

      // This Release() doesn't leak:
      /* */https://ffmpeg.org/doxygen/2.8/structAVFormatContext.html#a3019a56080ed2e3297ff25bc2ff88adf */
      mAVFormatContext->metadata = metadata.Release();
   }

   int64_t GetStartTimeRealtime() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->start_time_realtime;

      return {};
   }

   void SetStartTimeRealtime(int64_t start_time_realtime) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->start_time_realtime = start_time_realtime;
   }

   int GetFpsProbeSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->fps_probe_size;

      return {};
   }

   void SetFpsProbeSize(int fps_probe_size) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->fps_probe_size = fps_probe_size;
   }

   int GetErrorRecognition() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->error_recognition;

      return {};
   }

   void SetErrorRecognition(int error_recognition) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->error_recognition = error_recognition;
   }

   int64_t GetMaxInterleaveDelta() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_interleave_delta;

      return {};
   }

   void SetMaxInterleaveDelta(int64_t max_interleave_delta) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_interleave_delta = max_interleave_delta;
   }

   int GetStrictStdCompliance() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->strict_std_compliance;

      return {};
   }

   void SetStrictStdCompliance(int strict_std_compliance) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->strict_std_compliance = strict_std_compliance;
   }

   int GetAudioPreload() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->audio_preload;

      return {};
   }

   void SetAudioPreload(int audio_preload) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->audio_preload = audio_preload;
   }

   int GetMaxChunkDuration() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_chunk_duration;

      return {};
   }

   void SetMaxChunkDuration(int max_chunk_duration) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_chunk_duration = max_chunk_duration;
   }

   int GetMaxChunkSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->max_chunk_size;

      return {};
   }

   void SetMaxChunkSize(int max_chunk_size) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->max_chunk_size = max_chunk_size;
   }

   int GetUseWallclockAsTimestamps() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->use_wallclock_as_timestamps;

      return {};
   }

   void SetUseWallclockAsTimestamps(
      int use_wallclock_as_timestamps) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->use_wallclock_as_timestamps = use_wallclock_as_timestamps;
   }

   int GetAvoidNegativeTs() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->avoid_negative_ts;

      return {};
   }

   void SetAvoidNegativeTs(int avoid_negative_ts) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->avoid_negative_ts = avoid_negative_ts;
   }

   int GetAvioFlags() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->avio_flags;

      return {};
   }

   void SetAvioFlags(int avio_flags) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->avio_flags = avio_flags;
   }

   int64_t GetSkipInitialBytes() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->skip_initial_bytes;

      return {};
   }

   void SetSkipInitialBytes(int64_t skip_initial_bytes) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->skip_initial_bytes = skip_initial_bytes;
   }

   unsigned int GetCorrectTsOverflow() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->correct_ts_overflow;

      return {};
   }

   void SetCorrectTsOverflow(unsigned int correct_ts_overflow) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->correct_ts_overflow = correct_ts_overflow;
   }

   int GetSeek2any() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->seek2any;

      return {};
   }

   void SetSeek2any(int seek2any) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->seek2any = seek2any;
   }

   int GetFlushPackets() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->flush_packets;

      return {};
   }

   void SetFlushPackets(int flush_packets) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->flush_packets = flush_packets;
   }

   int GetProbeScore() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->probe_score;

      return {};
   }

   int GetFormatProbeSize() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->format_probesize;

      return {};
   }

   void SetFormatProbeSize(int format_probesize) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->format_probesize = format_probesize;
   }

   AVCodecWrapper* GetAudioCodec() const noexcept override
   {
      return mForcedAudioCodec.get();
   }

   void SetAudioCodec(std::unique_ptr<AVCodecWrapper> audio_codec) noexcept override
   {
      if (mAVFormatContext == nullptr)
         return;

      mAVFormatContext->audio_codec = const_cast<AVCodec*>(audio_codec->GetWrappedValue());
      mForcedAudioCodec = move(audio_codec);
   }

   void* GetOpaque() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->opaque;

      return {};
   }

   void SetOpaque(void* opaque) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->opaque = opaque;
   }

   int64_t GetOutputTsOffset() const noexcept override
   {
      if (mAVFormatContext != nullptr)
         return mAVFormatContext->output_ts_offset;

      return {};
   }

   void SetOutputTsOffset(int64_t output_ts_offset) noexcept override
   {
      if (mAVFormatContext != nullptr)
         mAVFormatContext->output_ts_offset = output_ts_offset;
   }

   void UpdateStreamList() noexcept override
   {
      mStreams.clear();

      for (int i = 0; i < mAVFormatContext->nb_streams; ++i)
         mStreams.emplace_back(mFFmpeg.CreateAVStreamWrapper(mAVFormatContext->streams[i], false));
   }
};

std::unique_ptr<AVFormatContextWrapper> CreateAVFormatContextWrapper (const FFmpegFunctions& ffmpeg)
{
   return std::make_unique<AVFormatContextWrapperImpl>(ffmpeg);
}
