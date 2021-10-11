/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFrameWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

class AVFrameWrapperImpl : public AVFrameWrapper
{
public:
   explicit
   AVFrameWrapperImpl(const FFmpegFunctions& ffmpeg)
      : AVFrameWrapper(ffmpeg)
   {
   }

   int GetNumDataPointers() const noexcept override
   {
      return AV_NUM_DATA_POINTERS;
   }

   uint8_t* GetData(int index) const noexcept override
   {
      if (mAVFrame == nullptr)
         return {};

      if (index < 0 || index >= AV_NUM_DATA_POINTERS)
         return {};

      return mAVFrame->data[index];
   }

   int GetLineSize(int index) const noexcept override
   {
      if (mAVFrame == nullptr)
         return {};

      if (index < 0 || index >= AV_NUM_DATA_POINTERS)
         return {};

      return mAVFrame->linesize[index];
   }

   uint64_t GetError(int index) const noexcept override
   {
      if (mAVFrame == nullptr)
         return {};

      if (index < 0 || index >= AV_NUM_DATA_POINTERS)
         return {};

      return mAVFrame->error[index];
   }

   uint8_t* GetExtendedData(int index) const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->extended_data[index];

      return {};
   }

   int GetWidth() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->width;

      return {};
   }

   int GetHeight() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->height;

      return {};
   }

   int GetSamplesCount() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->nb_samples;

      return {};
   }

   void SetSamplesCount(int count) noexcept override
   {
      if (mAVFrame != nullptr)
         mAVFrame->nb_samples = count;
   }

   AVSampleFormatFwd GetFormat() const noexcept override
   {
      if (mAVFrame != nullptr)
         return static_cast<AVSampleFormatFwd>(mAVFrame->format);

      return {};
   }

   void SetFormat(AVSampleFormatFwd format) noexcept override
   {
      if (mAVFrame != nullptr)
         mAVFrame->format = format;
   }

   int GetKeyFrame() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->key_frame;

      return {};
   }

   AudacityAVRational GetSampleAspectRatio() const noexcept override
   {
      if (mAVFrame != nullptr)
         return { mAVFrame->sample_aspect_ratio.num,
               mAVFrame->sample_aspect_ratio.den };

      return {};
   }

   int64_t GetPresentationTimestamp() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pts;

      return {};
   }

   int64_t GetPacketPresentationTimestamp() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pkt_pts;

      return {};
   }

   int64_t GetPacketDecompressionTimestamp() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pkt_dts;

      return {};
   }

   int GetCodedPictureNumber() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->coded_picture_number;

      return {};
   }

   int GetDisplayPictureNumber() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->display_picture_number;

      return {};
   }

   int GetQuality() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->quality;

      return {};
   }

   void* GetOpaque() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->opaque;

      return {};
   }

   void SetOpaque(void* opaque) noexcept override
   {
      if (mAVFrame != nullptr)
         mAVFrame->opaque = opaque;
   }

   int GetRepeatPict() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->repeat_pict;

      return {};
   }

   int GetInterlacedFrame() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->interlaced_frame;

      return {};
   }

   int GetTopFieldFirst() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->top_field_first;

      return {};
   }

   int GetPaletteHasChanged() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->palette_has_changed;

      return {};
   }

   int64_t GetReorderedOpaque() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->reordered_opaque;

      return {};
   }

   int GetSampleRate() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->sample_rate;

      return {};
   }

   uint64_t GetChannelLayout() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->channel_layout;

      return {};
   }

   void SetChannelLayout(uint64_t layout) noexcept override
   {
      if (mAVFrame != nullptr)
      {
         mAVFrame->channel_layout = layout;
#if LIBAVUTIL_VERSION_MAJOR >= 56
         mAVFrame->channels =
            mFFmpeg.av_get_channel_layout_nb_channels(layout);
#endif
      }
   }

   int GetSideDataCount() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->nb_side_data;

      return {};
   }

   int GetFlags() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->flags;

      return {};
   }

   int64_t GetBestEffortTimestamp() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->best_effort_timestamp;

      return {};
   }

   int64_t GetPacketPos() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pkt_pos;

      return {};
   }

   int64_t GetPacketDuration() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pkt_duration;

      return {};
   }

   AVDictionaryWrapper GetMetadata() const noexcept override
   {
      if (mAVFrame != nullptr)
         return AVDictionaryWrapper(mFFmpeg, mAVFrame->metadata);

      return AVDictionaryWrapper(mFFmpeg);
   }

   int GetDecodeErrorFlags() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->decode_error_flags;

      return {};
   }

   int GetChannels() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->channels;

      return {};
   }

   int GetPacketSize() const noexcept override
   {
      if (mAVFrame != nullptr)
         return mAVFrame->pkt_size;

      return {};
   }
};

std::unique_ptr<AVFrameWrapper> CreateAVFrameWrapper(const FFmpegFunctions& ffmpeg)
{
   return std::make_unique<AVFrameWrapperImpl>(ffmpeg);
}
