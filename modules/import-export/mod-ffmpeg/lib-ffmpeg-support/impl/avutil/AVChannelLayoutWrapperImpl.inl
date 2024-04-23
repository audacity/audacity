/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFrameWrapperImpl.inl

  Dmitry Vedenko

**********************************************************************/

class AVChannelLayoutWrapperImpl : public AVChannelLayoutWrapper
{
public:
#if HAS_AV_CHANNEL_LAYOUT
   AVChannelLayoutWrapperImpl(const FFmpegFunctions& ffmpeg, int channels)
   {
      ffmpeg.av_channel_layout_default(&mChannelLayout, channels);
   }

   explicit AVChannelLayoutWrapperImpl(const AVChannelLayout& layout) noexcept
       : mChannelLayout(layout)
   {
   }
#else
   AVChannelLayoutWrapperImpl(const FFmpegFunctions& ffmpeg, int channels)
       : mChannelsCount { channels }
   {
      mLegacyChannelLayout = ffmpeg.av_get_default_channel_layout(channels);
   }

   AVChannelLayoutWrapperImpl(uint64_t layout, int channelsCount) noexcept
       : mLegacyChannelLayout(layout)
       , mChannelsCount(channelsCount)
   {
   }
#endif

   uint64_t GetLegacyChannelLayout() const noexcept override
   {
#if HAS_AV_CHANNEL_LAYOUT
      return 0;
#else
      return mLegacyChannelLayout;
#endif
   }

   int GetChannelsCount() const noexcept override
   {
#if HAS_AV_CHANNEL_LAYOUT
      return mChannelLayout.nb_channels;
#else
      return mChannelsCount;
#endif
   }

   const AVChannelLayout* GetChannelLayout() const noexcept override
   {
#if HAS_AV_CHANNEL_LAYOUT
      return &mChannelLayout;
#else
      return nullptr;
#endif
   }

   std::unique_ptr<AVChannelLayoutWrapper> Clone() const override
   {
#if HAS_AV_CHANNEL_LAYOUT
      return std::make_unique<AVChannelLayoutWrapperImpl>(
         AVChannelLayoutWrapperImpl { mChannelLayout });
#else
      return std::make_unique<AVChannelLayoutWrapperImpl>(
         AVChannelLayoutWrapperImpl { mLegacyChannelLayout, mChannelsCount });
#endif
   }

private:
#if HAS_AV_CHANNEL_LAYOUT
   AVChannelLayout mChannelLayout {};
#else
   uint64_t mLegacyChannelLayout {};
   int mChannelsCount {};
#endif
}; // class AVChannelLayoutWrapperImpl

std::unique_ptr<AVChannelLayoutWrapper>
CreateDefaultChannelLayout(const FFmpegFunctions& functions, int channelsCount)
{
   return std::make_unique<AVChannelLayoutWrapperImpl>(
      functions, channelsCount);
}

std::unique_ptr<AVChannelLayoutWrapper> CreateLegacyChannelLayout(
   const FFmpegFunctions& functions, uint64_t layout, int channelsCount)
{
#if HAS_AV_CHANNEL_LAYOUT
   return nullptr;
#else
   return std::make_unique<AVChannelLayoutWrapperImpl>(layout, channelsCount);
#endif
}

std::unique_ptr<AVChannelLayoutWrapper> CreateAVChannelLayout(
   const FFmpegFunctions& functions, const AVChannelLayout* layout)
{
   if (layout == nullptr)
      return nullptr;

#if HAS_AV_CHANNEL_LAYOUT
   return std::make_unique<AVChannelLayoutWrapperImpl>(*layout);
#else
   return nullptr;
#endif
}
