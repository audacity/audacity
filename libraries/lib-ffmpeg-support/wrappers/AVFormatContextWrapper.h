/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatContextWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <vector>
#include <memory>

#include "FFmpegTypes.h"
#include "AVIOContextWrapper.h"
#include "AVPacketWrapper.h"


struct FFmpegFunctions;
typedef struct AVFormatContext AVFormatContext;

class AVDictionaryWrapper;
class AVStreamWrapper;
class AVInputFormatWrapper;
class AVOutputFormatWrapper;
class AVCodecWrapper;

class FFMPEG_SUPPORT_API AVFormatContextWrapper
{
public:
   using StreamsList = std::vector<std::unique_ptr<AVStreamWrapper>>;

   AVFormatContextWrapper(const AVFormatContextWrapper&) = delete;
   AVFormatContextWrapper& operator=(AVFormatContextWrapper&) = delete;

   AVFormatContextWrapper(AVFormatContextWrapper&&) = delete;
   AVFormatContextWrapper& operator=(AVFormatContextWrapper&&) = delete;

   explicit AVFormatContextWrapper(
      const FFmpegFunctions& ffmpeg) noexcept;

   //! @return null if OpenInputContext or OpenOutputContext has not been called
   AVFormatContext* GetWrappedValue() noexcept;
   //! @return null if OpenInputContext or OpenOutputContext has not been called
   const AVFormatContext* GetWrappedValue() const noexcept;

   virtual ~AVFormatContextWrapper();

   AVIOContextWrapper::OpenResult OpenInputContext(const wxString& path, const AVInputFormatWrapper* inputFormat, AVDictionaryWrapper options);

   AVIOContextWrapper::OpenResult OpenOutputContext(const wxString& path);

   //! @return is null at end of stream
   std::unique_ptr<AVPacketWrapper> ReadNextPacket();

   std::unique_ptr<AVStreamWrapper> CreateStream();

   const AVInputFormatWrapper* GetInputFormat() const noexcept;
   const AVOutputFormatWrapper* GetOutputFormat() const noexcept;

   virtual void SetOutputFormat(std::unique_ptr<AVOutputFormatWrapper> oformat) noexcept = 0;

   virtual AVIOContextWrapper* GetAVIOContext() const noexcept = 0;
   virtual void SetAVIOContext(std::unique_ptr<AVIOContextWrapper> pb) noexcept = 0;

   virtual int GetCtxFlags() const noexcept = 0;

   virtual unsigned int GetStreamsCount() const noexcept = 0;

   virtual const StreamsList& GetStreams() const noexcept = 0;
   virtual const AVStreamWrapper* GetStream(int index) const noexcept;

   virtual const char* GetFilename() const noexcept = 0;
   virtual void SetFilename(const char* filename) noexcept = 0;

   virtual int64_t GetStartTime() const noexcept = 0;

   virtual int64_t GetDuration() const noexcept = 0;

   virtual int GetBitRate() const noexcept = 0;
   virtual void SetBitRate(int bit_rate) noexcept = 0;

   virtual unsigned int GetPacketSize() const noexcept = 0;
   virtual void SetPacketSize(unsigned int packet_size) noexcept = 0;

   virtual int GetMaxDelay() const noexcept = 0;
   virtual void SetMaxDelay(int max_delay) noexcept = 0;

   virtual int GetFlags() const noexcept = 0;
   virtual void SetFlags(int flags) noexcept = 0;

   virtual unsigned int GetProbeSize() const noexcept = 0;
   virtual void SetProbeSize(unsigned int probesize) noexcept = 0;

   virtual int GetMaxAnalyzeDuration() const noexcept = 0;
   virtual void SetMaxAnalyzeDuration(int max_analyze_duration) noexcept = 0;

   virtual AVCodecIDFwd GetAudioCodecId() const noexcept = 0;
   virtual void SetAudioCodecId(AVCodecIDFwd audio_codec_id) noexcept = 0;

   virtual unsigned int GetMaxIndexSize() const noexcept = 0;
   virtual void SetMaxIndexSize(unsigned int max_index_size) noexcept = 0;

   virtual AVDictionaryWrapper GetMetadata() const noexcept = 0;
   virtual void SetMetadata(AVDictionaryWrapper metadata) noexcept = 0;

   virtual int64_t GetStartTimeRealtime() const noexcept = 0;
   virtual void SetStartTimeRealtime(int64_t start_time_realtime) noexcept = 0;

   virtual int GetFpsProbeSize() const noexcept = 0;
   virtual void SetFpsProbeSize(int fps_probe_size) noexcept = 0;

   virtual int GetErrorRecognition() const noexcept = 0;
   virtual void SetErrorRecognition(int error_recognition) noexcept = 0;

   virtual int64_t GetMaxInterleaveDelta() const noexcept = 0;
   virtual void SetMaxInterleaveDelta(int64_t max_interleave_delta) noexcept = 0;

   virtual int GetStrictStdCompliance() const noexcept = 0;
   virtual void SetStrictStdCompliance(int strict_std_compliance) noexcept = 0;

   virtual int GetAudioPreload() const noexcept = 0;
   virtual void SetAudioPreload(int audio_preload) noexcept = 0;

   virtual int GetMaxChunkDuration() const noexcept = 0;
   virtual void SetMaxChunkDuration(int max_chunk_duration) noexcept = 0;

   virtual int GetMaxChunkSize() const noexcept = 0;
   virtual void SetMaxChunkSize(int max_chunk_size) noexcept = 0;

   virtual int GetUseWallclockAsTimestamps() const noexcept = 0;
   virtual void SetUseWallclockAsTimestamps(int use_wallclock_as_timestamps) noexcept = 0;

   virtual int GetAvoidNegativeTs() const noexcept = 0;
   virtual void SetAvoidNegativeTs(int avoid_negative_ts) noexcept = 0;

   virtual int GetAvioFlags() const noexcept = 0;
   virtual void SetAvioFlags(int avio_flags) noexcept = 0;

   virtual int64_t GetSkipInitialBytes() const noexcept = 0;
   virtual void SetSkipInitialBytes(int64_t skip_initial_bytes) noexcept = 0;

   virtual unsigned int GetCorrectTsOverflow() const noexcept = 0;
   virtual void SetCorrectTsOverflow(unsigned int correct_ts_overflow) noexcept = 0;

   virtual int GetSeek2any() const noexcept = 0;
   virtual void SetSeek2any(int seek2any) noexcept = 0;

   virtual int GetFlushPackets() const noexcept = 0;
   virtual void SetFlushPackets(int flush_packets) noexcept = 0;

   virtual int GetProbeScore() const noexcept = 0;

   virtual int GetFormatProbeSize() const noexcept = 0;
   virtual void SetFormatProbeSize(int format_probesize) noexcept = 0;

   virtual AVCodecWrapper* GetAudioCodec() const noexcept = 0;
   virtual void SetAudioCodec(
      std::unique_ptr<AVCodecWrapper> audio_codec) noexcept = 0;

   virtual void* GetOpaque() const noexcept = 0;
   virtual void SetOpaque(void* opaque) noexcept = 0;

   virtual int64_t GetOutputTsOffset() const noexcept = 0;
   virtual void SetOutputTsOffset(int64_t output_ts_offset) noexcept = 0;
protected:
   virtual AVInputFormat* GetIFormat() const noexcept = 0;
   virtual AVOutputFormat* GetOFormat() const noexcept = 0;

   virtual void UpdateStreamList() noexcept = 0;

   const FFmpegFunctions& mFFmpeg;
   AVFormatContext* mAVFormatContext { nullptr };

   std::unique_ptr<AVIOContextWrapper> mAVIOContext;

   StreamsList mStreams;
   std::unique_ptr<AVInputFormatWrapper> mInputFormat;
   std::unique_ptr<AVOutputFormatWrapper> mOutputFormat;

   std::unique_ptr<AVCodecWrapper> mForcedAudioCodec;
};
