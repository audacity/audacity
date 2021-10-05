/**********************************************************************

Audacity: A Digital Audio Editor

ImportFFmpeg.cpp

Copyright 2008  LRN
Based on ImportFLAC.cpp by Sami Liedes and transcode_sample.c by ANYwebcam Pty Ltd
Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class FFmpegImportFileHandle
\brief An ImportFileHandle for FFmpeg data

*//****************************************************************//**

\class FFmpegImportPlugin
\brief An ImportPlugin for FFmpeg data

*//*******************************************************************/



// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include "../FFmpeg.h"
#include "FFmpegFunctions.h"

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/log.h>
#include <wx/window.h>
#endif

#include "../widgets/ProgressDialog.h"


#define DESC XO("FFmpeg-compatible files")

//TODO: remove non-audio extensions
#if defined(USE_FFMPEG)
static const auto exts = {
   wxT("4xm"),
   wxT("MTV"),
   wxT("roq"),
   wxT("aac"),
   wxT("ac3"),
   wxT("aif"),
   wxT("aiff"),
   wxT("afc"),
   wxT("aifc"),
   wxT("al"),
   wxT("amr"),
   wxT("apc"),
   wxT("ape"),
   wxT("apl"),
   wxT("mac"),
   wxT("asf"),
   wxT("wmv"),
   wxT("wma"),
   wxT("au"),
   wxT("avi"),
   wxT("avs"),
   wxT("bethsoftvid"),
   wxT("c93"),
   wxT("302"),
   wxT("daud"),
   wxT("dsicin"),
   wxT("dts"),
   wxT("dv"),
   wxT("dxa"),
   wxT("ea"),
   wxT("cdata"),
   wxT("ffm"),
   wxT("film_cpk"),
   wxT("flac"),
   wxT("flic"),
   wxT("flv"),
   wxT("gif"),
   wxT("gxf"),
   wxT("idcin"),
   wxT("image2"),
   wxT("image2pipe"),
   wxT("cgi"),
   wxT("ipmovie"),
   wxT("nut"),
   wxT("lmlm4"),
   wxT("m4v"),
   wxT("mkv"),
   wxT("mm"),
   wxT("mmf"),
   wxT("mov"),
   wxT("mp4"),
   wxT("m4a"),
   wxT("m4r"),
   wxT("3gp"),
   wxT("3g2"),
   wxT("mj2"),
   wxT("mp3"),
   wxT("mpc"),
   wxT("mpc8"),
   wxT("mpg"),
   wxT("mpeg"),
   wxT("ts"),
   wxT("mpegtsraw"),
   wxT("mpegvideo"),
   wxT("msnwctcp"),
   wxT("ul"),
   wxT("mxf"),
   wxT("nsv"),
   wxT("nuv"),
   wxT("ogg"),
   wxT("opus"),
   wxT("psxstr"),
   wxT("pva"),
   wxT("redir"),
   wxT("rl2"),
   wxT("rm"),
   wxT("ra"),
   wxT("rv"),
   wxT("rtsp"),
   wxT("s16be"),
   wxT("sw"),
   wxT("s8"),
   wxT("sb"),
   wxT("sdp"),
   wxT("shn"),
   wxT("siff"),
   wxT("vb"),
   wxT("son"),
   wxT("smk"),
   wxT("sol"),
   wxT("swf"),
   wxT("thp"),
   wxT("tiertexseq"),
   wxT("tta"),
   wxT("txd"),
   wxT("u16be"),
   wxT("uw"),
   wxT("ub"),
   wxT("u8"),
   wxT("vfwcap"),
   wxT("vmd"),
   wxT("voc"),
   wxT("wav"),
   wxT("wc3movie"),
   wxT("wsaud"),
   wxT("wsvqa"),
   wxT("wv")
};

// all the includes live here by default
#include "Import.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

class FFmpegImportFileHandle;

/// A representative of FFmpeg loader in
/// the Audacity import plugin list
class FFmpegImportPlugin final : public ImportPlugin
{
public:
   FFmpegImportPlugin():
      ImportPlugin( FileExtensions( exts.begin(), exts.end() ) )
   {
   }

   ~FFmpegImportPlugin() { }

   wxString GetPluginStringID() override { return wxT("libav"); }
   TranslatableString GetPluginFormatDescription() override;

   ///! Probes the file and opens it if appropriate
   std::unique_ptr<ImportFileHandle> Open(
      const FilePath &Filename, AudacityProject*) override;
};

struct StreamContext final
{
   int StreamIndex { -1 };

   std::unique_ptr<AVCodecContextWrapper> CodecContext;

   int InitialChannels { 0 };
   sampleFormat SampleFormat { floatSample };

   bool Use { true };
};

///! Does actual import, returned by FFmpegImportPlugin::Open
class FFmpegImportFileHandle final : public ImportFileHandle
{

public:
   FFmpegImportFileHandle(const FilePath & name);
   ~FFmpegImportFileHandle();

   ///! Format initialization
   ///\return true if successful, false otherwise
   bool Init();
   ///! Codec initialization
   ///\return true if successful, false otherwise
   bool InitCodecs();


   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;

   ///! Imports audio
   ///\return import status (see Import.cpp)
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks,
      Tags *tags) override;

   ///! Writes decoded data into WaveTracks.
   ///\param sc - stream context
   ProgressResult WriteData(StreamContext* sc, const AVPacketWrapper* packet);

   ///! Writes extracted metadata to tags object
   ///\param avf - file context
   ///\ tags - Audacity tags object
   void WriteMetadata(Tags *tags);

   ///! Retrieves metadata from FFmpeg and converts to wxString
   ///\param avf - file context
   ///\ tags - Audacity tags object
   ///\ tag - name of tag to set
   ///\ name - name of metadata item to retrieve
   void GetMetadata(Tags &tags, const wxChar *tag, const char *name);

   ///! Called by Import.cpp
   ///\return number of readable streams in the file
   wxInt32 GetStreamCount() override
   {
      return static_cast<wxInt32>(mStreamContexts.size());
   }

   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   const TranslatableStrings &GetStreamInfo() override
   {
      return mStreamInfo;
   }

   ///! Called by Import.cpp
   ///\param StreamID - index of the stream in mStreamInfo and mStreamContexts
   ///\param Use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 StreamID, bool Use) override
   {
      if (StreamID < static_cast<wxInt32>(mStreamContexts.size()))
         mStreamContexts[StreamID].Use = Use;
   }

private:
   // Construct this member first, so it is destroyed last, so the functions
   // remain loaded while other members are destroyed
   const std::shared_ptr<FFmpegFunctions> mFFmpeg = FFmpegFunctions::Load();

   std::vector<StreamContext> mStreamContexts;

   std::unique_ptr<AVFormatContextWrapper> mAVFormatContext;

   TranslatableStrings   mStreamInfo;    //!< Array of stream descriptions. After Init() and before Import(), same size as mStreamContexts

   wxInt64               mProgressPos = 0;   //!< Current timestamp, file position or whatever is used as first argument for Update()
   wxInt64               mProgressLen = 1;   //!< Duration, total length or whatever is used as second argument for Update()

   bool                  mCancelled = false;     //!< True if importing was canceled by user
   bool                  mStopped = false;       //!< True if importing was stopped by user
   const FilePath        mName;
   TrackHolders mChannels;               //!< 2-dimensional array of WaveTracks.
                                         //!< First dimension - streams,
                                         //!< After Import(), same size as mStreamContexts;
                                         //!< second - channels of a stream.
};


TranslatableString FFmpegImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> FFmpegImportPlugin::Open(
   const FilePath &filename, AudacityProject*)
{
   auto ffmpeg = FFmpegFunctions::Load();

   //Check if we're loading explicitly supported format
   wxString extension = filename.AfterLast(wxT('.'));
   if (SupportsExtension(extension))
   {
      //Audacity is trying to load something that is declared as
      //officially supported by this plugin.
      //If we don't have FFmpeg configured - tell the user about it.
      //Since this will be happening often, use disableable "FFmpeg not found" dialog
      //insdead of usual AudacityMessageBox()
      bool newsession = NewImportingSession.Read();
      if (!ffmpeg)
      {
         auto dontShowDlg = FFmpegNotFoundDontShow.Read();
         if (dontShowDlg == 0 && newsession)
         {
            NewImportingSession.Write(false);
            gPrefs->Flush();
            FFmpegNotFoundDialog{ nullptr }.ShowModal();

            ffmpeg = FFmpegFunctions::Load();
         }
      }
   }
   if (!ffmpeg)
   {
      return nullptr;
   }

   // Construct the handle only after any reloading of ffmpeg functions
   auto handle = std::make_unique<FFmpegImportFileHandle>(filename);

   // Open the file for import
   bool success = handle->Init();

   if (!success) {
      return nullptr;
   }

   return handle;
}

static Importer::RegisteredImportPlugin registered{ "FFmpeg",
   std::make_unique< FFmpegImportPlugin >()
};


FFmpegImportFileHandle::FFmpegImportFileHandle(const FilePath & name)
:  ImportFileHandle(name)
,  mName{ name }
{
}

bool FFmpegImportFileHandle::Init()
{
   if (!mFFmpeg)
      return false;

   mAVFormatContext = mFFmpeg->CreateAVFormatContext();

   const auto err = mAVFormatContext->OpenInputContext(mName, nullptr, AVDictionaryWrapper(*mFFmpeg));

   if (err != AVIOContextWrapper::OpenResult::Success)
   {
      wxLogError(wxT("FFmpeg : AVFormatContextWrapper::OpenInputContext() failed for file %s"), mName);
      return false;
   }

   if (!InitCodecs())
      return false;

   return true;
}

bool FFmpegImportFileHandle::InitCodecs()
{
   for (unsigned int i = 0; i < mAVFormatContext->GetStreamsCount(); i++)
   {
      const AVStreamWrapper* stream = mAVFormatContext->GetStream(i);

      if (stream->IsAudio())
      {
         const AVCodecIDFwd id = mAVFormatContext->GetStream(i)->GetAVCodecID();

         auto codec = mFFmpeg->CreateDecoder(id);
         auto name = mFFmpeg->avcodec_get_name(id);

         if (codec == NULL)
         {
            wxLogError(
               wxT("FFmpeg : CreateDecoder() failed. Index[%02d], Codec[%02x - %s]"),
               i, id, name);
            //FFmpeg can't decode this stream, skip it
            continue;
         }

         auto codecContextPtr = stream->GetAVCodecContext();

         if ( codecContextPtr->Open( codecContextPtr->GetCodec() ) < 0 )
         {
            wxLogError(wxT("FFmpeg : Open() failed. Index[%02d], Codec[%02x - %s]"),i,id,name);
            //Can't open decoder - skip this stream
            continue;
         }

         const int channels = codecContextPtr->GetChannels();
         const sampleFormat preferredFormat =
            codecContextPtr->GetPreferredAudacitySampleFormat();

         auto codecContext = codecContextPtr.get();

         mStreamContexts.emplace_back(
            StreamContext { stream->GetIndex(), std::move(codecContextPtr),
                            channels, preferredFormat, true });

         // Stream is decodeable and it is audio. Add it and its description to the arrays
         int duration = 0;
         if (stream->GetDuration() > 0)
            duration = stream->GetDuration() * stream->GetTimeBase().num / stream->GetTimeBase().den;
         else
            duration = mAVFormatContext->GetDuration() / AUDACITY_AV_TIME_BASE;

         wxString bitrate;
         if (codecContext->GetBitRate() > 0)
            bitrate.Printf(wxT("%d"),(int)codecContext->GetBitRate());
         else
            bitrate.Printf(wxT("?"));

         AVDictionaryWrapper streamMetadata = stream->GetMetadata();

         auto lang = std::string(streamMetadata.Get("language", {}));

         auto strinfo = XO(
/* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
"Index[%02x] Codec[%s], Language[%s], Bitrate[%s], Channels[%d], Duration[%d]")
            .Format(
               stream->GetIndex(),
               name,
               lang,
               bitrate,
               (int)codecContext->GetChannels(),
               (int)duration);

         mStreamInfo.push_back(strinfo);
      }
      //for video and unknown streams do nothing
   }
   //It doesn't really returns false, but GetStreamCount() will return 0 if file is composed entirely of unreadable streams
   return true;
}

TranslatableString FFmpegImportFileHandle::GetFileDescription()
{
   return DESC;
}


auto FFmpegImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO: Get Uncompressed byte count.
   return 0;
}

ProgressResult FFmpegImportFileHandle::Import(WaveTrackFactory *trackFactory,
              TrackHolders &outTracks,
              Tags *tags)
{
   outTracks.clear();

   CreateProgress();

   //! This may break the correspondence with mStreamInfo
   mStreamContexts.erase (std::remove_if (mStreamContexts.begin (), mStreamContexts.end (), [](const StreamContext& ctx) {
      return !ctx.Use;
   }), mStreamContexts.end());

   mChannels.resize(mStreamContexts.size());

   int s = -1;
   for (auto &stream : mChannels)
   {
      ++s;

      const StreamContext& sc = mStreamContexts[s];

      stream.resize(sc.InitialChannels);

      for (auto &channel : stream)
         channel = NewWaveTrack(*trackFactory, sc.SampleFormat, sc.CodecContext->GetSampleRate());
   }

   // Handles the start_time by creating silence. This may or may not be correct.
   // There is a possibility that we should ignore first N milliseconds of audio instead. I do not know.
   /// TODO: Nag FFmpeg devs about start_time until they finally say WHAT is this and HOW to handle it.
   s = -1;
   for (auto &stream : mChannels)
   {
      ++s;

      int64_t stream_delay = 0;
      const auto& sc = mStreamContexts[s];

      const int64_t streamStartTime =
         mAVFormatContext->GetStream(sc.StreamIndex)->GetStartTime();

      if (streamStartTime != int64_t(AUDACITY_AV_NOPTS_VALUE) && streamStartTime > 0)
      {
         stream_delay = streamStartTime;

         wxLogDebug(
            wxT("Stream %d start_time = %lld, that would be %f milliseconds."),
            s, (long long)streamStartTime, double(streamStartTime) / 1000);
      }

      if (stream_delay > 0)
      {
         int c = -1;
         for (auto &channel : stream)
         {
            ++c;

            WaveTrack *t = channel.get();
            t->InsertSilence(0,double(stream_delay)/AUDACITY_AV_TIME_BASE);
         }
      }
   }
   // This is the heart of the importing process
   // The result of Import() to be returned. It will be something other than zero if user canceled or some error appears.
   auto res = ProgressResult::Success;

   // Read frames.
   for (std::unique_ptr<AVPacketWrapper> packet;
        (packet = mAVFormatContext->ReadNextPacket()) != nullptr &&
        (res == ProgressResult::Success);)
   {
      // Find a matching StreamContext
      auto streamContextIt = std::find_if(
         mStreamContexts.begin(), mStreamContexts.end(),
         [index = packet->GetStreamIndex()](const StreamContext& ctx)
         { return ctx.StreamIndex == index;
      });

      if (streamContextIt == mStreamContexts.end())
         continue;

      res = WriteData(&(*streamContextIt), packet.get());
   }

   // Flush the decoders.
   if (!mStreamContexts.empty() && (res == ProgressResult::Success || res == ProgressResult::Stopped))
   {
      auto emptyPacket = mFFmpeg->CreateAVPacketWrapper();

      for (StreamContext& sc : mStreamContexts)
         WriteData(&sc, emptyPacket.get());
   }

   // Something bad happened - destroy everything!
   if (res == ProgressResult::Cancelled || res == ProgressResult::Failed)
      return res;
   //else if (res == 2), we just stop the decoding as if the file has ended

   // Copy audio from mChannels to newly created tracks (destroying mChannels elements in process)
   for (auto &stream : mChannels)
      for(auto &channel : stream)
         channel->Flush();

   outTracks.swap(mChannels);

   // Save metadata
   WriteMetadata(tags);

   return res;
}

ProgressResult FFmpegImportFileHandle::WriteData(StreamContext *sc, const AVPacketWrapper* packet)
{
   // Find the stream index in mStreamContexts array
   int streamid = -1;
   auto iter = mChannels.begin();

   for (int i = 0; i < static_cast<int>(mStreamContexts.size()); ++iter, ++i)
   {
      if (&mStreamContexts[i] == sc)
      {
         streamid = i;
         break;
      }
   }
   // Stream is not found. This should not really happen
   if (streamid == -1)
   {
      return ProgressResult::Success;
   }

   size_t nChannels = std::min(sc->CodecContext->GetChannels(), sc->InitialChannels);

   if (sc->SampleFormat == int16Sample)
   {
      auto data = sc->CodecContext->DecodeAudioPacketInt16(packet);

      const int channelsCount = sc->CodecContext->GetChannels();
      const int samplesPerChannel = data.size() / channelsCount;

      // Write audio into WaveTracks
      auto iter2 = iter->begin();
      for (size_t chn = 0; chn < nChannels; ++iter2, ++chn)
      {
         iter2->get()->Append(
            reinterpret_cast<samplePtr>(data.data()), sc->SampleFormat,
            samplesPerChannel,
            sc->CodecContext->GetChannels());
      }
   }
   else if (sc->SampleFormat == floatSample)
   {
      auto data = sc->CodecContext->DecodeAudioPacketFloat(packet);

      const int channelsCount = sc->CodecContext->GetChannels();
      const int samplesPerChannel = data.size() / channelsCount;

      // Write audio into WaveTracks
      auto iter2 = iter->begin();
      for (size_t chn = 0; chn < nChannels; ++iter2, ++chn)
      {
         iter2->get()->Append(
            reinterpret_cast<samplePtr>(data.data()), sc->SampleFormat,
            samplesPerChannel, sc->CodecContext->GetChannels());
      }
   }

   const AVStreamWrapper* avStream = mAVFormatContext->GetStream(sc->StreamIndex);

   // Try to update the progress indicator (and see if user wants to cancel)
   auto updateResult = ProgressResult::Success;
   int64_t filesize = mFFmpeg->avio_size(mAVFormatContext->GetAVIOContext()->GetWrappedValue());
   // PTS (presentation time) is the proper way of getting current position
   if (
      packet->GetPresentationTimestamp() != AUDACITY_AV_NOPTS_VALUE &&
      mAVFormatContext->GetDuration() != AUDACITY_AV_NOPTS_VALUE)
   {
      auto timeBase = avStream->GetTimeBase();

      mProgressPos =
         packet->GetPresentationTimestamp() * timeBase.num / timeBase.den;

      mProgressLen =
         (mAVFormatContext->GetDuration() > 0 ?
             mAVFormatContext->GetDuration() / AUDACITY_AV_TIME_BASE :
             1);
   }
   // When PTS is not set, use number of frames and number of current frame
   else if (
      avStream->GetFramesCount() > 0 && sc->CodecContext->GetFrameNumber() > 0 &&
      sc->CodecContext->GetFrameNumber() <= avStream->GetFramesCount())
   {
      mProgressPos = sc->CodecContext->GetFrameNumber();
      mProgressLen = avStream->GetFramesCount();
   }
   // When number of frames is unknown, use position in file
   else if (
      filesize > 0 && packet->GetPos() > 0 && packet->GetPos() <= filesize)
   {
      mProgressPos = packet->GetPos();
      mProgressLen = filesize;
   }
   updateResult = mProgress->Update(mProgressPos, mProgressLen != 0 ? mProgressLen : 1);

   return updateResult;
}

void FFmpegImportFileHandle::WriteMetadata(Tags *tags)
{
   Tags temp;

   GetMetadata(temp, TAG_TITLE, "title");
   GetMetadata(temp, TAG_COMMENTS, "comment");
   GetMetadata(temp, TAG_ALBUM, "album");
   GetMetadata(temp, TAG_TRACK, "track");
   GetMetadata(temp, TAG_GENRE, "genre");

   if (wxString(mAVFormatContext->GetInputFormat()->GetName()).Contains("m4a"))
   {
      GetMetadata(temp, TAG_ARTIST, "artist");
      GetMetadata(temp, TAG_YEAR, "date");
   }
   else if (wxString(mAVFormatContext->GetInputFormat()->GetName()).Contains("asf")) /* wma */
   {
      GetMetadata(temp, TAG_ARTIST, "artist");
      GetMetadata(temp, TAG_YEAR, "year");
   }
   else
   {
      GetMetadata(temp, TAG_ARTIST, "author");
      GetMetadata(temp, TAG_YEAR, "year");
   }

   if (!temp.IsEmpty())
   {
      *tags = temp;
   }
}

void FFmpegImportFileHandle::GetMetadata(Tags &tags, const wxChar *tag, const char *name)
{
   auto metadata = mAVFormatContext->GetMetadata();

   if (metadata.HasValue(name, DICT_IGNORE_SUFFIX))
      tags.SetTag(tag, wxString::FromUTF8(std::string(metadata.Get(name, {}, DICT_IGNORE_SUFFIX))));
}


FFmpegImportFileHandle::~FFmpegImportFileHandle()
{

}

#endif //USE_FFMPEG
