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

#include "../FFmpeg.h"      // which brings in avcodec.h, avformat.h
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

   ///! Reads next audio frame
   ///\return pointer to the stream context structure to which the frame belongs to or NULL on error, or 1 if stream is not to be imported.
   streamContext* ReadNextFrame();

   ///! Decodes the frame
   ///\param sc - stream context (from ReadNextFrame)
   ///\param flushing - true if flushing (no more frames left), false otherwise
   ///\return 0 on success, -1 if it can't decode any further
   int DecodeFrame(streamContext *sc, bool flushing);

   ///! Writes decoded data into WaveTracks. Called by DecodeFrame
   ///\param sc - stream context
   ProgressResult WriteData(streamContext *sc);

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
      return mNumStreams;
   }

   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   const TranslatableStrings &GetStreamInfo() override
   {
      return mStreamInfo;
   }

   ///! Called by Import.cpp
   ///\param StreamID - index of the stream in mStreamInfo and mScs arrays
   ///\param Use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 StreamID, bool Use) override
   {
      if (StreamID < mNumStreams)
         mScs->get()[StreamID]->m_use = Use;
   }

private:

   std::shared_ptr<FFmpegContext> mContext; // An object that does proper IO shutdown in its destructor; may be shared with decoder task.
   AVFormatContext      *mFormatContext; //!< Format description, also contains metadata and some useful info
   int                   mNumStreams;    //!< mNumstreams is less or equal to mFormatContext->nb_streams
   ScsPtr                mScs;           //!< Points to array of pointers to stream contexts, which may be shared with a decoder task.
   TranslatableStrings   mStreamInfo;    //!< Array of stream descriptions. Length is mNumStreams

   wxInt64               mProgressPos;   //!< Current timestamp, file position or whatever is used as first argument for Update()
   wxInt64               mProgressLen;   //!< Duration, total length or whatever is used as second argument for Update()

   bool                  mCancelled;     //!< True if importing was canceled by user
   bool                  mStopped;       //!< True if importing was stopped by user
   FilePath              mName;
   TrackHolders mChannels;               //!< 2-dimensional array of WaveTrack's.
                                         //!< First dimension - streams,
                                         //!< second - channels of a stream.
                                         //!< Length is mNumStreams
};


TranslatableString FFmpegImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> FFmpegImportPlugin::Open(
   const FilePath &filename, AudacityProject*)
{
   auto handle = std::make_unique<FFmpegImportFileHandle>(filename);

   //Check if we're loading explicitly supported format
   wxString extension = filename.AfterLast(wxT('.'));
   if (SupportsExtension(extension))
   {
      //Audacity is trying to load something that is declared as
      //officially supported by this plugin.
      //If we don't have FFmpeg configured - tell the user about it.
      //Since this will be happening often, use disableable "FFmpeg not found" dialog
      //insdead of usual AudacityMessageBox()
      bool newsession = false;
      gPrefs->Read(wxT("/NewImportingSession"), &newsession);
      if (!FFmpegLibsInst()->ValidLibsLoaded())
      {
         int dontShowDlg;
         gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
         if (dontShowDlg == 0 && newsession)
         {
            gPrefs->Write(wxT("/NewImportingSession"), false);
            gPrefs->Flush();
            FFmpegNotFoundDialog{ nullptr }.ShowModal();
         }
      }
   }
   if (!FFmpegLibsInst()->ValidLibsLoaded())
   {
      return nullptr;
   }

   // Open the file for import
   bool success = handle->Init();
   if (!success) {
      return nullptr;
   }

   // This std::move is needed to "upcast" the pointer type
   return std::move(handle);
}

static Importer::RegisteredImportPlugin registered{ "FFmpeg",
   std::make_unique< FFmpegImportPlugin >()
};


FFmpegImportFileHandle::FFmpegImportFileHandle(const FilePath & name)
:  ImportFileHandle(name)
{
   PickFFmpegLibs();

   mFormatContext = NULL;
   mNumStreams = 0;
   mCancelled = false;
   mStopped = false;
   mName = name;
   mProgressPos = 0;
   mProgressLen = 1;
}

bool FFmpegImportFileHandle::Init()
{
   //FFmpegLibsInst()->LoadLibs(NULL,false); //Loaded at startup or from Prefs now

   if (!FFmpegLibsInst()->ValidLibsLoaded())
      return false;

   av_log_set_callback(av_log_wx_callback);

   int err;
   std::unique_ptr<FFmpegContext> tempContext;
   err = ufile_fopen_input(tempContext, mName);
   if (err < 0)
   {
      wxLogError(wxT("FFmpeg : av_open_input_file() failed for file %s"), mName);
      return false;
   }
   wxASSERT(tempContext.get());
   mFormatContext = tempContext->ic_ptr;

   err = avformat_find_stream_info(mFormatContext, NULL);
   if (err < 0)
   {
      wxLogError(wxT("FFmpeg: avformat_find_stream_info() failed for file %s"),mName);
      return false;
   }

   if (!InitCodecs())
      return false;

   // Only now do we postpone destroying the FFmpegContext.
   // Move from unique to shared pointer
   mContext.reset(tempContext.release());

   return true;
}

bool FFmpegImportFileHandle::InitCodecs()
{
   // Allocate the array of pointers to hold stream contexts pointers
   // Some of the allocated space may be unused (corresponds to video, subtitle, or undecodeable audio streams)
   mScs = std::make_shared<Scs>(size_t{mFormatContext->nb_streams});
   // Fill the stream contexts
   for (unsigned int i = 0; i < mFormatContext->nb_streams; i++)
   {
      if (mFormatContext->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO)
      {
         //Create a context
         auto sc = std::make_unique<streamContext>();

         sc->m_stream = mFormatContext->streams[i];
         sc->m_codecCtx = sc->m_stream->codec;

         const AVCodecID id   = sc->m_codecCtx->codec_id;
         const char*     name = avcodec_get_name(id);
         const AVCodec *codec = avcodec_find_decoder(id);

         if (codec == NULL)
         {
            wxLogError(wxT("FFmpeg : avcodec_find_decoder() failed. Index[%02d], Codec[%02x - %s]"),i,id,name);
            //FFmpeg can't decode this stream, skip it
            continue;
         }

         if (codec->type != sc->m_codecCtx->codec_type)
         {
            wxLogError(wxT("FFmpeg : Codec type mismatch, skipping. Index[%02d], Codec[%02x - %s]"),i,id,name);
            //Non-audio codec reported as audio? Nevertheless, we don't need THIS.
            continue;
         }

         if (avcodec_open2(sc->m_codecCtx, codec, NULL) < 0)
         {
            wxLogError(wxT("FFmpeg : avcodec_open() failed. Index[%02d], Codec[%02x - %s]"),i,id,name);
            //Can't open decoder - skip this stream
            continue;
         }

         // Stream is decodeable and it is audio. Add it and its description to the arrays
         int duration = 0;
         if (sc->m_stream->duration > 0)
            duration = sc->m_stream->duration * sc->m_stream->time_base.num / sc->m_stream->time_base.den;
         else
            duration = mFormatContext->duration / AV_TIME_BASE;
         wxString bitrate;
         if (sc->m_codecCtx->bit_rate > 0)
            bitrate.Printf(wxT("%d"),(int)sc->m_codecCtx->bit_rate);
         else
            bitrate.Printf(wxT("?"));

         AVDictionaryEntry *tag = av_dict_get(sc->m_stream->metadata, "language", NULL, 0);
         wxString lang;
         if (tag)
         {
            lang.FromUTF8(tag->value);
         }
         auto strinfo = XO(
/* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
"Index[%02x] Codec[%s], Language[%s], Bitrate[%s], Channels[%d], Duration[%d]")
            .Format(
               sc->m_stream->id,
               codec->name,
               lang,
               bitrate,
               (int)sc->m_stream->codec->channels,
               (int)duration);
         mStreamInfo.push_back(strinfo);
         mScs->get()[mNumStreams++] = std::move(sc);
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

   // Remove stream contexts which are not marked for importing and adjust mScs and mNumStreams accordingly
   const auto scs = mScs->get();
   for (int i = 0; i < mNumStreams;)
   {
      if (!scs[i]->m_use)
      {
         for (int j = i; j < mNumStreams - 1; j++)
         {
            scs[j] = std::move(scs[j+1]);
         }
         mNumStreams--;
      }
      else i++;
   }

   mChannels.resize(mNumStreams);

   int s = -1;
   for (auto &stream : mChannels)
   {
      ++s;

      auto sc = scs[s].get();
      switch (sc->m_stream->codec->sample_fmt)
      {
         case AV_SAMPLE_FMT_U8:
         case AV_SAMPLE_FMT_S16:
         case AV_SAMPLE_FMT_U8P:
         case AV_SAMPLE_FMT_S16P:
            sc->m_osamplesize = sizeof(int16_t);
            sc->m_osamplefmt = int16Sample;
         break;
         default:
            sc->m_osamplesize = sizeof(float);
            sc->m_osamplefmt = floatSample;
         break;
      }

      // There is a possibility that number of channels will change over time, but we do not have WaveTracks for NEW channels. Remember the number of channels and stick to it.
      sc->m_initialchannels = sc->m_stream->codec->channels;
      stream.resize(sc->m_stream->codec->channels);
      for (auto &channel : stream)
         channel = NewWaveTrack(*trackFactory, sc->m_osamplefmt, sc->m_stream->codec->sample_rate);
   }

   // Handles the start_time by creating silence. This may or may not be correct.
   // There is a possibility that we should ignore first N milliseconds of audio instead. I do not know.
   /// TODO: Nag FFmpeg devs about start_time until they finally say WHAT is this and HOW to handle it.
   s = -1;
   for (auto &stream : mChannels)
   {
      ++s;

      int64_t stream_delay = 0;
      auto sc = scs[s].get();
      if (sc->m_stream->start_time != int64_t(AV_NOPTS_VALUE) && sc->m_stream->start_time > 0)
      {
         stream_delay = sc->m_stream->start_time;
         wxLogDebug(wxT("Stream %d start_time = %lld, that would be %f milliseconds."), s, (long long) sc->m_stream->start_time, double(sc->m_stream->start_time)/AV_TIME_BASE*1000);
      }
      if (stream_delay > 0)
      {
         int c = -1;
         for (auto &channel : stream)
         {
            ++c;

            WaveTrack *t = channel.get();
            t->InsertSilence(0,double(stream_delay)/AV_TIME_BASE);
         }
      }
   }
   // This is the heart of the importing process
   // The result of Import() to be returned. It will be something other than zero if user canceled or some error appears.
   auto res = ProgressResult::Success;

   // Read next frame.
   for (streamContext *sc; (sc = ReadNextFrame()) != NULL && (res == ProgressResult::Success);)
   {
      // ReadNextFrame returns 1 if stream is not to be imported
      if (sc != (streamContext*)1)
      {
         // Decode frame until it is not possible to decode any further
         while (sc->m_pktRemainingSiz > 0 && (res == ProgressResult::Success || res == ProgressResult::Stopped))
         {
            if (DecodeFrame(sc,false) < 0)
               break;

            // If something useable was decoded - write it to mChannels
            if (sc->m_frameValid)
               res = WriteData(sc);
         }

         // Cleanup after frame decoding
         sc->m_pkt.reset();
      }
   }

   // Flush the decoders.
   if ((mNumStreams != 0) && (res == ProgressResult::Success || res == ProgressResult::Stopped))
   {
      for (int i = 0; i < mNumStreams; i++)
      {
         auto sc = scs[i].get();
         sc->m_pkt.emplace();
         if (DecodeFrame(sc, true) == 0)
         {
            WriteData(sc);

            sc->m_pkt.reset();
         }
      }
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

streamContext *FFmpegImportFileHandle::ReadNextFrame()
{
   // Get pointer to array of contiguous unique_ptrs
   auto scs = mScs->get();
   // This reinterpret_cast to array of plain pointers is innocent
   return import_ffmpeg_read_next_frame
      (mFormatContext, reinterpret_cast<streamContext**>(scs), mNumStreams);
}

int FFmpegImportFileHandle::DecodeFrame(streamContext *sc, bool flushing)
{
   return import_ffmpeg_decode_frame(sc, flushing);
}

ProgressResult FFmpegImportFileHandle::WriteData(streamContext *sc)
{
   // Find the stream index in mScs array
   int streamid = -1;
   auto iter = mChannels.begin();
   auto scs = mScs->get();
   for (int i = 0; i < mNumStreams; ++iter, ++i)
   {
      if (scs[i].get() == sc)
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

   // Allocate the buffer to store audio.
   auto insamples = sc->m_decodedAudioSamplesValidSiz / sc->m_samplesize;
   size_t nChannels = std::min(sc->m_stream->codec->channels, sc->m_initialchannels);

   ArraysOf<uint8_t> tmp{ nChannels, sc->m_osamplesize * (insamples / nChannels) };

   // Separate the channels and convert input sample format to 16-bit
   uint8_t *in = sc->m_decodedAudioSamples.get();
   int index = 0;
   unsigned int pos = 0;
   while (pos < insamples)
   {
      for (size_t chn = 0; (int)chn < sc->m_stream->codec->channels; chn++)
      {
         if (chn < nChannels)
         {
            switch (sc->m_samplefmt)
            {
               case AV_SAMPLE_FMT_U8:
               case AV_SAMPLE_FMT_U8P:
                  ((int16_t *)tmp[chn].get())[index] = (int16_t) (*(uint8_t *)in - 0x80) << 8;
               break;

               case AV_SAMPLE_FMT_S16:
               case AV_SAMPLE_FMT_S16P:
                  ((int16_t *)tmp[chn].get())[index] = (int16_t) *(int16_t *)in;
               break;

               case AV_SAMPLE_FMT_S32:
               case AV_SAMPLE_FMT_S32P:
                  ((float *)tmp[chn].get())[index] = (float) *(int32_t *)in * (1.0 / (1u << 31));
               break;

               case AV_SAMPLE_FMT_FLT:
               case AV_SAMPLE_FMT_FLTP:
                  ((float *)tmp[chn].get())[index] = (float) *(float *)in;
               break;

               case AV_SAMPLE_FMT_DBL:
               case AV_SAMPLE_FMT_DBLP:
                  ((float *)tmp[chn].get())[index] = (float) *(double *)in;
               break;

               default:
                  wxLogError(wxT("Stream %d has unrecognized sample format %d."), streamid, sc->m_samplefmt);
                  return ProgressResult::Success;
               break;
            }
         }
         in += sc->m_samplesize;
         pos++;
      }
      index++;
   }

   // Write audio into WaveTracks
   auto iter2 = iter->begin();
   for (size_t chn=0; chn < nChannels; ++iter2, ++chn)
   {
      iter2->get()->Append((samplePtr)tmp[chn].get(), sc->m_osamplefmt, index);
   }

   // Try to update the progress indicator (and see if user wants to cancel)
   auto updateResult = ProgressResult::Success;
   int64_t filesize = avio_size(mFormatContext->pb);
   // PTS (presentation time) is the proper way of getting current position
   if (sc->m_pkt->pts != int64_t(AV_NOPTS_VALUE) && mFormatContext->duration != int64_t(AV_NOPTS_VALUE))
   {
      mProgressPos = sc->m_pkt->pts * sc->m_stream->time_base.num / sc->m_stream->time_base.den;
      mProgressLen = (mFormatContext->duration > 0 ? mFormatContext->duration / AV_TIME_BASE: 1);
   }
   // When PTS is not set, use number of frames and number of current frame
   else if (sc->m_stream->nb_frames > 0 && sc->m_codecCtx->frame_number > 0 && sc->m_codecCtx->frame_number <= sc->m_stream->nb_frames)
   {
      mProgressPos = sc->m_codecCtx->frame_number;
      mProgressLen = sc->m_stream->nb_frames;
   }
   // When number of frames is unknown, use position in file
   else if (filesize > 0 && sc->m_pkt->pos > 0 && sc->m_pkt->pos <= filesize)
   {
      mProgressPos = sc->m_pkt->pos;
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

   if (wxString(mFormatContext->iformat->name).Contains("m4a"))
   {
      GetMetadata(temp, TAG_ARTIST, "artist");
      GetMetadata(temp, TAG_YEAR, "date");
   }
   else if (wxString(mFormatContext->iformat->name).Contains("asf")) /* wma */
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
   AVDictionaryEntry *meta;

   meta = av_dict_get(mFormatContext->metadata, name, NULL, AV_DICT_IGNORE_SUFFIX);
   if (meta)
   {
      tags.SetTag(tag, wxString::FromUTF8(meta->value));
   }
}


FFmpegImportFileHandle::~FFmpegImportFileHandle()
{
   if (FFmpegLibsInst()->ValidLibsLoaded())
      av_log_set_callback(av_log_default_callback);

   // Do this before unloading the libraries
   mContext.reset();

   DropFFmpegLibs();
}

#endif //USE_FFMPEG
