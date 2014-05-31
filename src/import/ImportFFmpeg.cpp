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

#include "../Audacity.h"    // needed before FFmpeg.h
#include "../FFmpeg.h"      // which brings in avcodec.h, avformat.h
#include "../ondemand/ODManager.h"
#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif


#define DESC _("FFmpeg-compatible files")

//TODO: remove non-audio extensions
#if defined(USE_FFMPEG)
static const wxChar *exts[] =
{
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
#include "ImportFFmpeg.h"
#include "../Tags.h"
#include "../Internat.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"


#ifdef EXPERIMENTAL_OD_FFMPEG
#include "../ondemand/ODDecodeFFmpegTask.h"
#endif

extern FFmpegLibs *FFmpegLibsInst;

class FFmpegImportFileHandle;

/// A representative of FFmpeg loader in
/// the Audacity import plugin list
class FFmpegImportPlugin : public ImportPlugin
{
public:
   FFmpegImportPlugin():
      ImportPlugin(wxArrayString(WXSIZEOF(exts),exts))
      {

      }

   ~FFmpegImportPlugin() { }

   wxString GetPluginStringID() { return wxT("libav"); }
   wxString GetPluginFormatDescription();

   ///! Probes the file and opens it if appropriate
   ImportFileHandle *Open(wxString Filename);
};

///! Does acual import, returned by FFmpegImportPlugin::Open
class FFmpegImportFileHandle : public ImportFileHandle
{

public:
   FFmpegImportFileHandle(const wxString & name);
   ~FFmpegImportFileHandle();

   ///! Format initialization
   ///\return true if successful, false otherwise
   bool Init();
   ///! Codec initialization
   ///\return true if successful, false otherwise
   bool InitCodecs();


   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   ///! Imports audio
   ///\return import status (see Import.cpp)
   int Import(TrackFactory *trackFactory, Track ***outTracks,
      int *outNumTracks, Tags *tags);

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
   ///\return 0 on success, 1 on error or interruption
   int WriteData(streamContext *sc);

   ///! Writes extracted metadata to tags object
   ///\param avf - file context
   ///\ tags - Audacity tags object
   void WriteMetadata(Tags *tags);

   ///! Retrieves metadata from FFmpeg and converts to wxString
   ///\param avf - file context
   ///\ tags - Audacity tags object
   ///\ tag - name of tag to set
   ///\ name - name of metadata item to retrieve
   void GetMetadata(Tags *tags, const wxChar *tag, const char *name);

   ///! Called by Import.cpp
   ///\return number of readable streams in the file
   wxInt32 GetStreamCount()
   {
      return mNumStreams;
   }

   ///! Called by Import.cpp
   ///\return array of strings - descriptions of the streams
   wxArrayString *GetStreamInfo()
   {
      return mStreamInfo;
   }

   ///! Called by Import.cpp
   ///\param StreamID - index of the stream in mStreamInfo and mScs arrays
   ///\param Use - true if this stream should be imported, false otherwise
   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
      if (StreamID < mNumStreams)
         mScs[StreamID]->m_use = Use;
   }

private:

   AVFormatContext      *mFormatContext; //!< Format description, also contains metadata and some useful info
   int                   mNumStreams;    //!< mNumstreams is less or equal to mFormatContext->nb_streams
   streamContext       **mScs;           //!< Array of pointers to stream contexts. Length is mNumStreams.
   wxArrayString        *mStreamInfo;    //!< Array of stream descriptions. Length is mNumStreams

   wxInt64               mProgressPos;   //!< Current timestamp, file position or whatever is used as first argument for Update()
   wxInt64               mProgressLen;   //!< Duration, total length or whatever is used as second argument for Update()

   bool                  mCancelled;     //!< True if importing was canceled by user
   bool                  mStopped;       //!< True if importing was stopped by user
   wxString              mName;
   WaveTrack           ***mChannels;     //!< 2-dimentional array of WaveTrack's. First dimention - streams, second - channels of a stream. Length is mNumStreams
#ifdef EXPERIMENTAL_OD_FFMPEG
   bool                  mUsingOD;
#endif

};


void GetFFmpegImportPlugin(ImportPluginList *importPluginList,
                           UnusableImportPluginList *WXUNUSED(unusableImportPluginList))
{
   importPluginList->Append(new FFmpegImportPlugin);
}


wxString FFmpegImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *FFmpegImportPlugin::Open(wxString filename)
{
   FFmpegImportFileHandle *handle = new FFmpegImportFileHandle(filename);

   //Check if we're loading explicitly supported format
   wxString extension = filename.AfterLast(wxT('.'));
   if (SupportsExtension(extension))
   {
      //Audacity is trying to load something that is declared as
      //officially supported by this plugin.
      //If we don't have FFmpeg configured - tell the user about it.
      //Since this will be happening often, use disableable "FFmpeg not found" dialog
      //insdead of usual wxMessageBox()
      bool newsession = false;
      gPrefs->Read(wxT("/NewImportingSession"), &newsession);
      if (!FFmpegLibsInst->ValidLibsLoaded())
      {
         int dontShowDlg;
         FFmpegNotFoundDialog *dlg;
         gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
         if (dontShowDlg == 0 && newsession)
         {
            gPrefs->Write(wxT("/NewImportingSession"), false);
            gPrefs->Flush();
            dlg = new FFmpegNotFoundDialog(NULL);
            dlg->ShowModal();
            delete dlg;
         }
      }
   }
   if (!FFmpegLibsInst->ValidLibsLoaded())
   {
      delete handle;
      return NULL;
   }

   // Open the file for import
   bool success = handle->Init();
   if (!success) {
      delete handle;
      return NULL;
   }

   return handle;
}


FFmpegImportFileHandle::FFmpegImportFileHandle(const wxString & name)
:  ImportFileHandle(name)
{
   PickFFmpegLibs();

   mStreamInfo = new wxArrayString();
   mFormatContext = NULL;
   mNumStreams = 0;
   mScs = NULL;
   mCancelled = false;
   mStopped = false;
   mName = name;
   mChannels = NULL;
   mProgressPos = 0;
   mProgressLen = 1;
}

bool FFmpegImportFileHandle::Init()
{
   //FFmpegLibsInst->LoadLibs(NULL,false); //Loaded at startup or from Prefs now

   if (!FFmpegLibsInst->ValidLibsLoaded()) return false;

   av_log_set_callback(av_log_wx_callback);

   int err = ufile_fopen_input(&mFormatContext, mName);
   if (err < 0)
   {
      wxLogError(wxT("FFmpeg : av_open_input_file() failed for file %s"),mName.c_str());
      return false;
   }

   err = avformat_find_stream_info(mFormatContext, NULL);
   if (err < 0)
   {
      wxLogError(wxT("FFmpeg: avformat_find_stream_info() failed for file %s"),mName.c_str());
      return false;
   }

   InitCodecs();
   return true;
}

bool FFmpegImportFileHandle::InitCodecs()
{
   // Allocate the array of pointers to hold stream contexts pointers
   // Some of the allocated space may be unused (corresponds to video, subtitle, or undecodeable audio streams)
   mScs = (streamContext**)malloc(sizeof(streamContext**)*mFormatContext->nb_streams);
   // Fill the stream contexts
   for (unsigned int i = 0; i < mFormatContext->nb_streams; i++)
   {
      if (mFormatContext->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO)
      {
         //Create a context
         streamContext *sc = new streamContext;
         memset(sc,0,sizeof(*sc));

         sc->m_stream = mFormatContext->streams[i];
         sc->m_codecCtx = sc->m_stream->codec;

         AVCodec *codec = avcodec_find_decoder(sc->m_codecCtx->codec_id);
         if (codec == NULL)
         {
            wxLogError(wxT("FFmpeg : avcodec_find_decoder() failed. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //FFmpeg can't decode this stream, skip it
            delete sc;
            continue;
         }
         if (codec->type != sc->m_codecCtx->codec_type)
         {
            wxLogError(wxT("FFmpeg : Codec type mismatch, skipping. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //Non-audio codec reported as audio? Nevertheless, we don't need THIS.
            delete sc;
            continue;
         }

         if (avcodec_open2(sc->m_codecCtx, codec, NULL) < 0)
         {
            wxLogError(wxT("FFmpeg : avcodec_open() failed. Index[%02d], Codec[%02x - %s]"),i,sc->m_codecCtx->codec_id,sc->m_codecCtx->codec_name);
            //Can't open decoder - skip this stream
            delete sc;
            continue;
         }

         // Stream is decodeable and it is audio. Add it and its decription to the arrays
         wxString strinfo;
         int duration = 0;
         if (sc->m_stream->duration > 0)
            duration = sc->m_stream->duration * sc->m_stream->time_base.num / sc->m_stream->time_base.den;
         else
            duration = mFormatContext->duration / AV_TIME_BASE;
         wxString bitrate = wxT("");
         if (sc->m_codecCtx->bit_rate > 0)
            bitrate.Printf(wxT("%d"),sc->m_codecCtx->bit_rate);
         else
            bitrate.Printf(wxT("?"));

         AVDictionaryEntry *tag = av_dict_get(sc->m_stream->metadata, "language", NULL, 0);
         wxString lang;
         if (tag)
         {
            lang.FromUTF8(tag->value);
         }
         strinfo.Printf(_("Index[%02x] Codec[%S], Language[%S], Bitrate[%S], Channels[%d], Duration[%d]"),sc->m_stream->id,codec->name,lang.c_str(),bitrate.c_str(),sc->m_stream->codec->channels, duration);
         mStreamInfo->Add(strinfo);
         mScs[mNumStreams++] = sc;
      }
      //for video and unknown streams do nothing
   }
   //It doesn't really returns false, but GetStreamCount() will return 0 if file is composed entierly of unreadable streams
   return true;
}

wxString FFmpegImportFileHandle::GetFileDescription()
{
   return DESC;
}


int FFmpegImportFileHandle::GetFileUncompressedBytes()
{
   // TODO: Get Uncompressed byte count.
   return 0;
}

int FFmpegImportFileHandle::Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags)
{

   CreateProgress();

   // Remove stream contexts which are not marked for importing and adjust mScs and mNumStreams accordingly
   for (int i = 0; i < mNumStreams;)
   {
      if (!mScs[i]->m_use)
      {
         delete mScs[i];
         for (int j = i; j < mNumStreams - 1; j++)
         {
            mScs[j] = mScs[j+1];
         }
         mNumStreams--;
      }
      else i++;
   }

   mChannels = new WaveTrack **[mNumStreams];

   for (int s = 0; s < mNumStreams; s++)
   {
      switch (mScs[s]->m_stream->codec->sample_fmt)
      {
         case AV_SAMPLE_FMT_U8:
         case AV_SAMPLE_FMT_S16:
         case AV_SAMPLE_FMT_U8P:
         case AV_SAMPLE_FMT_S16P:
            mScs[s]->m_osamplesize = sizeof(int16_t);
            mScs[s]->m_osamplefmt = int16Sample;
         break;
         default:
            mScs[s]->m_osamplesize = sizeof(float);
            mScs[s]->m_osamplefmt = floatSample;
         break;
      }

      // There is a possibility that number of channels will change over time, but we do not have WaveTracks for new channels. Remember the number of channels and stick to it.
      mScs[s]->m_initialchannels = mScs[s]->m_stream->codec->channels;
      mChannels[s] = new WaveTrack *[mScs[s]->m_stream->codec->channels];
      int c;
      for (c = 0; c < mScs[s]->m_stream->codec->channels; c++)
      {
         mChannels[s][c] = trackFactory->NewWaveTrack(mScs[s]->m_osamplefmt, mScs[s]->m_stream->codec->sample_rate);

         if (mScs[s]->m_stream->codec->channels == 2)
         {
            switch (c)
            {
            case 0:
               mChannels[s][c]->SetChannel(Track::LeftChannel);
               mChannels[s][c]->SetLinked(true);
               break;
            case 1:
               mChannels[s][c]->SetChannel(Track::RightChannel);
               break;
            }
         }
         else
         {
            mChannels[s][c]->SetChannel(Track::MonoChannel);
         }
      }
   }

   // Handles the start_time by creating silence. This may or may not be correct.
   // There is a possibility that we should ignore first N milliseconds of audio instead. I do not know.
   /// TODO: Nag FFmpeg devs about start_time until they finally say WHAT is this and HOW to handle it.
   for (int s = 0; s < mNumStreams; s++)
   {
      int64_t stream_delay = 0;
      if (mScs[s]->m_stream->start_time != int64_t(AV_NOPTS_VALUE) && mScs[s]->m_stream->start_time > 0)
      {
         stream_delay = mScs[s]->m_stream->start_time;
         wxLogDebug(wxT("Stream %d start_time = %d, that would be %f milliseconds."), s, mScs[s]->m_stream->start_time, double(mScs[s]->m_stream->start_time)/AV_TIME_BASE*1000);
      }
      if (stream_delay != 0)
      {
         for (int c = 0; c < mScs[s]->m_stream->codec->channels; c++)
         {
            WaveTrack *t = mChannels[s][c];
            t->InsertSilence(0,double(stream_delay)/AV_TIME_BASE);
         }
      }
   }
   // This is the heart of the importing process
   // The result of Import() to be returend. It will be something other than zero if user canceled or some error appears.
   int res = eProgressSuccess;

#ifdef EXPERIMENTAL_OD_FFMPEG
   mUsingOD = false;
   gPrefs->Read(wxT("/Library/FFmpegOnDemand"), &mUsingOD);
   //at this point we know the file is good and that we have to load the number of channels in mScs[s]->m_stream->codec->channels;
   //so for OD loading we create the tracks and releasee the modal lock after starting the ODTask.
   if (mUsingOD) {
      std::vector<ODDecodeFFmpegTask*> tasks;
      //append blockfiles to each stream and add an individual ODDecodeTask for each one.
      for (int s = 0; s < mNumStreams; s++) {
         ODDecodeFFmpegTask* odTask=new ODDecodeFFmpegTask(mScs,mNumStreams,mChannels,mFormatContext, s);
         odTask->CreateFileDecoder(mFilename);

         //each stream has different duration.  We need to know it if seeking is to be allowed.
         sampleCount sampleDuration = 0;
         if (mScs[s]->m_stream->duration > 0)
            sampleDuration = ((sampleCount)mScs[s]->m_stream->duration * mScs[s]->m_stream->time_base.num) *mScs[s]->m_stream->codec->sample_rate / mScs[s]->m_stream->time_base.den;
         else
            sampleDuration = ((sampleCount)mFormatContext->duration *mScs[s]->m_stream->codec->sample_rate) / AV_TIME_BASE;

         //      printf(" OD duration samples %qi, sr %d, secs %d\n",sampleDuration, (int)mScs[s]->m_stream->codec->sample_rate,(int)sampleDuration/mScs[s]->m_stream->codec->sample_rate);

         //for each wavetrack within the stream add coded blockfiles
         for (int c = 0; c < mScs[s]->m_stream->codec->channels; c++) {
            WaveTrack *t = mChannels[s][c];
            odTask->AddWaveTrack(t);

            sampleCount maxBlockSize = t->GetMaxBlockSize();
            //use the maximum blockfile size to divide the sections (about 11secs per blockfile at 44.1khz)
            for (sampleCount i = 0; i < sampleDuration; i += maxBlockSize) {
               sampleCount blockLen = maxBlockSize;
               if (i + blockLen > sampleDuration)
                  blockLen = sampleDuration - i;

               t->AppendCoded(mFilename, i, blockLen, c,ODTask::eODFFMPEG);

               // This only works well for single streams since we assume
               // each stream is of the same duration and channels
               res = mProgress->Update(i+sampleDuration*c+ sampleDuration*mScs[s]->m_stream->codec->channels*s,
                                       sampleDuration*mScs[s]->m_stream->codec->channels*mNumStreams);
               if (res != eProgressSuccess)
                  break;
            }
         }
         tasks.push_back(odTask);
      }
      //Now we add the tasks and let them run, or delete them if the user cancelled
      for(int i=0; i < (int)tasks.size(); i++) {
         if(res==eProgressSuccess)
            ODManager::Instance()->AddNewTask(tasks[i]);
         else
            {
               delete tasks[i];
            }
      }
   } else {
#endif
   streamContext *sc = NULL;

   // Read next frame.
   while ((sc = ReadNextFrame()) != NULL && (res == eProgressSuccess))
   {
      // ReadNextFrame returns 1 if stream is not to be imported
      if (sc != (streamContext*)1)
      {
         // Decode frame until it is not possible to decode any further
         while (sc->m_pktRemainingSiz > 0 && (res == eProgressSuccess || res == eProgressStopped))
         {
            if (DecodeFrame(sc,false) < 0)
               break;

            // If something useable was decoded - write it to mChannels
            if (sc->m_frameValid)
               res = WriteData(sc);
         }

         // Cleanup after frame decoding
         if (sc->m_pktValid)
         {
            av_free_packet(&sc->m_pkt);
            sc->m_pktValid = 0;
         }
      }
   }

   // Flush the decoders.
   if ((mNumStreams != 0) && (res == eProgressSuccess || res == eProgressStopped))
   {
      for (int i = 0; i < mNumStreams; i++)
      {
         if (DecodeFrame(mScs[i], true) == 0)
         {
            WriteData(mScs[i]);

            if (mScs[i]->m_pktValid)
            {
               av_free_packet(&mScs[i]->m_pkt);
               mScs[i]->m_pktValid = 0;
            }
         }
      }
   }
#ifdef EXPERIMENTAL_OD_FFMPEG
   } // else -- !mUsingOD == true
#endif   //EXPERIMENTAL_OD_FFMPEG

   // Something bad happened - destroy everything!
   if (res == eProgressCancelled || res == eProgressFailed)
   {
      for (int s = 0; s < mNumStreams; s++)
      {
         delete[] mChannels[s];
      }
      delete[] mChannels;

      return res;
   }
   //else if (res == 2), we just stop the decoding as if the file has ended

   *outNumTracks = 0;
   for (int s = 0; s < mNumStreams; s++)
   {
      *outNumTracks += mScs[s]->m_initialchannels;
   }

   // Create new tracks
   *outTracks = new Track *[*outNumTracks];

   // Copy audio from mChannels to newly created tracks (destroying mChannels elements in process)
   int trackindex = 0;
   for (int s = 0; s < mNumStreams; s++)
   {
      for(int c = 0; c < mScs[s]->m_initialchannels; c++)
      {
         mChannels[s][c]->Flush();
         (*outTracks)[trackindex++] = mChannels[s][c];
      }
      delete[] mChannels[s];
   }
   delete[] mChannels;

   // Save metadata
   WriteMetadata(tags);

   return res;
}

streamContext *FFmpegImportFileHandle::ReadNextFrame()
{
   return import_ffmpeg_read_next_frame(mFormatContext, mScs, mNumStreams);
}

int FFmpegImportFileHandle::DecodeFrame(streamContext *sc, bool flushing)
{
   return import_ffmpeg_decode_frame(sc, flushing);
}

int FFmpegImportFileHandle::WriteData(streamContext *sc)
{
   // Find the stream index in mScs array
   int streamid = -1;
   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i] == sc)
      {
         streamid = i;
         break;
      }
   }
   // Stream is not found. This should not really happen
   if (streamid == -1)
   {
      return 1;
   }

   // Allocate the buffer to store audio.
   int insamples = sc->m_decodedAudioSamplesValidSiz / sc->m_samplesize;
   int nChannels = sc->m_stream->codec->channels < sc->m_initialchannels ? sc->m_stream->codec->channels : sc->m_initialchannels;
   uint8_t **tmp = (uint8_t **) malloc(sizeof(uint8_t *) * nChannels);
   for (int chn = 0; chn < nChannels; chn++)
   {
      tmp[chn] = (uint8_t *) malloc(sc->m_osamplesize * (insamples / nChannels));
   }

   // Separate the channels and convert input sample format to 16-bit
   uint8_t *in = sc->m_decodedAudioSamples;
   int index = 0;
   int pos = 0;
   while (pos < insamples)
   {
      for (int chn = 0; chn < sc->m_stream->codec->channels; chn++)
      {
         if (chn < nChannels)
         {
            switch (sc->m_samplefmt)
            {
               case AV_SAMPLE_FMT_U8:
               case AV_SAMPLE_FMT_U8P:
                  ((int16_t *)tmp[chn])[index] = (int16_t) (*(uint8_t *)in - 0x80) << 8;
               break;

               case AV_SAMPLE_FMT_S16:
               case AV_SAMPLE_FMT_S16P:
                  ((int16_t *)tmp[chn])[index] = (int16_t) *(int16_t *)in;
               break;

               case AV_SAMPLE_FMT_S32:
               case AV_SAMPLE_FMT_S32P:
                  ((float *)tmp[chn])[index] = (float) *(int32_t *)in * (1.0 / (1 << 31));
               break;

               case AV_SAMPLE_FMT_FLT:
               case AV_SAMPLE_FMT_FLTP:
                  ((float *)tmp[chn])[index] = (float) *(float *)in;
               break;

               case AV_SAMPLE_FMT_DBL:
               case AV_SAMPLE_FMT_DBLP:
                  ((float *)tmp[chn])[index] = (float) *(double *)in;
               break;

               default:
                  wxLogError(wxT("Stream %d has unrecognized sample format %d."), streamid, sc->m_samplefmt);
                  return 1;
               break;
            }
         }
         in += sc->m_samplesize;
         pos++;
      }
      index++;
   }

   // Write audio into WaveTracks
   for (int chn=0; chn < nChannels; chn++)
   {
      mChannels[streamid][chn]->Append((samplePtr)tmp[chn],sc->m_osamplefmt,index);
      free(tmp[chn]);
   }

   free(tmp);

   // Try to update the progress indicator (and see if user wants to cancel)
   int updateResult = eProgressSuccess;
   int64_t filesize = avio_size(mFormatContext->pb);
   // PTS (presentation time) is the proper way of getting current position
   if (sc->m_pkt.pts != int64_t(AV_NOPTS_VALUE) && mFormatContext->duration != int64_t(AV_NOPTS_VALUE))
   {
      mProgressPos = sc->m_pkt.pts * sc->m_stream->time_base.num / sc->m_stream->time_base.den;
      mProgressLen = (mFormatContext->duration > 0 ? mFormatContext->duration / AV_TIME_BASE: 1);
   }
   // When PTS is not set, use number of frames and number of current frame
   else if (sc->m_stream->nb_frames > 0 && sc->m_codecCtx->frame_number > 0 && sc->m_codecCtx->frame_number <= sc->m_stream->nb_frames)
   {
      mProgressPos = sc->m_codecCtx->frame_number;
      mProgressLen = sc->m_stream->nb_frames;
   }
   // When number of frames is unknown, use position in file
   else if (filesize > 0 && sc->m_pkt.pos > 0 && sc->m_pkt.pos <= filesize)
   {
      mProgressPos = sc->m_pkt.pos;
      mProgressLen = filesize;
   }
   updateResult = mProgress->Update(mProgressPos, mProgressLen != 0 ? mProgressLen : 1);

   return updateResult;
}

void FFmpegImportFileHandle::WriteMetadata(Tags *tags)
{
   tags->Clear();

   GetMetadata(tags, TAG_TITLE, "title");
   GetMetadata(tags, TAG_ARTIST, "author");
//   GetMetadata(tags, TAG_COPYRIGHT, "copyright");
   GetMetadata(tags, TAG_COMMENTS, "comment");
   GetMetadata(tags, TAG_ALBUM, "album");
   GetMetadata(tags, TAG_YEAR, "year");
   GetMetadata(tags, TAG_TRACK, "track");
   GetMetadata(tags, TAG_GENRE, "genre");
}

void FFmpegImportFileHandle::GetMetadata(Tags *tags, const wxChar *tag, const char *name)
{
   AVDictionaryEntry *meta;

   meta = av_dict_get(mFormatContext->metadata, name, NULL, AV_DICT_IGNORE_SUFFIX);
   if (meta)
   {
      tags->SetTag(tag, wxString::FromUTF8(meta->value));
   }
}


FFmpegImportFileHandle::~FFmpegImportFileHandle()
{
#ifdef EXPERIMENTAL_OD_FFMPEG
   //ODDecodeFFmpegTask takes ownership and deltes it there.
   if(!mUsingOD)
   {
#endif
   if (FFmpegLibsInst->ValidLibsLoaded())
   {
      if (mFormatContext) avformat_close_input(&mFormatContext);
      av_log_set_callback(av_log_default_callback);
   }

   for (int i = 0; i < mNumStreams; i++)
   {
      if (mScs[i]->m_decodedAudioSamples != NULL)
         av_free(mScs[i]->m_decodedAudioSamples);

      delete mScs[i];
   }
   free(mScs);
#ifdef EXPERIMENTAL_OD_FFMPEG
   }//mUsingOD
#endif


   delete mStreamInfo;

   DropFFmpegLibs();
}

#endif //USE_FFMPEG
