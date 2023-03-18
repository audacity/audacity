/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpeg.cpp

   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

******************************************************************//**

\class ExportFFmpeg
\brief Controlling class for FFmpeg exporting.  Creates the options
dialog of the appropriate type, adds tags and invokes the export
function.

*//*******************************************************************/




#include "../FFmpeg.h"
#include "FFmpegFunctions.h"

#include <wx/choice.h>
#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/window.h>
#include <wx/spinctrl.h>
#include <wx/combobox.h>

#include "Mix.h"
#include "ProjectRate.h"
#include "Tags.h"
#include "Track.h"
#include "AudacityMessageBox.h"
#include "ProgressDialog.h"
#include "wxFileNameWrapper.h"

#include "Export.h"

#include "ExportFFmpegDialogs.h"
#include "SelectFile.h"

#if defined(WIN32) && _MSC_VER < 1900
#define snprintf _snprintf
#endif

#if defined(USE_FFMPEG)

// Define this to automatically resample audio to the nearest supported sample rate
#define FFMPEG_AUTO_RESAMPLE 1

static bool CheckFFmpegPresence(bool quiet = false)
{
   bool result = true;
   auto ffmpeg = FFmpegFunctions::Load();

   if (!ffmpeg)
   {
      if (!quiet)
      {
         AudacityMessageBox(XO(
"Properly configured FFmpeg is required to proceed.\nYou can configure it at Preferences > Libraries."));
      }
      result = false;
   }

   return result;
}

static int AdjustFormatIndex(int format)
{
   int subFormat = -1;
   for (int i = 0; i <= FMT_OTHER; i++)
   {
      if (ExportFFmpegOptions::fmts[i].compiledIn) subFormat++;
      if (subFormat == format || i == FMT_OTHER)
      {
         subFormat = i;
         break;
      }
   }
   return subFormat;
}

//----------------------------------------------------------------------------
// ExportFFmpeg
//----------------------------------------------------------------------------

class ExportFFmpeg final : public ExportPlugin
{
public:

   ExportFFmpeg();
   ~ExportFFmpeg() override;

   /// Callback, called from GetFilename
   bool CheckFileName(wxFileName &filename, int format = 0) override;

   /// Format initialization
   bool Init(const char *shortname, AudacityProject *project, const Tags *metadata, int subformat);

   /// Writes metadata
   bool AddTags(const Tags *metadata);

   /// Sets individual metadata values
   void SetMetadata(const Tags *tags, const char *name, const wxChar *tag);

   /// Encodes audio
   bool EncodeAudioFrame(int16_t *pFrame, size_t frameSize);

   /// Flushes audio encoder
   bool Finalize();

   void FreeResources();

   /// Creates options panel
   ///\param format - index of export type
   void OptionsCreate(ShuttleGui &S, int format) override;

   /// Check whether or not current project sample rate is compatible with the export codec
   bool CheckSampleRate(int rate, int lowrate, int highrate, const int *sampRates);

   /// Asks user to resample the project or cancel the export procedure
   int  AskResample(int bitrate, int rate, int lowrate, int highrate, const int *sampRates);

   /// Exports audio
   ///\param project Audacity project
   ///\param fName output file name
   ///\param selectedOnly true if exporting only selected audio
   ///\param t0 audio start time
   ///\param t1 audio end time
   ///\param mixerSpec mixer
   ///\param metadata tags to write into file
   ///\param subformat index of export type
   ///\return true if export succeeded
   ProgressResult Export(AudacityProject *project,
      std::unique_ptr<BasicUI::ProgressDialog>& pDialog,
      unsigned channels,
      const wxFileNameWrapper &fName,
      bool selectedOnly,
      double t0,
      double t1,
      MixerSpec *mixerSpec = NULL,
      const Tags *metadata = NULL,
      int subformat = 0) override;

private:
   /// Codec initialization
   bool InitCodecs(AudacityProject* project);

   bool WritePacket(AVPacketWrapper& packet);

   int EncodeAudio(AVPacketWrapper& pkt, int16_t* audio_samples, int nb_samples);

   std::shared_ptr<FFmpegFunctions> mFFmpeg;

   std::unique_ptr<AVOutputFormatWrapper> mEncFormatDesc;       // describes our output file to libavformat
   int mDefaultFrameSize {};
   std::unique_ptr<AVStreamWrapper> mEncAudioStream; // the output audio stream (may remain NULL)
   int mEncAudioFifoOutBufSize {};

   wxFileNameWrapper mName;

   int               mSubFormat{};
   int               mBitRate{};
   int               mSampleRate{};
   unsigned          mChannels{};
   bool              mSupportsUTF8{};

   // Smart pointer fields, their order is the reverse in which they are reset in FreeResources():
   std::unique_ptr<AVFifoBufferWrapper> mEncAudioFifo; // FIFO to write incoming audio samples into
   AVDataBuffer<int16_t> mEncAudioFifoOutBuf; // buffer to read _out_ of the FIFO into
   std::unique_ptr<AVFormatContextWrapper> mEncFormatCtx; // libavformat's context for our output file
   std::unique_ptr<AVCodecContextWrapper> mEncAudioCodecCtx;    // the encoder for the output audio stream
};

ExportFFmpeg::ExportFFmpeg()
:  ExportPlugin()
{
   mEncFormatDesc = NULL;      // describes our output file to libavformat
   mEncAudioStream = NULL;     // the output audio stream (may remain NULL)
   #define MAX_AUDIO_PACKET_SIZE (128 * 1024)
   mEncAudioFifoOutBufSize = 0;

   mSampleRate = 0;
   mSupportsUTF8 = true;

   mFFmpeg = FFmpegFunctions::Load();

   int avfver = mFFmpeg ? mFFmpeg->AVFormatVersion.GetIntVersion() : 0;

   int newfmt;
   // Adds export types from the export type list
   for (newfmt = 0; newfmt < FMT_LAST; newfmt++)
   {
      wxString shortname(ExportFFmpegOptions::fmts[newfmt].shortname);
      // Don't hide export types when there's no av-libs, and don't hide FMT_OTHER
      if (newfmt < FMT_OTHER && mFFmpeg)
      {
         // Format/Codec support is compiled in?
         auto avoformat = mFFmpeg->GuessOutputFormat(shortname.mb_str(), nullptr, nullptr);
         auto avcodec = mFFmpeg->CreateEncoder(mFFmpeg->GetAVCodecID(ExportFFmpegOptions::fmts[newfmt].codecid));

         if (avoformat == NULL || avcodec == NULL)
         {
            ExportFFmpegOptions::fmts[newfmt].compiledIn = false;
            continue;
         }
      }
      int fmtindex = AddFormat() - 1;
      SetFormat(ExportFFmpegOptions::fmts[newfmt].name,fmtindex);
      AddExtension(ExportFFmpegOptions::fmts[newfmt].extension,fmtindex);
      // For some types add other extensions
      switch(newfmt)
      {
      case FMT_M4A:
         AddExtension(wxT("3gp"),fmtindex);
         AddExtension(wxT("m4r"),fmtindex);
         AddExtension(wxT("mp4"),fmtindex);
         break;
      case FMT_WMA2:
         AddExtension(wxT("asf"),fmtindex);
         AddExtension(wxT("wmv"),fmtindex);
         break;
      default:
         break;
      }

      SetMaxChannels(ExportFFmpegOptions::fmts[newfmt].maxchannels,fmtindex);
      SetDescription(ExportFFmpegOptions::fmts[newfmt].description, fmtindex);

      int canmeta = ExportFFmpegOptions::fmts[newfmt].canmetadata;
      if (canmeta && (canmeta == AV_CANMETA || canmeta <= avfver))
      {
         SetCanMetaData(true,fmtindex);
      }
      else
      {
         SetCanMetaData(false,fmtindex);
      }
   }
}

ExportFFmpeg::~ExportFFmpeg()
{
}

bool ExportFFmpeg::CheckFileName(wxFileName & WXUNUSED(filename), int WXUNUSED(format))
{
   bool result = true;

   // Show "Locate FFmpeg" dialog
   if (!CheckFFmpegPresence(true))
   {
      FindFFmpegLibs();
      mFFmpeg = FFmpegFunctions::Load();

      return LoadFFmpeg(true);
   }

   return result;
}

bool ExportFFmpeg::Init(const char *shortname, AudacityProject *project, const Tags *metadata, int subformat)
{
   // This will undo the acquisition of resources along any early exit path:
   auto deleter = [](ExportFFmpeg *This) {
      if (This)
         This->FreeResources();
   };
   std::unique_ptr<ExportFFmpeg, decltype(deleter)> cleanup{ this, deleter };
   //FFmpegLibsInst()->LoadLibs(NULL,true); //Loaded at startup or from Prefs now

   if (!mFFmpeg)
      return false;


   // See if libavformat has modules that can write our output format. If so, mEncFormatDesc
   // will describe the functions used to write the format (used internally by libavformat)
   // and the default video/audio codecs that the format uses.
   const auto path = mName.GetFullPath();
   if ((mEncFormatDesc = mFFmpeg->GuessOutputFormat(shortname, OSINPUT(path), nullptr)) == nullptr)
   {
      AudacityMessageBox(
         XO(
"FFmpeg : ERROR - Can't determine format description for file \"%s\".")
            .Format( path ),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION );
      return false;
   }

   // mEncFormatCtx is used by libavformat to carry around context data re our output file.
   mEncFormatCtx = mFFmpeg->CreateAVFormatContext();
   if (!mEncFormatCtx)
   {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - Can't allocate output format context."),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION);
      return false;
   }

   // Initialise the output format context.
   mEncFormatCtx->SetOutputFormat(mFFmpeg->CreateAVOutputFormatWrapper(mEncFormatDesc->GetWrappedValue()));
   mEncFormatCtx->SetFilename(OSINPUT(path));

   // At the moment Audacity can export only one audio stream
   if ((mEncAudioStream = mEncFormatCtx->CreateStream()) == nullptr)
   {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - Can't add audio stream to output file \"%s\".")
            .Format( path ),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION);
      return false;
   }

   // Documentation for avformat_new_stream says
   // "User is required to call avcodec_close() and avformat_free_context() to clean
   // up the allocation by avformat_new_stream()."

   // We use smart pointers that ensure these cleanups either in their destructors or
   // sooner if they are reset.  These are std::unique_ptr with nondefault deleter
   // template parameters.

   // mEncFormatCtx takes care of avformat_free_context(), so
   // mEncAudioStream can be a plain pointer.

   // mEncAudioCodecCtx now becomes responsible for closing the codec:
   mEncAudioCodecCtx = mEncAudioStream->GetAVCodecContext();
   mEncAudioStream->SetId(0);

   // Open the output file.
   if (!(mEncFormatDesc->GetFlags() & AUDACITY_AVFMT_NOFILE))
   {
      AVIOContextWrapper::OpenResult result =
         mEncFormatCtx->OpenOutputContext(path);

      if (result != AVIOContextWrapper::OpenResult::Success)
      {
         AudacityMessageBox(
            XO("FFmpeg : ERROR - Can't open output file \"%s\" to write. Error code is %d.")
               .Format(path, static_cast<int>(result)),
            XO("FFmpeg Error"),
            wxOK|wxCENTER|wxICON_EXCLAMATION);

         return false;
      }
   }

   // Open the audio stream's codec and initialise any stream related data.
   if (!InitCodecs(project))
      return false;

   if (mEncAudioStream->SetParametersFromContext(*mEncAudioCodecCtx) < 0)
      return false;

   if (metadata == NULL)
      metadata = &Tags::Get( *project );

   // Add metadata BEFORE writing the header.
   // At the moment that works with ffmpeg-git and ffmpeg-0.5 for MP4.
   if (GetCanMetaData(subformat))
   {
      mSupportsUTF8 = ExportFFmpegOptions::fmts[mSubFormat].canutf8;
      AddTags(metadata);
   }

   // Write headers to the output file.
   int err =
      mFFmpeg->avformat_write_header(mEncFormatCtx->GetWrappedValue(), nullptr);

   if (err < 0)
   {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - Can't write headers to output file \"%s\". Error code is %d.")
            .Format( path, err ),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION);
      return false;
   }

   // Only now, we can keep all the resources until after Finalize().
   // Cancel the local cleanup.
   cleanup.release();

   return true;
}

bool ExportFFmpeg::CheckSampleRate(int rate, int lowrate, int highrate, const int *sampRates)
{
   if (lowrate && highrate)
   {
      if (rate < lowrate || rate > highrate)
      {
         return false;
      }
   }

   if (sampRates)
   {
      for (int i = 0; sampRates[i] > 0; i++)
      {
         if (rate == sampRates[i])
         {
            return true;
         }
      }
   }

   return false;
}

bool ExportFFmpeg::InitCodecs(AudacityProject *project)
{
   std::unique_ptr<AVCodecWrapper> codec;

   AVDictionaryWrapper options(*mFFmpeg);

   // Get the sample rate from the passed settings if we haven't set it before.
   // Doing this only when not set allows us to carry the sample rate from one
   // iteration of ExportMultiple to the next.  This prevents multiple resampling
   // dialogs in the event the codec can't support the specified rate.
   if (!mSampleRate)
   {
      mSampleRate = (int)ProjectRate::Get(*project).GetRate();
   }

   // Configure the audio stream's codec context.

   const auto codecID = ExportFFmpegOptions::fmts[mSubFormat].codecid;

   mEncAudioCodecCtx->SetGlobalQuality(-99999); //quality mode is off by default;

   // Each export type has its own settings
   switch (mSubFormat)
   {
   case FMT_M4A:
   {
      int q = gPrefs->Read(wxT("/FileFormats/AACQuality"),-99999);

      q = wxClip( q, 98 * mChannels, 160 * mChannels );
      // Set bit rate to between 98 kbps and 320 kbps (if two channels)
      mEncAudioCodecCtx->SetBitRate(q * 1000);
      mEncAudioCodecCtx->SetProfile(AUDACITY_FF_PROFILE_AAC_LOW);
      mEncAudioCodecCtx->SetCutoff(0);

      break;
   }
   case FMT_AC3:
      mEncAudioCodecCtx->SetBitRate(gPrefs->Read(wxT("/FileFormats/AC3BitRate"), 192000));
      if (!CheckSampleRate(
             mSampleRate, ExportFFmpegAC3Options::iAC3SampleRates[0],
             ExportFFmpegAC3Options::iAC3SampleRates[2],
             &ExportFFmpegAC3Options::iAC3SampleRates[0]))
      {
         mSampleRate = AskResample(
            mEncAudioCodecCtx->GetBitRate(), mSampleRate,
            ExportFFmpegAC3Options::iAC3SampleRates[0],
            ExportFFmpegAC3Options::iAC3SampleRates[2],
            &ExportFFmpegAC3Options::iAC3SampleRates[0]);
      }
      break;
   case FMT_AMRNB:
      mSampleRate = 8000;
      mEncAudioCodecCtx->SetBitRate(gPrefs->Read(wxT("/FileFormats/AMRNBBitRate"), 12200));
      break;
   case FMT_OPUS:
      options.Set("b", gPrefs->Read(wxT("/FileFormats/OPUSBitRate"), wxT("128000")), 0);
      options.Set("vbr", gPrefs->Read(wxT("/FileFormats/OPUSVbrMode"), wxT("on")), 0);
      options.Set("compression_level", gPrefs->Read(wxT("/FileFormats/OPUSCompression"), wxT("10")), 0);
      options.Set("frame_duration", gPrefs->Read(wxT("/FileFormats/OPUSFrameDuration"), wxT("20")), 0);
      options.Set("application", gPrefs->Read(wxT("/FileFormats/OPUSApplication"), wxT("audio")), 0);
      options.Set("cutoff", gPrefs->Read(wxT("/FileFormats/OPUSCutoff"), wxT("0")), 0);
      options.Set("mapping_family", mChannels <= 2 ? "0" : "255", 0);
      break;
   case FMT_WMA2:
      mEncAudioCodecCtx->SetBitRate(gPrefs->Read(wxT("/FileFormats/WMABitRate"), 198000));
      if (!CheckSampleRate(
             mSampleRate, ExportFFmpegWMAOptions::iWMASampleRates[0],
             ExportFFmpegWMAOptions::iWMASampleRates[4],
             &ExportFFmpegWMAOptions::iWMASampleRates[0]))
      {
         mSampleRate = AskResample(
            mEncAudioCodecCtx->GetBitRate(), mSampleRate,
            ExportFFmpegWMAOptions::iWMASampleRates[0],
            ExportFFmpegWMAOptions::iWMASampleRates[4],
            &ExportFFmpegWMAOptions::iWMASampleRates[0]);
      }
      break;
   case FMT_OTHER:
   {
      AVDictionaryWrapper streamMetadata = mEncAudioStream->GetMetadata();
      streamMetadata.Set(
         "language",
         gPrefs->Read(wxT("/FileFormats/FFmpegLanguage"), wxT("")), 0);

      mEncAudioStream->SetMetadata(streamMetadata);

      mEncAudioCodecCtx->SetSampleRate(
         gPrefs->Read(wxT("/FileFormats/FFmpegSampleRate"), (long)0));

      if (mEncAudioCodecCtx->GetSampleRate() != 0)
         mSampleRate = mEncAudioCodecCtx->GetSampleRate();

      mEncAudioCodecCtx->SetBitRate(
         gPrefs->Read(wxT("/FileFormats/FFmpegBitRate"), (long)0));

      mEncAudioCodecCtx->SetCodecTagFourCC(
         gPrefs->Read(wxT("/FileFormats/FFmpegTag"), wxT(""))
            .mb_str(wxConvUTF8));

      mEncAudioCodecCtx->SetGlobalQuality(
         gPrefs->Read(wxT("/FileFormats/FFmpegQuality"), (long)-99999));
      mEncAudioCodecCtx->SetCutoff(
         gPrefs->Read(wxT("/FileFormats/FFmpegCutOff"), (long)0));
      mEncAudioCodecCtx->SetFlags2(0);

      if (gPrefs->Read(wxT("/FileFormats/FFmpegBitReservoir"), true))
         options.Set("reservoir", "1", 0);

      if (gPrefs->Read(wxT("/FileFormats/FFmpegVariableBlockLen"), true))
         mEncAudioCodecCtx->SetFlags2(
            mEncAudioCodecCtx->GetFlags2() | 0x0004); // WMA only?

      mEncAudioCodecCtx->SetCompressionLevel(
         gPrefs->Read(wxT("/FileFormats/FFmpegCompLevel"), -1));
      mEncAudioCodecCtx->SetFrameSize(
         gPrefs->Read(wxT("/FileFormats/FFmpegFrameSize"), (long)0));

      // FIXME The list of supported options for the selected encoder should be
      // extracted instead of a few hardcoded
      options.Set(
         "lpc_coeff_precision",
         gPrefs->Read(wxT("/FileFormats/FFmpegLPCCoefPrec"), (long)0));
      options.Set(
         "min_prediction_order",
         gPrefs->Read(wxT("/FileFormats/FFmpegMinPredOrder"), (long)-1));
      options.Set(
         "max_prediction_order",
         gPrefs->Read(wxT("/FileFormats/FFmpegMaxPredOrder"), (long)-1));
      options.Set(
         "min_partition_order",
         gPrefs->Read(wxT("/FileFormats/FFmpegMinPartOrder"), (long)-1));
      options.Set(
         "max_partition_order",
         gPrefs->Read(wxT("/FileFormats/FFmpegMaxPartOrder"), (long)-1));
      options.Set(
         "prediction_order_method",
         gPrefs->Read(wxT("/FileFormats/FFmpegPredOrderMethod"), (long)0));
      options.Set(
         "muxrate", gPrefs->Read(wxT("/FileFormats/FFmpegMuxRate"), (long)0));

      mEncFormatCtx->SetPacketSize(
         gPrefs->Read(wxT("/FileFormats/FFmpegPacketSize"), (long)0));

      codec = mFFmpeg->CreateEncoder(
         gPrefs->Read(wxT("/FileFormats/FFmpegCodec")));

      if (!codec)
         codec = mFFmpeg->CreateEncoder(mEncFormatDesc->GetAudioCodec());
   }
      break;
   default:
      return false;
   }

   // This happens if user refused to resample the project
   if (mSampleRate == 0) return false;

   if (mEncAudioCodecCtx->GetGlobalQuality() >= 0)
   {
      mEncAudioCodecCtx->SetFlags(
         mEncAudioCodecCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_QSCALE);
   }
   else
   {
      mEncAudioCodecCtx->SetGlobalQuality(0);
   }

   mEncAudioCodecCtx->SetGlobalQuality(mEncAudioCodecCtx->GetGlobalQuality() * AUDACITY_FF_QP2LAMBDA);
   mEncAudioCodecCtx->SetSampleRate(mSampleRate);
   mEncAudioCodecCtx->SetChannels(mChannels);
   mEncAudioCodecCtx->SetChannelLayout(mFFmpeg->av_get_default_channel_layout(mChannels));
   mEncAudioCodecCtx->SetTimeBase({ 1, mSampleRate });
   mEncAudioCodecCtx->SetSampleFmt(static_cast<AVSampleFormatFwd>(AUDACITY_AV_SAMPLE_FMT_S16));
   mEncAudioCodecCtx->SetStrictStdCompliance(
      AUDACITY_FF_COMPLIANCE_EXPERIMENTAL);

   if (codecID == AUDACITY_AV_CODEC_ID_AC3)
   {
      // As of Jan 4, 2011, the default AC3 encoder only accept SAMPLE_FMT_FLT samples.
      // But, currently, Audacity only supports SAMPLE_FMT_S16.  So, for now, look for the
      // "older" AC3 codec.  this is not a proper solution, but will suffice until other
      // encoders no longer support SAMPLE_FMT_S16.
      codec = mFFmpeg->CreateEncoder("ac3_fixed");
   }

   if (!codec)
   {
      codec = mFFmpeg->CreateEncoder(mFFmpeg->GetAVCodecID(codecID));
   }

   // Is the required audio codec compiled into libavcodec?
   if (codec == NULL)
   {
      AudacityMessageBox(
         XO(
/* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
"FFmpeg cannot find audio codec 0x%x.\nSupport for this codec is probably not compiled in.")
            .Format(static_cast<const unsigned int>(codecID.value)),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION);
      return false;
   }

   if (codec->GetSampleFmts()) {
      for (int i = 0; codec->GetSampleFmts()[i] != AUDACITY_AV_SAMPLE_FMT_NONE; i++)
      {
         AVSampleFormatFwd fmt = codec->GetSampleFmts()[i];

         if (
            fmt == AUDACITY_AV_SAMPLE_FMT_U8 ||
            fmt == AUDACITY_AV_SAMPLE_FMT_U8P ||
            fmt == AUDACITY_AV_SAMPLE_FMT_S16 ||
            fmt == AUDACITY_AV_SAMPLE_FMT_S16P ||
            fmt == AUDACITY_AV_SAMPLE_FMT_S32 ||
            fmt == AUDACITY_AV_SAMPLE_FMT_S32P ||
            fmt == AUDACITY_AV_SAMPLE_FMT_FLT ||
            fmt == AUDACITY_AV_SAMPLE_FMT_FLTP)
         {
            mEncAudioCodecCtx->SetSampleFmt(fmt);
         }

         if (
            fmt == AUDACITY_AV_SAMPLE_FMT_S16 ||
            fmt == AUDACITY_AV_SAMPLE_FMT_S16P)
            break;
      }
   }

   if (codec->GetSupportedSamplerates())
   {
      // Workaround for crash in bug #2378.  Proper fix is to get a newer version of FFmpeg.
      if (codec->GetId() == mFFmpeg->GetAVCodecID(AUDACITY_AV_CODEC_ID_AAC))
      {
         std::vector<int> rates;
         int i = 0;

         while (codec->GetSupportedSamplerates()[i] &&
                codec->GetSupportedSamplerates()[i] != 7350)
         {
            rates.push_back(codec->GetSupportedSamplerates()[i++]);
         }

         rates.push_back(0);

         if (!CheckSampleRate(mSampleRate, 0, 0, rates.data()))
         {
            mSampleRate = AskResample(0, mSampleRate, 0, 0, rates.data());
            mEncAudioCodecCtx->SetSampleRate(mSampleRate);
         }
      }
      else
      {
         if (!CheckSampleRate(
                mSampleRate, 0, 0, codec->GetSupportedSamplerates()))
         {
            mSampleRate = AskResample(
               0, mSampleRate, 0, 0, codec->GetSupportedSamplerates());
            mEncAudioCodecCtx->SetSampleRate(mSampleRate);
         }
      }

      // This happens if user refused to resample the project
      if (mSampleRate == 0)
      {
         return false;
      }
   }

   if (mEncFormatCtx->GetOutputFormat()->GetFlags() & AUDACITY_AVFMT_GLOBALHEADER)
   {
      mEncAudioCodecCtx->SetFlags(mEncAudioCodecCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER);
      mEncFormatCtx->SetFlags(mEncFormatCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER);
   }

   // Open the codec.
   int rc = mEncAudioCodecCtx->Open(codec.get(), &options);
   if (rc < 0)
   {
      TranslatableString errmsg;

      switch (rc)
      {
      case AUDACITY_AVERROR(EPERM):
         errmsg = XO("The codec reported a generic error (EPERM)");
         break;
      case AUDACITY_AVERROR(EINVAL):
         errmsg = XO("The codec reported an invalid parameter (EINVAL)");
         break;
      default:
         char buf[64];
         mFFmpeg->av_strerror(rc, buf, sizeof(buf));
         errmsg = Verbatim(buf);
      }

      AudacityMessageBox(
         /* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
         XO("Can't open audio codec \"%s\" (0x%x)\n\n%s")
         .Format(codec->GetName(), codecID.value, errmsg),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION);
      return false;
   }

   mDefaultFrameSize = mEncAudioCodecCtx->GetFrameSize();

   if (mDefaultFrameSize == 0)
      mDefaultFrameSize = 1024; // arbitrary non zero value;

   wxLogDebug(
      wxT("FFmpeg : Audio Output Codec Frame Size: %d samples."),
      mEncAudioCodecCtx->GetFrameSize());

   // The encoder may require a minimum number of raw audio samples for each encoding but we can't
   // guarantee we'll get this minimum each time an audio frame is decoded from the input file so
   // we use a FIFO to store up incoming raw samples until we have enough for one call to the codec.
   mEncAudioFifo = mFFmpeg->CreateFifoBuffer(mDefaultFrameSize);

   mEncAudioFifoOutBufSize = 2*MAX_AUDIO_PACKET_SIZE;
   // Allocate a buffer to read OUT of the FIFO into. The FIFO maintains its own buffer internally.
   mEncAudioFifoOutBuf = mFFmpeg->CreateMemoryBuffer<int16_t>(mEncAudioFifoOutBufSize);

   if (mEncAudioFifoOutBuf.empty())
   {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - Can't allocate buffer to read into from audio FIFO."),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION
      );
      return false;
   }

   return true;
}

bool ExportFFmpeg::WritePacket(AVPacketWrapper& pkt)
{
   // Set presentation time of frame (currently in the codec's timebase) in the
   // stream timebase.
   if (pkt.GetPresentationTimestamp() != AUDACITY_AV_NOPTS_VALUE)
      pkt.RescalePresentationTimestamp(
         mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());

   if (pkt.GetDecompressionTimestamp() != AUDACITY_AV_NOPTS_VALUE)
      pkt.RescaleDecompressionTimestamp(
         mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());

   if (pkt.GetDuration() > 0)
      pkt.RescaleDuration(
         mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());

   if (
      mFFmpeg->av_interleaved_write_frame(
         mEncFormatCtx->GetWrappedValue(), pkt.GetWrappedValue()) != 0)
   {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - Couldn't write audio frame to output file."),
         XO("FFmpeg Error"), wxOK | wxCENTER | wxICON_EXCLAMATION);
      return false;
   }

   return true;
}

// Returns 0 if no more output, 1 if more output, negative if error
int ExportFFmpeg::EncodeAudio(AVPacketWrapper& pkt, int16_t* audio_samples, int nb_samples)
{
   // Assume *pkt is already initialized.

   int i, ch, buffer_size, ret, got_output = 0;
   AVDataBuffer<uint8_t> samples;

   std::unique_ptr<AVFrameWrapper> frame;

   if (audio_samples) {
      frame = mFFmpeg->CreateAVFrameWrapper();

      if (!frame)
         return AUDACITY_AVERROR(ENOMEM);

      frame->SetSamplesCount(nb_samples);
      frame->SetFormat(mEncAudioCodecCtx->GetSampleFmt());
      frame->SetChannelLayout(mEncAudioCodecCtx->GetChannelLayout());

      buffer_size = mFFmpeg->av_samples_get_buffer_size(
         NULL, mEncAudioCodecCtx->GetChannels(), nb_samples,
         mEncAudioCodecCtx->GetSampleFmt(), 0);

      if (buffer_size < 0) {
         AudacityMessageBox(
            XO("FFmpeg : ERROR - Could not get sample buffer size"),
            XO("FFmpeg Error"),
            wxOK|wxCENTER|wxICON_EXCLAMATION
         );
         return buffer_size;
      }

      samples = mFFmpeg->CreateMemoryBuffer<uint8_t>(buffer_size);

      if (samples.empty()) {
         AudacityMessageBox(
            XO("FFmpeg : ERROR - Could not allocate bytes for samples buffer"),
            XO("FFmpeg Error"),
            wxOK|wxCENTER|wxICON_EXCLAMATION
         );

         return AUDACITY_AVERROR(ENOMEM);
      }
      /* setup the data pointers in the AVFrame */
      ret = mFFmpeg->avcodec_fill_audio_frame(
         frame->GetWrappedValue(), mEncAudioCodecCtx->GetChannels(),
         mEncAudioCodecCtx->GetSampleFmt(), samples.data(), buffer_size, 0);

      if (ret < 0) {
         AudacityMessageBox(
            XO("FFmpeg : ERROR - Could not setup audio frame"),
            XO("FFmpeg Error"),
            wxOK|wxCENTER|wxICON_EXCLAMATION
         );
         return ret;
      }

      const int channelsCount = mEncAudioCodecCtx->GetChannels();

      for (ch = 0; ch < mEncAudioCodecCtx->GetChannels(); ch++)
      {
         for (i = 0; i < nb_samples; i++) {
            switch (static_cast<AudacityAVSampleFormat>(
               mEncAudioCodecCtx->GetSampleFmt()))
            {
            case AUDACITY_AV_SAMPLE_FMT_U8:
               ((uint8_t*)(frame->GetData(0)))[ch + i*channelsCount] = audio_samples[ch + i*channelsCount]/258 + 128;
               break;
            case AUDACITY_AV_SAMPLE_FMT_U8P:
               ((uint8_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i*channelsCount]/258 + 128;
               break;
            case AUDACITY_AV_SAMPLE_FMT_S16:
               ((int16_t*)(frame->GetData(0)))[ch + i*channelsCount] = audio_samples[ch + i*channelsCount];
               break;
            case AUDACITY_AV_SAMPLE_FMT_S16P:
               ((int16_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i*channelsCount];
               break;
            case AUDACITY_AV_SAMPLE_FMT_S32:
               ((int32_t*)(frame->GetData(0)))[ch + i*channelsCount] = audio_samples[ch + i*channelsCount]<<16;
               break;
            case AUDACITY_AV_SAMPLE_FMT_S32P:
               ((int32_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i*channelsCount]<<16;
               break;
            case AUDACITY_AV_SAMPLE_FMT_FLT:
               ((float*)(frame->GetData(0)))[ch + i*channelsCount] = audio_samples[ch + i*channelsCount] / 32767.0;
               break;
            case AUDACITY_AV_SAMPLE_FMT_FLTP:
               ((float*)(frame->GetData(ch)))[i] = audio_samples[ch + i*channelsCount] / 32767.;
               break;
            default:
               wxASSERT(false);
               break;
            }
         }
      }
   }

   pkt.ResetData();

   pkt.SetStreamIndex(mEncAudioStream->GetIndex());

   if (mFFmpeg->avcodec_send_frame != nullptr)
   {
      ret = mFFmpeg->avcodec_send_frame(
         mEncAudioCodecCtx->GetWrappedValue(),
         frame ? frame->GetWrappedValue() : nullptr);

      while (ret >= 0)
      {
         ret = mFFmpeg->avcodec_receive_packet(
            mEncAudioCodecCtx->GetWrappedValue(), pkt.GetWrappedValue());

         if (ret == AUDACITY_AVERROR(EAGAIN) || ret == AUDACITY_AVERROR_EOF)
         {
            ret = 0;
            break;
         }
         else if (ret < 0)
            break;

         if (!WritePacket(pkt))
            return -1;

         got_output = true;
      }
   }
   else
   {
      ret = mFFmpeg->avcodec_encode_audio2(
         mEncAudioCodecCtx->GetWrappedValue(), pkt.GetWrappedValue(),
         frame ? frame->GetWrappedValue() : nullptr, &got_output);

      if (ret == 0)
      {
         if (!WritePacket(pkt))
            return -1;
      }
   }

   if (ret < 0 && ret != AUDACITY_AVERROR_EOF) {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - encoding frame failed"),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION
      );

      char buf[64];
      mFFmpeg->av_strerror(ret, buf, sizeof(buf));
      wxLogDebug(buf);

      return ret;
   }

   pkt.ResetTimestamps(); // We don't set frame timestamps thus don't trust the AVPacket timestamps

   return got_output;
}


bool ExportFFmpeg::Finalize()
{
   // Flush the audio FIFO and encoder.
   for (;;)
   {
      std::unique_ptr<AVPacketWrapper> pkt = mFFmpeg->CreateAVPacketWrapper();

      const int nFifoBytes = mFFmpeg->av_fifo_size(
         mEncAudioFifo->GetWrappedValue()); // any bytes left in audio FIFO?

      int encodeResult = 0;

      // Flush the audio FIFO first if necessary. It won't contain a _full_ audio frame because
      // if it did we'd have pulled it from the FIFO during the last encodeAudioFrame() call
      if (nFifoBytes > 0)
      {
         const int nAudioFrameSizeOut = mDefaultFrameSize * mEncAudioCodecCtx->GetChannels() * sizeof(int16_t);

         if (nAudioFrameSizeOut > mEncAudioFifoOutBufSize || nFifoBytes > mEncAudioFifoOutBufSize) {
            AudacityMessageBox(
               XO("FFmpeg : ERROR - Too much remaining data."),
               XO("FFmpeg Error"),
               wxOK | wxCENTER | wxICON_EXCLAMATION
            );
            return false;
         }

         // We have an incomplete buffer of samples left, encode it.
         // If codec supports CODEC_CAP_SMALL_LAST_FRAME, we can feed it with smaller frame
         // Or if frame_size is 1, then it's some kind of PCM codec, they don't have frames and will be fine with the samples
         // Otherwise we'll send a full frame of audio + silence padding to ensure all audio is encoded
         int frame_size = mDefaultFrameSize;
         if (
            mEncAudioCodecCtx->GetCodec()->GetCapabilities() &
               AUDACITY_AV_CODEC_CAP_SMALL_LAST_FRAME ||
            frame_size == 1)
         {
            frame_size = nFifoBytes /
                         (mEncAudioCodecCtx->GetChannels() * sizeof(int16_t));
         }

         wxLogDebug(wxT("FFmpeg : Audio FIFO still contains %d bytes, writing %d sample frame ..."),
            nFifoBytes, frame_size);

         // Fill audio buffer with zeroes. If codec tries to read the whole buffer,
         // it will just read silence. If not - who cares?
         memset(mEncAudioFifoOutBuf.data(), 0, mEncAudioFifoOutBufSize);
         //const AVCodec *codec = mEncAudioCodecCtx->codec;

         // Pull the bytes out from the FIFO and feed them to the encoder.
         if (mFFmpeg->av_fifo_generic_read(mEncAudioFifo->GetWrappedValue(), mEncAudioFifoOutBuf.data(), nFifoBytes, nullptr) == 0)
         {
            encodeResult = EncodeAudio(*pkt, mEncAudioFifoOutBuf.data(), frame_size);
         }
         else
         {
            wxLogDebug(wxT("FFmpeg : Reading from Audio FIFO failed, aborting"));
            // TODO: more precise message
            ShowExportErrorDialog("FFmpeg:825");
            return false;
         }
      }
      else
      {
         // Fifo is empty, flush encoder. May be called multiple times.
         encodeResult =
            EncodeAudio(*pkt.get(), nullptr, 0);
      }

      if (encodeResult < 0) {
         // TODO: more precise message
            ShowExportErrorDialog("FFmpeg:837");
         return false;
      }
      else if (encodeResult == 0)
         break;      
   }

   // Write any file trailers.
   if (mFFmpeg->av_write_trailer(mEncFormatCtx->GetWrappedValue()) != 0)
   {
      // TODO: more precise message
      ShowExportErrorDialog("FFmpeg:868");
      return false;
   }

   return true;
}

void ExportFFmpeg::FreeResources()
{

}

// All paths in this that fail must report their error to the user.
bool ExportFFmpeg::EncodeAudioFrame(int16_t *pFrame, size_t frameSize)
{
   int nBytesToWrite = 0;
   uint8_t *pRawSamples = nullptr;
   int nAudioFrameSizeOut = mDefaultFrameSize * mEncAudioCodecCtx->GetChannels() * sizeof(int16_t);
   int ret;

   nBytesToWrite = frameSize;
   pRawSamples  = (uint8_t*)pFrame;
   if (mFFmpeg->av_fifo_realloc2(mEncAudioFifo->GetWrappedValue(), mFFmpeg->av_fifo_size(mEncAudioFifo->GetWrappedValue()) + frameSize) < 0) {
      ShowExportErrorDialog("FFmpeg:905");
      return false;
   }

   // Put the raw audio samples into the FIFO.
   ret = mFFmpeg->av_fifo_generic_write(
      mEncAudioFifo->GetWrappedValue(), pRawSamples, nBytesToWrite, nullptr);

   if (ret != nBytesToWrite) {
      ShowExportErrorDialog("FFmpeg:913");
      return false;
   }

   if (nAudioFrameSizeOut > mEncAudioFifoOutBufSize) {
      AudacityMessageBox(
         XO("FFmpeg : ERROR - nAudioFrameSizeOut too large."),
         XO("FFmpeg Error"),
         wxOK|wxCENTER|wxICON_EXCLAMATION
      );
      return false;
   }

   // Read raw audio samples out of the FIFO in nAudioFrameSizeOut byte-sized groups to encode.
   while (mFFmpeg->av_fifo_size(mEncAudioFifo->GetWrappedValue()) >= nAudioFrameSizeOut)
   {
      ret = mFFmpeg->av_fifo_generic_read(
         mEncAudioFifo->GetWrappedValue(), mEncAudioFifoOutBuf.data(),
         nAudioFrameSizeOut, nullptr);

      std::unique_ptr<AVPacketWrapper> pkt = mFFmpeg->CreateAVPacketWrapper();

      ret = EncodeAudio(
         *pkt,                       // out
         mEncAudioFifoOutBuf.data(), // in
         mDefaultFrameSize);

      if (ret < 0)
      {
         AudacityMessageBox(
            XO("FFmpeg : ERROR - Can't encode audio frame."),
            XO("FFmpeg Error"),
            wxOK|wxCENTER|wxICON_EXCLAMATION
         );
         return false;
      }
   }
   return true;
}


ProgressResult ExportFFmpeg::Export(
   AudacityProject* project, std::unique_ptr<BasicUI::ProgressDialog>& pDialog,
   unsigned channels, const wxFileNameWrapper& fName,
   bool selectionOnly, double t0, double t1,
   MixerSpec *mixerSpec, const Tags *metadata, int subformat)
{
   if (!CheckFFmpegPresence())
      return ProgressResult::Cancelled;
   mChannels = channels;
   // subformat index may not correspond directly to fmts[] index, convert it
   mSubFormat = AdjustFormatIndex(subformat);
   if (channels > ExportFFmpegOptions::fmts[mSubFormat].maxchannels)
   {
      AudacityMessageBox(
         XO(
"Attempted to export %d channels, but maximum number of channels for selected output format is %d")
            .Format(
               channels,
               ExportFFmpegOptions::fmts[mSubFormat].maxchannels ),
         XO("Error"));
      return ProgressResult::Cancelled;
   }
   mName = fName;
   const auto &tracks = TrackList::Get( *project );
   bool ret = true;

   if (mSubFormat >= FMT_LAST) {
      // TODO: more precise message
      ShowExportErrorDialog("FFmpeg:996");
      return ProgressResult::Cancelled;
   }

   wxString shortname(ExportFFmpegOptions::fmts[mSubFormat].shortname);
   if (mSubFormat == FMT_OTHER)
      shortname = gPrefs->Read(wxT("/FileFormats/FFmpegFormat"),wxT("matroska"));
   ret = Init(shortname.mb_str(),project, metadata, subformat);
   auto cleanup = finally ( [&] { FreeResources(); } );

   if (!ret) {
      // TODO: more precise message
      ShowExportErrorDialog("FFmpeg:1008");
      return ProgressResult::Cancelled;
   }

   size_t pcmBufferSize = mDefaultFrameSize;

   auto mixer = CreateMixer(tracks, selectionOnly,
      t0, t1,
      channels, pcmBufferSize, true,
      mSampleRate, int16Sample, mixerSpec);

   auto updateResult = ProgressResult::Success;
   {
      InitProgress( pDialog, fName,
         selectionOnly
            ? XO("Exporting selected audio as %s")
                 .Format( ExportFFmpegOptions::fmts[mSubFormat].description )
            : XO("Exporting the audio as %s")
                 .Format( ExportFFmpegOptions::fmts[mSubFormat].description ) );
      auto &progress = *pDialog;

      while (updateResult == ProgressResult::Success) {
         auto pcmNumSamples = mixer->Process();
         if (pcmNumSamples == 0)
            break;

         short *pcmBuffer = (short *)mixer->GetBuffer();

         if (!EncodeAudioFrame(
            pcmBuffer, (pcmNumSamples)*sizeof(int16_t)*mChannels)) {
            // All errors should already have been reported.
            //ShowDiskFullExportErrorDialog(mName);
            updateResult = ProgressResult::Cancelled;
            break;
         }

         updateResult = progress.Poll(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   if ( updateResult != ProgressResult::Cancelled )
      if ( !Finalize() ) // Finalize makes its own messages
         return ProgressResult::Cancelled;

   // Flush the file
   mEncFormatCtx.reset();

   return updateResult;
}

void AddStringTagUTF8(char field[], int size, wxString value)
{
      memset(field,0,size);
      memcpy(field,value.ToUTF8(),(int)strlen(value.ToUTF8()) > size -1 ? size -1 : strlen(value.ToUTF8()));
}

void AddStringTagANSI(char field[], int size, wxString value)
{
      memset(field,0,size);
      memcpy(field,value.mb_str(),(int)strlen(value.mb_str()) > size -1 ? size -1 : strlen(value.mb_str()));
}

bool ExportFFmpeg::AddTags(const Tags *tags)
{
   if (tags == NULL)
   {
      return false;
   }

   SetMetadata(tags, "album", TAG_ALBUM);
   SetMetadata(tags, "comment", TAG_COMMENTS);
   SetMetadata(tags, "genre", TAG_GENRE);
   SetMetadata(tags, "title", TAG_TITLE);
   SetMetadata(tags, "track", TAG_TRACK);

   // Bug 2564: Add m4a tags
   if (mEncFormatDesc->GetAudioCodec() == mFFmpeg->GetAVCodecID(AUDACITY_AV_CODEC_ID_AAC))
   {
      SetMetadata(tags, "artist", TAG_ARTIST);
      SetMetadata(tags, "date", TAG_YEAR);
   }
   else
   {
      SetMetadata(tags, "author", TAG_ARTIST);
      SetMetadata(tags, "year", TAG_YEAR);
   }

   return true;
}

void ExportFFmpeg::SetMetadata(const Tags *tags, const char *name, const wxChar *tag)
{
   if (tags->HasTag(tag))
   {
      wxString value = tags->GetTag(tag);

      AVDictionaryWrapper metadata = mEncFormatCtx->GetMetadata();

      metadata.Set(name, mSupportsUTF8 ? value : value.mb_str(), 0);
      mEncFormatCtx->SetMetadata(metadata);
   }
}


//----------------------------------------------------------------------------
// AskResample dialog
//----------------------------------------------------------------------------

int ExportFFmpeg::AskResample(int bitrate, int rate, int lowrate, int highrate, const int *sampRates)
{
#if defined(FFMPEG_AUTO_RESAMPLE)
   std::vector<int> rates;

   for (int i = 0; sampRates[i]; ++i)
   {
      rates.push_back(sampRates[i]);
   }

   std::sort(rates.begin(), rates.end());

   int bestRate = 0;
   for (auto i : rates)
   {
      bestRate = i;
      if (i > rate)
      {
         break;
      }
   }

   return bestRate;
#else
   wxDialogWrapper d(nullptr, wxID_ANY, XO("Invalid sample rate"));
   d.SetName();
   wxChoice *choice;
   ShuttleGui S(&d, eIsCreating);

   int selected = -1;

   S.StartVerticalLay();
   {
      S.SetBorder(10);
      S.StartStatic(XO("Resample"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            S.AddTitle(
               (bitrate == 0
                  ? XO(
"The project sample rate (%d) is not supported by the current output\nfile format. ")
                       .Format( rate )
                  : XO(
"The project sample rate (%d) and bit rate (%d kbps) combination is not\nsupported by the current output file format. ")
                       .Format( rate, bitrate/1000))
               + XO("You may resample to one of the rates below.")
            );
         }
         S.EndHorizontalLay();

         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            choice = S.AddChoice(XO("Sample Rates"),
               [&]{
                  TranslatableStrings choices;
                  for (int i = 0; sampRates[i] > 0; i++)
                  {
                     int label = sampRates[i];
                     if ((!lowrate || label >= lowrate) && (!highrate || label <= highrate))
                     {
                        wxString name = wxString::Format(wxT("%d"),label);
                        choices.push_back( Verbatim( name ) );
                        if (label <= rate)
                           selected = i;
                     }
                  }
                  return choices;
               }(),
               std::max( 0, selected )
            );
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   d.Layout();
   d.Fit();
   d.SetMinSize(d.GetSize());
   d.Center();

   if (d.ShowModal() == wxID_CANCEL) {
      return 0;
   }

   return wxAtoi(choice->GetStringSelection());
#endif
}

void ExportFFmpeg::OptionsCreate(ShuttleGui &S, int format)
{
   // subformat index may not correspond directly to fmts[] index, convert it
   mSubFormat = AdjustFormatIndex(format);
   if (mSubFormat == FMT_M4A)
   {
      S.AddWindow(
         safenew ExportFFmpegAACOptions{ S.GetParent(), format } );
      return;
   }
   else if (mSubFormat == FMT_AC3)
   {
      S.AddWindow(
         safenew ExportFFmpegAC3Options{ S.GetParent(), format } );
      return;
   }
   else if (mSubFormat == FMT_AMRNB)
   {
      S.AddWindow(
         safenew ExportFFmpegAMRNBOptions{ S.GetParent(), format } );
      return;
   }
   else if (mSubFormat == FMT_OPUS)
   {
      S.AddWindow(
         safenew ExportFFmpegOPUSOptions{ S.GetParent(), format });
      return;
   }
   else if (mSubFormat == FMT_WMA2)
   {
      S.AddWindow(
         safenew ExportFFmpegWMAOptions{ S.GetParent(), format } );
      return;
   }
   else if (mSubFormat == FMT_OTHER)
   {
      S.AddWindow(
         safenew ExportFFmpegCustomOptions{ S.GetParent(), format } );
      return;
   }

   ExportPlugin::OptionsCreate(S, format);
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "FFmpeg",
   []{ return std::make_unique< ExportFFmpeg >(); }
};

#endif

