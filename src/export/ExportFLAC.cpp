/**********************************************************************

Audacity: A Digital Audio Editor

ExportFLAC.cpp

Frederik M.J.V

This program is distributed under the GNU General Public License, version 2.
A copy of this license is included with this source.

Based on ExportOGG.cpp by:
Joshua Haberman

**********************************************************************/



#ifdef USE_LIBFLAC

#include "Export.h"

#include <wx/ffile.h>
#include <wx/log.h>

#include "FLAC++/encoder.h"

#include "float_cast.h"
#include "ProjectRate.h"
#include "Mix.h"
#include "Prefs.h"
#include "ShuttleGui.h"

#include "Tags.h"
#include "Track.h"

#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "wxFileNameWrapper.h"

//----------------------------------------------------------------------------
// ExportFLACOptions Class
//----------------------------------------------------------------------------

class ExportFLACOptions final : public wxPanelWrapper
{
public:

   ExportFLACOptions(wxWindow *parent, int format);
   virtual ~ExportFLACOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;
};

///
///
ExportFLACOptions::ExportFLACOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

///
///
ExportFLACOptions::~ExportFLACOptions()
{
   TransferDataFromWindow();
}

ChoiceSetting FLACBitDepth{
   wxT("/FileFormats/FLACBitDepth"),
   {
      ByColumns,
      { XO("16 bit") , XO("24 bit") , },
      { wxT("16") ,    wxT("24") , }
   },
   0 // "16",
};

ChoiceSetting FLACLevel{
   wxT("/FileFormats/FLACLevel"),
   {
      ByColumns,
      {
         XO("0 (fastest)") ,
         XO("1") ,
         XO("2") ,
         XO("3") ,
         XO("4") ,
         XO("5") ,
         XO("6") ,
         XO("7") ,
         XO("8 (best)") ,
      },
      {
         wxT("0") ,
         wxT("1") ,
         wxT("2") ,
         wxT("3") ,
         wxT("4") ,
         wxT("5") ,
         wxT("6") ,
         wxT("7") ,
         wxT("8") ,
      }
   },
   5 //"5"
};

///
///
void ExportFLACOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieChoice( XXO("Level:"), FLACLevel);
            S.TieChoice( XXO("Bit depth:"), FLACBitDepth);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   return;
}

///
///
bool ExportFLACOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportFLACOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportFLAC Class
//----------------------------------------------------------------------------

#define SAMPLES_PER_RUN 8192u

/* FLACPP_API_VERSION_CURRENT is 6 for libFLAC++ from flac-1.1.3 (see <FLAC++/export.h>) */
#if !defined FLACPP_API_VERSION_CURRENT || FLACPP_API_VERSION_CURRENT < 6
#define LEGACY_FLAC
#else
#undef LEGACY_FLAC
#endif

static struct
{
   bool        do_exhaustive_model_search;
   bool        do_escape_coding;
   bool        do_mid_side_stereo;
   bool        loose_mid_side_stereo;
   unsigned    qlp_coeff_precision;
   unsigned    min_residual_partition_order;
   unsigned    max_residual_partition_order;
   unsigned    rice_parameter_search_dist;
   unsigned    max_lpc_order;
} flacLevels[] = {
   {  false,   false,   false,   false,   0, 2, 2, 0, 0  },
   {  false,   false,   true,    true,    0, 2, 2, 0, 0  },
   {  false,   false,   true,    false,   0, 0, 3, 0, 0  },
   {  false,   false,   false,   false,   0, 3, 3, 0, 6  },
   {  false,   false,   true,    true,    0, 3, 3, 0, 8  },
   {  false,   false,   true,    false,   0, 3, 3, 0, 8  },
   {  false,   false,   true,    false,   0, 0, 4, 0, 8  },
   {  true,    false,   true,    false,   0, 0, 6, 0, 8  },
   {  true,    false,   true,    false,   0, 0, 6, 0, 12 },
};

//----------------------------------------------------------------------------

struct FLAC__StreamMetadataDeleter {
   void operator () (FLAC__StreamMetadata *p) const
   { if (p) ::FLAC__metadata_object_delete(p); }
};
using FLAC__StreamMetadataHandle = std::unique_ptr<
   FLAC__StreamMetadata, FLAC__StreamMetadataDeleter
>;

class ExportFLAC final : public ExportPlugin
{
public:

   ExportFLAC();

   // Required

   void OptionsCreate(ShuttleGui &S, int format) override;
   ProgressResult Export(AudacityProject *project,
               std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
               unsigned channels,
               const wxFileNameWrapper &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;

private:

   bool GetMetadata(AudacityProject *project, const Tags *tags);

   // Should this be a stack variable instead in Export?
   FLAC__StreamMetadataHandle mMetadata;
};

//----------------------------------------------------------------------------

ExportFLAC::ExportFLAC()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("FLAC"),0);
   AddExtension(wxT("flac"),0);
   SetMaxChannels(FLAC__MAX_CHANNELS,0);
   SetCanMetaData(true,0);
   SetDescription(XO("FLAC Files"),0);
}

ProgressResult ExportFLAC::Export(AudacityProject *project,
                        std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
                        unsigned numChannels,
                        const wxFileNameWrapper &fName,
                        bool selectionOnly,
                        double t0,
                        double t1,
                        MixerSpec *mixerSpec,
                        const Tags *metadata,
                        int WXUNUSED(subformat))
{
   double    rate    = ProjectRate::Get(*project).GetRate();
   const auto &tracks = TrackList::Get( *project );

   wxLogNull logNo;            // temporarily disable wxWidgets error messages
   auto updateResult = ProgressResult::Success;

   long levelPref;
   FLACLevel.Read().ToLong( &levelPref );

   auto bitDepthPref = FLACBitDepth.Read();

   FLAC::Encoder::File encoder;

   bool success = true;
   success = success &&
#ifdef LEGACY_FLAC
   encoder.set_filename(OSOUTPUT(fName)) &&
#endif
   encoder.set_channels(numChannels) &&
   encoder.set_sample_rate(lrint(rate));

   // See note in GetMetadata() about a bug in libflac++ 1.1.2
   if (success && !GetMetadata(project, metadata)) {
      // TODO: more precise message
      ShowExportErrorDialog("FLAC:283");
      return ProgressResult::Cancelled;
   }

   if (success && mMetadata) {
      // set_metadata expects an array of pointers to metadata and a size.
      // The size is 1.
      FLAC__StreamMetadata *p = mMetadata.get();
      success = encoder.set_metadata(&p, 1);
   }

   auto cleanup1 = finally( [&] {
      mMetadata.reset(); // need this?
   } );

   sampleFormat format;
   if (bitDepthPref == wxT("24")) {
      format = int24Sample;
      success = success && encoder.set_bits_per_sample(24);
   } else { //convert float to 16 bits
      format = int16Sample;
      success = success && encoder.set_bits_per_sample(16);
   }


   // Duplicate the flac command line compression levels
   if (levelPref < 0 || levelPref > 8) {
      levelPref = 5;
   }
   success = success &&
   encoder.set_do_exhaustive_model_search(flacLevels[levelPref].do_exhaustive_model_search) &&
   encoder.set_do_escape_coding(flacLevels[levelPref].do_escape_coding);

   if (numChannels != 2) {
      success = success &&
      encoder.set_do_mid_side_stereo(false) &&
      encoder.set_loose_mid_side_stereo(false);
   }
   else {
      success = success &&
      encoder.set_do_mid_side_stereo(flacLevels[levelPref].do_mid_side_stereo) &&
      encoder.set_loose_mid_side_stereo(flacLevels[levelPref].loose_mid_side_stereo);
   }

   success = success &&
   encoder.set_qlp_coeff_precision(flacLevels[levelPref].qlp_coeff_precision) &&
   encoder.set_min_residual_partition_order(flacLevels[levelPref].min_residual_partition_order) &&
   encoder.set_max_residual_partition_order(flacLevels[levelPref].max_residual_partition_order) &&
   encoder.set_rice_parameter_search_dist(flacLevels[levelPref].rice_parameter_search_dist) &&
   encoder.set_max_lpc_order(flacLevels[levelPref].max_lpc_order);

   if (!success) {
      // TODO: more precise message
      ShowExportErrorDialog("FLAC:336");
      return ProgressResult::Cancelled;
   }

#ifdef LEGACY_FLAC
   encoder.init();
#else
   wxFFile f;     // will be closed when it goes out of scope
   const auto path = fName.GetFullPath();
   if (!f.Open(path, wxT("w+b"))) {
      AudacityMessageBox( XO("FLAC export couldn't open %s").Format( path ) );
      return ProgressResult::Cancelled;
   }

   // Even though there is an init() method that takes a filename, use the one that
   // takes a file handle because wxWidgets can open a file with a Unicode name and
   // libflac can't (under Windows).
   int status = encoder.init(f.fp());
   if (status != FLAC__STREAM_ENCODER_INIT_STATUS_OK) {
      AudacityMessageBox(
         XO("FLAC encoder failed to initialize\nStatus: %d")
            .Format( status ) );
      return ProgressResult::Cancelled;
   }
#endif

   mMetadata.reset();

   auto cleanup2 = finally( [&] {
      if (!(updateResult == ProgressResult::Success ||
            updateResult == ProgressResult::Stopped)) {
#ifndef LEGACY_FLAC
         f.Detach(); // libflac closes the file
#endif
         encoder.finish();
      }
   } );

   auto mixer = CreateMixer(tracks, selectionOnly,
                            t0, t1,
                            numChannels, SAMPLES_PER_RUN, false,
                            rate, format, mixerSpec);

   ArraysOf<FLAC__int32> tmpsmplbuf{ numChannels, SAMPLES_PER_RUN, true };

   InitProgress( pDialog, fName,
      selectionOnly
         ? XO("Exporting the selected audio as FLAC")
         : XO("Exporting the audio as FLAC") );
   auto &progress = *pDialog;

   while (updateResult == ProgressResult::Success) {
      auto samplesThisRun = mixer->Process();
      if (samplesThisRun == 0) //stop encoding
         break;
      else {
         for (size_t i = 0; i < numChannels; i++) {
            auto mixed = mixer->GetBuffer(i);
            if (format == int24Sample) {
               for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                  tmpsmplbuf[i][j] = ((const int *)mixed)[j];
               }
            }
            else {
               for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                  tmpsmplbuf[i][j] = ((const short *)mixed)[j];
               }
            }
         }
         if (! encoder.process(
               reinterpret_cast<FLAC__int32**>( tmpsmplbuf.get() ),
               samplesThisRun) ) {
            // TODO: more precise message
            ShowDiskFullExportErrorDialog(fName);
            updateResult = ProgressResult::Cancelled;
            break;
         }
         if (updateResult == ProgressResult::Success)
            updateResult =
               progress.Poll(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   if (updateResult == ProgressResult::Success ||
       updateResult == ProgressResult::Stopped) {
#ifndef LEGACY_FLAC
      f.Detach(); // libflac closes the file
#endif
      if (!encoder.finish())
         // Do not reassign updateResult, see cleanup2
         return ProgressResult::Failed;
#ifdef LEGACY_FLAC
      if (!f.Flush() || !f.Close())
         return ProgressResult::Failed;
#endif
   }

   return updateResult;
}

void ExportFLAC::OptionsCreate(ShuttleGui &S, int format)
{
   S.AddWindow( safenew ExportFLACOptions{ S.GetParent(), format } );
}

// LL:  There's a bug in libflac++ 1.1.2 that prevents us from using
//      FLAC::Metadata::VorbisComment directly.  The set_metadata()
//      function allocates an array on the stack, but the base library
//      expects that array to be valid until the stream is initialized.
//
//      This has been fixed in 1.1.4.
bool ExportFLAC::GetMetadata(AudacityProject *project, const Tags *tags)
{
   // Retrieve tags if needed
   if (tags == NULL)
      tags = &Tags::Get( *project );

   mMetadata.reset(::FLAC__metadata_object_new(FLAC__METADATA_TYPE_VORBIS_COMMENT));

   wxString n;
   for (const auto &pair : tags->GetRange()) {
      n = pair.first;
      const auto &v = pair.second;
      if (n == TAG_YEAR) {
         n = wxT("DATE");
      }
      else if (n == TAG_COMMENTS) {
         // Some apps like Foobar use COMMENT and some like Windows use DESCRIPTION,
         // so add both to try and make everyone happy.
         n = wxT("COMMENT");
         FLAC::Metadata::VorbisComment::Entry entry(n.mb_str(wxConvUTF8),
                                                    v.mb_str(wxConvUTF8));
         if (! ::FLAC__metadata_object_vorbiscomment_append_comment(mMetadata.get(),
                                                              entry.get_entry(),
                                                              true) ) {
            return false;
         }
         n = wxT("DESCRIPTION");
      }
      FLAC::Metadata::VorbisComment::Entry entry(n.mb_str(wxConvUTF8),
                                                 v.mb_str(wxConvUTF8));
      if (! ::FLAC__metadata_object_vorbiscomment_append_comment(mMetadata.get(),
                                                           entry.get_entry(),
                                                           true) ) {
         return false;
      }
   }

   return true;
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "FLAC",
   []{ return std::make_unique< ExportFLAC >(); }
};

#ifdef HAS_CLOUD_UPLOAD
#include "CloudExporterPlugin.h"
#include "CloudExportersRegistry.h"

class FlacCloudHelper : public cloud::CloudExporterPlugin
{
public:
   wxString GetExporterID() const override
   {
      return "FLAC";
   }

   FileExtension GetFileExtension() const override
   {
      return "flac";
   }

   void OnBeforeExport() override
   {
      FLACBitDepth.Write("24");
      FLACLevel.Write("5");
   }

}; // WavPackCloudHelper

static bool cloudExporterRegisterd = cloud::RegisterCloudExporter(
   "audio/x-flac",
   [](const AudacityProject&) { return std::make_unique<FlacCloudHelper>(); });
#endif

#endif // USE_LIBFLAC

