/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.cpp

  Joshua Haberman

  This program is distributed under the GNU General Public License, version 2.
  A copy of this license is included with this source.

  Portions from vorbis-tools, copyright 2000-2002 Michael Smith
  <msmith@labyrinth.net.au>; Vorbize, Kenneth Arnold <kcarnold@yahoo.com>;
  and libvorbis examples, Monty <monty@xiph.org>

**********************************************************************/

#include "../Audacity.h"

#ifdef USE_LIBVORBIS

#include "ExportOGG.h"
#include "Export.h"

#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/slider.h>
 
#include <vorbis/vorbisenc.h>

#include "../FileIO.h"
#include "../Project.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "../Internat.h"
#include "../Tags.h"
#include "../Track.h"

//----------------------------------------------------------------------------
// ExportOGGOptions
//----------------------------------------------------------------------------

class ExportOGGOptions final : public wxPanelWrapper
{
public:

   ExportOGGOptions(wxWindow *parent, int format);
   virtual ~ExportOGGOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:

   int mOggQualityUnscaled;
};

///
///
ExportOGGOptions::ExportOGGOptions(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   mOggQualityUnscaled = gPrefs->Read(wxT("/FileFormats/OggExportQuality"),50)/10;

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportOGGOptions::~ExportOGGOptions()
{
   TransferDataFromWindow();
}

///
///
void ExportOGGOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(2, wxCENTER);
         {
            S.SetStretchyCol(1);
            S.Prop(1).TieSlider(_("Quality:"), mOggQualityUnscaled, 10);
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportOGGOptions::TransferDataToWindow()
{
   return true;
}

///
///
bool ExportOGGOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Write(wxT("/FileFormats/OggExportQuality"),mOggQualityUnscaled * 10);
   gPrefs->Flush();

   return true;
}

//----------------------------------------------------------------------------
// ExportOGG
//----------------------------------------------------------------------------

#define SAMPLES_PER_RUN 8192

class ExportOGG final : public ExportPlugin
{
public:

   ExportOGG();

   // Required
   wxWindow *OptionsCreate(wxWindow *parent, int format) override;

   int Export(AudacityProject *project,
               unsigned channels,
               const wxString &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;

private:

   bool FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata);
};

ExportOGG::ExportOGG()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("OGG"),0);
   AddExtension(wxT("ogg"),0);
   SetMaxChannels(255,0);
   SetCanMetaData(true,0);
   SetDescription(_("Ogg Vorbis Files"),0);
}

int ExportOGG::Export(AudacityProject *project,
                       unsigned numChannels,
                       const wxString &fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       const Tags *metadata,
                       int WXUNUSED(subformat))
{
   double    rate    = project->GetRate();
   const TrackList *tracks = project->GetTracks();
   double    quality = (gPrefs->Read(wxT("/FileFormats/OggExportQuality"), 50)/(float)100.0);

   wxLogNull logNo;            // temporarily disable wxWidgets error messages
   int updateResult = eProgressSuccess;
   int       eos = 0;

   FileIO outFile(fName, FileIO::Output);

   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }

   // All the Ogg and Vorbis encoding data
   ogg_stream_state stream;
   ogg_page         page;
   ogg_packet       packet;

   vorbis_info      info;
   vorbis_comment   comment;
   vorbis_dsp_state dsp;
   vorbis_block     block;

   // Encoding setup
   vorbis_info_init(&info);
   vorbis_encode_init_vbr(&info, numChannels, (int)(rate + 0.5), quality);

   // Retrieve tags
   if (!FillComment(project, &comment, metadata)) {
      return false;
   }

   // Set up analysis state and auxiliary encoding storage
   vorbis_analysis_init(&dsp, &info);
   vorbis_block_init(&dsp, &block);

   // Set up packet->stream encoder.  According to encoder example,
   // a random serial number makes it more likely that you can make
   // chained streams with concatenation.
   srand(time(NULL));
   ogg_stream_init(&stream, rand());

   // First we need to write the required headers:
   //    1. The Ogg bitstream header, which contains codec setup params
   //    2. The Vorbis comment header
   //    3. The bitstream codebook.
   //
   // After we create those our responsibility is complete, libvorbis will
   // take care of any other ogg bistream constraints (again, according
   // to the example encoder source)
   ogg_packet bitstream_header;
   ogg_packet comment_header;
   ogg_packet codebook_header;

   vorbis_analysis_headerout(&dsp, &comment, &bitstream_header, &comment_header,
         &codebook_header);

   // Place these headers into the stream
   ogg_stream_packetin(&stream, &bitstream_header);
   ogg_stream_packetin(&stream, &comment_header);
   ogg_stream_packetin(&stream, &codebook_header);

   // Flushing these headers now guarentees that audio data will
   // start on a NEW page, which apparently makes streaming easier
   while (ogg_stream_flush(&stream, &page)) {
      outFile.Write(page.header, page.header_len);
      outFile.Write(page.body, page.body_len);
   }

   const WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(selectionOnly, false);
   {
      auto mixer = CreateMixer(waveTracks,
         tracks->GetTimeTrack(),
         t0, t1,
         numChannels, SAMPLES_PER_RUN, false,
         rate, floatSample, true, mixerSpec);

      ProgressDialog progress(wxFileName(fName).GetName(),
         selectionOnly ?
         _("Exporting the selected audio as Ogg Vorbis") :
         _("Exporting the entire project as Ogg Vorbis"));

      while (updateResult == eProgressSuccess && !eos) {
         float **vorbis_buffer = vorbis_analysis_buffer(&dsp, SAMPLES_PER_RUN);
         auto samplesThisRun = mixer->Process(SAMPLES_PER_RUN);

         if (samplesThisRun == 0) {
            // Tell the library that we wrote 0 bytes - signalling the end.
            vorbis_analysis_wrote(&dsp, 0);
         }
         else {

            for (int i = 0; i < numChannels; i++) {
               float *temp = (float *)mixer->GetBuffer(i);
               memcpy(vorbis_buffer[i], temp, sizeof(float)*SAMPLES_PER_RUN);
            }

            // tell the encoder how many samples we have
            vorbis_analysis_wrote(&dsp, samplesThisRun);
         }

         // I don't understand what this call does, so here is the comment
         // from the example, verbatim:
         //
         //    vorbis does some data preanalysis, then divvies up blocks
         //    for more involved (potentially parallel) processing. Get
         //    a single block for encoding now
         while (vorbis_analysis_blockout(&dsp, &block) == 1) {

            // analysis, assume we want to use bitrate management
            vorbis_analysis(&block, NULL);
            vorbis_bitrate_addblock(&block);

            while (vorbis_bitrate_flushpacket(&dsp, &packet)) {

               // add the packet to the bitstream
               ogg_stream_packetin(&stream, &packet);

               // From vorbis-tools-1.0/oggenc/encode.c:
               //   If we've gone over a page boundary, we can do actual output,
               //   so do so (for however many pages are available).

               while (!eos) {
                  int result = ogg_stream_pageout(&stream, &page);
                  if (!result) {
                     break;
                  }

                  outFile.Write(page.header, page.header_len);
                  outFile.Write(page.body, page.body_len);

                  if (ogg_page_eos(&page)) {
                     eos = 1;
                  }
               }
            }
         }

         updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   ogg_stream_clear(&stream);

   vorbis_block_clear(&block);
   vorbis_dsp_clear(&dsp);
   vorbis_info_clear(&info);
   vorbis_comment_clear(&comment);

   outFile.Close();

   return updateResult;
}

wxWindow *ExportOGG::OptionsCreate(wxWindow *parent, int format)
{
   wxASSERT(parent); // to justify safenew
   return safenew ExportOGGOptions(parent, format);
}

bool ExportOGG::FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata)
{
   // Retrieve tags from project if not over-ridden
   if (metadata == NULL)
      metadata = project->GetTags();

   vorbis_comment_init(comment);

   wxString n;
   for (const auto &pair : metadata->GetRange()) {
      n = pair.first;
      const auto &v = pair.second;
      if (n == TAG_YEAR) {
         n = wxT("DATE");
      }
      vorbis_comment_add_tag(comment,
                             (char *) (const char *) n.mb_str(wxConvUTF8),
                             (char *) (const char *) v.mb_str(wxConvUTF8));
   }

   return true;
}

movable_ptr<ExportPlugin> New_ExportOGG()
{
   return make_movable<ExportOGG>();
}

#endif // USE_LIBVORBIS

