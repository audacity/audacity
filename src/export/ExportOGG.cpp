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

#include "Export.h"
#include "ExportOGG.h"

#include <wx/log.h>
#include <wx/msgdlg.h>

#include <vorbis/vorbisenc.h>

#include "../FileIO.h"
#include "../Project.h"
#include "../Mix.h"
#include "../Prefs.h"

#include "../Internat.h"
#include "../Tags.h"

//----------------------------------------------------------------------------
// ExportOGGOptions
//----------------------------------------------------------------------------

class ExportOGGOptions : public wxDialog
{
public:

   ExportOGGOptions(wxWindow *parent, int format);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   int mOggQualityUnscaled;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportOGGOptions, wxDialog)
   EVT_BUTTON(wxID_OK, ExportOGGOptions::OnOK)
END_EVENT_TABLE()

///
///
ExportOGGOptions::ExportOGGOptions(wxWindow *parent, int WXUNUSED(format))
:  wxDialog(parent, wxID_ANY,
            wxString(_("Specify Ogg Vorbis Options")))
{
   ShuttleGui S(this, eIsCreatingFromPrefs);

   mOggQualityUnscaled = gPrefs->Read(wxT("/FileFormats/OggExportQuality"),50)/10;

   PopulateOrExchange(S);
}

///
///
void ExportOGGOptions::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.StartStatic(_("Ogg Vorbis Export Setup"), 1);
      {
         S.StartMultiColumn(2, wxEXPAND);
         {
            S.SetStretchyCol(1);
            S.TieSlider(_("Quality:"), mOggQualityUnscaled, 10);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

///
///
void ExportOGGOptions::OnOK(wxCommandEvent& WXUNUSED(event))
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Write(wxT("/FileFormats/OggExportQuality"),mOggQualityUnscaled * 10);
   gPrefs->Flush();

   EndModal(wxID_OK);

   return;
}

//----------------------------------------------------------------------------
// ExportOGG
//----------------------------------------------------------------------------

#define SAMPLES_PER_RUN 8192

class ExportOGG : public ExportPlugin
{
public:

   ExportOGG();
   void Destroy();

   // Required

   bool DisplayOptions(wxWindow *parent, int format = 0);
   int Export(AudacityProject *project,
               int channels,
               wxString fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               Tags *metadata = NULL,
               int subformat = 0);

private:

   bool FillComment(AudacityProject *project, vorbis_comment *comment, Tags *metadata);
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

void ExportOGG::Destroy()
{
   delete this;
}

int ExportOGG::Export(AudacityProject *project,
                       int numChannels,
                       wxString fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       Tags *metadata,
                       int WXUNUSED(subformat))
{
   double    rate    = project->GetRate();
   TrackList *tracks = project->GetTracks();
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
   vorbis_encode_init_vbr(&info, numChannels, int(rate + 0.5), quality);

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
   // start on a new page, which apparently makes streaming easier
   while (ogg_stream_flush(&stream, &page)) {
      outFile.Write(page.header, page.header_len);
      outFile.Write(page.body, page.body_len);
   }

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = CreateMixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            numChannels, SAMPLES_PER_RUN, false,
                            rate, floatSample, true, mixerSpec);
   delete [] waveTracks;

   ProgressDialog *progress = new ProgressDialog(wxFileName(fName).GetName(),
      selectionOnly ?
      _("Exporting the selected audio as Ogg Vorbis") :
      _("Exporting the entire project as Ogg Vorbis"));

   while (updateResult == eProgressSuccess && !eos) {
      float **vorbis_buffer = vorbis_analysis_buffer(&dsp, SAMPLES_PER_RUN);
      sampleCount samplesThisRun = mixer->Process(SAMPLES_PER_RUN);

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

      updateResult = progress->Update(mixer->MixGetCurrentTime()-t0, t1-t0);
   }

   delete progress;;

   delete mixer;

   ogg_stream_clear(&stream);

   vorbis_block_clear(&block);
   vorbis_dsp_clear(&dsp);
   vorbis_info_clear(&info);
   vorbis_comment_clear(&comment);

   outFile.Close();

   return updateResult;
}

bool ExportOGG::DisplayOptions(wxWindow *parent, int format)
{
   ExportOGGOptions od(parent, format);

   od.ShowModal();

   return true;
}

bool ExportOGG::FillComment(AudacityProject *project, vorbis_comment *comment, Tags *metadata)
{
   // Retrieve tags from project if not over-ridden
   if (metadata == NULL)
      metadata = project->GetTags();

   vorbis_comment_init(comment);

   wxString n, v;
   for (bool cont = metadata->GetFirst(n, v); cont; cont = metadata->GetNext(n, v)) {
      if (n == TAG_YEAR) {
         n = wxT("DATE");
      }
      vorbis_comment_add_tag(comment,
                             (char *) (const char *) n.mb_str(wxConvUTF8),
                             (char *) (const char *) v.mb_str(wxConvUTF8));
   }

   return true;
}

ExportPlugin *New_ExportOGG()
{
   return new ExportOGG();
}

#endif // USE_LIBVORBIS

