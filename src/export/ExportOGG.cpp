/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.cpp

  Joshua Haberman

  This program is distributed under the GNU General Public License, version 2.
  A copy of this license is included with this source.

**********************************************************************/



#ifdef USE_LIBVORBIS

#include "Export.h"

#include <wx/log.h>
#include <wx/stream.h>
 
#include <vorbis/vorbisenc.h>

#include "FileIO.h"
#include "ProjectRate.h"
#include "Mix.h"

#include "Tags.h"
#include "Track.h"

#include "ExportUtils.h"
#include "ExportProgressListener.h"
#include "PlainExportOptionsEditor.h"

namespace
{
   enum : int {
      OptionIDOGGQuality = 0
   };

   const ExportOption OGGQualityOption {
         OptionIDOGGQuality, XO("Quality"),
         5,
         ExportOption::TypeRange,
         { 0, 10 }
   };

   class ExportOptionOGGEditor final : public ExportOptionsEditor
   {
      int mQualityUnscaled;
   public:

      ExportOptionOGGEditor()
      {
         mQualityUnscaled = *std::get_if<int>(&OGGQualityOption.defaultValue);
      }

      int GetOptionsCount() const override
      {
         return 1;
      }

      bool GetOption(int, ExportOption& option) const override
      {
         option = OGGQualityOption;
         return true;
      }

      bool GetValue(ExportOptionID, ExportValue& value) const override
      {
         value = mQualityUnscaled;
         return true;
      }

      bool SetValue(ExportOptionID, const ExportValue& value) override
      {
         if(auto num = std::get_if<int>(&value))
         {
            mQualityUnscaled = *num;
            return true;
         }
         return false;
      }

      void Load(const wxConfigBase& config) override
      {
         mQualityUnscaled = config.Read(wxT("/FileFormats/OggExportQuality"),50)/10;
      }

      void Store(wxConfigBase& config) const override
      {
         config.Write(wxT("/FileFormats/OggExportQuality"), mQualityUnscaled * 10);
      }

   };
}

#define SAMPLES_PER_RUN 8192u

class ExportOGG final : public ExportPluginEx
{
public:

   ExportOGG();

   int GetFormatCount() const override;
   FormatInfo GetFormatInfo(int) const override;

   std::unique_ptr<ExportOptionsEditor>
   CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

   void Export(AudacityProject *project,
               ExportProgressListener &pDialog,
               const Parameters& parameters,
               unsigned channels,
               const wxFileNameWrapper &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec,
               const Tags *metadata,
               int subformat) override;

private:

   bool FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata);
};

ExportOGG::ExportOGG() = default;

int ExportOGG::GetFormatCount() const
{
   return 1;
}

FormatInfo ExportOGG::GetFormatInfo(int) const
{
   return {
      wxT("OGG"), XO("Ogg Vorbis Files"), { wxT("ogg") }, 255, true
   };
}

void ExportOGG::Export(AudacityProject *project,
                       ExportProgressListener &progressListener,
                       const Parameters& parameters,
                       unsigned numChannels,
                       const wxFileNameWrapper &fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       const Tags *metadata,
                       int)
{
   ExportBegin();
   
   double    rate    = ProjectRate::Get( *project ).GetRate();
   const auto &tracks = TrackList::Get( *project );
   double    quality = ExportUtils::GetParameterValue(parameters, 0, 5) / 10.0;

   wxLogNull logNo;            // temporarily disable wxWidgets error messages
   int       eos = 0;

   FileIO outFile(fName, FileIO::Output);

   if (!outFile.IsOpened()) {
      SetErrorString(XO("Unable to open target file for writing"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   // All the Ogg and Vorbis encoding data
   ogg_stream_state stream;
   ogg_page         page;
   ogg_packet       packet;

   vorbis_info      info;
   vorbis_comment   comment;
   vorbis_dsp_state dsp;
   vorbis_block     block;


   auto cleanup1 = finally( [&] {
      vorbis_info_clear(&info);
   } );


   // Many of the library functions called below return 0 for success and
   // various nonzero codes for failure.

   // Encoding setup
   vorbis_info_init(&info);
   if (vorbis_encode_init_vbr(&info, numChannels, (int)(rate + 0.5), quality)) {
      // TODO: more precise message
      SetErrorString(XO("Unable to export - rate or quality problem"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   auto cleanup2 = finally( [&] {
      ogg_stream_clear(&stream);

      vorbis_block_clear(&block);
      vorbis_dsp_clear(&dsp);
      vorbis_comment_clear(&comment);
   } );

   // Retrieve tags
   if (!FillComment(project, &comment, metadata)) {
      SetErrorString(XO("Unable to export - problem with metadata"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   // Set up analysis state and auxiliary encoding storage
   if (vorbis_analysis_init(&dsp, &info) ||
       vorbis_block_init(&dsp, &block)) {
      SetErrorString(XO("Unable to export - problem initialising"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   // Set up packet->stream encoder.  According to encoder example,
   // a random serial number makes it more likely that you can make
   // chained streams with concatenation.
   srand(time(NULL));
   if (ogg_stream_init(&stream, rand())) {
      SetErrorString(XO("Unable to export - problem creating stream"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   // First we need to write the required headers:
   //    1. The Ogg bitstream header, which contains codec setup params
   //    2. The Vorbis comment header
   //    3. The bitstream codebook.
   //
   // After we create those our responsibility is complete, libvorbis will
   // take care of any other ogg bitstream constraints (again, according
   // to the example encoder source)
   ogg_packet bitstream_header;
   ogg_packet comment_header;
   ogg_packet codebook_header;

   if(vorbis_analysis_headerout(&dsp, &comment, &bitstream_header, &comment_header,
         &codebook_header) ||
      // Place these headers into the stream
      ogg_stream_packetin(&stream, &bitstream_header) ||
      ogg_stream_packetin(&stream, &comment_header) ||
      ogg_stream_packetin(&stream, &codebook_header)) {
      SetErrorString(XO("Unable to export - problem with packets"));
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   // Flushing these headers now guarantees that audio data will
   // start on a NEW page, which apparently makes streaming easier
   while (ogg_stream_flush(&stream, &page)) {
      if ( outFile.Write(page.header, page.header_len).GetLastError() ||
           outFile.Write(page.body, page.body_len).GetLastError()) {
         SetErrorString(XO("Unable to export - problem with file"));
         progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
         return;
      }
   }

   int err;
   
   {
      auto mixer = ExportUtils::CreateMixer(tracks, selectionOnly,
         t0, t1,
         numChannels, SAMPLES_PER_RUN, false,
         rate, floatSample, mixerSpec);

      SetStatusString(selectionOnly
         ? XO("Exporting the selected audio as Ogg Vorbis")
         : XO("Exporting the audio as Ogg Vorbis"));

      while (!IsCancelled() && !IsStopped() && !eos) {
         float **vorbis_buffer = vorbis_analysis_buffer(&dsp, SAMPLES_PER_RUN);
         auto samplesThisRun = mixer->Process();

         if (samplesThisRun == 0) {
            // Tell the library that we wrote 0 bytes - signalling the end.
            err = vorbis_analysis_wrote(&dsp, 0);
         }
         else {

            for (size_t i = 0; i < numChannels; i++) {
               float *temp = (float *)mixer->GetBuffer(i);
               memcpy(vorbis_buffer[i], temp, sizeof(float)*SAMPLES_PER_RUN);
            }

            // tell the encoder how many samples we have
            err = vorbis_analysis_wrote(&dsp, samplesThisRun);
         }

         // I don't understand what this call does, so here is the comment
         // from the example, verbatim:
         //
         //    vorbis does some data preanalysis, then divvies up blocks
         //    for more involved (potentially parallel) processing. Get
         //    a single block for encoding now
         while (!err && vorbis_analysis_blockout(&dsp, &block) == 1) {

            // analysis, assume we want to use bitrate management
            err = vorbis_analysis(&block, NULL);
            if (!err)
               err = vorbis_bitrate_addblock(&block);

            while (!err && vorbis_bitrate_flushpacket(&dsp, &packet)) {

               // add the packet to the bitstream
               err = ogg_stream_packetin(&stream, &packet);

               // From vorbis-tools-1.0/oggenc/encode.c:
               //   If we've gone over a page boundary, we can do actual output,
               //   so do so (for however many pages are available).

               while (!err && !eos) {
                  int result = ogg_stream_pageout(&stream, &page);
                  if (!result) {
                     break;
                  }

                  if ( outFile.Write(page.header, page.header_len).GetLastError() ||
                       outFile.Write(page.body, page.body_len).GetLastError()) {
                     // TODO: more precise message
                     ShowDiskFullExportErrorDialog(fName);
                     progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
                     return;
                  }

                  if (ogg_page_eos(&page)) {
                     eos = 1;
                  }
               }
            }
         }

         if (err) {
            // TODO: more precise message
            ShowExportErrorDialog("OGG:355");
            break;
         }
         progressListener.OnExportProgress(ExportUtils::EvalExportProgress(*mixer, t0, t1));
      }
   }

   if ( !outFile.Close() ) {
      // TODO: more precise message
      ShowExportErrorDialog("OGG:366");
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
      return;
   }

   if(err)
      progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
   else
      ExportFinish(progressListener);
}

std::unique_ptr<ExportOptionsEditor>
ExportOGG::CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const
{
   return std::make_unique<ExportOptionOGGEditor>();
}


bool ExportOGG::FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata)
{
   // Retrieve tags from project if not over-ridden
   if (metadata == NULL)
      metadata = &Tags::Get( *project );

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

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "OGG",
   []{ return std::make_unique< ExportOGG >(); }
};

#endif // USE_LIBVORBIS

