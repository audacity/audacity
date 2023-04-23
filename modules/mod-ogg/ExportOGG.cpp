/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.cpp

  Joshua Haberman

  This program is distributed under the GNU General Public License, version 2.
  A copy of this license is included with this source.

**********************************************************************/

#include "Export.h"

#include <wx/log.h>
#include <wx/stream.h>
 
#include <vorbis/vorbisenc.h>

#include "ExportPluginHelpers.h"
#include "FileIO.h"
#include "ProjectRate.h"
#include "Mix.h"

#include "Tags.h"
#include "Track.h"
#include "wxFileNameWrapper.h"

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

class ExportOGG final : public ExportPlugin
{
public:

   ExportOGG();

   int GetFormatCount() const override;
   FormatInfo GetFormatInfo(int) const override;

   std::unique_ptr<ExportOptionsEditor>
   CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

   std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
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

class OGGExportProcessor final : public ExportProcessor
{
   TranslatableString mStatus;
   unsigned mChannels;
   double mT0;
   double mT1;
   wxFileNameWrapper mFileName;

   std::unique_ptr<Mixer> mixer;
   std::unique_ptr<FileIO> outFile;
   // All the Ogg and Vorbis encoding data
   vorbis_info      info{};
   ogg_stream_state stream{};
   ogg_page         page{};
   ogg_packet       packet{};

   vorbis_comment   comment{};
   vorbis_dsp_state dsp{};
   vorbis_block     block{};
   
public:

   ~OGGExportProcessor()
   {
      ogg_stream_clear(&stream);

      vorbis_block_clear(&block);
      vorbis_dsp_clear(&dsp);
      vorbis_comment_clear(&comment);

      vorbis_info_clear(&info);
   }

   void Initialize(AudacityProject& project,
      const Parameters& parameters,
      const wxFileNameWrapper& fName,
      double t0, double t1, bool selectionOnly,
      unsigned numChannels,
      MixerOptions::Downmix* mixerSpec,
      const Tags* metadata) override
   {
      mFileName = fName;
      mChannels = numChannels;
      mT0 = t0;
      mT1 = t1;

      double    rate    = ProjectRate::Get( project ).GetRate();
      const auto &tracks = TrackList::Get( project );
      double    quality = ExportPluginHelpers::GetParameterValue(parameters, 0, 5) / 10.0;

      wxLogNull logNo;            // temporarily disable wxWidgets error messages
      
      outFile = std::make_unique<FileIO>(fName, FileIO::Output);

      if (!outFile->IsOpened()) {
         throw ExportException(_("Unable to open target file for writing"));
      }

      // Many of the library functions called below return 0 for success and
      // various nonzero codes for failure.

      // Encoding setup
      vorbis_info_init(&info);
      if (vorbis_encode_init_vbr(&info, numChannels, (int)(rate + 0.5), quality)) {
         // TODO: more precise message
         throw ExportException(_("Unable to export - rate or quality problem"));
      }
      
      // Retrieve tags
      FillComment(&project, &comment, metadata);

      // Set up analysis state and auxiliary encoding storage
      if (vorbis_analysis_init(&dsp, &info) ||
          vorbis_block_init(&dsp, &block)) {
         throw ExportException(_("Unable to export - problem initialising"));
      }

      // Set up packet->stream encoder.  According to encoder example,
      // a random serial number makes it more likely that you can make
      // chained streams with concatenation.
      srand(time(NULL));
      if (ogg_stream_init(&stream, rand())) {
         throw ExportException(_("Unable to export - problem creating stream"));
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
         throw ExportException(_("Unable to export - problem with packets"));
      }

      // Flushing these headers now guarantees that audio data will
      // start on a NEW page, which apparently makes streaming easier
      while (ogg_stream_flush(&stream, &page)) {
         if ( outFile->Write(page.header, page.header_len).GetLastError() ||
              outFile->Write(page.body, page.body_len).GetLastError()) {
            throw ExportException(_("Unable to export - problem with file"));
         }
      }

      mixer = ExportPluginHelpers::CreateMixer(tracks, selectionOnly,
            t0, t1,
            numChannels, SAMPLES_PER_RUN, false,
            rate, floatSample, mixerSpec);

      mStatus = selectionOnly
            ? XO("Exporting the selected audio as Ogg Vorbis")
            : XO("Exporting the audio as Ogg Vorbis");
   }

   ExportResult Process(ExportPluginDelegate& delegate) override
   {
      int err;
      int eos = 0;

      auto exportResult = ExportResult::Success;

      delegate.SetStatusString(mStatus);

      while (exportResult == ExportResult::Success && !eos) {
         float **vorbis_buffer = vorbis_analysis_buffer(&dsp, SAMPLES_PER_RUN);
         auto samplesThisRun = mixer->Process();

         if (samplesThisRun == 0) {
            // Tell the library that we wrote 0 bytes - signalling the end.
            err = vorbis_analysis_wrote(&dsp, 0);
         }
         else {

            for (size_t i = 0; i < mChannels; i++) {
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

                  if ( outFile->Write(page.header, page.header_len).GetLastError() ||
                       outFile->Write(page.body, page.body_len).GetLastError()) {
                     // TODO: more precise message
                     throw ExportDiskFullError(mFileName.GetName());
                  }

                  if (ogg_page_eos(&page)) {
                     eos = 1;
                  }
               }
            }
         }

         if (err) {
            // TODO: more precise message
            throw ExportErrorCodeException("OGG:355");
         }
         exportResult = ExportPluginHelpers::UpdateProgress(
            delegate, *mixer, mT0, mT1);
      }

      if ( !outFile->Close() ) {
         // TODO: more precise message
         throw ExportErrorCodeException("OGG:366");
      }

      if(err)
         exportResult = ExportResult::Error;
      return exportResult;
   }

private:
   static void FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata);
};

std::unique_ptr<ExportProcessor> ExportOGG::CreateProcessor(int) const
{
   return std::make_unique<OGGExportProcessor>();
}

std::unique_ptr<ExportOptionsEditor>
ExportOGG::CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const
{
   return std::make_unique<ExportOptionOGGEditor>();
}


void OGGExportProcessor::FillComment(AudacityProject *project, vorbis_comment *comment, const Tags *metadata)
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
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "OGG",
   []{ return std::make_unique< ExportOGG >(); }
};
