/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.cpp

  Joshua Haberman

  This program is distributed under the GNU General Public License, version 2.
  A copy of this license is included with this source.

**********************************************************************/

#include "ExportOGG.h"

#include <wx/log.h>
#include <wx/stream.h>

ExportOptionOGGEditor::ExportOptionOGGEditor()
{
    mQualityUnscaled = *std::get_if<int>(&OGGQualityOption.defaultValue);
}

std::string ExportOptionOGGEditor::GetName() const
{
    return "ogg";
}

int ExportOptionOGGEditor::GetOptionsCount() const
{
    return 1;
}

bool ExportOptionOGGEditor::GetOption(int, ExportOption& option) const
{
    option = OGGQualityOption;
    return true;
}

bool ExportOptionOGGEditor::GetValue(ExportOptionID, ExportValue& value) const
{
    value = mQualityUnscaled;
    return true;
}

bool ExportOptionOGGEditor::SetValue(ExportOptionID, const ExportValue& value)
{
    if (auto num = std::get_if<int>(&value)) {
        mQualityUnscaled = *num;
        return true;
    }
    return false;
}

ExportOptionOGGEditor::SampleRateList ExportOptionOGGEditor::GetSampleRateList() const
{
    return {};
}

void ExportOptionOGGEditor::Load(const audacity::BasicSettings& config)
{
    mQualityUnscaled = config.Read(wxT("/FileFormats/OggExportQuality"), 50) / 10;
}

void ExportOptionOGGEditor::Store(audacity::BasicSettings& config) const
{
    config.Write(wxT("/FileFormats/OggExportQuality"), mQualityUnscaled * 10);
}

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

OGGExportProcessor::~OGGExportProcessor()
{
    if (context.stream_ok) {
        ogg_stream_clear(&context.stream);
    }

    if (context.analysis_state_ok) {
        vorbis_comment_clear(&context.comment);
        vorbis_block_clear(&context.block);
        vorbis_dsp_clear(&context.dsp);
    }

    vorbis_info_clear(&context.info);
}

bool OGGExportProcessor::Initialize(AudacityProject& project,
                                    const Parameters& parameters,
                                    const wxFileNameWrapper& fName,
                                    double t0, double t1, bool selectionOnly,
                                    double sampleRate, unsigned numChannels,
                                    MixerOptions::Downmix* mixerSpec,
                                    const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.numChannels = numChannels;

    double quality = ExportPluginHelpers::GetParameterValue(parameters, 0, 5) / 10.0;

    wxLogNull logNo;           // temporarily disable wxWidgets error messages

    // Many of the library functions called below return 0 for success and
    // various nonzero codes for failure.

    // Encoding setup

    vorbis_info_init(&context.info);

    if (vorbis_encode_init_vbr(&context.info, numChannels, (int)(sampleRate + 0.5), quality)) {
        // TODO: more precise message
        throw ExportException(_("Unable to export - rate or quality problem"));
    }

    context.outFile = std::make_unique<FileIO>(fName, FileIO::Output);

    if (!context.outFile->IsOpened()) {
        throw ExportException(_("Unable to open target file for writing"));
    }

    context.analysis_state_ok = vorbis_analysis_init(&context.dsp, &context.info) == 0
                                && vorbis_block_init(&context.dsp, &context.block) == 0;
    // Set up analysis state and auxiliary encoding storage
    if (!context.analysis_state_ok) {
        throw ExportException(_("Unable to export - problem initialising"));
    }

    // Retrieve tags
    FillComment(&project, &context.comment, metadata);

    // Set up packet->stream encoder.  According to encoder example,
    // a random serial number makes it more likely that you can make
    // chained streams with concatenation.
    srand(time(NULL));
    context.stream_ok = ogg_stream_init(&context.stream, rand()) == 0;
    if (!context.stream_ok) {
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

    if (vorbis_analysis_headerout(&context.dsp, &context.comment, &bitstream_header, &comment_header,
                                  &codebook_header)
        ||// Place these headers into the stream
        ogg_stream_packetin(&context.stream, &bitstream_header)
        || ogg_stream_packetin(&context.stream, &comment_header)
        || ogg_stream_packetin(&context.stream, &codebook_header)) {
        throw ExportException(_("Unable to export - problem with packets"));
    }

    // Flushing these headers now guarantees that audio data will
    // start on a NEW page, which apparently makes streaming easier
    while (ogg_stream_flush(&context.stream, &context.page)) {
        if (context.outFile->Write(context.page.header, context.page.header_len).GetLastError()
            || context.outFile->Write(context.page.body, context.page.body_len).GetLastError()) {
            throw ExportException(_("Unable to export - problem with file"));
        }
    }

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, numChannels, SAMPLES_PER_RUN, false,
        sampleRate, floatSample, mixerSpec);

    context.status = selectionOnly
                     ? XO("Exporting the selected audio as Ogg Vorbis")
                     : XO("Exporting the audio as Ogg Vorbis");

    return true;
}

ExportResult OGGExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    auto exportResult = ExportResult::Success;
    {
        int err;
        int eos = 0;
        while (exportResult == ExportResult::Success && !eos) {
            float** vorbis_buffer = vorbis_analysis_buffer(&context.dsp, SAMPLES_PER_RUN);
            auto samplesThisRun = context.mixer->Process();

            if (samplesThisRun == 0) {
                // Tell the library that we wrote 0 bytes - signalling the end.
                err = vorbis_analysis_wrote(&context.dsp, 0);
            } else {
                for (size_t i = 0; i < context.numChannels; i++) {
                    float* temp = (float*)context.mixer->GetBuffer(i);
                    memcpy(vorbis_buffer[i], temp, sizeof(float) * SAMPLES_PER_RUN);
                }

                // tell the encoder how many samples we have
                err = vorbis_analysis_wrote(&context.dsp, samplesThisRun);
            }

            // I don't understand what this call does, so here is the comment
            // from the example, verbatim:
            //
            //    vorbis does some data preanalysis, then divvies up blocks
            //    for more involved (potentially parallel) processing. Get
            //    a single block for encoding now
            while (!err && vorbis_analysis_blockout(&context.dsp, &context.block) == 1) {
                // analysis, assume we want to use bitrate management
                err = vorbis_analysis(&context.block, NULL);
                if (!err) {
                    err = vorbis_bitrate_addblock(&context.block);
                }

                while (!err && vorbis_bitrate_flushpacket(&context.dsp, &context.packet)) {
                    // add the packet to the bitstream
                    err = ogg_stream_packetin(&context.stream, &context.packet);

                    // From vorbis-tools-1.0/oggenc/encode.c:
                    //   If we've gone over a page boundary, we can do actual output,
                    //   so do so (for however many pages are available).

                    while (!err && !eos) {
                        int result = ogg_stream_pageout(&context.stream, &context.page);
                        if (!result) {
                            break;
                        }

                        if (context.outFile->Write(context.page.header, context.page.header_len).GetLastError()
                            || context.outFile->Write(context.page.body, context.page.body_len).GetLastError()) {
                            // TODO: more precise message
                            throw ExportDiskFullError(context.fName);
                        }

                        if (ogg_page_eos(&context.page)) {
                            eos = 1;
                        }
                    }
                }
            }

            if (err) {
                // TODO: more precise message
                throw ExportErrorException("OGG:355");
            }
            exportResult = ExportPluginHelpers::UpdateProgress(
                delegate, *context.mixer, context.t0, context.t1);
        }
    }

    if (!context.outFile->Close()) {
        // TODO: more precise message
        throw ExportErrorException("OGG:366");
    }

    return exportResult;
}

std::unique_ptr<ExportOptionsEditor>
ExportOGG::CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const
{
    return std::make_unique<ExportOptionOGGEditor>();
}

std::unique_ptr<ExportProcessor> ExportOGG::CreateProcessor(int format) const
{
    return std::make_unique<OGGExportProcessor>();
}

void OGGExportProcessor::FillComment(AudacityProject* project, vorbis_comment* comment, const Tags* metadata)
{
    // Retrieve tags from project if not over-ridden
    if (metadata == NULL) {
        metadata = &Tags::Get(*project);
    }

    vorbis_comment_init(comment);

    wxString n;
    for (const auto& pair : metadata->GetRange()) {
        n = pair.first;
        const auto& v = pair.second;
        if (n == TAG_YEAR) {
            n = wxT("DATE");
        }
        vorbis_comment_add_tag(comment,
                               (char*)(const char*)n.mb_str(wxConvUTF8),
                               (char*)(const char*)v.mb_str(wxConvUTF8));
    }
}
