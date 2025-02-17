/**********************************************************************

Audacity: A Digital Audio Editor

ExportFLAC.cpp

Frederik M.J.V

This program is distributed under the GNU General Public License, version 2.
A copy of this license is included with this source.

Based on ExportOGG.cpp by:
Joshua Haberman

**********************************************************************/

#include <rapidjson/document.h>

#include "Export.h"

#include <wx/ffile.h>
#include <wx/log.h>

#include "FLAC++/encoder.h"

#include "float_cast.h"
#include "Mix.h"
#include "Prefs.h"

#include "Tags.h"
#include "Track.h"

#include "wxFileNameWrapper.h"

#include "ExportPluginHelpers.h"
#include "ExportPluginRegistry.h"
#include "PlainExportOptionsEditor.h"

//----------------------------------------------------------------------------
// ExportFLACOptions Class
//----------------------------------------------------------------------------

namespace {
enum : int {
    FlacOptionIDBitDepth = 0,
    FlacOptionIDLevel
};

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> FlacOptions {
    {
        {
            FlacOptionIDBitDepth, XO("Bit Depth"),
            std::string("16"),
            ExportOption::TypeEnum,
            { std::string("16"), std::string("24") },
            { XO("16 bit"), XO("24 bit") }
        }, wxT("/FileFormats/FLACBitDepth")
    },
    {
        {
            FlacOptionIDLevel, XO("Level"),
            std::string("5"),
            ExportOption::TypeEnum,
            {
                std::string("0"),
                std::string("1"),
                std::string("2"),
                std::string("3"),
                std::string("4"),
                std::string("5"),
                std::string("6"),
                std::string("7"),
                std::string("8"),
            },
            {
                XO("0 (fastest)"),
                XO("1"),
                XO("2"),
                XO("3"),
                XO("4"),
                XO("5"),
                XO("6"),
                XO("7"),
                XO("8 (best)"),
            }
        }, wxT("/FileFormats/FLACLevel")
    }
};
}

ChoiceSetting FLACBitDepth{
    wxT("/FileFormats/FLACBitDepth"),
    {
        ByColumns,
        { XO("16 bit"), XO("24 bit"), },
        { wxT("16"),    wxT("24"), }
    },
    0 // "16",
};

ChoiceSetting FLACLevel{
    wxT("/FileFormats/FLACLevel"),
    {
        ByColumns,
        {
            XO("0 (fastest)"),
            XO("1"),
            XO("2"),
            XO("3"),
            XO("4"),
            XO("5"),
            XO("6"),
            XO("7"),
            XO("8 (best)"),
        },
        {
            wxT("0"),
            wxT("1"),
            wxT("2"),
            wxT("3"),
            wxT("4"),
            wxT("5"),
            wxT("6"),
            wxT("7"),
            wxT("8"),
        }
    },
    5 //"5"
};

#define SAMPLES_PER_RUN 8192u

/* FLACPP_API_VERSION_CURRENT is 6 for libFLAC++ from flac-1.1.3 (see <FLAC++/export.h>) */
#if !defined FLACPP_API_VERSION_CURRENT || FLACPP_API_VERSION_CURRENT < 6
#define LEGACY_FLAC
#else
#undef LEGACY_FLAC
#endif

static struct
{
    bool do_exhaustive_model_search;
    bool do_escape_coding;
    bool do_mid_side_stereo;
    bool loose_mid_side_stereo;
    unsigned qlp_coeff_precision;
    unsigned min_residual_partition_order;
    unsigned max_residual_partition_order;
    unsigned rice_parameter_search_dist;
    unsigned max_lpc_order;
} flacLevels[] = {
    { false,   false,   false,   false,   0, 2, 2, 0, 0 },
    { false,   false,   true,    true,    0, 2, 2, 0, 0 },
    { false,   false,   true,    false,   0, 0, 3, 0, 0 },
    { false,   false,   false,   false,   0, 3, 3, 0, 6 },
    { false,   false,   true,    true,    0, 3, 3, 0, 8 },
    { false,   false,   true,    false,   0, 3, 3, 0, 8 },
    { false,   false,   true,    false,   0, 0, 4, 0, 8 },
    { true,    false,   true,    false,   0, 0, 6, 0, 8 },
    { true,    false,   true,    false,   0, 0, 6, 0, 12 },
};

//----------------------------------------------------------------------------

struct FLAC__StreamMetadataDeleter {
    void operator ()(FLAC__StreamMetadata* p) const
    {
        if (p) {
            ::FLAC__metadata_object_delete(p);
        }
    }
};
using FLAC__StreamMetadataHandle = std::unique_ptr<
    FLAC__StreamMetadata, FLAC__StreamMetadataDeleter
    >;

class FLACExportProcessor final : public ExportProcessor
{
    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        unsigned numChannels;
        wxFileNameWrapper fName;
        sampleFormat format;
        FLAC::Encoder::File encoder;
        wxFFile f;
        std::unique_ptr<Mixer> mixer;
    } context;

public:

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleFormat, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    FLAC__StreamMetadataHandle MakeMetadata(AudacityProject* project, const Tags* tags) const;
};

class ExportFLAC final : public ExportPlugin
{
public:

    ExportFLAC();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    bool ParseConfig(int, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

//----------------------------------------------------------------------------

ExportFLAC::ExportFLAC() = default;

int ExportFLAC::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportFLAC::GetFormatInfo(int) const
{
    return {
        wxT("FLAC"), XO("FLAC Files"), { wxT("flac") }, FLAC__MAX_CHANNELS, true
    };
}

bool ExportFLAC::ParseConfig(int, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const
{
    if (!config.IsObject()
        || !config.HasMember("level") || !config["level"].IsNumber()
        || !config.HasMember("bit_depth") || !config["bit_depth"].IsNumber()) {
        return false;
    }

    const auto level = ExportValue(std::to_string(config["level"].GetInt()));
    const auto bitDepth = ExportValue(std::to_string(config["bit_depth"].GetInt()));

    for (const auto& desc : FlacOptions) {
        const auto& option = desc.option;
        if ((option.id == FlacOptionIDLevel
             && std::find(option.values.begin(),
                          option.values.end(),
                          level) == option.values.end())
            ||
            (desc.option.id == FlacOptionIDBitDepth
             && std::find(option.values.begin(),
                          option.values.end(),
                          bitDepth) == option.values.end())) {
            return false;
        }
    }
    ExportProcessor::Parameters result {
        { FlacOptionIDLevel, level },
        { FlacOptionIDBitDepth, bitDepth }
    };
    std::swap(parameters, result);
    return true;
}

std::vector<std::string> ExportFLAC::GetMimeTypes(int) const
{
    return { "audio/x-flac" };
}

std::unique_ptr<ExportOptionsEditor>
ExportFLAC::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<PlainExportOptionsEditor>(FlacOptions, listener);
}

std::unique_ptr<ExportProcessor> ExportFLAC::CreateProcessor(int) const
{
    return std::make_unique<FLACExportProcessor>();
}

bool FLACExportProcessor::Initialize(AudacityProject& project,
                                     const Parameters& parameters,
                                     const wxFileNameWrapper& fName,
                                     double t0, double t1, bool selectionOnly,
                                     double sampleRate, unsigned numChannels,
                                     MixerOptions::Downmix* mixerSpec,
                                     const Tags* tags)
{
    context.t0 = t0;
    context.t1 = t1;
    context.numChannels = numChannels;
    context.fName = fName;

    wxLogNull logNo;           // temporarily disable wxWidgets error messages

    long levelPref = std::stol(ExportPluginHelpers::GetParameterValue<std::string>(parameters, FlacOptionIDLevel));
    auto bitDepthPref = ExportPluginHelpers::GetParameterValue<std::string>(parameters, FlacOptionIDBitDepth);

    auto& encoder = context.encoder;

    bool success = true;
    success = success &&
#ifdef LEGACY_FLAC
              encoder.set_filename(OSOUTPUT(fName)) &&
#endif
              encoder.set_channels(numChannels)
              && encoder.set_sample_rate(lrint(sampleRate));

    // See note in MakeMetadata() about a bug in libflac++ 1.1.2
    FLAC__StreamMetadataHandle metadata;
    if (success) {
        metadata = MakeMetadata(&project, tags);
    }

    if (success && !metadata) {
        // TODO: more precise message
        throw ExportErrorException("FLAC:283");
    }

    if (success && metadata) {
        // set_metadata expects an array of pointers to metadata and a size.
        // The size is 1.
        FLAC__StreamMetadata* p = metadata.get();
        success = encoder.set_metadata(&p, 1);
    }

    if (bitDepthPref == "24") {
        context.format = int24Sample;
        success = success && encoder.set_bits_per_sample(24);
    } else { //convert float to 16 bits
        context.format = int16Sample;
        success = success && encoder.set_bits_per_sample(16);
    }

    // Duplicate the flac command line compression levels
    if (levelPref < 0 || levelPref > 8) {
        levelPref = 5;
    }
    success = success
              && encoder.set_do_exhaustive_model_search(flacLevels[levelPref].do_exhaustive_model_search)
              && encoder.set_do_escape_coding(flacLevels[levelPref].do_escape_coding);

    if (numChannels != 2) {
        success = success
                  && encoder.set_do_mid_side_stereo(false)
                  && encoder.set_loose_mid_side_stereo(false);
    } else {
        success = success
                  && encoder.set_do_mid_side_stereo(flacLevels[levelPref].do_mid_side_stereo)
                  && encoder.set_loose_mid_side_stereo(flacLevels[levelPref].loose_mid_side_stereo);
    }

    success = success
              && encoder.set_qlp_coeff_precision(flacLevels[levelPref].qlp_coeff_precision)
              && encoder.set_min_residual_partition_order(flacLevels[levelPref].min_residual_partition_order)
              && encoder.set_max_residual_partition_order(flacLevels[levelPref].max_residual_partition_order)
              && encoder.set_rice_parameter_search_dist(flacLevels[levelPref].rice_parameter_search_dist)
              && encoder.set_max_lpc_order(flacLevels[levelPref].max_lpc_order);

    if (!success) {
        // TODO: more precise message
        throw ExportErrorException("FLAC:336");
    }

#ifdef LEGACY_FLAC
    encoder.init();
#else
    const auto path = fName.GetFullPath();
    if (!context.f.Open(path, wxT("w+b"))) {
        throw ExportException(XO("FLAC export couldn't open %s")
                              .Format(path)
                              .Translation());
    }

    // Even though there is an init() method that takes a filename, use the one that
    // takes a file handle because wxWidgets can open a file with a Unicode name and
    // libflac can't (under Windows).
    int status = encoder.init(context.f.fp());
    if (status != FLAC__STREAM_ENCODER_INIT_STATUS_OK) {
        throw ExportException(XO("FLAC encoder failed to initialize\nStatus: %d")
                              .Format(status)
                              .Translation());
    }
#endif

    metadata.reset();

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, numChannels, SAMPLES_PER_RUN, false,
        sampleRate, context.format, mixerSpec);

    context.status = selectionOnly
                     ? XO("Exporting the selected audio as FLAC")
                     : XO("Exporting the audio as FLAC");

    return true;
}

ExportResult FLACExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);

    auto exportResult = ExportResult::Success;

    auto cleanup2 = finally([&] {
        if (exportResult == ExportResult::Cancelled || exportResult == ExportResult::Error) {
#ifndef LEGACY_FLAC
            context.f.Detach(); // libflac closes the file
#endif
            context.encoder.finish();
        }
    });

    ArraysOf<FLAC__int32> tmpsmplbuf{ context.numChannels, SAMPLES_PER_RUN, true };

    while (exportResult == ExportResult::Success) {
        auto samplesThisRun = context.mixer->Process();
        if (samplesThisRun == 0) { //stop encoding
            break;
        }

        for (size_t i = 0; i < context.numChannels; i++) {
            auto mixed = context.mixer->GetBuffer(i);
            if (context.format == int24Sample) {
                for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                    tmpsmplbuf[i][j] = ((const int*)mixed)[j];
                }
            } else {
                for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                    tmpsmplbuf[i][j] = ((const short*)mixed)[j];
                }
            }
        }
        if (!context.encoder.process(
                reinterpret_cast<FLAC__int32**>(tmpsmplbuf.get()),
                samplesThisRun)) {
            // TODO: more precise message
            throw ExportDiskFullError(context.fName);
        }
        exportResult = ExportPluginHelpers::UpdateProgress(
            delegate, *context.mixer, context.t0, context.t1);
    }

    if (exportResult != ExportResult::Cancelled && exportResult != ExportResult::Error) {
#ifndef LEGACY_FLAC
        context.f.Detach(); // libflac closes the file
#endif
        if (!context.encoder.finish()) {
            return ExportResult::Error;
        }
#ifdef LEGACY_FLAC
        if (!context.f.Flush() || !context.f.Close()) {
            return ExportResult::Error;
        }
#endif
    }
    return exportResult;
}

// LL:  There's a bug in libflac++ 1.1.2 that prevents us from using
//      FLAC::Metadata::VorbisComment directly.  The set_metadata()
//      function allocates an array on the stack, but the base library
//      expects that array to be valid until the stream is initialized.
//
//      This has been fixed in 1.1.4.
FLAC__StreamMetadataHandle FLACExportProcessor::MakeMetadata(AudacityProject* project, const Tags* tags) const
{
    // Retrieve tags if needed
    if (tags == NULL) {
        tags = &Tags::Get(*project);
    }

    auto metadata = FLAC__StreamMetadataHandle(
        ::FLAC__metadata_object_new(FLAC__METADATA_TYPE_VORBIS_COMMENT)
        );

    wxString n;
    for (const auto& pair : tags->GetRange()) {
        n = pair.first;
        const auto& v = pair.second;
        if (n == TAG_YEAR) {
            n = wxT("DATE");
        } else if (n == TAG_COMMENTS) {
            // Some apps like Foobar use COMMENT and some like Windows use DESCRIPTION,
            // so add both to try and make everyone happy.
            n = wxT("COMMENT");
            FLAC::Metadata::VorbisComment::Entry entry(n.mb_str(wxConvUTF8),
                                                       v.mb_str(wxConvUTF8));
            if (!::FLAC__metadata_object_vorbiscomment_append_comment(metadata.get(),
                                                                      entry.get_entry(),
                                                                      true)) {
                return {};
            }
            n = wxT("DESCRIPTION");
        }
        FLAC::Metadata::VorbisComment::Entry entry(n.mb_str(wxConvUTF8),
                                                   v.mb_str(wxConvUTF8));
        if (!::FLAC__metadata_object_vorbiscomment_append_comment(metadata.get(),
                                                                  entry.get_entry(),
                                                                  true)) {
            return {};
        }
    }

    return metadata;
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "FLAC",
                                                                 []{ return std::make_unique< ExportFLAC >(); }
};
