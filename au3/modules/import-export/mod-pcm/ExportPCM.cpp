/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>

#include <wx/app.h>
#include <wx/dynlib.h>
#include <wx/filename.h>

#include <sndfile.h>

#include "Dither.h"
#include "FileFormats.h"
#include "Mix.h"
#include "Prefs.h"
#include "Tags.h"
#include "Track.h"
#include "wxFileNameWrapper.h"

#include "Export.h"
#include "ExportOptionsEditor.h"

#include "ExportPluginHelpers.h"
#include "ExportPluginRegistry.h"

#ifdef USE_LIBID3TAG
   #include <id3tag.h>
// DM: the following functions were supposed to have been
// included in id3tag.h - should be fixed in the next release
// of mad.
extern "C" {
struct id3_frame* id3_frame_new(char const*);
id3_length_t id3_latin1_length(id3_latin1_t const*);
void id3_latin1_decode(id3_latin1_t const*, id3_ucs4_t*);
}
#endif

namespace {
struct
{
    int format;
    const wxChar* name;
    const TranslatableString desc;
}
const kFormats[] =
{
#if defined(__WXMAC__)
    {
        SF_FORMAT_AIFF | SF_FORMAT_PCM_16, wxT("AIFF"),   XO("AIFF (Apple/SGI)")
    },
#endif
    {
        SF_FORMAT_WAV | SF_FORMAT_PCM_16,  wxT("WAV"),    XO("WAV (Microsoft)")
    },
};

enum
{
#if defined(__WXMAC__)
    FMT_AIFF,
#endif
    FMT_WAV,
    FMT_OTHER
};

int LoadOtherFormat(const audacity::BasicSettings& config, int def)
{
    return config.Read(wxString("/FileFormats/ExportFormat_SF1"), def);
}

void SaveOtherFormat(audacity::BasicSettings& config, int val)
{
    config.Write(wxT("/FileFormats/ExportFormat_SF1"), val);
}

int LoadEncoding(const audacity::BasicSettings& config, int type, int def)
{
    return config.Read(wxString::Format(wxT("/FileFormats/ExportFormat_SF1_Type/%s_%x"),
                                        sf_header_shortname(type), type), def);
}

void SaveEncoding(audacity::BasicSettings& config, int type, int val)
{
    config.Write(wxString::Format(wxT("/FileFormats/ExportFormat_SF1_Type/%s_%x"),
                                  sf_header_shortname(type), type), val);
}

void GetEncodings(int type, std::vector<ExportValue>& values, TranslatableStrings& names)
{
    // Setup for queries
    SF_INFO info = {};
    info.samplerate = 44100;
    info.channels = 1;
    info.sections = 1;

    for (int i = 0, num = sf_num_encodings(); i < num; ++i) {
        int sub = sf_encoding_index_to_subtype(i);

        // Since we're traversing the subtypes linearly, we have to
        // make sure it can be paired with our current type.
        info.format = type | sub;
        if (sf_format_check(&info)) {
            // Store subtype and name
            values.emplace_back(sub);
            names.push_back(Verbatim(sf_encoding_index_name(i)));
        }
    }
}

enum : int
{
    OptionIDSFType = 0
};

class ExportOptionsSFTypedEditor final : public ExportOptionsEditor
{
    const int mType;
    ExportOption mEncodingOption;
    int mEncoding;
public:

    explicit ExportOptionsSFTypedEditor(int type)
        : mType(type)
    {
        GetEncodings(type, mEncodingOption.values, mEncodingOption.names);

        mEncodingOption.id = type;
        mEncodingOption.title = XO("Encoding");
        mEncodingOption.flags = ExportOption::TypeEnum;
        mEncodingOption.defaultValue = mEncodingOption.values[0];

        mEncoding = *std::get_if<int>(&mEncodingOption.defaultValue);
    }

    int GetOptionsCount() const override
    {
        return 1;
    }

    bool GetOption(int, ExportOption& option) const override
    {
        option = mEncodingOption;
        return true;
    }

    bool GetValue(ExportOptionID, ExportValue& value) const override
    {
        value = mEncoding;
        return true;
    }

    bool SetValue(ExportOptionID, const ExportValue& value) override
    {
        if (std::find(mEncodingOption.values.begin(),
                      mEncodingOption.values.end(), value) != mEncodingOption.values.end()) {
            mEncoding = *std::get_if<int>(&value);
            return true;
        }
        return false;
    }

    SampleRateList GetSampleRateList() const override
    {
        return {};
    }

    void Load(const audacity::BasicSettings& config) override
    {
        mEncoding = LoadEncoding(config, mType, mEncoding);
    }

    void Store(audacity::BasicSettings& config) const override
    {
        SaveEncoding(config, mType, mEncoding);
    }
};

class ExportOptionsSFEditor final : public ExportOptionsEditor
{
    Listener* const mListener;
    int mType;
    std::unordered_map<int, int> mEncodings;

    std::vector<ExportOption> mOptions;

    bool IsValidType(const ExportValue& typeValue) const
    {
        if (std::holds_alternative<int>(typeValue)) {
            const auto& typeOption = mOptions.front();
            return std::find(typeOption.values.begin(),
                             typeOption.values.end(),
                             typeValue) != typeOption.values.end();
        }
        return false;
    }

public:

    explicit ExportOptionsSFEditor(Listener* listener)
        : mListener(listener)
    {
        ExportOption typeOption {
            OptionIDSFType, XO("Header"),
            0,
            ExportOption::TypeEnum
        };

        auto hasDefaultType = false;
        for (int i = 0, num = sf_num_headers(); i < num; ++i) {
            const auto type = static_cast<int>(sf_header_index_to_type(i));
            switch (type) {
            // On the Mac, do not include in header list
#if defined(__WXMAC__)
            case SF_FORMAT_AIFF: break;
#endif
            // Do not include in header list
            case SF_FORMAT_WAV: break;
            default:
            {
                typeOption.values.emplace_back(type);
                typeOption.names.push_back(Verbatim(sf_header_index_name(i)));
                ExportOption encodingOption {
                    type,
                    XO("Encoding"),
                    0,
                    ExportOption::TypeEnum
                };
                GetEncodings(type, encodingOption.values, encodingOption.names);
                encodingOption.defaultValue = encodingOption.values[0];
                if (!hasDefaultType) {
                    mType = type;
                    typeOption.defaultValue = type;
                    hasDefaultType = true;
                } else {
                    encodingOption.flags |= ExportOption::Hidden;
                }
                mOptions.push_back(std::move(encodingOption));
                mEncodings[type] = *std::get_if<int>(&encodingOption.defaultValue);
            } break;
            }
        }
        typeOption.defaultValue = typeOption.values[0];
        mOptions.insert(mOptions.begin(), std::move(typeOption));
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(mOptions.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index >= 0 && index < static_cast<int>(mOptions.size())) {
            option = mOptions[index];
            return true;
        }
        return false;
    }

    bool GetValue(ExportOptionID id, ExportValue& value) const override
    {
        if (id == OptionIDSFType) {
            value = mType;
            return true;
        }
        auto it = mEncodings.find(id);
        if (it != mEncodings.end()) {
            value = it->second;
            return true;
        }
        return false;
    }

    bool SetValue(ExportOptionID id, const ExportValue& value) override
    {
        if (id == OptionIDSFType && IsValidType(value)) {
            const auto newType = *std::get_if<int>(&value);
            if (newType == mType) {
                return true;
            }

            if (mListener) {
                mListener->OnExportOptionChangeBegin();
            }

            for (auto& option : mOptions) {
                if (option.id == mType) {
                    option.flags |= ExportOption::Hidden;
                    if (mListener) {
                        mListener->OnExportOptionChange(option);
                    }
                } else if (option.id == newType) {
                    option.flags &= ~ExportOption::Hidden;
                    if (mListener) {
                        mListener->OnExportOptionChange(option);
                    }
                }
            }
            mType = newType;
            Store(*gPrefs);
            if (mListener) {
                mListener->OnExportOptionChangeEnd();
                mListener->OnFormatInfoChange();
            }
            return true;
        }

        auto it = mEncodings.find(id);
        if (it != mEncodings.end() && std::holds_alternative<int>(value)) {
            it->second = *std::get_if<int>(&value);
            Store(*gPrefs);
            return true;
        }
        return false;
    }

    SampleRateList GetSampleRateList() const override
    {
        return {};
    }

    void Load(const audacity::BasicSettings& config) override
    {
        mType = LoadOtherFormat(config, mType);
        for (auto& p : mEncodings) {
            p.second = LoadEncoding(config, p.first, p.second);
        }

        // Prior to v2.4.0, sf_format will include the subtype.
        if (mType & SF_FORMAT_SUBMASK) {
            const auto type = mType & SF_FORMAT_TYPEMASK;
            const auto enc = mType & SF_FORMAT_SUBMASK;
            mEncodings[type] = enc;
            mType = type;
        }

        for (auto& option : mOptions) {
            const auto it = mEncodings.find(option.id);
            if (it == mEncodings.end()) {
                continue;
            }

            if (mType == it->first) {
                option.flags &= ~ExportOption::Hidden;
            } else {
                option.flags |= ExportOption::Hidden;
            }
        }
    }

    void Store(audacity::BasicSettings& config) const override
    {
        SaveOtherFormat(config, mType);
        for (auto& [type, encoding] : mEncodings) {
            SaveEncoding(config, type, encoding);
        }
    }
};
}

class PCMExportProcessor final : public ExportProcessor
{
    constexpr static size_t maxBlockLen = 44100 * 5;

    struct
    {
        int subformat;
        double t0;
        double t1;
        std::unique_ptr<Mixer> mixer;
        TranslatableString status;
        SF_INFO info;
        sampleFormat format;
        wxFile f;
        SNDFILE* sf;
        int sf_format;
        wxFileNameWrapper fName;
        int fileFormat;
        std::unique_ptr<Tags> metadata;
    } context;

public:

    PCMExportProcessor(int subformat)
    {
        context.sf = nullptr;
        context.subformat = subformat;
    }

    ~PCMExportProcessor() override
    {
        if (context.f.IsOpened()) {
            if (context.sf != nullptr) {
                sf_close(context.sf);
            }
            context.f.Close();
        }
    }

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    static ArrayOf<char> AdjustString(const wxString& wxStr, int sf_format);
    static void AddStrings(SNDFILE* sf, const Tags* tags, int sf_format);
    static bool AddID3Chunk(
        const wxFileNameWrapper& fName, const Tags* tags, int sf_format);
};

class ExportPCM final : public ExportPlugin
{
public:

    ExportPCM();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int index) const override;

    std::vector<std::string> GetMimeTypes(int formatIndex) const override;

    bool ParseConfig(int formatIndex, const rapidjson::Value&, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    /**
     *
     * @param format Control whether we are doing a "preset" export to a popular
     * file type, or giving the user full control over libsndfile.
     */
    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

ExportPCM::ExportPCM() = default;

int ExportPCM::GetFormatCount() const
{
    return WXSIZEOF(kFormats) + 1;// + FMT_OTHER
}

FormatInfo ExportPCM::GetFormatInfo(int index) const
{
    if (index == FMT_OTHER) {
        SF_INFO si = {};
        //VS: returned format info depends on the format that was used last time.
        //That could be a source of unexpected behavior
        si.format = LoadOtherFormat(*gPrefs, kFormats[0].format & SF_FORMAT_TYPEMASK);
        si.format |= LoadEncoding(*gPrefs, si.format, kFormats[0].format);

        for (si.channels = 1; sf_format_check(&si); si.channels++) {
            // just counting
        }
        --si.channels;

        return {
            sf_header_shortname(si.format),
            XO("Other uncompressed files"),
            { sf_header_extension(si.format) },
            static_cast<unsigned>(si.channels),
            true
        };
    }

    if (!(index >= 0 && index < FMT_OTHER)) {
        index = 0;
    }

    return {
        kFormats[index].name,
        kFormats[index].desc,
        { sf_header_extension(kFormats[index].format) },
        255,
        true
    };
}

std::vector<std::string> ExportPCM::GetMimeTypes(int formatIndex) const
{
    if (formatIndex == FMT_WAV) {
        return { "audio/x-wav" }
    }
    return {};
}

bool ExportPCM::ParseConfig(int formatIndex, const rapidjson::Value&, ExportProcessor::Parameters& parameters) const
{
    if (formatIndex == FMT_WAV) {
        //no parameters available...
        parameters = {};
        return true;
    }
    return false;
}

std::unique_ptr<ExportOptionsEditor>
ExportPCM::CreateOptionsEditor(int format, ExportOptionsEditor::Listener* listener) const
{
    if (format < FMT_OTHER) {
        return std::make_unique<ExportOptionsSFTypedEditor>(
            kFormats[format].format & SF_FORMAT_TYPEMASK);
    }
    return std::make_unique<ExportOptionsSFEditor>(listener);
}

std::unique_ptr<ExportProcessor> ExportPCM::CreateProcessor(int format) const
{
    return std::make_unique<PCMExportProcessor>(format);
}

bool PCMExportProcessor::Initialize(AudacityProject& project,
                                    const Parameters& parameters,
                                    const wxFileNameWrapper& fName,
                                    double t0, double t1, bool selectionOnly,
                                    double sampleRate, unsigned numChannels,
                                    MixerOptions::Downmix* mixerSpec,
                                    const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.fName = fName;

    // Set a default in case the settings aren't found
    int& sf_format = context.sf_format;

    switch (context.subformat) {
#if defined(__WXMAC__)
    case FMT_AIFF:
        sf_format = SF_FORMAT_AIFF;
        break;
#endif

    case FMT_WAV:
        sf_format = SF_FORMAT_WAV;
        break;

    default:
        // Retrieve the current format.
        sf_format = ExportPluginHelpers::GetParameterValue(parameters, OptionIDSFType, 0);
        break;
    }

    sf_format |= ExportPluginHelpers::GetParameterValue(parameters, sf_format, 0);

    // If subtype is still not specified, supply a default.
    if (!(sf_format & SF_FORMAT_SUBMASK)) {
        sf_format |= SF_FORMAT_PCM_16;
    }

    int& fileFormat = context.fileFormat;
    fileFormat = sf_format & SF_FORMAT_TYPEMASK;

    {
        wxFile& f = context.f;
        SNDFILE*& sf = context.sf;

        wxString formatStr;
        SF_INFO& info = context.info;
        //int          err;

        //This whole operation should not occur while a file is being loaded on OD,
        //(we are worried about reading from a file being written to,) so we block.
        //Furthermore, we need to do this because libsndfile is not threadsafe.
        formatStr = SFCall<wxString>(sf_header_name, fileFormat);

        // Use libsndfile to export file

        info.samplerate = (unsigned int)(sampleRate + 0.5);
        info.frames = (unsigned int)((t1 - t0) * sampleRate + 0.5);
        info.channels = numChannels;
        info.format = sf_format;
        info.sections = 1;
        info.seekable = 0;

        // Bug 46.  Trap here, as sndfile.c does not trap it properly.
        if ((numChannels != 1) && ((sf_format & SF_FORMAT_SUBMASK) == SF_FORMAT_GSM610)) {
            throw ExportException(_("GSM 6.10 requires mono"));
        }

        if (sf_format == SF_FORMAT_WAVEX + SF_FORMAT_GSM610) {
            throw ExportException(_("WAVEX and GSM 6.10 formats are not compatible"));
        }

        // If we can't export exactly the format they requested,
        // try the default format for that header type...
        //
        // LLL: I don't think this is valid since libsndfile checks
        // for all allowed subtypes explicitly and doesn't provide
        // for an unspecified subtype.
        if (!sf_format_check(&info)) {
            info.format = (info.format & SF_FORMAT_TYPEMASK);
        }
        if (!sf_format_check(&info)) {
            throw ExportException(_("Cannot export audio in this format."));
        }
        const auto path = fName.GetFullPath();
        if (f.Open(path, wxFile::write)) {
            // Even though there is an sf_open() that takes a filename, use the one that
            // takes a file descriptor since wxWidgets can open a file with a Unicode name and
            // libsndfile can't (under Windows).
            sf = sf_open_fd(f.fd(), SFM_WRITE, &info, FALSE);
            //add clipping for integer formats.  We allow floats to clip.
            sf_command(sf, SFC_SET_CLIPPING, NULL, sf_subtype_is_integer(sf_format) ? SF_TRUE : SF_FALSE);
        }

        if (!sf) {
            throw ExportException(_("Cannot export audio to %s").Format(path));
        }
        // Retrieve tags if not given a set
        if (metadata == NULL) {
            metadata = &Tags::Get(project);
        }

        // Install the meta data at the beginning of the file (except for
        // WAV and WAVEX formats)
        if (fileFormat != SF_FORMAT_WAV
            && fileFormat != SF_FORMAT_WAVEX) {
            AddStrings(sf, metadata, sf_format);
        }
        context.metadata = std::make_unique<Tags>(*metadata);

        if (sf_subtype_more_than_16_bits(info.format)) {
            context.format = floatSample;
        } else {
            context.format = int16Sample;
        }

        // Bug 2200
        // Only trap size limit for file types we know have an upper size limit.
        // The error message mentions aiff and wav.
        if ((fileFormat == SF_FORMAT_WAV)
            || (fileFormat == SF_FORMAT_WAVEX)
            || (fileFormat == SF_FORMAT_AIFF)) {
            float sampleCount = (float)(t1 - t0) * sampleRate * info.channels;
            float byteCount = sampleCount * sf_subtype_bytes_per_sample(info.format);
            // Test for 4 Gibibytes, rather than 4 Gigabytes
            if (byteCount > 4.295e9) {
                //Temporary translation hack, to say 'WAV or AIFF' rather than 'WAV'
                const auto message
                    =XO("You have attempted to Export a WAV or AIFF file which would be greater than 4GB.\n"
                        "Audacity cannot do this, the Export was abandoned.");
                throw ExportErrorException(message,
                                           wxT("Size_limits_for_WAV_and_AIFF_files"));
            }
        }

        context.status = (selectionOnly
                          ? XO("Exporting the selected audio as %s")
                          : XO("Exporting the audio as %s")).Format(formatStr);

        wxASSERT(info.channels >= 0);
        context.mixer = ExportPluginHelpers::CreateMixer(
            project, selectionOnly, t0, t1, info.channels, maxBlockLen, true,
            sampleRate, context.format, mixerSpec);
    }

    return true;
}

ExportResult PCMExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);

    auto exportResult = ExportResult::Success;

    {
        std::vector<char> dither;
        if ((context.info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24) {
            dither.reserve(maxBlockLen * context.info.channels * SAMPLE_SIZE(int24Sample));
        }

        while (exportResult == ExportResult::Success) {
            sf_count_t samplesWritten;
            size_t numSamples = context.mixer->Process();
            if (numSamples == 0) {
                break;
            }

            auto mixed = context.mixer->GetBuffer();

            // Bug 1572: Not ideal, but it does add the desired dither
            if ((context.info.format & SF_FORMAT_SUBMASK) == SF_FORMAT_PCM_24) {
                for (int c = 0; c < context.info.channels; ++c) {
                    CopySamples(
                        mixed + (c * SAMPLE_SIZE(context.format)), context.format,
                        dither.data() + (c * SAMPLE_SIZE(int24Sample)), int24Sample,
                        numSamples, gHighQualityDither, context.info.channels, context.info.channels
                        );
                    // Copy back without dither
                    CopySamples(
                        dither.data() + (c * SAMPLE_SIZE(int24Sample)), int24Sample,
                        const_cast<samplePtr>(mixed) // PRL fix this!
                        + (c * SAMPLE_SIZE(context.format)), context.format,
                        numSamples, DitherType::none, context.info.channels, context.info.channels);
                }
            }

            if (context.format == int16Sample) {
                samplesWritten = SFCall<sf_count_t>(sf_writef_short, context.sf, (const short*)mixed, numSamples);
            } else {
                samplesWritten = SFCall<sf_count_t>(sf_writef_float, context.sf, (const float*)mixed, numSamples);
            }

            if (static_cast<size_t>(samplesWritten) != numSamples) {
                char buffer2[1000];
                sf_error_str(context.sf, buffer2, 1000);
                //Used to give this error message
#if 0
                AudacityMessageBox(
                    XO(
                        /* i18n-hint: %s will be the error message from libsndfile, which
                         * is usually something unhelpful (and untranslated) like "system
                         * error" */
                        "Error while writing %s file (disk full?).\nLibsndfile says \"%s\"")
                    .Format(formatStr, wxString::FromAscii(buffer2)));
#else
                // But better to give the same error message as for
                // other cases of disk exhaustion.
                // The thrown exception doesn't escape but GuardedCall
                // will enqueue a message.
                GuardedCall([&]{
                    throw FileException {
                        FileException::Cause::Write, context.fName };
                });
#endif
                exportResult = ExportResult::Error;
                break;
            }
            if (exportResult == ExportResult::Success) {
                exportResult = ExportPluginHelpers::UpdateProgress(
                    delegate, *context.mixer, context.t0, context.t1);
            }
        }
    }

    // Install the WAV metata in a "LIST" chunk at the end of the file
    if (exportResult != ExportResult::Cancelled && exportResult != ExportResult::Error) {
        if (context.fileFormat == SF_FORMAT_WAV
            || context.fileFormat == SF_FORMAT_WAVEX) {
            AddStrings(context.sf, context.metadata.get(), context.sf_format);
        }
    }

    if (0 != sf_close(context.sf)) {
        // TODO: more precise message
        throw ExportErrorException("PCM:681");
    }

    context.sf = nullptr;
    context.f.Close();

    if (exportResult != ExportResult::Cancelled && exportResult != ExportResult::Error) {
        if ((context.fileFormat == SF_FORMAT_AIFF)
            || (context.fileFormat == SF_FORMAT_WAV)) {
            // Note: file has closed, and gets reopened and closed again here:
            if (!AddID3Chunk(context.fName, context.metadata.get(), context.sf_format)) {
                // TODO: more precise message
                throw ExportErrorException("PCM:694");
            }
        }
    }
    return exportResult;
}

ArrayOf<char> PCMExportProcessor::AdjustString(const wxString& wxStr, int sf_format)
{
    bool b_aiff = false;
    if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) {
        b_aiff = true;     // Apple AIFF file
    }
    // We must convert the string to 7 bit ASCII
    size_t sz = wxStr.length();
    if (sz == 0) {
        return {}
    }
    // Size for secure allocation in case of local wide char usage
    size_t sr = (sz + 4) * 2;

    ArrayOf<char> pDest{ sr, true };
    if (!pDest) {
        return {}
    }
    ArrayOf<char> pSrc{ sr, true };
    if (!pSrc) {
        return {}
    }

    if (wxStr.mb_str(wxConvISO8859_1)) {
        strncpy(pSrc.get(), wxStr.mb_str(wxConvISO8859_1), sz);
    } else if (wxStr.mb_str()) {
        strncpy(pSrc.get(), wxStr.mb_str(), sz);
    } else {
        return {}
    }

    char* pD = pDest.get();
    char* pS = pSrc.get();
    unsigned char c;

    // ISO Latin to 7 bit ascii conversion table (best approximation)
    static char aASCII7Table[256] = {
        0x00, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
        0x5f, 0x09, 0x0a, 0x5f, 0x0d, 0x5f, 0x5f, 0x5f,
        0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
        0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f, 0x5f,
        0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
        0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
        0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
        0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
        0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
        0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
        0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
        0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
        0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
        0x45, 0x20, 0x2c, 0x53, 0x22, 0x2e, 0x2b, 0x2b,
        0x5e, 0x25, 0x53, 0x28, 0x4f, 0x20, 0x5a, 0x20,
        0x20, 0x27, 0x27, 0x22, 0x22, 0x2e, 0x2d, 0x5f,
        0x22, 0x54, 0x73, 0x29, 0x6f, 0x20, 0x7a, 0x59,
        0x20, 0x21, 0x63, 0x4c, 0x6f, 0x59, 0x7c, 0x53,
        0x22, 0x43, 0x61, 0x22, 0x5f, 0x2d, 0x43, 0x2d,
        0x6f, 0x7e, 0x32, 0x33, 0x27, 0x75, 0x50, 0x27,
        0x2c, 0x31, 0x6f, 0x22, 0x5f, 0x5f, 0x5f, 0x3f,
        0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x43,
        0x45, 0x45, 0x45, 0x45, 0x49, 0x49, 0x49, 0x49,
        0x44, 0x4e, 0x4f, 0x4f, 0x4f, 0x4f, 0x4f, 0x78,
        0x4f, 0x55, 0x55, 0x55, 0x55, 0x59, 0x70, 0x53,
        0x61, 0x61, 0x61, 0x61, 0x61, 0x61, 0x61, 0x63,
        0x65, 0x65, 0x65, 0x65, 0x69, 0x69, 0x69, 0x69,
        0x64, 0x6e, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x2f,
        0x6f, 0x75, 0x75, 0x75, 0x75, 0x79, 0x70, 0x79
    };

    size_t i;
    for (i = 0; i < sr; i++) {
        c = (unsigned char)*pS++;
        *pD++ = aASCII7Table[c];
        if (c == 0) {
            break;
        }
    }
    *pD = '\0';

    if (b_aiff) {
        int len = (int)strlen(pDest.get());
        if ((len % 2) != 0) {
            // In case of an odd length string, add a space char
            strcat(pDest.get(), " ");
        }
    }

    return pDest;
}

void PCMExportProcessor::AddStrings(SNDFILE* sf, const Tags* tags, int sf_format)
{
    if (tags->HasTag(TAG_TITLE)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_TITLE), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_TITLE, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_ALBUM)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_ALBUM), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_ALBUM, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_ARTIST)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_ARTIST), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_ARTIST, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_COMMENTS)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_COMMENTS), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_COMMENT, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_YEAR)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_YEAR), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_DATE, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_GENRE)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_GENRE), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_GENRE, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_COPYRIGHT)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_COPYRIGHT), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_COPYRIGHT, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_SOFTWARE)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_SOFTWARE), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_SOFTWARE, ascii7Str.get());
        }
    }

    if (tags->HasTag(TAG_TRACK)) {
        auto ascii7Str = AdjustString(tags->GetTag(TAG_TRACK), sf_format);
        if (ascii7Str) {
            sf_set_string(sf, SF_STR_TRACKNUMBER, ascii7Str.get());
        }
    }
}

#ifdef USE_LIBID3TAG
struct id3_tag_deleter {
    void operator ()(id3_tag* p) const
    {
        if (p) {
            id3_tag_delete(p);
        }
    }
};
using id3_tag_holder = std::unique_ptr<id3_tag, id3_tag_deleter>;
#endif

bool PCMExportProcessor::AddID3Chunk(
    const wxFileNameWrapper& fName, const Tags* tags, int sf_format)
{
#ifdef USE_LIBID3TAG
    id3_tag_holder tp { id3_tag_new() };

    for (const auto& pair : tags->GetRange()) {
        const auto& n = pair.first;
        const auto& v = pair.second;
        const char* name = "TXXX";

        if (n.CmpNoCase(TAG_TITLE) == 0) {
            name = ID3_FRAME_TITLE;
        } else if (n.CmpNoCase(TAG_ARTIST) == 0) {
            name = ID3_FRAME_ARTIST;
        } else if (n.CmpNoCase(TAG_ALBUM) == 0) {
            name = ID3_FRAME_ALBUM;
        } else if (n.CmpNoCase(TAG_YEAR) == 0) {
            name = ID3_FRAME_YEAR;
        } else if (n.CmpNoCase(TAG_GENRE) == 0) {
            name = ID3_FRAME_GENRE;
        } else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
            name = ID3_FRAME_COMMENT;
        } else if (n.CmpNoCase(TAG_TRACK) == 0) {
            name = ID3_FRAME_TRACK;
        } else if (n.CmpNoCase(wxT("composer")) == 0) {
            name = "TCOM";
        }

        struct id3_frame* frame = id3_frame_new(name);

        if (!n.IsAscii() || !v.IsAscii()) {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
        } else {
            id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
        }

        MallocString<id3_ucs4_t> ucs4{
            id3_utf8_ucs4duplicate((id3_utf8_t*)(const char*)v.mb_str(wxConvUTF8)) };

        if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
            // A hack to get around iTunes not recognizing the comment.  The
            // language defaults to XXX and, since it's not a valid language,
            // iTunes just ignores the tag.  So, either set it to a valid language
            // (which one???) or just clear it.  Unfortunately, there's no supported
            // way of clearing the field, so do it directly.
            id3_field* f = id3_frame_field(frame, 1);
            memset(f->immediate.value, 0, sizeof(f->immediate.value));
            id3_field_setfullstring(id3_frame_field(frame, 3), ucs4.get());
        } else if (strcmp(name, "TXXX") == 0) {
            id3_field_setstring(id3_frame_field(frame, 2), ucs4.get());

            ucs4.reset(id3_utf8_ucs4duplicate((id3_utf8_t*)(const char*)n.mb_str(wxConvUTF8)));

            id3_field_setstring(id3_frame_field(frame, 1), ucs4.get());
        } else {
            auto addr = ucs4.get();
            id3_field_setstrings(id3_frame_field(frame, 1), 1, &addr);
        }

        id3_tag_attachframe(tp.get(), frame);
    }

    tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

    // If this version of libid3tag supports it, use v2.3 ID3
    // tags instead of the newer, but less well supported, v2.4
    // that libid3tag uses by default.
#ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
    tp->options |= ID3_TAG_OPTION_ID3V2_3;
#endif

    id3_length_t len;

    len = id3_tag_render(tp.get(), 0);
    if (len == 0) {
        return true;
    }

    if ((len % 2) != 0) {
        len++;                  // Length must be even.
    }
    ArrayOf<id3_byte_t> buffer { len, true };
    if (buffer == NULL) {
        return false;
    }

    // Zero all locations, for ending odd UTF16 content
    // correctly, i.e., two '\0's at the end.

    id3_tag_render(tp.get(), buffer.get());

    wxFFile f(fName.GetFullPath(), wxT("r+b"));
    if (f.IsOpened()) {
        wxUint32 sz;

        sz = (wxUint32)len;
        if (!f.SeekEnd(0)) {
            return false;
        }
        if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV) {
            if (4 != f.Write("id3 ", 4)) {// Must be lower case for foobar2000.
                return false;
            }
        } else {
            if (4 != f.Write("ID3 ", 4)) {
                return false;
            }
            sz = wxUINT32_SWAP_ON_LE(sz);
        }
        if (4 != f.Write(&sz, 4)) {
            return false;
        }

        if (len != f.Write(buffer.get(), len)) {
            return false;
        }

        sz = (wxUint32)f.Tell() - 8;
        if ((sf_format & SF_FORMAT_TYPEMASK) == SF_FORMAT_AIFF) {
            sz = wxUINT32_SWAP_ON_LE(sz);
        }

        if (!f.Seek(4)) {
            return false;
        }
        if (4 != f.Write(&sz, 4)) {
            return false;
        }

        if (!f.Flush()) {
            return false;
        }

        if (!f.Close()) {
            return false;
        }
    } else {
        return false;
    }
#endif
    return true;
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "PCM",
                                                                 []{ return std::make_unique< ExportPCM >(); }
};
