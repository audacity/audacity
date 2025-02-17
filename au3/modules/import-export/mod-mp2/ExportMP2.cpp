/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP2.cpp

  Joshua Haberman
  Markus Meyer

  Copyright 2002, 2003 Joshua Haberman.
  Copyright 2006 Markus Meyer
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*******************************************************************//**

\class MP2Exporter
\brief Class used to export MP2 files

*/

#include <wx/defs.h>
#include <wx/dynlib.h>
#include <wx/log.h>
#include <wx/stream.h>

#include "Export.h"
#include "FileIO.h"
#include "Mix.h"
#include "Tags.h"
#include "Track.h"

#include "ExportPluginHelpers.h"
#include "PlainExportOptionsEditor.h"

#define LIBTWOLAME_STATIC
#include "ExportPluginRegistry.h"
#include "twolame.h"

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

//----------------------------------------------------------------------------
// ExportMP2Options
//----------------------------------------------------------------------------

namespace {
// i18n-hint kbps abbreviates "thousands of bits per second"
inline TranslatableString n_kbps(int n) { return XO("%d kbps").Format(n); }

const TranslatableStrings BitRateMPEG1Names {
    n_kbps(32),
    n_kbps(48),
    n_kbps(56),
    n_kbps(64),
    n_kbps(80),
    n_kbps(96),
    n_kbps(112),
    n_kbps(128),
    n_kbps(160),
    n_kbps(192),//default
    n_kbps(224),
    n_kbps(256),
    n_kbps(320),
    n_kbps(384),
};

const TranslatableStrings BitRateMPEG2Names {
    n_kbps(8),
    n_kbps(16),
    n_kbps(24),
    n_kbps(32),
    n_kbps(40),
    n_kbps(48),
    n_kbps(56),
    n_kbps(64),
    n_kbps(80),
    n_kbps(96),//default
    n_kbps(112),
    n_kbps(128),
    n_kbps(144),
    n_kbps(160)
};

enum : int {
    MP2OptionIDVersion = 0,
    MP2OptionIDBitRateMPEG1 = 1,
    MP2OptionIDBitRateMPEG2 = 2,
};

const std::initializer_list<ExportOption> MP2Options {
    {
        MP2OptionIDVersion, XO("Version"),
        1,
        ExportOption::TypeEnum,
        { 0, 1 },
        { XO("MPEG2"), XO("MPEG1") },
    },
    {
        MP2OptionIDBitRateMPEG1, XO("Bit Rate"),
        192,
        ExportOption::TypeEnum,
        { 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384 },
        BitRateMPEG1Names
    },
    {
        MP2OptionIDBitRateMPEG2, XO("Bit Rate"),
        96,
        ExportOption::TypeEnum | ExportOption::Hidden,
        { 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160 },
        BitRateMPEG2Names,
    }
};
}

class MP2ExportOptionsEditor final : public ExportOptionsEditor
{
    std::vector<ExportOption> mOptions { MP2Options };
    std::unordered_map<ExportOptionID, ExportValue> mValues;
    Listener* mListener{};
public:
    MP2ExportOptionsEditor(Listener* listener)
        : mListener(listener)
    {
        for (auto& option : mOptions) {
            mValues[option.id] = option.defaultValue;
        }
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(mOptions.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index >= 0 && index < mOptions.size()) {
            option = mOptions[index];
            return true;
        }
        return false;
    }

    bool GetValue(ExportOptionID id, ExportValue& value) const override
    {
        const auto it = mValues.find(id);
        if (it != mValues.end()) {
            value = it->second;
            return true;
        }
        return false;
    }

    bool SetValue(ExportOptionID id, const ExportValue& value) override
    {
        auto it = mValues.find(id);
        if (it == mValues.end() || it->second.index() != value.index()) {
            return false;
        }

        it->second = value;

        if (id == MP2OptionIDVersion) {
            OnVersionChanged();

            if (mListener != nullptr) {
                mListener->OnExportOptionChangeBegin();
                mListener->OnExportOptionChange(mOptions[MP2OptionIDBitRateMPEG1]);
                mListener->OnExportOptionChange(mOptions[MP2OptionIDBitRateMPEG2]);
                mListener->OnExportOptionChangeEnd();

                mListener->OnSampleRateListChange();
            }
        }
        return true;
    }

    SampleRateList GetSampleRateList() const override
    {
        auto it = mValues.find(MP2OptionIDVersion);
        if (*std::get_if<int>(&it->second) == TWOLAME_MPEG1) {
            return { 32000, 44100, 48000 }
        }
        return { 16000, 22050, 24000 };
    }

    void Store(audacity::BasicSettings& config) const override
    {
        auto it = mValues.find(MP2OptionIDVersion);
        config.Write(wxT("/FileFormats/MP2Version"), *std::get_if<int>(&it->second));
        it = mValues.find(MP2OptionIDBitRateMPEG1);
        config.Write(wxT("/FileFormats/MP2BitrateMPEG1"), *std::get_if<int>(&it->second));
        it = mValues.find(MP2OptionIDBitRateMPEG2);
        config.Write(wxT("/FileFormats/MP2BitrateMPEG2"), *std::get_if<int>(&it->second));
    }

    void Load(const audacity::BasicSettings& config) override
    {
        config.Read(wxT("/FileFormats/MP2Version"), std::get_if<int>(&mValues[MP2OptionIDVersion]));
        config.Read(wxT("/FileFormats/MP2BitrateMPEG1"), std::get_if<int>(&mValues[MP2OptionIDBitRateMPEG1]));
        config.Read(wxT("/FileFormats/MP2BitrateMPEG2"), std::get_if<int>(&mValues[MP2OptionIDBitRateMPEG2]));
        OnVersionChanged();
    }

    void OnVersionChanged()
    {
        if (*std::get_if<int>(&mValues[MP2OptionIDVersion]) == TWOLAME_MPEG1) {
            mOptions[MP2OptionIDBitRateMPEG2].flags |= ExportOption::Hidden;
            mOptions[MP2OptionIDBitRateMPEG1].flags &= ~ExportOption::Hidden;
        } else {
            mOptions[MP2OptionIDBitRateMPEG2].flags &= ~ExportOption::Hidden;
            mOptions[MP2OptionIDBitRateMPEG1].flags |= ExportOption::Hidden;
        }
    }
};

class MP2ExportProcessor final : public ExportProcessor
{
    // Values taken from the twolame simple encoder sample
    constexpr static size_t pcmBufferSize = 9216 / 2; // number of samples
    constexpr static size_t mp2BufferSize = 16384u; // bytes

    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        wxFileNameWrapper fName;
        std::unique_ptr<Mixer> mixer;
        ArrayOf<char> id3buffer;
        int id3len;
        twolame_options* encodeOptions{};
        std::unique_ptr<FileIO> outFile;
    } context;

public:

    ~MP2ExportProcessor() override;

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:
    static int AddTags(ArrayOf<char>& buffer, bool* endOfFile, const Tags* tags);
#ifdef USE_LIBID3TAG
    static void AddFrame(struct id3_tag* tp, const wxString& n, const wxString& v, const char* name);
#endif
};

class ExportMP2 final : public ExportPlugin
{
public:

    ExportMP2();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int) const override;
};

ExportMP2::ExportMP2() = default;

int ExportMP2::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportMP2::GetFormatInfo(int) const
{
    return {
        wxT("MP2"), XO("MP2 Files"), { wxT("mp2") }, 2, true
    };
}

std::unique_ptr<ExportOptionsEditor>
ExportMP2::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<MP2ExportOptionsEditor>(listener);
}

std::unique_ptr<ExportProcessor> ExportMP2::CreateProcessor(int) const
{
    return std::make_unique<MP2ExportProcessor>();
}

MP2ExportProcessor::~MP2ExportProcessor()
{
    if (context.encodeOptions) {
        twolame_close(&context.encodeOptions);
    }
}

bool MP2ExportProcessor::Initialize(AudacityProject& project,
                                    const Parameters& parameters,
                                    const wxFileNameWrapper& fName,
                                    double t0, double t1, bool selectionOnly,
                                    double sampleRate, unsigned channels,
                                    MixerOptions::Downmix* mixerSpec,
                                    const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.fName = fName;

    bool stereo = (channels == 2);
    const auto version = static_cast<TWOLAME_MPEG_version>(
        ExportPluginHelpers::GetParameterValue(parameters,
                                               MP2OptionIDVersion, 1));

    const auto bitrate = version == TWOLAME_MPEG1
                         ? ExportPluginHelpers::GetParameterValue(
        parameters,
        MP2OptionIDBitRateMPEG1, 192)
                         : ExportPluginHelpers::GetParameterValue(
        parameters,
        MP2OptionIDBitRateMPEG2, 96);

    wxLogNull logNo;            /* temporarily disable wxWidgets error messages */

    twolame_options*& encodeOptions = context.encodeOptions;
    encodeOptions = twolame_init();

    twolame_set_version(encodeOptions, version);
    twolame_set_in_samplerate(encodeOptions, static_cast<int>(sampleRate));
    twolame_set_out_samplerate(encodeOptions, static_cast<int>(sampleRate));
    twolame_set_bitrate(encodeOptions, bitrate);
    twolame_set_num_channels(encodeOptions, stereo ? 2 : 1);

    if (twolame_init_params(encodeOptions) != 0) {
        throw ExportException(_("Cannot export MP2 with this sample rate and bit rate"));
    }

    // Put ID3 tags at beginning of file
    if (metadata == NULL) {
        metadata = &Tags::Get(project);
    }

    context.outFile = std::make_unique<FileIO>(fName, FileIO::Output);
    if (!context.outFile->IsOpened()) {
        throw ExportException(_("Unable to open target file for writing"));
    }

    bool endOfFile;
    context.id3len = AddTags(context.id3buffer, &endOfFile, metadata);
    if (context.id3len && !endOfFile) {
        if (context.outFile->Write(context.id3buffer.get(), context.id3len).GetLastError()) {
            // TODO: more precise message
            throw ExportErrorException("MP2:292");
        }
        context.id3len = 0;
        context.id3buffer.reset();
    }

    context.status = selectionOnly
                     ? XO("Exporting selected audio at %ld kbps")
                     .Format(bitrate)
                     : XO("Exporting the audio at %ld kbps")
                     .Format(bitrate);

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, stereo ? 2 : 1, pcmBufferSize, true,
        sampleRate, int16Sample, mixerSpec);

    return true;
}

ExportResult MP2ExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    // We allocate a buffer which is twice as big as the
    // input buffer, which should always be enough.
    // We have to multiply by 4 because one sample is 2 bytes wide!
    ArrayOf<unsigned char> mp2Buffer{ mp2BufferSize };

    auto exportResult = ExportResult::Success;

    {
        while (exportResult == ExportResult::Success) {
            auto pcmNumSamples = context.mixer->Process();
            if (pcmNumSamples == 0) {
                break;
            }

            short* pcmBuffer = (short*)context.mixer->GetBuffer();

            int mp2BufferNumBytes = twolame_encode_buffer_interleaved(
                context.encodeOptions,
                pcmBuffer,
                pcmNumSamples,
                mp2Buffer.get(),
                mp2BufferSize);

            if (mp2BufferNumBytes < 0) {
                // TODO: more precise message
                throw ExportErrorException("MP2:339");
            }

            if (context.outFile->Write(mp2Buffer.get(), mp2BufferNumBytes).GetLastError()) {
                // TODO: more precise message
                throw ExportDiskFullError(context.fName);
            }
            exportResult = ExportPluginHelpers::UpdateProgress(
                delegate, *context.mixer, context.t0, context.t1);
        }
    }

    int mp2BufferNumBytes = twolame_encode_flush(
        context.encodeOptions,
        mp2Buffer.get(),
        mp2BufferSize);

    if (mp2BufferNumBytes > 0) {
        if (context.outFile->Write(mp2Buffer.get(), mp2BufferNumBytes).GetLastError()) {
            // TODO: more precise message
            throw ExportErrorException("MP2:362");
        }
    }

    /* Write ID3 tag if it was supposed to be at the end of the file */

    if (context.id3len) {
        if (context.outFile->Write(context.id3buffer.get(), context.id3len).GetLastError()) {
            // TODO: more precise message
            throw ExportErrorException("MP2:371");
        }
    }

    if (!context.outFile->Close()) {
        // TODO: more precise message
        throw ExportErrorException("MP2:377");
    }
    return exportResult;
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

// returns buffer len; caller frees
int MP2ExportProcessor::AddTags(ArrayOf< char >& buffer,
                                bool* endOfFile, const Tags* tags)
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
            // LLL:  Some apps do not like the newer frame ID (ID3_FRAME_YEAR),
            //       so we add old one as well.
            AddFrame(tp.get(), n, v, "TYER");
            name = ID3_FRAME_YEAR;
        } else if (n.CmpNoCase(TAG_GENRE) == 0) {
            name = ID3_FRAME_GENRE;
        } else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
            name = ID3_FRAME_COMMENT;
        } else if (n.CmpNoCase(TAG_TRACK) == 0) {
            name = ID3_FRAME_TRACK;
        }

        AddFrame(tp.get(), n, v, name);
    }

    tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

    // If this version of libid3tag supports it, use v2.3 ID3
    // tags instead of the newer, but less well supported, v2.4
    // that libid3tag uses by default.
   #ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
    tp->options |= ID3_TAG_OPTION_ID3V2_3;
   #endif

    *endOfFile = false;

    id3_length_t len;

    len = id3_tag_render(tp.get(), 0);
    buffer.reinit(len);
    len = id3_tag_render(tp.get(), (id3_byte_t*)buffer.get());

    return len;
#else //ifdef USE_LIBID3TAG
    return 0;
#endif
}

#ifdef USE_LIBID3TAG
void MP2ExportProcessor::AddFrame(struct id3_tag* tp, const wxString& n, const wxString& v, const char* name)
{
    struct id3_frame* frame = id3_frame_new(name);

    if (!n.IsAscii() || !v.IsAscii()) {
        id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
    } else {
        id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
    }

    MallocString<id3_ucs4_t> ucs4 {
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

    id3_tag_attachframe(tp, frame);
}

#endif

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "MP2",
                                                                 []{ return std::make_unique< ExportMP2 >(); }
};
