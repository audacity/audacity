/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistent, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.


  Copyright 2002, 2003 Joshua Haberman.
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

\class MP3Exporter
\brief Class used to export MP3 files

*//********************************************************************/

#include "ExportMP3.h"

#include <wx/app.h>
#include <wx/defs.h>

#include <wx/dynlib.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/mimetype.h>

#include "FileNames.h"
#include "float_cast.h"
#include "Mix.h"
#include "Prefs.h"
#include "libraries/lib-tags/Tags.h"
#include "Track.h"
#include "wxFileNameWrapper.h"
#include "Project.h"

#include "libraries/lib-import-export/Export.h"
#include "BasicUI.h"

#include <lame/lame.h>

#ifdef USE_LIBID3TAG
#include <id3tag.h>
#endif

#include "libraries/lib-import-export/ExportOptionsEditor.h"
#include "libraries/lib-import-export/ExportPluginHelpers.h"
#include "libraries/lib-import-export/ExportPluginRegistry.h"

#if defined(__WXMSW__)
#include <winsock2.h>
#include <windows.h>
#endif

//----------------------------------------------------------------------------
// ExportMP3Options
//----------------------------------------------------------------------------

enum : int {
    QUALITY_2 = 2,

    //ROUTINE_FAST = 0,
    //ROUTINE_STANDARD = 1,

    PRESET_INSANE = 0,
    PRESET_EXTREME = 1,
    PRESET_STANDARD = 2,
    PRESET_MEDIUM = 3,
};

namespace {
/* i18n-hint: kbps is the bitrate of the MP3 file, kilobits per second*/
inline TranslatableString n_kbps(int n) { return XO("%d kbps").Format(n); }
}

static const TranslatableStrings fixRateNames {
    n_kbps(320),
    n_kbps(256),
    n_kbps(224),
    n_kbps(192),
    n_kbps(160),
    n_kbps(144),
    n_kbps(128),
    n_kbps(112),
    n_kbps(96),
    n_kbps(80),
    n_kbps(64),
    n_kbps(56),
    n_kbps(48),
    n_kbps(40),
    n_kbps(32),
    n_kbps(24),
    n_kbps(16),
    n_kbps(8),
};

static const std::vector<ExportValue> fixRateValues {
    320,
    256,
    224,
    192,
    160,
    144,
    128,
    112,
    96,
    80,
    64,
    56,
    48,
    40,
    32,
    24,
    16,
    8,
};

static const TranslatableStrings varRateNames {
    XO("220-260 kbps (Best Quality)"),
    XO("200-250 kbps"),
    XO("170-210 kbps"),
    XO("155-195 kbps"),
    XO("145-185 kbps"),
    XO("110-150 kbps"),
    XO("95-135 kbps"),
    XO("80-120 kbps"),
    XO("65-105 kbps"),
    XO("45-85 kbps (Smaller files)"),
};
/*
static const TranslatableStrings varModeNames {
   XO("Fast"),
   XO("Standard"),
};
*/
static const TranslatableStrings setRateNames {
    XO("Excessive, 320 kbps"),
    XO("Extreme, 220-260 kbps"),
    XO("Standard, 170-210 kbps"),
    XO("Medium, 145-185 kbps"),
};

static const TranslatableStrings setRateNamesShort {
    XO("Excessive"),
    XO("Extreme"),
    XO("Standard"),
    XO("Medium"),
};

static const std::vector< int > sampRates {
    8000,
    11025,
    12000,
    16000,
    22050,
    24000,
    32000,
    44100,
    48000,
};

enum MP3OptionID : int {
    MP3OptionIDMode = 0,
    MP3OptionIDQualitySET,
    MP3OptionIDQualityVBR,
    MP3OptionIDQualityABR,
    MP3OptionIDQualityCBR
};

//Option order should exactly match to the id values
const std::initializer_list<ExportOption> MP3Options {
    {
        MP3OptionIDMode, XO("Bit Rate Mode"),
        std::string("SET"),
        ExportOption::TypeEnum,
        {
            // for migrating old preferences the
            // order should be preserved
            std::string("SET"),
            std::string("VBR"),
            std::string("ABR"),
            std::string("CBR")
        },
        {
            XO("Preset"),
            XO("Variable"),
            XO("Average"),
            XO("Constant")
        }
    },
    {
        MP3OptionIDQualitySET, XO("Quality"),
        PRESET_STANDARD,
        ExportOption::TypeEnum,
        { 0, 1, 2, 3 },
        setRateNames
    },
    {
        MP3OptionIDQualityVBR, XO("Quality"),
        QUALITY_2,
        ExportOption::TypeEnum | ExportOption::Hidden,
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
        varRateNames
    },
    {
        MP3OptionIDQualityABR, XO("Quality"),
        192,
        ExportOption::TypeEnum | ExportOption::Hidden,
        fixRateValues,
        fixRateNames
    },
    {
        MP3OptionIDQualityCBR, XO("Quality"),
        192,
        ExportOption::TypeEnum | ExportOption::Hidden,
        fixRateValues,
        fixRateNames
    }
};

MP3ExportOptionsEditor::MP3ExportOptionsEditor(Listener* listener)
    : mOptions(MP3Options)
    , mListener(listener)
{
    mValues.reserve(mOptions.size());
    for (auto& option : mOptions) {
        mValues[option.id] = option.defaultValue;
    }
}

int MP3ExportOptionsEditor::GetOptionsCount() const
{
    return static_cast<int>(mOptions.size());
}

bool MP3ExportOptionsEditor::GetOption(int index, ExportOption& option) const
{
    if (index >= 0 && index < static_cast<int>(mOptions.size())) {
        option = mOptions[index];
        return true;
    }
    return false;
}

bool MP3ExportOptionsEditor::SetValue(int id, const ExportValue& value)
{
    const auto it = mValues.find(id);
    if (it == mValues.end()) {
        return false;
    }
    if (value.index() != it->second.index()) {
        return false;
    }

    it->second = value;

    switch (id) {
    case MP3OptionIDMode:
    {
        const auto mode = *std::get_if<std::string>(&value);
        OnModeChange(mode);
        if (mListener) {
            mListener->OnExportOptionChangeBegin();
            mListener->OnExportOptionChange(mOptions[MP3OptionIDQualitySET]);
            mListener->OnExportOptionChange(mOptions[MP3OptionIDQualityABR]);
            mListener->OnExportOptionChange(mOptions[MP3OptionIDQualityCBR]);
            mListener->OnExportOptionChange(mOptions[MP3OptionIDQualityVBR]);
            mListener->OnExportOptionChangeEnd();

            mListener->OnSampleRateListChange();
        }
    } break;
    case MP3OptionIDQualityABR:
    case MP3OptionIDQualityCBR:
    case MP3OptionIDQualitySET:
    case MP3OptionIDQualityVBR:
    {
        if (mListener) {
            mListener->OnSampleRateListChange();
        }
    } break;
    default: break;
    }
    return true;
}

bool MP3ExportOptionsEditor::GetValue(int id, ExportValue& value) const
{
    const auto it = mValues.find(id);
    if (it != mValues.end()) {
        value = it->second;
        return true;
    }
    return false;
}

MP3ExportOptionsEditor::SampleRateList MP3ExportOptionsEditor::GetSampleRateList() const
{
    // Retrieve preferences
    int highrate = 48000;
    int lowrate = 8000;

    const auto rmode = *std::get_if<std::string>(&mValues.find(MP3OptionIDMode)->second);

    if (rmode == "ABR") {
        auto bitrate = *std::get_if<int>(&mValues.find(MP3OptionIDQualityABR)->second);
        if (bitrate > 160) {
            lowrate = 32000;
        } else if (bitrate < 32 || bitrate == 144) {
            highrate = 24000;
        }
    } else if (rmode == "CBR") {
        auto bitrate = *std::get_if<int>(&mValues.find(MP3OptionIDQualityCBR)->second);

        if (bitrate > 160) {
            lowrate = 32000;
        } else if (bitrate < 32 || bitrate == 144) {
            highrate = 24000;
        }
    }

    SampleRateList result;
    result.reserve(sampRates.size());
    for (auto rate : sampRates) {
        if (rate >= lowrate && rate <= highrate) {
            result.push_back(rate);
        }
    }

    return result;
}

void MP3ExportOptionsEditor::Load(const audacity::BasicSettings& config)
{
    wxString mode;
    if (config.Read(wxT("/FileFormats/MP3RateModeChoice"), &mode)) {
        mValues[MP3OptionIDMode] = mode.ToStdString();
    } else {
        //attempt to recover from old-style preference
        int index;
        if (config.Read(wxT("/FileFormats/MP3RateMode"), &index)) {
            mValues[MP3OptionIDMode] = mOptions[MP3OptionIDMode].values[index];
        }
    }

    config.Read(wxT("/FileFormats/MP3SetRate"), std::get_if<int>(&mValues[MP3OptionIDQualitySET]));
    config.Read(wxT("/FileFormats/MP3AbrRate"), std::get_if<int>(&mValues[MP3OptionIDQualityABR]));
    config.Read(wxT("/FileFormats/MP3CbrRate"), std::get_if<int>(&mValues[MP3OptionIDQualityCBR]));
    config.Read(wxT("/FileFormats/MP3VbrRate"), std::get_if<int>(&mValues[MP3OptionIDQualityVBR]));

    OnModeChange(*std::get_if<std::string>(&mValues[MP3OptionIDMode]));
}

void MP3ExportOptionsEditor::Store(audacity::BasicSettings& config) const
{
    auto it = mValues.find(MP3OptionIDMode);
    config.Write(wxT("/FileFormats/MP3RateModeChoice"), wxString(*std::get_if<std::string>(&it->second)));

    it = mValues.find(MP3OptionIDQualitySET);
    config.Write(wxT("/FileFormats/MP3SetRate"), *std::get_if<int>(&it->second));
    it = mValues.find(MP3OptionIDQualityABR);
    config.Write(wxT("/FileFormats/MP3AbrRate"), *std::get_if<int>(&it->second));
    it = mValues.find(MP3OptionIDQualityCBR);
    config.Write(wxT("/FileFormats/MP3CbrRate"), *std::get_if<int>(&it->second));
    it = mValues.find(MP3OptionIDQualityVBR);
    config.Write(wxT("/FileFormats/MP3VbrRate"), *std::get_if<int>(&it->second));
}

void MP3ExportOptionsEditor::OnModeChange(const std::string& mode)
{
    mOptions[MP3OptionIDQualitySET].flags |= ExportOption::Hidden;
    mOptions[MP3OptionIDQualityABR].flags |= ExportOption::Hidden;
    mOptions[MP3OptionIDQualityCBR].flags |= ExportOption::Hidden;
    mOptions[MP3OptionIDQualityVBR].flags |= ExportOption::Hidden;

    if (mode == "SET") {
        mOptions[MP3OptionIDQualitySET].flags &= ~ExportOption::Hidden;
    } else if (mode == "ABR") {
        mOptions[MP3OptionIDQualityABR].flags &= ~ExportOption::Hidden;
    } else if (mode == "CBR") {
        mOptions[MP3OptionIDQualityCBR].flags &= ~ExportOption::Hidden;
    } else if (mode == "VBR") {
        mOptions[MP3OptionIDQualityVBR].flags &= ~ExportOption::Hidden;
    }
}

namespace {
int ValidateValue(int nValues, int value, int defaultValue)
{
    return (value >= 0 && value < nValues) ? value : defaultValue;
}

int ValidateValue(const std::vector<int>& values, int value, int defaultValue)
{
    auto start = values.begin(), finish = values.end(),
         iter = std::find(start, finish, value);
    return (iter != finish) ? value : defaultValue;
}

int ValidateIndex(const std::vector<int>& values, int value, int defaultIndex)
{
    auto start = values.begin(), finish = values.end(),
         iter = std::find(start, finish, value);
    return (iter != finish) ? static_cast<int>(iter - start) : defaultIndex;
}
}

//----------------------------------------------------------------------------
// MP3Exporter
//----------------------------------------------------------------------------

MP3Exporter::MP3Exporter()
{
// We could use #defines rather than this variable.
// The idea of the variable is that if we wanted, we could allow
// a dynamic override of the library, e.g. with a newer faster version,
// or to fix CVEs in the underlying library.
// for now though the 'variable' is a constant.
#ifdef MP3_EXPORT_BUILT_IN
    mLibIsExternal = false;
#else
    mLibIsExternal = true;
#endif

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    mLibraryLoaded = false;
#endif // DISABLE_DYNAMIC_LOADING_LAME
    mEncoding = false;
    mGF = NULL;

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    if (gPrefs) {
        mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    mBitrate = 128;
    mQuality = QUALITY_2;
    mMode = MODE_CBR;
    //mRoutine = ROUTINE_FAST;
}

MP3Exporter::~MP3Exporter()
{
    FreeLibrary();
}

#ifndef DISABLE_DYNAMIC_LOADING_LAME

bool MP3Exporter::LoadEncoderLibrary(AskUser askuser)
{
    if (ValidLibraryLoaded()) {
        FreeLibrary();
        mLibraryLoaded = false;
    }

#if defined(__WXMSW__)
    mBladeVersion = {};
#endif

    if (!mLibIsExternal) {
        mLibraryLoaded = InitLibraryInternal();
        return mLibraryLoaded;
    }

    // First try loading it from a previously located path
    if (!mLibPath.empty()) {
        wxLogMessage(wxT("Attempting to load LAME from previously defined path"));
        mLibraryLoaded = InitLibrary(mLibPath);
    }

    // If not successful, try loading using system search paths
    if (!ValidLibraryLoaded()) {
        wxLogMessage(wxT("Attempting to load LAME from system search paths"));
        mLibPath = GetLibraryName();
        mLibraryLoaded = InitLibrary(mLibPath);
    }

    // If not successful, try loading using compiled in path
    if (!ValidLibraryLoaded()) {
        wxLogMessage(wxT("Attempting to load LAME from builtin path"));
        wxFileName fn(GetLibraryPath(), GetLibraryName());
        mLibPath = fn.GetFullPath();
        mLibraryLoaded = InitLibrary(mLibPath);
    }

    // If not successful, must ask the user
    // TODO: need to ask on QT side
    // if (!ValidLibraryLoaded()) {
    //     wxLogMessage(wxT("(Maybe) ask user for library"));
    //     if (askuser == MP3Exporter::Maybe && FindLibrary(parent)) {
    //         mLibraryLoaded = InitLibrary(mLibPath);
    //     }
    // }

    // Oh well, just give up
    if (!ValidLibraryLoaded()) {
#if defined(__WXMSW__)
        if (askuser && !mBladeVersion.empty()) {
            BasicUI::ShowMessageBox(mBladeVersion);
        }
#endif
        wxLogMessage(wxT("Failed to locate LAME library"));

        return false;
    }

    wxLogMessage(wxT("LAME library successfully loaded"));

    return true;
}

bool MP3Exporter::ValidLibraryLoaded()
{
    return mLibraryLoaded;
}

#endif // DISABLE_DYNAMIC_LOADING_LAME

void MP3Exporter::SetMode(int mode)
{
    mMode = mode;
}

void MP3Exporter::SetBitrate(int rate)
{
    mBitrate = rate;
}

void MP3Exporter::SetQuality(int q /*, int r*/)
{
    mQuality = q;
}

bool MP3Exporter::InitLibrary(wxString libpath)
{
    return mLibIsExternal ? InitLibraryExternal(libpath) : InitLibraryInternal();
}

bool MP3Exporter::InitLibraryInternal()
{
    wxLogMessage(wxT("Using internal LAME"));

// The global ::lame_something symbols only exist if LAME is built in.
// So we don't reference them unless they are.
#ifdef MP3_EXPORT_BUILT_IN

    lame_init = ::lame_init;
    get_lame_version = ::get_lame_version;
    lame_init_params = ::lame_init_params;
    lame_encode_buffer_ieee_float = ::lame_encode_buffer_ieee_float;
    lame_encode_buffer_interleaved_ieee_float = ::lame_encode_buffer_interleaved_ieee_float;
    lame_encode_flush = ::lame_encode_flush;
    lame_close = ::lame_close;

    lame_set_in_samplerate = ::lame_set_in_samplerate;
    lame_set_out_samplerate = ::lame_set_out_samplerate;
    lame_set_num_channels = ::lame_set_num_channels;
    lame_set_quality = ::lame_set_quality;
    lame_set_brate = ::lame_set_brate;
    lame_set_VBR = ::lame_set_VBR;
    lame_set_VBR_q = ::lame_set_VBR_q;
    lame_set_VBR_min_bitrate_kbps = ::lame_set_VBR_min_bitrate_kbps;
    lame_set_mode = ::lame_set_mode;
    lame_set_preset = ::lame_set_preset;
    lame_set_error_protection = ::lame_set_error_protection;
    lame_set_disable_reservoir = ::lame_set_disable_reservoir;
    lame_set_bWriteVbrTag = ::lame_set_bWriteVbrTag;

    // These are optional
    //lame_get_lametag_frame = ::lame_get_lametag_frame;
    lame_get_lametag_frame = NULL;
    lame_mp3_tags_fid = ::lame_mp3_tags_fid;

#if defined(__WXMSW__)
    //beWriteInfoTag = ::beWriteInfoTag;
    //beVersion = ::beVersion;
    beWriteInfoTag = NULL;
    beVersion = NULL;
#endif

    mGF = lame_init();
    if (mGF == NULL) {
        return false;
    }
#endif

    return true;
}

bool MP3Exporter::InitLibraryExternal(wxString libpath)
{
    wxLogMessage(wxT("Loading LAME from %s"), libpath);

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    if (!lame_lib.Load(libpath, wxDL_LAZY)) {
        wxLogMessage(wxT("load failed"));
        return false;
    }

    wxLogMessage(wxT("Actual LAME path %s"),
                 FileNames::PathFromAddr(lame_lib.GetSymbol(wxT("lame_init"))));

    lame_init = (lame_init_t*)
                lame_lib.GetSymbol(wxT("lame_init"));
    get_lame_version = (get_lame_version_t*)
                       lame_lib.GetSymbol(wxT("get_lame_version"));
    lame_init_params = (lame_init_params_t*)
                       lame_lib.GetSymbol(wxT("lame_init_params"));
    lame_encode_buffer_ieee_float = (lame_encode_buffer_ieee_float_t*)
                                    lame_lib.GetSymbol(wxT("lame_encode_buffer_ieee_float"));
    lame_encode_buffer_interleaved_ieee_float = (lame_encode_buffer_interleaved_ieee_float_t*)
                                                lame_lib.GetSymbol(wxT("lame_encode_buffer_interleaved_ieee_float"));
    lame_encode_flush = (lame_encode_flush_t*)
                        lame_lib.GetSymbol(wxT("lame_encode_flush"));
    lame_close = (lame_close_t*)
                 lame_lib.GetSymbol(wxT("lame_close"));

    lame_set_in_samplerate = (lame_set_in_samplerate_t*)
                             lame_lib.GetSymbol(wxT("lame_set_in_samplerate"));
    lame_set_out_samplerate = (lame_set_out_samplerate_t*)
                              lame_lib.GetSymbol(wxT("lame_set_out_samplerate"));
    lame_set_num_channels = (lame_set_num_channels_t*)
                            lame_lib.GetSymbol(wxT("lame_set_num_channels"));
    lame_set_quality = (lame_set_quality_t*)
                       lame_lib.GetSymbol(wxT("lame_set_quality"));
    lame_set_brate = (lame_set_brate_t*)
                     lame_lib.GetSymbol(wxT("lame_set_brate"));
    lame_set_VBR = (lame_set_VBR_t*)
                   lame_lib.GetSymbol(wxT("lame_set_VBR"));
    lame_set_VBR_q = (lame_set_VBR_q_t*)
                     lame_lib.GetSymbol(wxT("lame_set_VBR_q"));
    lame_set_VBR_min_bitrate_kbps = (lame_set_VBR_min_bitrate_kbps_t*)
                                    lame_lib.GetSymbol(wxT("lame_set_VBR_min_bitrate_kbps"));
    lame_set_mode = (lame_set_mode_t*)
                    lame_lib.GetSymbol(wxT("lame_set_mode"));
    lame_set_preset = (lame_set_preset_t*)
                      lame_lib.GetSymbol(wxT("lame_set_preset"));
    lame_set_error_protection = (lame_set_error_protection_t*)
                                lame_lib.GetSymbol(wxT("lame_set_error_protection"));
    lame_set_disable_reservoir = (lame_set_disable_reservoir_t*)
                                 lame_lib.GetSymbol(wxT("lame_set_disable_reservoir"));
    lame_set_bWriteVbrTag = (lame_set_bWriteVbrTag_t*)
                            lame_lib.GetSymbol(wxT("lame_set_bWriteVbrTag"));

    // These are optional
    lame_get_lametag_frame = (lame_get_lametag_frame_t*)
                             lame_lib.GetSymbol(wxT("lame_get_lametag_frame"));
    lame_mp3_tags_fid = (lame_mp3_tags_fid_t*)
                        lame_lib.GetSymbol(wxT("lame_mp3_tags_fid"));
#if defined(__WXMSW__)
    beWriteInfoTag = (beWriteInfoTag_t*)
                     lame_lib.GetSymbol(wxT("beWriteInfoTag"));
    beVersion = (beVersion_t*)
                lame_lib.GetSymbol(wxT("beVersion"));
#endif

    if (!lame_init
        || !get_lame_version
        || !lame_init_params
        || !lame_encode_buffer_ieee_float
        || !lame_encode_buffer_interleaved_ieee_float
        || !lame_encode_flush
        || !lame_close
        || !lame_set_in_samplerate
        || !lame_set_out_samplerate
        || !lame_set_num_channels
        || !lame_set_quality
        || !lame_set_brate
        || !lame_set_VBR
        || !lame_set_VBR_q
        || !lame_set_mode
        || !lame_set_preset
        || !lame_set_error_protection
        || !lame_set_disable_reservoir
        || !lame_set_bWriteVbrTag) {
        wxLogMessage(wxT("Failed to find a required symbol in the LAME library."));
#if defined(__WXMSW__)
        if (beVersion) {
            be_version v;
            beVersion(&v);

            mBladeVersion = XO(
                "You are linking to lame_enc.dll v%d.%d. This version is not compatible with Audacity %d.%d.%d.\nPlease download the latest version of 'LAME for Audacity'.")
                            .Format(
                v.byMajorVersion,
                v.byMinorVersion,
                AUDACITY_VERSION,
                AUDACITY_RELEASE,
                AUDACITY_REVISION);
        }
#endif

        lame_lib.Unload();
        return false;
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    mGF = lame_init();
    if (mGF == NULL) {
        return false;
    }

    return true;
}

void MP3Exporter::FreeLibrary()
{
    if (mGF) {
        lame_close(mGF);
        mGF = NULL;
    }

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    lame_lib.Unload();
#endif // DISABLE_DYNAMIC_LOADING_LAME

    return;
}

wxString MP3Exporter::GetLibraryVersion()
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
    if (!mLibraryLoaded) {
        return wxT("");
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    return wxString::Format(wxT("LAME %hs"), get_lame_version());
}

int MP3Exporter::InitializeStream(unsigned channels, int sampleRate)
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
    if (!mLibraryLoaded) {
        return -1;
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    if (channels > 2) {
        return -1;
    }

    lame_set_error_protection(mGF, false);
    lame_set_num_channels(mGF, channels);
    lame_set_in_samplerate(mGF, sampleRate);
    lame_set_out_samplerate(mGF, sampleRate);
    lame_set_disable_reservoir(mGF, false);
    // Add the VbrTag for all types.  For ABR/VBR, a Xing tag will be created.
    // For CBR, it will be a Lame Info tag.
    lame_set_bWriteVbrTag(mGF, true);

    // Set the VBR quality or ABR/CBR bitrate
    switch (mMode) {
    case MODE_SET:
    {
        int preset;

        if (mQuality == PRESET_INSANE) {
            preset = INSANE;
        }
        //else if (mRoutine == ROUTINE_FAST) {
        else if (mQuality == PRESET_EXTREME) {
            preset = EXTREME_FAST;
        } else if (mQuality == PRESET_STANDARD) {
            preset = STANDARD_FAST;
        } else {
            preset = 1007;       // Not defined until 3.96
        }
        //}
        /*
        else {
           if (mQuality == PRESET_EXTREME) {
              preset = EXTREME;
           }
           else if (mQuality == PRESET_STANDARD) {
              preset = STANDARD;
           }
           else {
              preset = 1006;    // Not defined until 3.96
           }
        }
        */
        lame_set_preset(mGF, preset);
    }
    break;

    case MODE_VBR:
        lame_set_VBR(mGF, vbr_mtrh);
        lame_set_VBR_q(mGF, mQuality);
        break;

    case MODE_ABR:
        lame_set_preset(mGF, mBitrate);
        break;

    default:
        lame_set_VBR(mGF, vbr_off);
        lame_set_brate(mGF, mBitrate);
        break;
    }

    // Set the channel mode
    MPEG_mode mode;

    if (channels == 1) {
        mode = MONO;
    } else {
        mode = JOINT_STEREO;
    }

    lame_set_mode(mGF, mode);

    int rc = lame_init_params(mGF);
    if (rc < 0) {
        return rc;
    }

#if 0
    dump_config(mGF);
#endif

    mInfoTagLen = 0;
    mEncoding = true;

    return mSamplesPerChunk;
}

int MP3Exporter::GetOutBufferSize()
{
    if (!mEncoding) {
        return -1;
    }

    return mOutBufferSize;
}

int MP3Exporter::EncodeBuffer(float inbuffer[], unsigned char outbuffer[])
{
    if (!mEncoding) {
        return -1;
    }

    return lame_encode_buffer_interleaved_ieee_float(mGF, inbuffer, mSamplesPerChunk,
                                                     outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainder(float inbuffer[], int nSamples,
                                 unsigned char outbuffer[])
{
    if (!mEncoding) {
        return -1;
    }

    return lame_encode_buffer_interleaved_ieee_float(mGF, inbuffer, nSamples, outbuffer,
                                                     mOutBufferSize);
}

int MP3Exporter::EncodeBufferMono(float inbuffer[], unsigned char outbuffer[])
{
    if (!mEncoding) {
        return -1;
    }

    return lame_encode_buffer_ieee_float(mGF, inbuffer, inbuffer, mSamplesPerChunk,
                                         outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainderMono(float inbuffer[], int nSamples,
                                     unsigned char outbuffer[])
{
    if (!mEncoding) {
        return -1;
    }

    return lame_encode_buffer_ieee_float(mGF, inbuffer, inbuffer, nSamples, outbuffer,
                                         mOutBufferSize);
}

int MP3Exporter::FinishStream(unsigned char outbuffer[])
{
    if (!mEncoding) {
        return -1;
    }

    mEncoding = false;

    int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);

#if defined(DISABLE_DYNAMIC_LOADING_LAME)
    mInfoTagLen = lame_get_lametag_frame(mGF, mInfoTagBuf, sizeof(mInfoTagBuf));
#else
    if (lame_get_lametag_frame) {
        mInfoTagLen = lame_get_lametag_frame(mGF, mInfoTagBuf, sizeof(mInfoTagBuf));
    }
#endif

    return result;
}

void MP3Exporter::CancelEncoding()
{
    mEncoding = false;
}

bool MP3Exporter::PutInfoTag(wxFFile& f, wxFileOffset off)
{
    if (mGF) {
        if (mInfoTagLen > 0) {
            // FIXME: TRAP_ERR Seek and writ ein MP3 exporter could fail.
            if (!f.Seek(off, wxFromStart)) {
                return false;
            }
            if (mInfoTagLen > f.Write(mInfoTagBuf, mInfoTagLen)) {
                return false;
            }
        }
#if defined(__WXMSW__)
        else if (beWriteInfoTag) {
            if (!f.Flush()) {
                return false;
            }
            // PRL:  What is the correct error check on the return value?
            wxCharBuffer utf8 = f.GetName().ToUTF8();
            char* filename = utf8.data();
            beWriteInfoTag(mGF, filename);
            mGF = NULL;
        }
#endif
        else if (lame_mp3_tags_fid != NULL) {
            lame_mp3_tags_fid(mGF, f.fp());
        }
    }

    if (!f.SeekEnd()) {
        return false;
    }

    return true;
}

#if defined(__WXMSW__)
/* values for Windows */

wxString MP3Exporter::GetLibraryPath()
{
    HKEY hKey;
    const wchar_t* subkey = L"Software\\Lame for Audacity";
    const wchar_t* valueName = L"InstallPath";
    wchar_t pathBuffer[MAX_PATH];
    DWORD pathBufferSize = sizeof(pathBuffer);

    if (RegOpenKeyExW(HKEY_LOCAL_MACHINE, subkey, 0, KEY_READ, &hKey) == ERROR_SUCCESS) {
        if (RegQueryValueExW(hKey, valueName, nullptr, nullptr,
                             reinterpret_cast<LPBYTE>(pathBuffer), &pathBufferSize) == ERROR_SUCCESS) {
            RegCloseKey(hKey);
            return wxString(pathBuffer);
        }
        RegCloseKey(hKey);
    }

    return wxString{};
}

wxString MP3Exporter::GetLibraryName()
{
    return wxT("lame_enc.dll");
}

FileNames::FileTypes MP3Exporter::GetLibraryTypes()
{
    return {
        { XO("Only lame_enc.dll"), { wxT("lame_enc.dll") } },
        FileNames::DynamicLibraries,
        FileNames::AllFiles
    };
}

#elif defined(__WXMAC__)
/* values for Mac OS X */

wxString MP3Exporter::GetLibraryPath()
{
    wxString path;

    path = wxT("/Library/Application Support/audacity/libs");
    if (wxFileExists(path + wxT("/") + GetLibraryName())) {
        return path;
    }

    path = wxT("/usr/local/lib/audacity");
    if (wxFileExists(path + wxT("/") + GetLibraryName())) {
        return path;
    }

    return wxT("/Library/Application Support/audacity/libs");
}

wxString MP3Exporter::GetLibraryName()
{
    if (sizeof(void*) == 8) {
        return wxT("libmp3lame64bit.dylib");
    }
    return wxT("libmp3lame.dylib");
}

FileNames::FileTypes MP3Exporter::GetLibraryTypes()
{
    return {
        (sizeof(void*) == 8)
        ? FileNames::FileType{
            XO("Only libmp3lame64bit.dylib"), { wxT("libmp3lame64bit.dylib") }
        }
        : FileNames::FileType{
            XO("Only libmp3lame.dylib"), { wxT("libmp3lame.dylib") }
        }
        ,
        FileNames::DynamicLibraries,
        FileNames::AllFiles
    };
}

#elif defined(__OpenBSD__)
/* Values for OpenBSD systems */

wxString MP3Exporter::GetLibraryPath()
{
    return wxT(LIBDIR);
}

wxString MP3Exporter::GetLibraryName()
{
    return wxT("libmp3lame.so");
}

FileNames::FileTypes MP3Exporter::GetLibraryTypes()
{
    return {
        { XO("Only libmp3lame.so"), { wxT("libmp3lame.so") } },
        { XO("Primary shared object files"), { wxT("so") }, true },
        { XO("Extended libraries"), { wxT("so*") }, true },
        FileNames::AllFiles
    };
}

#else //!__OpenBSD__
/* Values for Linux / Unix systems */

wxString MP3Exporter::GetLibraryPath()
{
    return wxT(LIBDIR);
}

wxString MP3Exporter::GetLibraryName()
{
    return wxT("libmp3lame.so.0");
}

FileNames::FileTypes MP3Exporter::GetLibraryTypes()
{
    return {
        { XO("Only libmp3lame.so.0"), { wxT("libmp3lame.so.0") } },
        { XO("Primary shared object files"), { wxT("so") }, true },
        { XO("Extended libraries"), { wxT("so*") }, true },
        FileNames::AllFiles
    };
}

#endif

#if 0
// Debug routine from BladeMP3EncDLL.c in the libmp3lame distro
static void dump_config(lame_global_flags* gfp)
{
    wxPrintf(wxT("\n\nLame_enc configuration options:\n"));
    wxPrintf(wxT("==========================================================\n"));

    wxPrintf(wxT("version                =%d\n"), lame_get_version(gfp));
    wxPrintf(wxT("Layer                  =3\n"));
    wxPrintf(wxT("mode                   ="));
    switch (lame_get_mode(gfp)) {
    case STEREO:       wxPrintf(wxT("Stereo\n"));
        break;
    case JOINT_STEREO: wxPrintf(wxT("Joint-Stereo\n"));
        break;
    case DUAL_CHANNEL: wxPrintf(wxT("Forced Stereo\n"));
        break;
    case MONO:         wxPrintf(wxT("Mono\n"));
        break;
    case NOT_SET:        /* FALLTHROUGH */
    default:           wxPrintf(wxT("Error (unknown)\n"));
        break;
    }

    wxPrintf(wxT("Input sample rate      =%.1f kHz\n"), lame_get_in_samplerate(gfp) / 1000.0);
    wxPrintf(wxT("Output sample rate     =%.1f kHz\n"), lame_get_out_samplerate(gfp) / 1000.0);

    wxPrintf(wxT("bitrate                =%d kbps\n"), lame_get_brate(gfp));
    wxPrintf(wxT("Quality Setting        =%d\n"), lame_get_quality(gfp));

    wxPrintf(wxT("Low pass frequency     =%d\n"), lame_get_lowpassfreq(gfp));
    wxPrintf(wxT("Low pass width         =%d\n"), lame_get_lowpasswidth(gfp));

    wxPrintf(wxT("High pass frequency    =%d\n"), lame_get_highpassfreq(gfp));
    wxPrintf(wxT("High pass width        =%d\n"), lame_get_highpasswidth(gfp));

    wxPrintf(wxT("No short blocks        =%d\n"), lame_get_no_short_blocks(gfp));
    wxPrintf(wxT("Force short blocks     =%d\n"), lame_get_force_short_blocks(gfp));

    wxPrintf(wxT("de-emphasis            =%d\n"), lame_get_emphasis(gfp));
    wxPrintf(wxT("private flag           =%d\n"), lame_get_extension(gfp));

    wxPrintf(wxT("copyright flag         =%d\n"), lame_get_copyright(gfp));
    wxPrintf(wxT("original flag          =%d\n"),      lame_get_original(gfp));
    wxPrintf(wxT("CRC                    =%s\n"), lame_get_error_protection(gfp) ? wxT("on") : wxT("off"));
    wxPrintf(wxT("Fast mode              =%s\n"), (lame_get_quality(gfp)) ? wxT("enabled") : wxT("disabled"));
    wxPrintf(wxT("Force mid/side stereo  =%s\n"), (lame_get_force_ms(gfp)) ? wxT("enabled") : wxT("disabled"));
    wxPrintf(wxT("Padding Type           =%d\n"), (int)lame_get_padding_type(gfp));
    wxPrintf(wxT("Disable Reservoir      =%d\n"), lame_get_disable_reservoir(gfp));
    wxPrintf(wxT("Allow diff-short       =%d\n"), lame_get_allow_diff_short(gfp));
    wxPrintf(wxT("Interchannel masking   =%d\n"), lame_get_interChRatio(gfp));   // supposed to be a float, but in lib-src/lame/lame/lame.h it's int
    wxPrintf(wxT("Strict ISO Encoding    =%s\n"), (lame_get_strict_ISO(gfp)) ? wxT("Yes") : wxT("No"));
    wxPrintf(wxT("Scale                  =%5.2f\n"), lame_get_scale(gfp));

    wxPrintf(wxT("VBR                    =%s, VBR_q =%d, VBR method ="),
             (lame_get_VBR(gfp) != vbr_off) ? wxT("enabled") : wxT("disabled"),
             lame_get_VBR_q(gfp));

    switch (lame_get_VBR(gfp)) {
    case vbr_off:     wxPrintf(wxT("vbr_off\n"));
        break;
    case vbr_mt:     wxPrintf(wxT("vbr_mt \n"));
        break;
    case vbr_rh:     wxPrintf(wxT("vbr_rh \n"));
        break;
    case vbr_mtrh:    wxPrintf(wxT("vbr_mtrh \n"));
        break;
    case vbr_abr:
        wxPrintf(wxT("vbr_abr (average bitrate %d kbps)\n"), lame_get_VBR_mean_bitrate_kbps(gfp));
        break;
    default:
        wxPrintf(wxT("error, unknown VBR setting\n"));
        break;
    }

    wxPrintf(wxT("Vbr Min bitrate        =%d kbps\n"), lame_get_VBR_min_bitrate_kbps(gfp));
    wxPrintf(wxT("Vbr Max bitrate        =%d kbps\n"), lame_get_VBR_max_bitrate_kbps(gfp));

    wxPrintf(wxT("Write VBR Header       =%s\n"), (lame_get_bWriteVbrTag(gfp)) ? wxT("Yes") : wxT("No"));
    wxPrintf(wxT("VBR Hard min           =%d\n"), lame_get_VBR_hard_min(gfp));

    wxPrintf(wxT("ATH Only               =%d\n"), lame_get_ATHonly(gfp));
    wxPrintf(wxT("ATH short              =%d\n"), lame_get_ATHshort(gfp));
    wxPrintf(wxT("ATH no                 =%d\n"), lame_get_noATH(gfp));
    wxPrintf(wxT("ATH type               =%d\n"), lame_get_ATHtype(gfp));
    wxPrintf(wxT("ATH lower              =%f\n"), lame_get_ATHlower(gfp));
    wxPrintf(wxT("ATH aa                 =%d\n"), lame_get_athaa_type(gfp));
    wxPrintf(wxT("ATH aa  loudapprox     =%d\n"), lame_get_athaa_loudapprox(gfp));
    wxPrintf(wxT("ATH aa  sensitivity    =%f\n"), lame_get_athaa_sensitivity(gfp));

    wxPrintf(wxT("Experimental nspsytune =%d\n"), lame_get_exp_nspsytune(gfp));
    wxPrintf(wxT("Experimental X         =%d\n"), lame_get_experimentalX(gfp));
    wxPrintf(wxT("Experimental Y         =%d\n"), lame_get_experimentalY(gfp));
    wxPrintf(wxT("Experimental Z         =%d\n"), lame_get_experimentalZ(gfp));
}

#endif

//----------------------------------------------------------------------------
// ExportMP3
//----------------------------------------------------------------------------

ExportMP3::ExportMP3() = default;

int ExportMP3::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportMP3::GetFormatInfo(int) const
{
    return {
        wxT("MP3"), XO("MP3 Files"), { wxT("mp3") }, 2u, true
    };
}

std::unique_ptr<ExportOptionsEditor>
ExportMP3::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<MP3ExportOptionsEditor>(listener);
}

std::unique_ptr<ExportProcessor> ExportMP3::CreateProcessor(int format) const
{
    return std::make_unique<MP3ExportProcessor>();
}

std::vector<std::string> ExportMP3::GetMimeTypes(int) const
{
    return { "audio/mpeg" };
}

// bool ExportMP3::ParseConfig(
//     int formatIndex, const rapidjson::Value& document,
//     ExportProcessor::Parameters& parameters) const
// {
//     if (!document.IsObject()) {
//         return false;
//     }

//     MP3OptionID qualityMode;

//     if (document.HasMember("mode")) {
//         auto& mode = document["mode"];
//         if (!mode.IsString()) {
//             return false;
//         }

//         auto value = mode.GetString();

//         if (value == std::string_view { "SET" }) {
//             qualityMode = MP3OptionIDQualitySET;
//         } else if (value == std::string_view { "VBR" }) {
//             qualityMode = MP3OptionIDQualityVBR;
//         } else if (value == std::string_view { "ABR" }) {
//             qualityMode = MP3OptionIDQualityABR;
//         } else if (value == std::string_view { "CBR" }) {
//             qualityMode = MP3OptionIDQualityCBR;
//         } else {
//             return false;
//         }

//         parameters.push_back(std::make_tuple(MP3OptionIDMode, value));
//     } else {
//         return false;
//     }

//     if (document.HasMember("quality")) {
//         auto& qualityMember = document["quality"];

//         if (!qualityMember.IsInt()) {
//             return false;
//         }

//         const auto quality = qualityMember.GetInt();

//         if (qualityMode == MP3OptionIDQualitySET && (quality < 0 || quality > 3)) {
//             return false;
//         } else if (
//             qualityMode == MP3OptionIDQualityVBR && (quality < 0 || quality > 9)) {
//             return false;
//         } else if (
//             qualityMode == MP3OptionIDQualityABR
//             && std::find(
//                 fixRateValues.begin(), fixRateValues.end(),
//                 ExportValue { quality })
//             == fixRateValues.end()) {
//             return false;
//         } else if (
//             qualityMode == MP3OptionIDQualityCBR
//             && std::find(
//                 fixRateValues.begin(), fixRateValues.end(),
//                 ExportValue { quality })
//             == fixRateValues.end()) {
//             return false;
//         }

//         parameters.push_back(std::make_tuple(qualityMode, quality));
//     } else {
//         return false;
//     }

//     return true;
// }

bool ExportMP3::CheckFileName(wxFileName& WXUNUSED(filename), int WXUNUSED(format)) const
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
    MP3Exporter exporter;

    if (!exporter.LoadEncoderLibrary(MP3Exporter::Maybe)) {
        BasicUI::ShowMessageBox(XO("Could not open MP3 encoding library!"),
                                BasicUI::MessageBoxOptions()
                                .IconStyle(BasicUI::Icon::Error)
                                .Caption(XO("Error")));
        gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
        gPrefs->Flush();

        return false;
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    return true;
}

bool MP3ExportProcessor::Initialize(AudacityProject& project,
                                    const Parameters& parameters,
                                    const wxFileNameWrapper& fName,
                                    double t0, double t1, bool selectionOnly,
                                    double sampleRate, unsigned channels,
                                    MixerOptions::Downmix* mixerSpec,
                                    const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.channels = channels;

    int rate = lrint(sampleRate);
    auto& exporter = context.exporter;

#ifdef DISABLE_DYNAMIC_LOADING_LAME
    if (!exporter.InitLibrary(wxT(""))) {
        gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
        gPrefs->Flush();
        throw ExportException(_("Could not initialize MP3 encoding library!"));
    }
#else
    if (!exporter.LoadEncoderLibrary(MP3Exporter::Maybe)) {
        gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
        gPrefs->Flush();
        throw ExportException(_("Could not open MP3 encoding library!"));
    }

    if (!exporter.ValidLibraryLoaded()) {
        gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
        gPrefs->Flush();
        throw ExportException(_("Not a valid or supported MP3 encoding library!"));
    }
#endif // DISABLE_DYNAMIC_LOADING_LAME

    // Retrieve preferences
    int highrate = 48000;
    int lowrate = 8000;
    int bitrate = 0;
    int quality;

    auto rmode = ExportPluginHelpers::GetParameterValue(
        parameters,
        MP3OptionIDMode,
        std::string("CBR"));
    // Set the bitrate/quality and mode
    if (rmode == "SET") {
        quality = ExportPluginHelpers::GetParameterValue<int>(
            parameters,
            MP3OptionIDQualitySET,
            PRESET_STANDARD);
        exporter.SetMode(MODE_SET);
        exporter.SetQuality(quality);
    } else if (rmode == "VBR") {
        quality = ExportPluginHelpers::GetParameterValue<int>(
            parameters,
            MP3OptionIDQualityVBR,
            QUALITY_2);
        exporter.SetMode(MODE_VBR);
        exporter.SetQuality(quality);
    } else if (rmode == "ABR") {
        bitrate = ExportPluginHelpers::GetParameterValue(
            parameters,
            MP3OptionIDQualityABR,
            128);
        exporter.SetMode(MODE_ABR);
        exporter.SetBitrate(bitrate);
        if (bitrate > 160) {
            lowrate = 32000;
        } else if (bitrate < 32 || bitrate == 144) {
            highrate = 24000;
        }
    } else {
        bitrate = ExportPluginHelpers::GetParameterValue(parameters, MP3OptionIDQualityCBR, 128);
        exporter.SetMode(MODE_CBR);
        exporter.SetBitrate(bitrate);

        if (bitrate > 160) {
            lowrate = 32000;
        } else if (bitrate < 32 || bitrate == 144) {
            highrate = 24000;
        }
    }

    // Verify sample rate
    if (!make_iterator_range(sampRates).contains(rate)
        || (rate < lowrate) || (rate > highrate)) {
        // Force valid sample rate in macros.
        if (project.mBatchMode) {
            if (!make_iterator_range(sampRates).contains(rate)) {
                auto const bestRateIt = std::lower_bound(sampRates.begin(),
                                                         sampRates.end(), rate);
                rate = (bestRateIt == sampRates.end()) ? highrate : *bestRateIt;
            }
            if (rate < lowrate) {
                rate = lowrate;
            } else if (rate > highrate) {
                rate = highrate;
            }
        }
        // else validate or prompt
        else {
            if (!make_iterator_range(sampRates).contains(rate)
                || (rate < lowrate) || (rate > highrate)) {
                //This call should go away once export project rate option
                //is available as an export dialog option
                // TODO: implement AskResample
            }
            if (rate == 0) {
                return false;
            }
        }
    }

    context.inSamples = exporter.InitializeStream(channels, rate);
    if (context.inSamples < 0) {
        throw ExportException(_("Unable to initialize MP3 stream"));
    }

    // Put ID3 tags at beginning of file
    if (metadata == nullptr) {
        metadata = &Tags::Get(project);
    }

    // Open file for writing
    if (!context.outFile.Open(fName.GetFullPath(), wxT("w+b"))) {
        throw ExportException(_("Unable to open target file for writing"));
    }

    bool endOfFile;
    context.id3len = AddTags(context.id3buffer, &endOfFile, metadata);
    if (context.id3len && !endOfFile) {
        if (context.id3len > context.outFile.Write(context.id3buffer.get(), context.id3len)) {
            // TODO: more precise message
            throw ExportErrorException("MP3:1882");
        }
        context.id3len = 0;
        context.id3buffer.reset();
    }

    context.infoTagPos = context.outFile.Tell();

    context.bufferSize = std::max(0, exporter.GetOutBufferSize());
    if (context.bufferSize == 0) {
        // TODO: more precise message
        throw ExportErrorException("MP3:1849");
    }

    if (rmode == "SET") {
        context.status = (selectionOnly
                          ? XO("Exporting selected audio with %s preset")
                          : XO("Exporting the audio with %s preset"))
                         .Format(setRateNamesShort[quality]);
    } else if (rmode == "VBR") {
        context.status = (selectionOnly
                          ? XO("Exporting selected audio with VBR quality %s")
                          : XO("Exporting the audio with VBR quality %s"))
                         .Format(varRateNames[quality]);
    } else {
        context.status = (selectionOnly
                          ? XO("Exporting selected audio at %d Kbps")
                          : XO("Exporting the audio at %d Kbps"))
                         .Format(bitrate);
    }

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, channels, context.inSamples, true, rate,
        floatSample, mixerSpec);

    return true;
}

ExportResult MP3ExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);

    auto& exporter = context.exporter;
    int bytes = 0;

    ArrayOf<unsigned char> buffer{ context.bufferSize };
    wxASSERT(buffer);

    auto exportResult = ExportResult::Success;

    {
        while (exportResult == ExportResult::Success) {
            auto blockLen = context.mixer->Process();
            if (blockLen == 0) {
                break;
            }

            float* mixed = (float*)context.mixer->GetBuffer();

            if ((int)blockLen < context.inSamples) {
                if (context.channels > 1) {
                    bytes = exporter.EncodeRemainder(mixed, blockLen, buffer.get());
                } else {
                    bytes = exporter.EncodeRemainderMono(mixed, blockLen, buffer.get());
                }
            } else {
                if (context.channels > 1) {
                    bytes = exporter.EncodeBuffer(mixed, buffer.get());
                } else {
                    bytes = exporter.EncodeBufferMono(mixed, buffer.get());
                }
            }

            if (bytes < 0) {
                throw ExportException(XO("Error %ld returned from MP3 encoder")
                                      .Format(bytes)
                                      .Translation());
            }

            if (bytes > (int)context.outFile.Write(buffer.get(), bytes)) {
                // TODO: more precise message
                throw ExportDiskFullError(context.outFile.GetName());
            }

            if (exportResult == ExportResult::Success) {
                exportResult = ExportPluginHelpers::UpdateProgress(
                    delegate, *context.mixer, context.t0, context.t1);
            }
        }
    }

    if (exportResult == ExportResult::Success) {
        bytes = exporter.FinishStream(buffer.get());

        if (bytes < 0) {
            // TODO: more precise message
            throw ExportErrorException("MP3:1981");
        }

        if (bytes > 0) {
            if (bytes > (int)context.outFile.Write(buffer.get(), bytes)) {
                // TODO: more precise message
                throw ExportErrorException("MP3:1988");
            }
        }

        // Write ID3 tag if it was supposed to be at the end of the file
        if (context.id3len > 0) {
            if (bytes > (int)context.outFile.Write(context.id3buffer.get(), context.id3len)) {
                // TODO: more precise message
                throw ExportErrorException("MP3:1997");
            }
        }

        // Always write the info (Xing/Lame) tag.  Until we stop supporting Lame
        // versions before 3.98, we must do this after the MP3 file has been
        // closed.
        //
        // Also, if beWriteInfoTag() is used, mGF will no longer be valid after
        // this call, so do not use it.
        if (!exporter.PutInfoTag(context.outFile, context.infoTagPos)
            || !context.outFile.Flush()
            || !context.outFile.Close()) {
            // TODO: more precise message
            throw ExportErrorException("MP3:2012");
        }
    }
    return exportResult;
}

int MP3ExportProcessor::AskResample(int bitrate, int rate, int lowrate, int highrate)
{
    // NOT_IMPLEMENTED;
    return rate;
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
unsigned long MP3ExportProcessor::AddTags(ArrayOf<char>& buffer, bool* endOfFile, const Tags* tags)
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

    unsigned long len;

    len = id3_tag_render(tp.get(), 0);
    buffer.reinit(len);
    len = id3_tag_render(tp.get(), (id3_byte_t*)buffer.get());

    return len;
#else //ifdef USE_LIBID3TAG
    return 0;
#endif
}

#ifdef USE_LIBID3TAG
void MP3ExportProcessor::AddFrame(struct id3_tag* tp, const wxString& n, const wxString& v, const char* name)
{
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
        struct id3_frame* frame2 = id3_frame_new(name);
        id3_field_setfullstring(id3_frame_field(frame2, 3), ucs4.get());
        id3_field* f2 = id3_frame_field(frame2, 1);
        memset(f2->immediate.value, 0, sizeof(f2->immediate.value));
        id3_tag_attachframe(tp, frame2);
        // Now install a second frame with the standard default language = "XXX"
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

//----------------------------------------------------------------------------
// Return library version
//----------------------------------------------------------------------------

TranslatableString GetMP3Version(bool prompt)
{
    MP3Exporter exporter;
    auto versionString = XO("MP3 export library not found");

#ifndef DISABLE_DYNAMIC_LOADING_LAME
    if (exporter.LoadEncoderLibrary(prompt ? MP3Exporter::Yes : MP3Exporter::No)) {
#endif // DISABLE_DYNAMIC_LOADING_LAME
    versionString = Verbatim(exporter.GetLibraryVersion());
#ifdef MP3_EXPORT_BUILT_IN
    versionString.Join(XO("(Built-in)"), " ");
#endif

#ifndef DISABLE_DYNAMIC_LOADING_LAME
}

#endif // DISABLE_DYNAMIC_LOADING_LAME

    return versionString;
}
