/*
* Audacity: A Digital Audio Editor
*/
#include "au3exportparameters.h"

#include <algorithm>
#include <array>
#include <string>

#include <sndfile.h>

#include "framework/global/log.h"

using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
constexpr int WAV_ENCODING_OPTION_ID = SF_FORMAT_WAV;

constexpr int MP3_MODE_OPTION_ID = 0;
constexpr int MP3_PRESET_OPTION_ID = 1;
constexpr int MP3_VBR_QUALITY_OPTION_ID = 2;
constexpr int MP3_ABR_BIT_RATE_OPTION_ID = 3;
constexpr int MP3_CBR_BIT_RATE_OPTION_ID = 4;
constexpr int MP3_HIGH_BIT_RATE_MIN = 161;
constexpr int MP3_HIGH_BIT_RATE_MIN_SAMPLE_RATE = 32000;
constexpr int MP3_LOW_BIT_RATE_MAX = 31;
constexpr int MP3_ODD_BIT_RATE = 144;
constexpr int MP3_LOW_BIT_RATE_MAX_SAMPLE_RATE = 24000;
constexpr std::array MP3_SAMPLE_RATES { 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 48000 };

constexpr int FLAC_BIT_DEPTH_OPTION_ID = 0;
constexpr int FLAC_LEVEL_OPTION_ID = 1;

constexpr int OGG_QUALITY_OPTION_ID = 1;

int wavEncodingSubtype(WavEncoding encoding)
{
    switch (encoding) {
    case WavEncoding::Pcm16: return SF_FORMAT_PCM_16;
    case WavEncoding::Pcm24: return SF_FORMAT_PCM_24;
    case WavEncoding::Pcm32: return SF_FORMAT_PCM_32;
    case WavEncoding::Float32: return SF_FORMAT_FLOAT;
    }
    return SF_FORMAT_PCM_16;
}

Ret checkMp3SampleRate(int sampleRate)
{
    if (std::ranges::find(MP3_SAMPLE_RATES, sampleRate) == MP3_SAMPLE_RATES.end()) {
        LOGE() << "sample rate not supported by MP3: " << sampleRate;
        return make_ret(Ret::Code::BadArgs);
    }
    return make_ok();
}

Ret checkMp3BitRateSampleRate(Mp3BitRate bitRate, int sampleRate)
{
    const Ret rateRet = checkMp3SampleRate(sampleRate);
    if (!rateRet) {
        return rateRet;
    }

    const int kbps = static_cast<int>(bitRate);
    if (kbps >= MP3_HIGH_BIT_RATE_MIN && sampleRate < MP3_HIGH_BIT_RATE_MIN_SAMPLE_RATE) {
        LOGE() << "MP3 bit rate " << kbps << " needs a sample rate of at least " << MP3_HIGH_BIT_RATE_MIN_SAMPLE_RATE;
        return make_ret(Ret::Code::BadArgs);
    }

    if ((kbps <= MP3_LOW_BIT_RATE_MAX || kbps == MP3_ODD_BIT_RATE) && sampleRate > MP3_LOW_BIT_RATE_MAX_SAMPLE_RATE) {
        LOGE() << "MP3 bit rate " << kbps << " needs a sample rate of at most " << MP3_LOW_BIT_RATE_MAX_SAMPLE_RATE;
        return make_ret(Ret::Code::BadArgs);
    }

    return make_ok();
}

RetVal<ExportParameters> mp3Parameters(const std::string& mode, int qualityOptionId, int quality, const Ret& sampleRateRet)
{
    if (!sampleRateRet) {
        return RetVal<ExportParameters>::make_ret(sampleRateRet);
    }

    return RetVal<ExportParameters>::make_ok({ { MP3_MODE_OPTION_ID, mode }, { qualityOptionId, quality } });
}

RetVal<ExportParameters> parametersOf(const WavSettings& s, int)
{
    return RetVal<ExportParameters>::make_ok({ { WAV_ENCODING_OPTION_ID, wavEncodingSubtype(s.encoding) } });
}

RetVal<ExportParameters> parametersOf(const Mp3PresetSettings& s, int sampleRate)
{
    return mp3Parameters("SET", MP3_PRESET_OPTION_ID, static_cast<int>(s.preset), checkMp3SampleRate(sampleRate));
}

RetVal<ExportParameters> parametersOf(const Mp3VbrSettings& s, int sampleRate)
{
    return mp3Parameters("VBR", MP3_VBR_QUALITY_OPTION_ID, static_cast<int>(s.quality), checkMp3SampleRate(sampleRate));
}

RetVal<ExportParameters> parametersOf(const Mp3AbrSettings& s, int sampleRate)
{
    return mp3Parameters("ABR", MP3_ABR_BIT_RATE_OPTION_ID, static_cast<int>(s.bitRate),
                         checkMp3BitRateSampleRate(s.bitRate, sampleRate));
}

RetVal<ExportParameters> parametersOf(const Mp3CbrSettings& s, int sampleRate)
{
    return mp3Parameters("CBR", MP3_CBR_BIT_RATE_OPTION_ID, static_cast<int>(s.bitRate),
                         checkMp3BitRateSampleRate(s.bitRate, sampleRate));
}

RetVal<ExportParameters> parametersOf(const FlacSettings& s, int)
{
    return RetVal<ExportParameters>::make_ok({
            { FLAC_BIT_DEPTH_OPTION_ID, EnumContract<FlacBitDepth>::str_val(s.bitDepth) },
            { FLAC_LEVEL_OPTION_ID, std::to_string(static_cast<int>(s.level)) },
        });
}

RetVal<ExportParameters> parametersOf(const OggSettings& s, int)
{
    return RetVal<ExportParameters>::make_ok({ { OGG_QUALITY_OPTION_ID, static_cast<int>(s.quality) } });
}
}

RetVal<ExportParameters> au::importexport::au3ExportParameters(const ExportFormatSettings& settings, int sampleRate)
{
    return std::visit([sampleRate](const auto& s) { return parametersOf(s, sampleRate); }, settings);
}
