#include "ExportCVSD.h"

#include <cstdlib>

void cvsd_encode(const int16_t *voice_frame, size_t len, T_CVSD_MAIN_STRUCT& params,
                 CVSD_BITPACKER& packer, std::vector<u_int8_t>& cvsd_out)
{
    int16_t current_bit;
    int32_t tmp1, tmp2;
    size_t i;


    for (i = 0; i<len; i++)
    {
        params.In_current = voice_frame[i];

        if (params.In_current >(params.product))
            current_bit = 1;
        else
            current_bit = -1;

        params.bit_accum = current_bit + params.prev1 + params.prev2;

        tmp1 = (SYLLABIC_CONST * params.step) >> 15;

        if (std::abs(params.bit_accum) == 3)
            params.step = tmp1 + DELTA_MAX;
        else
            params.step = tmp1 + DELTA_MIN;
        tmp1 = (PRM_INTEG_CONST * params.product) >> 15;
        tmp2 = params.step*current_bit;

        params.product = tmp1 + tmp2;

        // Shift
        params.prev2 = params.prev1;
        params.prev1 = current_bit;

        params.Out_current = (current_bit + 1) >> 1; // -1 --> 0; 1 --> 1

        // Most significant bit first, matching cvsd_decode()'s (byte >> (7 - i % 8)) & 1
        packer.partialByte <<= 1;
        packer.partialByte |= params.Out_current;
        if (++packer.partialBits == 8) {
            cvsd_out.push_back(packer.partialByte);
            packer.partialByte = 0;
            packer.partialBits = 0;
        }
    }
}

// Left-aligns any leftover bits into a final byte.
// Only valid after the last block.
void cvsd_encode_flush(CVSD_BITPACKER& packer, std::vector<u_int8_t>& cvsd_out)
{
    if (packer.partialBits > 0) {
        packer.partialByte <<= (8 - packer.partialBits);
        cvsd_out.push_back(packer.partialByte);
        packer.partialByte = 0;
        packer.partialBits = 0;
    }
}

void CVSDDecode(std::unique_ptr<wxFFile> openedFile)
{

};

int ExportCVSD::GetFormatCount() const
{
    return 1;
};

FormatInfo ExportCVSD::GetFormatInfo(int) const
{
    return {
        wxT("CVSD"), XO("CVSD Files"), { wxT("cvsd") }, 1, true
    };
};

// Todo: change this
enum : int {
    CLOptionIDCommand = 0,
    CLOptionIDShowOutput
};

const std::vector<ExportOption> CLOptions {
        { CLOptionIDCommand, {}, std::string() },
        { CLOptionIDShowOutput, {}, false }
};

std::string ExportOptionCVSDEditor::GetName() const
{
    return "CVSD";
}

int ExportOptionCVSDEditor::GetOptionsCount() const
{
    return 0;
}

bool ExportOptionCVSDEditor::GetOption(int, ExportOption& option) const
{
    return false;
}

bool ExportOptionCVSDEditor::GetValue(ExportOptionID, ExportValue& value) const
{
    value = mQualityUnscaled;
    return true;
}

bool ExportOptionCVSDEditor::SetValue(ExportOptionID, const ExportValue& value)
{
    if (auto num = std::get_if<int>(&value)) {
        mQualityUnscaled = *num;
        return true;
    }
    return false;
}

ExportOptionCVSDEditor::SampleRateList ExportOptionCVSDEditor::GetSampleRateList() const
{
    return {64000};
}

void ExportOptionCVSDEditor::Load(const audacity::BasicSettings& config)
{
}

void ExportOptionCVSDEditor::Store(audacity::BasicSettings& config) const
{
}


std::unique_ptr<ExportProcessor> ExportCVSD::CreateProcessor(int format) const
{
    return std::make_unique<ExportCVSDProcessor>();
}

bool ExportCVSDProcessor::Initialize(AudacityProject& project,
     const Parameters& parameters,
     const wxFileNameWrapper& filename,
     double t0, double t1, bool selectedOnly,
     double sampleRate, unsigned channels,
     MixerOptions::Downmix* mixerSpec,
     const Tags* tags)
{
    // set the context
    context.status = TranslatableString("import-export", "Exporting the selected audio as CVSD");
    context.mfile = std::make_unique<FileIO>(filename, FileIO::Output);
    context.t0 = t0;
    context.t1 = t1;
    context.channels = 1;
    // sampling rate 64kHz
    context.max_block_len = 64000*1;

    if (!context.mfile->IsOpened()) {
        return false;
    }

    // get the current_config
    const size_t maxBlockLen = context.max_block_len;
    unsigned cvsdNumOfChannels = context.channels;

    // using floatSample because CVSD, math is easier with floats (-1.0 to 1.0)
    context.mMixer = ExportPluginHelpers::CreateMixer(
        project, selectedOnly, t0, t1,
        cvsdNumOfChannels,
        maxBlockLen, true, sampleRate,
        floatSample, mixerSpec);

    return (context.mMixer != nullptr);
};

ExportResult ExportCVSDProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    auto exportResult = ExportResult::Success;
    {
        int eos = 0;
        while (exportResult == ExportResult::Success) {
            // get the number of samples
            auto pcmNumSamples = context.mMixer->Process();
            if (pcmNumSamples == 0 ) {
                context.status = TranslatableString("import-export", "Exporting is done");
                delegate.SetStatusString(context.status);
                break;
            }

            // we receive interleaved audio
            // GetBuffer spits out char*
            float* pcmBuffer = (float*)context.mMixer->GetBuffer();
            std::vector<int16_t> LinearPCMBuffer = {};

            std::vector<u_int8_t> EncoderOuputFromBuffer;
            // convert the sample buffer to Linear 16 bit PCM
            for (int i=0; i<pcmNumSamples; i++)
            {
                const float sample = pcmBuffer[i];
                // Map float range [-1.0, 1.0] to int16 range [-32768, 32767]
                int16_t linearSample = static_cast<int16_t>(std::clamp(sample * 32768.0f, -32768.0f, 32767.0f));
                LinearPCMBuffer.push_back(linearSample);
            }
            EncoderOuputFromBuffer = CVSDEncode(LinearPCMBuffer, config, pcmNumSamples);

            std::vector<u_int8_t> EncodeData;
            EncodeData.reserve(pcmNumSamples / 8 + 1);
            cvsd_encode(LinearPCMBuffer.data(), pcmNumSamples, mEncoderParams, mBitPacker, EncodeData);
            if (!EncodeData.empty()) {
                context.mfile->Write(EncodeData.data(), EncodeData.size());
            }
        }

        // Emit the trailing partial byte, if the total sample count was not a multiple of 8
        std::vector<u_int8_t> FinalByte;
        cvsd_encode_flush(mBitPacker, FinalByte);
        if (!FinalByte.empty()) {
            context.mfile->Write(FinalByte.data(), FinalByte.size());
        }
    }

    return exportResult;
};

std::unique_ptr<ExportOptionsEditor> ExportCVSD::CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<ExportOptionCVSDEditor>();
}
