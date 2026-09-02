#include "ExportCVSD.h"

std::vector<u_int8_t> CVSDEncode(std::vector<int16_t> temp, CVSD_CONFIG& current_config, size_t numSamples)
{
    const auto& audioBuffer = temp;
    std::vector<u_int8_t> EncoderOutput;
    u_int8_t currentByte = 0;
    int bitCount = 0;

    // process sample-by-sample
    for (size_t i = 0; i < numSamples; ++i) {
        int16_t x = audioBuffer[i];

        // 1. Compute current bit:
        current_config.b = (x >= current_config.accumulator);

        // 2. Check if last 4 bits are all 0s or all 1s
        current_config.alpha = (current_config.bitHistory == 0x00 || current_config.bitHistory == 0x0F);

        // 3. Update step size (δ(k)) based on PREVIOUS J bits
        if (current_config.alpha) {
            current_config.accumulatorStepSize = std::min(
                current_config.accumulatorStepSize * static_cast<float>(current_config.syllabicCompandingFactor),
                static_cast<float>(current_config.maxAccumulatorStepSize));
        } else {
            current_config.accumulatorStepSize = std::max(
                current_config.accumulatorStepSize * static_cast<float>(current_config.stepSizeDecay),
                static_cast<float>(current_config.minAccumulatorStepSize));
        }

        // 4. Update bit history (J-bit window for syllabic companding)
        current_config.bitHistory <<= 1;
        current_config.bitHistory |= current_config.b ? 1 : 0;
        current_config.bitHistory &= 0x0F;  // Keep only last 4 bits

        // 5. Pack the bit
        currentByte <<= 1;
        currentByte |= current_config.b ? 1 : 0;
        bitCount++;

        // 6. Push full bytes
        if (bitCount == 8) {
            EncoderOutput.push_back(currentByte);
            currentByte = 0;
            bitCount = 0;
        }

        // 7. Update accumulator: y(k) = y(k-1) + b(k) * δ(k)
        current_config.accumulator += (current_config.b ? 1.0f : -1.0f) * current_config.accumulatorStepSize;
        current_config.accumulator = std::clamp(
            current_config.accumulator,
            current_config.minAccumulatorSize,
            current_config.maxAccumulatorSize);
    }

    // Push remaining bits at the end
    if (bitCount > 0) {
        currentByte <<= (8 - bitCount);
        EncoderOutput.push_back(currentByte);
    }
    return EncoderOutput;
};

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
            context.mfile->Write( EncoderOuputFromBuffer.data(),
                EncoderOuputFromBuffer.size());
        }
    }

    return exportResult;
};

std::unique_ptr<ExportOptionsEditor> ExportCVSD::CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<ExportOptionCVSDEditor>();
}
