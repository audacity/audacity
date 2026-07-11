#include "ExportCVSD.h"

std::vector<u_int8_t> CVSDEncode(constSamplePtr temp, CVSD_CONFIG& current_config, size_t temp_size)
{
    // get the raw floating bits
    float* audioBuffer = (float*)temp;
    std::vector<u_int8_t> EncoderOuput;
    u_int8_t currentByte = 0;
    int bitCount = 0;
    for (size_t i = 0; i < temp_size; ++i)
    {
        float x = audioBuffer[i];
        float encoder_input_bit_from_the_float_sample = current_config.accumulatorDecay * current_config.accumulator;
        current_config.b = (x >= encoder_input_bit_from_the_float_sample);

        // update history
        // syllabic Companding
        current_config.bitHistory <<= 1;
        current_config.bitHistory |= current_config.b ? 1 : 0;
        current_config.bitHistory &= 0x0F;

        // update alpha
        current_config.alpha =  (current_config.bitHistory == 0x00 ||
            current_config.bitHistory == 0x0F);

        // update delta
        if (current_config.alpha) {
            current_config.accumulatorStepSize =
                std::min(
                    current_config.accumulatorStepSize +
                    current_config.minAccumulatorStepSize,
                    (float)current_config.maxAccumulatorStepSize);
        } else {
            current_config.accumulatorStepSize =
                std::max(
                    (float)(current_config.stepSizeDecay *
                    current_config.accumulatorStepSize),
                    (float)current_config.minAccumulatorStepSize);
        }

        // emit one CVSD bit
        float encoderPredictedOutput = encoder_input_bit_from_the_float_sample +
            (current_config.b ? 1.0f : -1.0f) * current_config.accumulatorStepSize;

        // push the bit
        // pack output bit
        currentByte <<= 1;
        currentByte |= current_config.b ? 1 : 0;
        bitCount++;

        // bit packing works
        if (bitCount == 8) {
            EncoderOuput.push_back(currentByte);
            currentByte = 0;
            bitCount = 0;
        }
        if(bitCount){
            currentByte <<= (8-bitCount);
            EncoderOuput.push_back(currentByte);
        }

        // update accumulator
        current_config.accumulator =
            std::clamp(
                encoderPredictedOutput,
                current_config.minAccumulatorSize,
                current_config.maxAccumulatorSize);
        }
    return EncoderOuput;
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
    context.max_block_len = 44100*5;

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
        while (exportResult == ExportResult::Success && !eos)
        {
            // get the samples
            auto samplesThisRun = context.mMixer->Process();
            if (samplesThisRun == 0 ) {
                context.status = TranslatableString("import-export", "Exporting is done");
                delegate.SetStatusString(context.status);
                break;
            } else {
                for (size_t i = 0; i < context.channels; i++) {
                    constSamplePtr temp = context.mMixer->GetBuffer(i);
                    std::vector<u_int8_t> EncoderOuputFromBuffer;
                    EncoderOuputFromBuffer = CVSDEncode(temp, config, sizeof(temp));
                    context.mfile->Write( EncoderOuputFromBuffer.data(),
                        EncoderOuputFromBuffer.size());
                }
            }
        }
    }

    return exportResult;
};

std::unique_ptr<ExportOptionsEditor> ExportCVSD::CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const
{
    return nullptr;
}
