#include "ImportCVSD.h"

#include <MacTypes.h>
#include <wx/log.h>
#include <wx/setup.h>
#include <wx/ffile.h>
#include <_deps/wavpack/include/wavpack/wavpack.h>

#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "CVSD.h"
#include "au3-import-export/ImportUtils.h"
#include "au3-wave-track/WaveTrack.h"
#include "io/path.h"

#define DESC XO("CVSD files")

static const auto exts = {
  wxT("CVSD"), wxT("CVSDM")
};

void CVSDDecode(wxFFile* wxCVSDFile, int16_t* PCMbuffer, uint32_t& samplesRead, const uint32_t samples_to_read, CVSD_CONFIG& mDecoderConfig)
{
    std::vector<uint8_t> CVSDBuffer(samples_to_read, 0);

    // Read the entire file into the buffer
    const size_t bytesRead = wxCVSDFile->Read(CVSDBuffer.data(), samples_to_read);

    // don't need to handle eof for now

    // raw binary file
    int sampleIndex = 0;
    for (size_t i=0; i<bytesRead; i++)
    {
        uint8_t rawCVSDbyte = CVSDBuffer[i];

        // unpack bit-by-bit
        for (int bitPos = 0; bitPos <= 7; bitPos++) {
            bool bit = (rawCVSDbyte >> bitPos) & 0x01;

            // Update decoder state (bitHistory, accumulator, etc.)
            mDecoderConfig.bitHistory <<= 1;
            mDecoderConfig.bitHistory |= bit ? 1 : 0;
            mDecoderConfig.bitHistory &= 0x0F;

            bool alpha = (mDecoderConfig.bitHistory == 0x00 || mDecoderConfig.bitHistory == 0x0F);
            if (mDecoderConfig.alpha) {
                mDecoderConfig.accumulatorStepSize = std::min(
                    mDecoderConfig.accumulatorStepSize * static_cast<float>(mDecoderConfig.syllabicCompandingFactor),
                    static_cast<float>(mDecoderConfig.maxAccumulatorStepSize));
            } else {
                mDecoderConfig.accumulatorStepSize = std::max(
                    mDecoderConfig.accumulatorStepSize * static_cast<float>(mDecoderConfig.stepSizeDecay),
                    static_cast<float>(mDecoderConfig.minAccumulatorStepSize));
            }

            mDecoderConfig.accumulator += (bit ? 1.0f : -1.0f) * mDecoderConfig.accumulatorStepSize;
            mDecoderConfig.accumulator = std::clamp(
                mDecoderConfig.accumulator,
                mDecoderConfig.minAccumulatorSize,
                mDecoderConfig.maxAccumulatorSize);

            // Assign to PCMBuffer using index
            PCMbuffer[sampleIndex++] = static_cast<int16_t>(mDecoderConfig.accumulator);
        }
    }
    samplesRead=sampleIndex;
}

CVSDImportPlugin::CVSDImportPlugin()
    :  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
{
}

FileExtensions CVSDImportPlugin::GetSupportedExtensions()
{
    return wxArrayString{"*.cvsd"};
}

TranslatableString CVSDImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

wxString CVSDImportPlugin::GetPluginStringID()
{
    // Todo: we are not using any library
    return wxT("cvsd");
}

std::unique_ptr<ImportFileHandle> CVSDImportPlugin::Open(const FilePath& Filename, AudacityProject*) {
    char errMessage[100]; // To hold possible error message

    auto handle = std::make_unique<CVSDImportFileHandle>(Filename);

    return std::move(handle);
}

CVSDImportFileHandle::CVSDImportFileHandle(const FilePath& filename) :
    ImportFileHandleEx(filename), mBytesPerSample(0),
    wxCVSDFile(std::make_unique<wxFFile>(filename, wxT("rb")))
{
    wxCVSDFile->SeekEnd(0);
    // Todo check if it's indeed int64_t
    int64_t fileSizeBytes = wxCVSDFile->Tell();
    wxLogDebug(wxT("File size: %lld"), fileSizeBytes);
    wxCVSDFile->Seek(0); // Reset to start of file

    // 1->1 mapping, CVSD
    mNumSamples = fileSizeBytes * 8;
    mBitsPerSample = 1;
    mFormat = int16Sample;
    mSampleRate = 64000;
    // CVSD is always mono
    mNumChannels = 1;
}

CVSDImportFileHandle::~CVSDImportFileHandle()
{
    if (wxCVSDFile && wxCVSDFile->IsOpened())
        wxCVSDFile->Close();
}

TranslatableString CVSDImportFileHandle::GetFileDescription() {
    return DESC;
}

double CVSDImportFileHandle::GetDuration() const {
    if (mSampleRate <= 0 || mNumSamples <= 0) {
        return 0.0;
    }
    double duration = static_cast<double>(mNumSamples) / mSampleRate;

    return duration;
}

int CVSDImportFileHandle::GetRequiredTrackCount() const
{
    return ImportUtils::RequiredTrackCountFromChannels(mNumChannels);
}

ImportFileHandle::ByteCount CVSDImportFileHandle::GetFileUncompressedBytes()
{
    return wxCVSDFile->Length();
}

void CVSDImportFileHandle::Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags)
{
    BeginImport();

    outTracks.clear();

    auto tracks = trackFactory->Create(mNumChannels, mFormat, mSampleRate);
    const size_t SAMPLES_TO_READ = (tracks->GetMaxBlockSize());
    int totalSamplesRead = 0;
    {
        const uint32_t bufferSize = (mNumChannels * SAMPLES_TO_READ)/8;
        ArrayOf<int16_t> int16Buffer { bufferSize*8 };
        uint32_t samplesRead = 0;
        do
        {
            CVSDDecode(wxCVSDFile.get(), int16Buffer.get(), samplesRead, bufferSize, mDecoderConfig);
            ImportUtils::ForEachChannel(*tracks, [&](auto& channel)
            {
                channel.AppendBuffer(
                        reinterpret_cast<constSamplePtr>(int16Buffer.get()),
                        mFormat,
                        samplesRead,
                        mNumChannels,
                        mFormat
                        );
            });

            totalSamplesRead += samplesRead;
        } while (!IsCancelled() && !IsStopped() && samplesRead != 0);

    }

    if (IsCancelled()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Cancelled);
    }

    if (totalSamplesRead < mNumSamples && !IsStopped()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }

    std::vector<std::shared_ptr<WaveTrack>> outTrack;
    outTrack.push_back(tracks);
    ImportUtils::FinalizeImport(outTracks, outTrack);

    progressListener.OnImportResult(IsStopped()
                                    ? ImportProgressListener::ImportResult::Stopped
                                    : ImportProgressListener::ImportResult::Success);
}

wxInt32 CVSDImportFileHandle::GetStreamCount()
{
    return 1;
}

const TranslatableStrings& CVSDImportFileHandle::GetStreamInfo()
{
    static TranslatableStrings empty;
    return empty;
}

void CVSDImportFileHandle::SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use))
{
}
