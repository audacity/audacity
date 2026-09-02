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

void CVSDDecode(wxFFile* wxCVSDFile, int32_t* PCMbuffer, uint32_t& samplesRead, const int samples_to_read, CVSD_CONFIG& mDecoderConfig)
{
    // resetting the seek just in case
    wxCVSDFile->Seek(0);

    // bits -> Samples is a 1:1 mapping
    // Todo: check how many bytes this is
    const int number_of_bytes_to_read = std::ceil(samples_to_read/8);
    std::vector<uint8_t> CVSDBuffer(number_of_bytes_to_read);

    // Read the entire file into the buffer
    const size_t bytesRead = wxCVSDFile->Read(CVSDBuffer.data(), number_of_bytes_to_read);

    // don't need to handle eof for now

    // raw binary file
    int sampleIndex = 0;
    for (size_t i=0; i<CVSDBuffer.size(); i++)
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
            PCMbuffer[sampleIndex++] = static_cast<int32_t>(mDecoderConfig.accumulator);
            samplesRead+=sampleIndex;
        }
    }
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

    return static_cast<double>(mNumSamples) / static_cast<double>(mSampleRate);
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

    auto tracks = trackFactory->CreateMany(mNumChannels, mFormat, mSampleRate);
    const int SAMPLES_TO_READ = (*tracks->Any<WaveTrack>().begin())->GetMaxBlockSize();
    int totalSamplesRead = 0;
    {
        const uint32_t bufferSize = mNumChannels * SAMPLES_TO_READ;
        ArrayOf<int32_t> CVSDBuffer{ bufferSize };
        ArrayOf<int16_t> int16Buffer;
        uint32_t samplesRead = 0;

        // The buffer is always going to be a int16Sample
        int16Buffer.reinit(bufferSize);
        unsigned chn = 0;
        do
        {
            CVSDDecode(wxCVSDFile.get(), CVSDBuffer.get(), samplesRead, SAMPLES_TO_READ, mDecoderConfig);
            ImportUtils::ForEachChannel(*tracks, [&](auto& channel)
            {
                channel.AppendBuffer(
                        reinterpret_cast<constSamplePtr>(int16Buffer.get() + chn),
                        mFormat,
                        samplesRead,
                        mNumChannels,
                        mFormat
                        );
                    ++chn;
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

    ImportUtils::FinalizeImport(outTracks, std::move(*tracks));

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

// class CVSDImportFileHandle final : public ImportFileHandle
// {
// public:
//   CVSDImportFileHandle(const FilePath &name)
//       : mFilename(name)
//   {
//     mFile = std::make_unique<wxFFile>(mFilename, wxT("rb"));
//
//     mAccumulator = 0.0f;
//     mStepSize = 0.01f;
//   }
//
//   ~CVSDImportFileHandle() override = default;
//
//   FilePath GetFilename() const override { return mFilename; }
//
//   TranslatableString GetFileDescription() override { return DESC; }
//
//   // 1 bit encoded -> 32 bit PCM (ratio of 32 bytes out for every 1 byte in)
//   ByteCount GetFileUncompressedBytes() override {
//     return mFile->IsOpened() ? (ByteCount)mFile->Length() * 32 : 0;
//   }
//
//   wxInt32 GetStreamCount() override { return 1; }
//   const TranslatableStrings &GetStreamInfo() override { return mStreamInfo; }
//   void SetStreamUsage(wxInt32 StreamID, bool Use) override { }
//
//   void Import(
//       ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
//       TrackHolders& outTracks, Tags* tags,
//       std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override
//   {
//       // could not import the file
//       if (!mFile->IsOpened()) return;
//
//       // decode the wave itself
//       CVSDDecode(mFile);
//
//       const size_t bufferSize = 1024;
//       std::vector<uint8_t> inBuffer(bufferSize);
//       std::vector<float> outBuffer(bufferSize * 8);
//
//       mFile->Seek(0);
//       long long processed = 0;
//       long long total = mFile->Length();
//
//       while (!mFile->Eof()) {
//           size_t read = mFile->Read(inBuffer.data(), bufferSize);
//           if (read == 0) break;
//
//           for (size_t i = 0; i < read; ++i) {
//               // Extract 8 bits from each byte
//               for (int bitPos = 7; bitPos >= 0; --bitPos) {
//                   bool bit = (inBuffer[i] >> bitPos) & 0x01;
//
//                   // --- CVSD Decoding Logic ---
//                   // 1. Adjust accumulator based on bit (1 = up, 0 = down)
//                   if (bit) mAccumulator += mStepSize;
//                   else mAccumulator -= mStepSize;
//
//                   // 2. Simple Leaky Integrator (prevents DC offset build-up)
//                   mAccumulator *= 0.99f;
//
//                   // 3. Clamp to Audacity's float range [-1.0, 1.0]
//                   if (mAccumulator > 1.0f) mAccumulator = 1.0f;
//                   if (mAccumulator < -1.0f) mAccumulator = -1.0f;
//
//                   outBuffer[(i * 8) + (7 - bitPos)] = mAccumulator;
//               }
//           }
//
//           newTrack->Append((samplePtr)outBuffer.data(), floatSample, read * 8);
//
//           processed += read;
//           if (progressListener.Update(processed, total) != ProgressResult::Success) {
//               break;
//           }
//       }
//
//       outTracks.push_back(std::move(newTrack));
//       progressListener.OnImportResult(ImportProgressListener::ImportResult::Success);
//   }
//
//   void Cancel() override { }
//   void Stop() override { }
//
// private:
//   FilePath mFilename;
//   std::unique_ptr<wxFFile> mFile;
//   TranslatableStrings mStreamInfo;
//
//   // Decoder state
//   float mAccumulator;
//   float mStepSize;
// };
//
// // --- The Plugin (The Entry Point) ---
// class CVSDImportPlugin final : public ImportPlugin
// {
// public:
//   CVSDImportPlugin()
//       : ImportPlugin(FileExtensions(exts.begin(), exts.end()))
//   {
//   }
//
//   wxString GetPluginStringID() override { return wxT("cvsd"); }
//
//   TranslatableString GetPluginFormatDescription() override {
//     return DESC;
//   }
//
//   std::unique_ptr<ImportFileHandle> Open(
//       const FilePath &Filename, AudacityProject*) override
//   {
//     // The Plugin creates the Handle and hands over the Filename
//     return std::make_unique<CVSDImportFileHandle>(Filename);
//   }
// };
//
// // Register the plugin with Audacity's Importer
// static Importer::RegisteredImportPlugin registered{ "CVSD",
//    std::make_unique< CVSDImportPlugin >()
// };
