#include "Import.h"
#include <wx/log.h>
#include <wx/setup.h>
#include <wx/ffile.h>

#include "ImportPlugin.h"
#include "ImportProgressListener.h"
#include "CVSD.cpp"
#include "ImportUtils.h"
#include "WaveTrack.h"

#define DESC XO("CVSD files")

static const auto exts = {
  wxT("cvsd"), wxT("cvsdm")
};

class CVSDImportFileHandle final : public ImportFileHandle
{
public:
  CVSDImportFileHandle(const FilePath &name)
      : mFilename(name)
  {
    mFile = std::make_unique<wxFFile>(mFilename, wxT("rb"));

    mAccumulator = 0.0f;
    mStepSize = 0.01f; 
  }

  ~CVSDImportFileHandle() override = default;

  FilePath GetFilename() const override { return mFilename; }

  TranslatableString GetFileDescription() override { return DESC; }

  // 1 bit encoded -> 32 bit PCM (ratio of 32 bytes out for every 1 byte in)
  ByteCount GetFileUncompressedBytes() override {
    return mFile->IsOpened() ? (ByteCount)mFile->Length() * 32 : 0;
  }

  wxInt32 GetStreamCount() override { return 1; }
  const TranslatableStrings &GetStreamInfo() override { return mStreamInfo; }
  void SetStreamUsage(wxInt32 StreamID, bool Use) override { }

  void Import(
      ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
      TrackHolders& outTracks, Tags* tags,
      std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override
  {
      // could not import the file
      if (!mFile->IsOpened()) return;

      // decode the wave itself
      CVSDDecode(mFile);

      const size_t bufferSize = 1024;
      std::vector<uint8_t> inBuffer(bufferSize);
      std::vector<float> outBuffer(bufferSize * 8);

      mFile->Seek(0);
      long long processed = 0;
      long long total = mFile->Length();

      while (!mFile->Eof()) {
          size_t read = mFile->Read(inBuffer.data(), bufferSize);
          if (read == 0) break;

          for (size_t i = 0; i < read; ++i) {
              // Extract 8 bits from each byte
              for (int bitPos = 7; bitPos >= 0; --bitPos) {
                  bool bit = (inBuffer[i] >> bitPos) & 0x01;

                  // --- CVSD Decoding Logic ---
                  // 1. Adjust accumulator based on bit (1 = up, 0 = down)
                  if (bit) mAccumulator += mStepSize;
                  else mAccumulator -= mStepSize;

                  // 2. Simple Leaky Integrator (prevents DC offset build-up)
                  mAccumulator *= 0.99f; 

                  // 3. Clamp to Audacity's float range [-1.0, 1.0]
                  if (mAccumulator > 1.0f) mAccumulator = 1.0f;
                  if (mAccumulator < -1.0f) mAccumulator = -1.0f;

                  outBuffer[(i * 8) + (7 - bitPos)] = mAccumulator;
              }
          }

          newTrack->Append((samplePtr)outBuffer.data(), floatSample, read * 8);

          processed += read;
          if (progressListener.Update(processed, total) != ProgressResult::Success) {
              break; 
          }
      }

      outTracks.push_back(std::move(newTrack));
      progressListener.OnImportResult(ImportProgressListener::ImportResult::Success);
  }

  void Cancel() override { }
  void Stop() override { }

private:
  FilePath mFilename;
  std::unique_ptr<wxFFile> mFile;
  TranslatableStrings mStreamInfo;

  // Decoder state
  float mAccumulator;
  float mStepSize;
};

// --- The Plugin (The Entry Point) ---
class CVSDImportPlugin final : public ImportPlugin
{
public:
  CVSDImportPlugin()
      : ImportPlugin(FileExtensions(exts.begin(), exts.end()))
  {
  }

  wxString GetPluginStringID() override { return wxT("cvsd"); }

  TranslatableString GetPluginFormatDescription() override {
    return DESC;
  }

  std::unique_ptr<ImportFileHandle> Open(
      const FilePath &Filename, AudacityProject*) override
  {
    // The Plugin creates the Handle and hands over the Filename
    return std::make_unique<CVSDImportFileHandle>(Filename);
  }
};

// Register the plugin with Audacity's Importer
static Importer::RegisteredImportPlugin registered{ "CVSD",
   std::make_unique< CVSDImportPlugin >()
};
