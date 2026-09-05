#include "ImportCVSD.h"

#include <cstdlib>

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

void cvsd_decode(int16_t *voice_frame, const uint8_t *cvsd_in_pack, size_t len, T_CVSD_MAIN_STRUCT& params)
{
    int32_t tmp1, tmp2, tmp3;
    size_t i;

    for (i = 0; i<len; i++)
    {
        params.In_current = (cvsd_in_pack[i / 8] >> (7 - i % 8)) & 1; //extract current bit from byte

        params.In_current = 2 * params.In_current - 1;   // 0 --> -1; 1 --> 1

        params.bit_accum = params.In_current + params.prev1 + params.prev2;

        tmp1 = (SYLLABIC_CONST * params.dec_step) >> 15;

        if (std::abs(params.bit_accum) == 3)
            params.dec_step = tmp1 + DELTA_MAX;
        else
            params.dec_step = tmp1 + DELTA_MIN;

        //  Primary reconstruction integration
        tmp1 = (INTEG_B1 * params.dec_prev1) >> 15;
        tmp2 = (INTEG_B2 * params.dec_prev2) >> 15;

        tmp3 = (INTEG_G2D * params.dec_step) >> 15;
        tmp3 = tmp3 * params.In_current;

        tmp1 = tmp1 - tmp2 + tmp3;

        //  Saturation process
        if (tmp1 >= 32767)        // up overflow
            params.Out_current = 32767;
        else if (tmp1 <= -32768)  // down overflow
            params.Out_current = -32768;
        else                      // no overflow
            params.Out_current = tmp1;

        //  Shift
        params.prev2 = params.prev1;
        params.prev1 = params.In_current;

        params.dec_prev2 = params.dec_prev1;
        params.dec_prev1 = params.Out_current;

        voice_frame[i] = params.Out_current;
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
    double const duration = static_cast<double>(mNumSamples) / mSampleRate;

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
    int64_t totalSamplesRead = 0;
    {
        const size_t bytesToRead = (mNumChannels * SAMPLES_TO_READ)/8;
        ArrayOf<uint8_t> cvsdBuffer { bytesToRead };
        ArrayOf<int16_t> int16Buffer { bytesToRead*8 };

        while (!IsCancelled() && !IsStopped())
        {
            const size_t bytesRead = wxCVSDFile->Read(cvsdBuffer.get(), bytesToRead);
            if (bytesRead == 0) {
                break;
            }

            const size_t samplesRead = bytesRead * 8;
            cvsd_decode(int16Buffer.get(), cvsdBuffer.get(), samplesRead, mDecoderParams);

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
        }
    }

    if (IsCancelled()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Cancelled);
        return;
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
