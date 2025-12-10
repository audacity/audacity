/**********************************************************************

   SPDX-License-Identifier: GPL-2.0-or-later

   Audacity: A Digital Audio Editor

   ImportOpus.cpp

   Dmitry Vedenko

**********************************************************************/

#include "ImportOpus.h"

#include <string_view>

#include <wx/string.h>
#include <wx/log.h>

#include <stdlib.h>

#define DESC XO("Opus files")

static const auto exts = { L"opus", L"ogg" };

// ============================================================================
// OpusImportPlugin
// ============================================================================

OpusImportPlugin::OpusImportPlugin()
    :  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
{
}

OpusImportPlugin::~OpusImportPlugin()
{
}

wxString OpusImportPlugin::GetPluginStringID()
{
    return wxT("libopus");
}

TranslatableString OpusImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> OpusImportPlugin::Open(const FilePath& filename, AudacityProject*)
{
    auto handle = std::make_unique<OpusImportFileHandle>(filename);

    if (!handle->IsOpen()) {
        return {};
    }

    return std::move(handle);
}

// ============================================================================
// OpusImportFileHandle
// ============================================================================

OpusImportFileHandle::OpusImportFileHandle(const FilePath& filename)
    : ImportFileHandleEx{filename}
{
    // Try to open the file for reading
    if (!mFile.Open(filename, wxFile::read)) {
        return;
    }

    OpusFileCallbacks callbacks = {
        OpusReadCallback,
        OpusSeekCallback,
        OpusTellCallback,
        OpusCloseCallback
    };

    int error = 0;
    mOpusFile = op_open_callbacks(this, &callbacks, nullptr, 0, &error);

    if (mOpusFile == nullptr) {
        LogOpusError("Error while opening Opus file", error);
        return;
    }

    mNumChannels = op_channel_count(mOpusFile, -1);
    mNumSamples = op_pcm_total(mOpusFile, -1);
}

TranslatableString OpusImportFileHandle::GetFileDescription()
{
    return DESC;
}

auto OpusImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
    return 0;
}

void OpusImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
    TrackHolders& outTracks, Tags* tags,
    std::optional<LibFileFormats::AcidizerTags>&)
{
    BeginImport();

    outTracks.clear();

    auto track = ImportUtils::NewWaveTrack(
        *trackFactory,
        mNumChannels,
        mFormat,
        mSampleRate);

    /* The number of samples to read in each loop */
    const size_t SAMPLES_TO_READ = track->GetMaxBlockSize();
    uint64_t totalSamplesRead = 0;

    const auto bufferSize = mNumChannels * SAMPLES_TO_READ;

    ArrayOf<float> floatBuffer { bufferSize };

    uint64_t samplesRead = 0;

    do{
        int linkIndex { -1 };
        auto samplesPerChannelRead = op_read_float(mOpusFile, floatBuffer.get(), SAMPLES_TO_READ, &linkIndex);

        if (samplesPerChannelRead < 0 && samplesPerChannelRead != OP_HOLE) {
            NotifyImportFailed(progressListener, samplesPerChannelRead);
            return;
        }

        auto linkChannels = op_head(mOpusFile, linkIndex)->channel_count;

        if (linkChannels != mNumChannels) {
            NotifyImportFailed(progressListener, XO("File has changed the number of channels in the middle."));
            return;
        }

        unsigned chn = 0;
        ImportUtils::ForEachChannel(*track, [&](auto& channel)
        {
            channel.AppendBuffer(
                reinterpret_cast<constSamplePtr>(floatBuffer.get()
                                                 + chn), mFormat, samplesPerChannelRead, mNumChannels, mFormat
                );
            ++chn;
        });

        samplesRead = samplesPerChannelRead;
        totalSamplesRead += samplesRead;

        progressListener.OnImportProgress(double(totalSamplesRead) / mNumSamples);
    } while (!IsCancelled() && !IsStopped() && samplesRead != 0);

    if (IsCancelled()) {
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Cancelled);
        return;
    }

    if (totalSamplesRead < mNumSamples && !IsStopped()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }

    ImportUtils::FinalizeImport(outTracks, *track);

    auto opusTags = op_tags(mOpusFile, -1);

    if (opusTags != nullptr) {
        for (int i = 0; i < opusTags->comments; ++i) {
            const auto comment = opusTags->user_comments[i];
            const auto commentLength = opusTags->comment_lengths[i];

            std::string_view tag { comment,
                                   std::string_view::size_type(commentLength) };

            const auto separator = tag.find('=');

            if (separator != std::string_view::npos) {
                auto name = audacity::ToWXString(tag.substr(0, separator));
                const auto value = audacity::ToWXString(tag.substr(separator + 1));

                // See: ImportOGG.cpp tags parsing
                if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
                    long val;

                    if (value.length() == 4 && value.ToLong(&val)) {
                        name = TAG_YEAR;
                    }
                }

                tags->SetTag(name, value);
            }
        }
    }

    progressListener.OnImportResult(IsStopped()
                                    ? ImportProgressListener::ImportResult::Stopped
                                    : ImportProgressListener::ImportResult::Success);
}

wxInt32 OpusImportFileHandle::GetStreamCount()
{
    return 1;
}

const TranslatableStrings& OpusImportFileHandle::GetStreamInfo()
{
    static TranslatableStrings empty;
    return empty;
}

void OpusImportFileHandle::SetStreamUsage(wxInt32, bool)
{
}

int OpusImportFileHandle::OpusReadCallback(
    void* pstream, unsigned char* ptr, int nbytes)
{
    auto stream = static_cast<OpusImportFileHandle*>(pstream);

    if (!stream->mFile.IsOpened()) {
        return EOF;
    }

    // OpusFile never reads more than 2^31 bytes at a time,
    // so we can safely cast ssize_t to int.
    return int(stream->mFile.Read(ptr, nbytes));
}

int OpusImportFileHandle::OpusSeekCallback(
    void* pstream, opus_int64 offset, int whence)
{
    auto stream = static_cast<OpusImportFileHandle*>(pstream);

    if (!stream->mFile.IsOpened()) {
        return -1;
    }

    wxSeekMode wxWhence = whence == SEEK_SET ? wxFromStart
                          : whence == SEEK_CUR ? wxFromCurrent
                          : whence == SEEK_END ? wxFromEnd : wxFromStart;

    return stream->mFile.Seek(offset, wxWhence) != wxInvalidOffset ? 0 : -1;
}

opus_int64 OpusImportFileHandle::OpusTellCallback(void* pstream)
{
    auto stream = static_cast<OpusImportFileHandle*>(pstream);

    return static_cast<opus_int64>(stream->mFile.Tell());
}

int OpusImportFileHandle::OpusCloseCallback(void* pstream)
{
    auto stream = static_cast<OpusImportFileHandle*>(pstream);

    if (stream->mFile.IsOpened()) {
        return stream->mFile.Close() ? 0 : EOF;
    }

    return 0;
}

TranslatableString OpusImportFileHandle::GetOpusErrorString(int error)
{
    switch (error) {
    case OP_EREAD:
        return XO("IO error reading from file");
    case OP_EFAULT:
        return XO("internal error");
    case OP_EIMPL:
        return XO("not implemented");
    case OP_EINVAL:
        return XO("invalid argument");
    case OP_ENOTFORMAT:
        return XO("not an Opus file");
    case OP_EBADHEADER:
        return XO("invalid header");
    case OP_EVERSION:
        return XO("unsupported version");
    case OP_EBADPACKET:
        return XO("invalid packet");
    case OP_EBADLINK:
        return XO("invalid stream structure");
    case OP_ENOSEEK:
        return XO("stream is not seekable");
    case OP_EBADTIMESTAMP:
        return XO("invalid timestamp");
    default:
        return {};
    }
}

void OpusImportFileHandle::LogOpusError(const char* method, int error)
{
    if (error == 0) {
        return;
    }

    if (error == OP_ENOTFORMAT) {
        wxLogDebug("%s: Not Opus format", GetOpusErrorString(error).Translation());
    } else {
        wxLogError("%s: %s", method, GetOpusErrorString(error).Translation());
    }
}

void OpusImportFileHandle::NotifyImportFailed(
    ImportProgressListener& progressListener, int error)
{
    NotifyImportFailed(progressListener, GetOpusErrorString(error));
}

void OpusImportFileHandle::NotifyImportFailed(
    ImportProgressListener& progressListener, const TranslatableString& error)
{
    ImportUtils::ShowMessageBox(
        XO("Failed to decode Opus file: %s").Format(error));

    if (IsCancelled()) {
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Cancelled);
    } else if (!IsStopped()) {
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Error);
    } else {
        progressListener.OnImportResult(
            ImportProgressListener::ImportResult::Stopped);
    }
}

bool OpusImportFileHandle::IsOpen() const
{
    return mOpusFile != nullptr;
}

OpusImportFileHandle::~OpusImportFileHandle()
{
    if (mOpusFile != nullptr) {
        op_free(mOpusFile);
    }
}
