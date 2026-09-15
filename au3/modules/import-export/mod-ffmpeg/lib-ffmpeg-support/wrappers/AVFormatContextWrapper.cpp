/**********************************************************************

  Audacity: A Digital Audio Editor

  AVFormatContextWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVFormatContextWrapper.h"

#include "../FFmpegFunctions.h"

#include "AVInputFormatWrapper.h"
#include "AVOutputFormatWrapper.h"
#include "AVStreamWrapper.h"

AVFormatContextWrapper::AVFormatContextWrapper(const FFmpegFunctions& ffmpeg) noexcept
    : mFFmpeg(ffmpeg)
{
}

AVFormatContext* AVFormatContextWrapper::GetWrappedValue() noexcept
{
    return mAVFormatContext;
}

const AVFormatContext* AVFormatContextWrapper::GetWrappedValue() const noexcept
{
    return mAVFormatContext;
}

AVFormatContextWrapper::~AVFormatContextWrapper()
{
    if (mAVFormatContext == nullptr) {
        return;
    }

    if (mIsInputContext && mFFmpeg.avformat_close_input != nullptr) {
        // Also runs the demuxer's read_close hook, which frees whatever it
        // allocated inside its private data. Freeing the context alone leaks
        // that: measured at roughly 0.9 MB per open for Matroska, MP4 and
        // WebM, and far more for MPEG-TS, which allocates per PID.
        //
        // This does not touch the AVIOContext that mAVIOContext owns.
        // avformat_open_input sets AVFMT_FLAG_CUSTOM_IO when it is handed a
        // context whose pb is already set, and avformat_close_input skips
        // avio_close for exactly that case.
        mFFmpeg.avformat_close_input(&mAVFormatContext);
        return;
    }

    mFFmpeg.avformat_free_context(mAVFormatContext);
}

AVIOContextWrapper::OpenResult AVFormatContextWrapper::OpenInputContext(
    const wxString& path,
    const AVInputFormatWrapper* inputFormat,
    AVDictionaryWrapper options)
{
    auto ioContext = mFFmpeg.CreateAVIOContext();

    const auto result = ioContext->Open(path, false);

    if (result != AVIOContextWrapper::OpenResult::Success) {
        return result;
    }

    SetAVIOContext(std::move(ioContext));

    AVDictionary* dict = options.Release();

    /*
       Documentation for the last argument:
       "A dictionary filled with AVFormatContext and demuxer-private options.
       On return this parameter will be destroyed and replaced with a
       dict containing options that were not found.
       May be NULL."
     */
    int rc = (mFFmpeg.avformat_open_input(
                  &mAVFormatContext, path.c_str(),
                  inputFormat != nullptr ? inputFormat->GetWrappedValue() : nullptr,
                  &dict));

    // Don't leak the replacement dictionary
    AVDictionaryWrapper cleanup{ mFFmpeg, dict };

    if (rc) {
        // On failure libavformat has already freed the context and nulled the
        // pointer, so there is nothing left to own.
        return AVIOContextWrapper::OpenResult::InternalError;
    }

    // Set before find_stream_info: the context is open and needs the input
    // teardown from here on, even if the call below fails.
    mIsInputContext = true;

    if (mFFmpeg.avformat_find_stream_info(mAVFormatContext, nullptr) < 0) {
        return AVIOContextWrapper::OpenResult::InternalError;
    }

    UpdateStreamList();

    mInputFormat = mFFmpeg.CreateAVInputFormatWrapper(GetIFormat());

    return result;
}

AVIOContextWrapper::OpenResult
AVFormatContextWrapper::OpenOutputContext(const wxString& path)
{
    auto ioContext = mFFmpeg.CreateAVIOContext();

    const auto result = ioContext->Open(path, true);

    if (result != AVIOContextWrapper::OpenResult::Success) {
        return result;
    }

    SetAVIOContext(std::move(ioContext));

    return result;
}

std::unique_ptr<AVPacketWrapper> AVFormatContextWrapper::ReadNextPacket()
{
    std::unique_ptr<AVPacketWrapper> packet = mFFmpeg.CreateAVPacketWrapper();

    if (mFFmpeg.av_read_frame(mAVFormatContext, packet->GetWrappedValue()) < 0) {
        return {};
    }

    return packet;
}

std::unique_ptr<AVStreamWrapper> AVFormatContextWrapper::CreateStream()
{
    // The complementary deallocation happens in avformat_free_context
    AVStream* stream = mFFmpeg.avformat_new_stream(mAVFormatContext, nullptr);

    if (stream == nullptr) {
        return {};
    }

    UpdateStreamList();

    return mFFmpeg.CreateAVStreamWrapper(stream, true);
}

const AVInputFormatWrapper*
AVFormatContextWrapper::GetInputFormat() const noexcept
{
    return mInputFormat.get();
}

const AVOutputFormatWrapper*
AVFormatContextWrapper::GetOutputFormat() const noexcept
{
    return mOutputFormat.get();
}

const AVStreamWrapper*
AVFormatContextWrapper::GetStream(int index) const noexcept
{
    if (index < GetStreamsCount()) {
        return GetStreams()[index].get();
    }

    return nullptr;
}
