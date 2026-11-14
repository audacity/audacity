/**********************************************************************

  Audacity: A Digital Audio Editor

  AVIOContextWrapper.cpp

  Dmitry Vedenko

**********************************************************************/

#include "AVIOContextWrapper.h"

#include "../FFmpegFunctions.h"

#define AVSEEK_FORCE 0x20000
#define AVSEEK_SIZE 0x10000

constexpr int BufferSize = 32 * 1024;

AVIOContextWrapper::AVIOContextWrapper(
    const FFmpegFunctions& ffmpeg) noexcept
    : mFFmpeg(ffmpeg)
{
}

AVIOContext* AVIOContextWrapper::GetWrappedValue() noexcept
{
    return mAVIOContext;
}

const AVIOContext* AVIOContextWrapper::GetWrappedValue() const noexcept
{
    return mAVIOContext;
}

AVIOContextWrapper::~AVIOContextWrapper()
{
    if (mAVIOContext == nullptr) {
        return;
    }

    if (mFFmpeg.avio_context_free != nullptr) {
        mFFmpeg.avio_context_free(&mAVIOContext);
    } else {
        mFFmpeg.av_free(mAVIOContext);
    }
}

AVIOContextWrapper::OpenResult
AVIOContextWrapper::Open(const wxString& fileName, bool forWriting)
{
    auto pFile = std::make_unique<wxFile>();
    if (!pFile->Open(fileName, forWriting ? wxFile::write : wxFile::read)) {
        return OpenResult::FileOpenFailed;
    }

    unsigned char* buffer
        =static_cast<unsigned char*>(mFFmpeg.av_malloc(BufferSize));

    if (buffer == nullptr) {
        return OpenResult::NoMemory;
    }

    /*
     "The buffer must be allocated with av_malloc() and friends. It may be freed
     and replaced with a new buffer by libavformat. AVIOContext.buffer holds the
     buffer currently in use, which must be later freed with av_free()."

     See ~AVIOContextWrapperImpl() for the deallocation
     */
    mAVIOContext = mFFmpeg.avio_alloc_context(
        buffer, BufferSize,
        static_cast<int>(forWriting),
        this,
        FileRead, FileWrite, FileSeek
        );

    if (mAVIOContext == nullptr) {
        mFFmpeg.av_free(buffer);
        return OpenResult::NoMemory;
    }

    mpFile = move(pFile);

    return OpenResult::Success;
}

int AVIOContextWrapper::FileRead(void* opaque, uint8_t* buf, int size)
{
    AVIOContextWrapper* wrapper = static_cast<AVIOContextWrapper*>(opaque);

    if (wrapper == nullptr) {
        return AUDACITY_AVERROR(EINVAL);
    }

    return wrapper->Read(buf, size);
}

int AVIOContextWrapper::FileWrite(void* opaque, const uint8_t* buf, int size)
{
    AVIOContextWrapper* wrapper = static_cast<AVIOContextWrapper*>(opaque);
    if (!(wrapper && wrapper->mpFile)) {
        return {};
    }
    return wrapper->mpFile->Write(buf, size);
}

int64_t AVIOContextWrapper::FileSeek(void* opaque, int64_t pos, int whence)
{
    AVIOContextWrapper* wrapper = static_cast<AVIOContextWrapper*>(opaque);
    if (!(wrapper && wrapper->mpFile)) {
        return {};
    }

    wxSeekMode mode = wxFromStart;

    switch (whence & ~AVSEEK_FORCE) {
    case (SEEK_SET):
        mode = wxFromStart;
        break;
    case (SEEK_CUR):
        mode = wxFromCurrent;
        break;
    case (SEEK_END):
        mode = wxFromEnd;
        break;
    case (AVSEEK_SIZE):
        return wrapper->mpFile->Length();
    }

    return wrapper->mpFile->Seek(pos, mode);
}
