/**********************************************************************

  Audacity: A Digital Audio Editor

  AVIOContextWrapper.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <memory>

#include <wx/file.h>

struct FFmpegFunctions;
typedef struct AVIOContext AVIOContext;

class FFMPEG_SUPPORT_API AVIOContextWrapper
{
public:
   enum class OpenResult
   {
      Success,
      FileOpenFailed,
      NoMemory,
      InternalError
   };

   AVIOContextWrapper(const AVIOContextWrapper&) = delete;
   AVIOContextWrapper& operator=(AVIOContextWrapper&) = delete;

   AVIOContextWrapper(AVIOContextWrapper&&) = delete;
   AVIOContextWrapper& operator=(AVIOContextWrapper&&) = delete;

   explicit AVIOContextWrapper(const FFmpegFunctions& ffmpeg) noexcept;

   AVIOContext* GetWrappedValue() noexcept;
   const AVIOContext* GetWrappedValue() const noexcept;

   virtual ~AVIOContextWrapper();

   OpenResult Open(const wxString& fileName, bool forWriting);

   virtual unsigned char* GetBuffer() const noexcept = 0;
   virtual int GetBufferSize() const noexcept = 0;
   virtual unsigned char* GetBufPtr() const noexcept = 0;
   virtual unsigned char* GetBufEnd() const noexcept = 0;

   virtual void* GetOpaque() const noexcept = 0;
   virtual void SetOpaque(void* opaque) noexcept = 0;

   virtual int64_t GetPos() const noexcept = 0;

   virtual int GetEofReached() const noexcept = 0;

   virtual int GetWriteFlag() const noexcept = 0;
   virtual void SetWriteFlag(int write_flag) noexcept = 0;

   virtual int GetError() const noexcept = 0;
   virtual void SetError(int error) noexcept = 0;

   virtual int GetSeekable() const noexcept = 0;
   virtual void SetSeekable(int seekable) noexcept = 0;

   virtual int GetDirect() const noexcept = 0;
   virtual void SetDirect(int direct) noexcept = 0;
protected:
   const FFmpegFunctions& mFFmpeg;
   AVIOContext* mAVIOContext { nullptr };

private:
   static int FileRead(void* opaque, uint8_t* buf, int size);
   static int FileWrite(void* opaque, const uint8_t* buf, int size);
   static int64_t FileSeek(void* opaque, int64_t pos, int whence);

   //! This is held indirectly by unique_ptr just so it can be swapped
   std::unique_ptr<wxFile> mpFile;
};
