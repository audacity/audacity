/**********************************************************************

  Audacity: A Digital Audio Editor

  FileIO.cpp

  Leland Lucius

**********************************************************************/

#include "FileIO.h"

#include <wx/defs.h>
#include <wx/crt.h>
#include <wx/filename.h>
#include <wx/wfstream.h>
#include "wxFileNameWrapper.h"

FileIO::FileIO(const wxFileNameWrapper & name, FileIOMode mode)
: mMode(mode),
  mOpen(false)
{
   wxString scheme;

      auto path = name.GetFullPath();
      if (mMode == FileIO::Input) {
         mInputStream = std::make_unique<wxFFileInputStream>(path);
         if (mInputStream == NULL || !mInputStream->IsOk()) {
            wxPrintf(wxT("Couldn't get input stream: %s\n"), path);
            return;
         }
      }
      else {
         mOutputStream = std::make_unique<wxFFileOutputStream>(path);
         if (mOutputStream == NULL || !mOutputStream->IsOk()) {
            wxPrintf(wxT("Couldn't get output stream: %s\n"), path);
            return;
         }
      }

      mOpen = true;
}

FileIO::~FileIO()
{
   Close();
}

bool FileIO::IsOpened()
{
   return mOpen;
}

bool FileIO::Close()
{
   bool success = true;
   if (mOutputStream) {
      // mOutputStream->Sync() returns void!  Rrr!
      success = mOutputStream->GetFile()->Flush() &&
         mOutputStream->Close();
      mOutputStream.reset();
   }
   mInputStream.reset();
   mOpen = false;
   return success;
}

wxInputStream & FileIO::Read(void *buf, size_t size)
{
   if (mInputStream == NULL) {
      return *mInputStream;
   }

   return mInputStream->Read(buf, size);
}

wxOutputStream & FileIO::Write(const void *buf, size_t size)
{
   if (mOutputStream == NULL) {
      return *mOutputStream;
   }

   return mOutputStream->Write(buf, size);
}
