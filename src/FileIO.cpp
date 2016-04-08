/**********************************************************************

  Audacity: A Digital Audio Editor

  FileIO.cpp

  Leland Lucius

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/crt.h>
#include <wx/filename.h>
#include <wx/wfstream.h>

#include "FileIO.h"

FileIO::FileIO(const wxString & name, FileIOMode mode)
: mName(name),
  mMode(mode),
  mOpen(false)
{
   wxString scheme;

      if (mMode == FileIO::Input) {
         mInputStream = std::make_unique<wxFFileInputStream>(mName);
         if (mInputStream == NULL || !mInputStream->IsOk()) {
            wxPrintf(wxT("Couldn't get input stream: %s\n"), name.c_str());
            return;
         }
      }
      else {
         mOutputStream = std::make_unique<wxFFileOutputStream>(mName);
         if (mOutputStream == NULL || !mOutputStream->IsOk()) {
            wxPrintf(wxT("Couldn't get output stream: %s\n"), name.c_str());
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

void FileIO::Close()
{
   mOutputStream.reset();
   mInputStream.reset();
   mOpen = false;
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
