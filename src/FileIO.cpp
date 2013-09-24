/**********************************************************************

  Audacity: A Digital Audio Editor
  
  FileIO.cpp
 
  Leland Lucius

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/filename.h>
#include <wx/wfstream.h>

#include "FileIO.h"

FileIO::FileIO(const wxString name, FileIOMode mode)
: mName(name),
  mMode(mode),
  mFtp(NULL),
  mInputStream(NULL),
  mOutputStream(NULL),
  mOpen(false)
{
   wxString scheme;

   mUri.Create(mName);
   scheme = mUri.GetScheme();

   if (scheme == wxT("ftp")) {
      wxString user = mUri.GetUser();
      wxString pass = mUri.GetPassword();;

      mFtp = new wxFTP();
      mFtp->SetDefaultTimeout(60);

      if (!user.IsEmpty()) {
         mFtp->SetUser(user);
      }

      if (!pass.IsEmpty()) {
         mFtp->SetPassword(pass);
      }

      if (!mFtp->Connect(mUri.GetServer())) {
         wxPrintf(wxT("Couldn't connect to server\n"));

         delete mFtp;
         mFtp = NULL;

         return;
      }

      mFtp->SetBinary();
      mFtp->SetPassive(true);

      if (mMode == FileIO::Input) {
         mInputStream = mFtp->GetInputStream(mUri.GetPath());
         if (mInputStream == NULL) {
            wxPrintf(wxT("Couldn't get input stream: %s\n"), mUri.GetPath().c_str());
   
            delete mFtp;
            mFtp = NULL;
   
            return;
         }
      }
      else {
         mOutputStream = mFtp->GetOutputStream(mUri.GetPath());
         if (mOutputStream == NULL) {
            wxPrintf(wxT("Couldn't get output stream: %s\n"), mUri.GetPath().c_str());
   
            delete mFtp;
            mFtp = NULL;
   
            return;
         }
      }

      mOpen = true;
   }
   else {
      if (mMode == FileIO::Input) {
         mInputStream = new wxFFileInputStream(mName);
         if (mInputStream == NULL) {
            wxPrintf(wxT("Couldn't get input stream: %s\n"), name.c_str());
            return;
         }
      }
      else {
         mOutputStream = new wxFFileOutputStream(mName);
         if (mOutputStream == NULL) {
            wxPrintf(wxT("Couldn't get output stream: %s\n"), name.c_str());
            return;
         }
      }

      mOpen = true;
   }
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
   if (mOutputStream) {
      delete mOutputStream;
      mOutputStream = NULL;
   }

   if (mInputStream) {
      delete mInputStream;
      mInputStream = NULL;
   }

   if (mFtp) {
      delete mFtp;
      mFtp = NULL;
   }

   SetCatalogInfo();

   mOpen = false;
}

// MacOS: set the file type/creator so that the OS knows it's an MP3
// file which was created by Audacity
      
void FileIO::SetCatalogInfo()
{
#ifdef __WXMAC__
   if (!mOpen || mFtp != NULL) {
      return;
   }

   wxUint32 type;
   wxFileName fn(mName);
   wxString ext = fn.GetExt().MakeUpper() + wxT("    ");

   type = (ext[0] & 0xff) << 24 |
          (ext[1] & 0xff) << 16 |
          (ext[2] & 0xff) << 8  |
          (ext[3] & 0xff);

   SetCatalogInfo(type);
#endif

   return;
}

void FileIO::SetCatalogInfo(wxUint32 type)
{
#ifdef __WXMAC__
   if (!mOpen || mFtp != NULL) {
      return;
   }

   wxFileName fn(mName);

   fn.MacSetTypeAndCreator(type, AUDACITY_CREATOR);
#endif

   return;
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
