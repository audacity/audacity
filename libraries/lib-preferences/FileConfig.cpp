/**********************************************************************

  Audacity: A Digital Audio Editor

  FileConfig.cpp

  Leland Lucius

**********************************************************************/

#include <errno.h>
#include <wx/wfstream.h>

#include "FileConfig.h"

#include <cerrno> // for ENOENT

#if !defined(F_OK)
#define F_OK 0x00
#endif
#if !defined(W_OK)
#define W_OK 0x02
#endif
#if !defined(R_OK)
#define R_OK 0x04
#endif

FileConfig::FileConfig(const wxString& appName,
                       const wxString& vendorName,
                       const wxString& localFilename,
                       const wxString& globalFilename,
                       long style,
                       const wxMBConv& conv)
:  wxConfigBase(appName, vendorName, localFilename, globalFilename, style),
   mAppName(appName),
   mVendorName(vendorName),
   mLocalFilename(localFilename),
   mGlobalFilename(globalFilename),
   mStyle(style),
   mConv(conv),
   mDirty(false)
{
}

void FileConfig::Init()
{
   while (true)
   {
      mConfig = std::make_unique<wxFileConfig>
         (mAppName, mVendorName, mLocalFilename, mGlobalFilename, mStyle, mConv);

      // Prevent wxFileConfig from attempting a Flush() during object deletion. This happens
      // because we don't use the wxFileConfig::Flush() method and so the wxFileConfig dirty
      // flag never gets reset. During deletion, the dirty flag is checked and a Flush()
      // performed. This can (and probably will) create bogus temporary files.
      mConfig->DisableAutoSave();

      bool canRead = false;
      bool canWrite = false;
      int fd;

      fd = wxOpen(mLocalFilename, O_RDONLY, S_IREAD);
      if (fd != -1 || errno == ENOENT)
      {
         canRead = true;
         if (fd != -1)
         {
            wxClose(fd);
         }
      }

      fd = wxOpen(mLocalFilename, O_WRONLY | O_CREAT, S_IWRITE);
      if (fd != -1)
      {
         canWrite = true;
         wxClose(fd);
      }

      if (canRead && canWrite)
      {
         break;
      }

      Warn();
   }
}

FileConfig::~FileConfig()
{
   wxASSERT(mDirty == false);
}

void FileConfig::SetPath(const wxString& strPath)
{
   mConfig->SetPath(strPath);
}

const wxString& FileConfig::GetPath() const
{
   return mConfig->GetPath();
}

bool FileConfig::GetFirstGroup(wxString& str, long& lIndex) const
{
   return mConfig->GetFirstGroup(str, lIndex);
}

bool FileConfig::GetNextGroup(wxString& str, long& lIndex) const
{
   return mConfig->GetNextGroup(str, lIndex);
}

bool FileConfig::GetFirstEntry(wxString& str, long& lIndex) const
{
   return mConfig->GetFirstEntry(str, lIndex);
}

bool FileConfig::GetNextEntry(wxString& str, long& lIndex) const
{
   return mConfig->GetNextEntry(str, lIndex);
}

size_t FileConfig::GetNumberOfEntries(bool bRecursive) const
{
   return mConfig->GetNumberOfEntries(bRecursive);
}

size_t FileConfig::GetNumberOfGroups(bool bRecursive) const
{
   return mConfig->GetNumberOfGroups(bRecursive);
}

bool FileConfig::HasGroup(const wxString& strName) const
{
   return mConfig->HasGroup(strName);
}

bool FileConfig::HasEntry(const wxString& strName) const
{
   return mConfig->HasEntry(strName);
}

bool FileConfig::Flush(bool WXUNUSED(bCurrentOnly))
{
   if (!mDirty)
   {
      return true;
   }

   while (true)
   {
      FilePath backup = mLocalFilename + ".bkp";

      if (!wxFileExists(backup) || (wxRemove(backup) == 0))
      {
         if (!wxFileExists(mLocalFilename) || (wxRename(mLocalFilename, backup) == 0))
         {
            wxFileOutputStream stream(mLocalFilename);
            if (stream.IsOk())
            {
               if (mConfig->Save(stream))
               {
                  stream.Sync();
                  if (stream.IsOk() && stream.Close())
                  {
                     if (!wxFileExists(backup) || (wxRemove(backup) == 0))
                     {
                        mDirty = false;
                        return true;
                     }
                  }
               }
            }

            if (wxFileExists(backup))
            {
               wxRemove(mLocalFilename);
               wxRename(backup, mLocalFilename);
            }
         }
      }

      Warn();
   }

   return false;
}

bool FileConfig::RenameEntry(const wxString& oldName, const wxString& newName)
{
   auto res = mConfig->RenameEntry(oldName, newName);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::RenameGroup(const wxString& oldName, const wxString& newName)
{
   auto res = mConfig->RenameGroup(oldName, newName);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DeleteEntry(const wxString& key, bool bDeleteGroupIfEmpty)
{
   auto res = mConfig->DeleteEntry(key, bDeleteGroupIfEmpty);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DeleteGroup(const wxString& key)
{
   auto res = mConfig->DeleteGroup(key);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DeleteAll()
{
   auto res = mConfig->DeleteAll();
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DoReadString(const wxString& key, wxString *pStr) const
{
   return mConfig->Read(key, pStr);
}

bool FileConfig::DoReadLong(const wxString& key, long *pl) const
{
   return mConfig->Read(key, pl);
}

#if wxUSE_BASE64
bool FileConfig::DoReadBinary(const wxString& key, wxMemoryBuffer* buf) const
{
   return mConfig->Read(key, buf);
}
#endif // wxUSE_BASE64

bool FileConfig::DoWriteString(const wxString& key, const wxString& szValue)
{
   bool res = mConfig->Write(key, szValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DoWriteLong(const wxString& key, long lValue)
{
   bool res = mConfig->Write(key, lValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

#if wxUSE_BASE64
bool FileConfig::DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf)
{
   bool res = mConfig->Write(key, buf);
   if (res)
   {
      mDirty = true;
   }
   return res;
}
#endif // wxUSE_BASE64
