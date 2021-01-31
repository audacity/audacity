/**********************************************************************

  Audacity: A Digital Audio Editor

  FileConfig.cpp

  Leland Lucius

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/bmpbuttn.h>
#include <wx/button.h>
#include <wx/filefn.h>
#include <wx/fileconf.h>
#include <wx/sizer.h>
#include <wx/wfstream.h>

#include "FileConfig.h"

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
:  wxFileConfig(appName, vendorName, localFilename, globalFilename, style, conv),
   mConfigPath(localFilename),
   mDirty(false)
{
}

void FileConfig::Init()
{
   // Prevent wxFileConfig from attempting a Flush() during object deletion. This happens
   // because we don't use the wxFileConfig::Flush() method and so the wxFileConfig dirty
   // flag never gets reset. During deleting it is checked and a Flush() performed. This
   // can (and probably will) create bogus temporary files.
   DisableAutoSave();

   while (true)
   {
      bool canRead = false;
      bool canWrite = false;
      int fd;

      fd = wxOpen(mConfigPath, O_RDONLY, S_IREAD);
      if (fd != -1 || errno == ENOENT)
      {
         canRead = true;
         if (fd != -1)
         {
            wxClose(fd);
         }
      }

      fd = wxOpen(mConfigPath, O_WRONLY | O_CREAT, S_IWRITE);
      if (fd != -1)
      {
         canWrite = true;
         wxClose(fd);
      }

      if (canRead && canWrite)
      {
         break;
      }

      // If we can't read an existing config file, we must not allow the user to retry
      // since the wxFileConfig initialization will not have read it and the caller
      // will assume that the file didn't exist and possibly initialize it. This
      // could lead to wiping out the original contents.
      //
      // If the wxFileConfig class allowed us to call wxFileConfig::Init(), we wouldn't
      // have to do all this mess.
      // (Note that invocation of virtual Warn() can't be done in the ctor,
      // which is why this is two-phase construction.)
      Warn(canRead == true);
   }
}

FileConfig::~FileConfig()
{
   wxASSERT(mDirty == false);
}

bool FileConfig::Flush(bool WXUNUSED(bCurrentOnly))
{
   if (!mDirty)
   {
      return true;
   }

   while (true)
   {
      FilePath backup = mConfigPath + ".bkp";

      if (!wxFileExists(backup) || (wxRemove(backup) == 0))
      {
         if (!wxFileExists(mConfigPath) || (wxRename(mConfigPath, backup) == 0))
         {
            wxFileOutputStream stream(mConfigPath);
            if (stream.IsOk())
            {
               if (Save(stream))
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
               wxRemove(mConfigPath);
               wxRename(backup, mConfigPath);
            }
         }
      }

      Warn();
   }

   return false;
}

bool FileConfig::DoWriteString(const wxString& key, const wxString& szValue)
{
   bool res = wxFileConfig::DoWriteString(key, szValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DoWriteLong(const wxString& key, long lValue)
{
   bool res = wxFileConfig::DoWriteLong(key, lValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

#if wxUSE_BASE64
bool FileConfig::DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf)
{
   bool res = wxFileConfig::DoWriteBinary(key, buf);
   if (res)
   {
      mDirty = true;
   }
   return res;
}
#endif // wxUSE_BASE64
