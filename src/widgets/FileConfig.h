/**********************************************************************

  Audacity: A Digital Audio Editor

  FileConfig.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_FILECONFIG__
#define __AUDACITY_WIDGETS_FILECONFIG__

#include <wx/defs.h>
#include <wx/fileconf.h>

#include "audacity/Types.h"

class FileConfig : public wxFileConfig
{
public:
   FileConfig(const wxString& appName = wxEmptyString,
              const wxString& vendorName = wxEmptyString,
              const wxString& localFilename = wxEmptyString,
              const wxString& globalFilename = wxEmptyString,
              long style = wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_GLOBAL_FILE,
              const wxMBConv& conv = wxConvAuto());
   virtual ~FileConfig();

   virtual bool Flush(bool bCurrentOnly = false) wxOVERRIDE;

   // Set and Get values of the version major/minor/micro keys in audacity.cfg when Audacity first opens
   void SetVersionKeysInit( int major, int minor, int micro)
   {
      mVersionMajorKeyInit = major;
      mVersionMinorKeyInit = minor;
      mVersionMicroKeyInit = micro;
   }
   void GetVersionKeysInit( int& major, int& minor, int& micro) const
   {
      major = mVersionMajorKeyInit;
      minor = mVersionMinorKeyInit;
      micro = mVersionMicroKeyInit;
   }

protected:
   virtual bool DoWriteString(const wxString& key, const wxString& szValue) wxOVERRIDE;
   virtual bool DoWriteLong(const wxString& key, long lValue) wxOVERRIDE;
#if wxUSE_BASE64
   virtual bool DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf) wxOVERRIDE;
#endif // wxUSE_BASE64

private:
   void Warn(bool canRetry = true);

   const FilePath mConfigPath;

   // values of the version major/minor/micro keys in audacity.cfg
   // when Audacity first opens
   int mVersionMajorKeyInit{};
   int mVersionMinorKeyInit{};
   int mVersionMicroKeyInit{};

   bool mDirty;
};

#endif

