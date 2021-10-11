/**********************************************************************

  Audacity: A Digital Audio Editor

  FileConfig.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_FILECONFIG__
#define __AUDACITY_WIDGETS_FILECONFIG__

#include <memory>

#include <wx/defs.h>
#include <wx/fileconf.h>
#include "Identifier.h"

class PREFERENCES_API FileConfig : public wxConfigBase
{
public:
   FileConfig(const wxString& appName = wxEmptyString,
              const wxString& vendorName = wxEmptyString,
              const wxString& localFilename = wxEmptyString,
              const wxString& globalFilename = wxEmptyString,
              long style = wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_GLOBAL_FILE,
              const wxMBConv& conv = wxConvAuto());
   void Init();
   virtual ~FileConfig();

   virtual void SetPath(const wxString& strPath) wxOVERRIDE;
   virtual const wxString& GetPath() const wxOVERRIDE;
   virtual bool GetFirstGroup(wxString& str, long& lIndex) const wxOVERRIDE;
   virtual bool GetNextGroup(wxString& str, long& lIndex) const wxOVERRIDE;
   virtual bool GetFirstEntry(wxString& str, long& lIndex) const wxOVERRIDE;
   virtual bool GetNextEntry(wxString& str, long& lIndex) const wxOVERRIDE;
   virtual size_t GetNumberOfEntries(bool bRecursive = false) const wxOVERRIDE;
   virtual size_t GetNumberOfGroups(bool bRecursive = false) const wxOVERRIDE;
   virtual bool HasGroup(const wxString& strName) const wxOVERRIDE;
   virtual bool HasEntry(const wxString& strName) const wxOVERRIDE;
   virtual bool Flush(bool bCurrentOnly = false) wxOVERRIDE;
   virtual bool RenameEntry(const wxString& oldName, const wxString& newName) wxOVERRIDE;
   virtual bool RenameGroup(const wxString& oldName, const wxString& newName) wxOVERRIDE;
   virtual bool DeleteEntry(const wxString& key, bool bDeleteGroupIfEmpty = true) wxOVERRIDE;
   virtual bool DeleteGroup(const wxString& key) wxOVERRIDE;
   virtual bool DeleteAll() wxOVERRIDE;

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
   virtual bool DoReadString(const wxString& key, wxString *pStr) const wxOVERRIDE;
   virtual bool DoReadLong(const wxString& key, long *pl) const wxOVERRIDE;
#if wxUSE_BASE64
   virtual bool DoReadBinary(const wxString& key, wxMemoryBuffer* buf) const wxOVERRIDE;
#endif // wxUSE_BASE64

   virtual bool DoWriteString(const wxString& key, const wxString& szValue) wxOVERRIDE;
   virtual bool DoWriteLong(const wxString& key, long lValue) wxOVERRIDE;
#if wxUSE_BASE64
   virtual bool DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf) wxOVERRIDE;
#endif // wxUSE_BASE64

protected:
   //! Override to notify the user of error conditions involving writability of config files
   virtual void Warn() = 0;

   const FilePath &GetFilePath() const { return mLocalFilename; }

private:
   const wxString mAppName;
   const wxString mVendorName;
   const wxString mLocalFilename;
   const wxString mGlobalFilename;
   const long mStyle;
   const wxMBConv & mConv;

   std::unique_ptr<wxFileConfig> mConfig;

   // values of the version major/minor/micro keys in audacity.cfg
   // when Audacity first opens
   int mVersionMajorKeyInit{};
   int mVersionMinorKeyInit{};
   int mVersionMicroKeyInit{};

   bool mDirty;
};

#endif

