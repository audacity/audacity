/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedPrefs.cpp

  Dmitry Vedenko

**********************************************************************/
#include "MockedPrefs.h"

#include "Prefs.h"

#include <unordered_map>

class MockedFileConfig final : public FileConfig
{
public:
   void SetPath(const wxString& path) override
   {
      // To make GetPath to work consistently
      mPath = path;
   }

   const wxString& GetPath() const override
   {
      return mPath;
   }

   bool GetFirstGroup(wxString& str, long& lIndex) const override
   {
      return false;
   }

   bool GetNextGroup(wxString& str, long& lIndex) const override
   {
      return false;
   }

   bool GetFirstEntry(wxString& str, long& lIndex) const override
   {
      return false;
   }

   bool GetNextEntry(wxString& str, long& lIndex) const override
   {
      return false;
   }

   size_t GetNumberOfEntries(bool bRecursive = false) const override
   {
      return 0;
   }

   size_t GetNumberOfGroups(bool bRecursive = false) const override
   {
      return 0;
   }

   bool HasGroup(const wxString& strName) const override
   {
      return true;
   }

   bool HasEntry(const wxString& strName) const override
   {
      return true;
   }

   bool Flush(bool bCurrentOnly = false) override
   {
      return false;
   }

   bool RenameEntry(const wxString& oldName, const wxString& newName)
      override
   {
      return false;
   }

   bool RenameGroup(const wxString& oldName, const wxString& newName)
      override
   {
      return false;
   }

   bool DeleteEntry(const wxString& key, bool bDeleteGroupIfEmpty = true)
      override
   {
      return false;
   }

   bool DeleteGroup(const wxString& key) override
   {
      return false;
   }

   bool DeleteAll() override
   {
      return false;
   }

   bool
   DoReadString(const wxString& key, wxString* pStr) const override
   {
      auto it = mStringValues.find(key);

      if (it == mStringValues.end())
         return false;

      *pStr = it->second;
      return true;
   }

   bool DoReadLong(const wxString& key, long* pl) const override
   {
      auto it = mLongValues.find(key);

      if (it == mLongValues.end())
         return false;

      *pl = it->second;
      return true;
   }

   bool DoReadBinary(const wxString& key, wxMemoryBuffer* buf) const
      override
   {
      auto it = mBinaryValues.find(key);

      if (it == mBinaryValues.end())
         return false;

      *buf = it->second;
      return true;
   }

   bool DoWriteString(const wxString& key, const wxString& szValue)
      override
   {
      mStringValues[key] = szValue;
      return true;
   }

   bool DoWriteLong(const wxString& key, long lValue) override
   {
      mLongValues[key] = lValue;
      return true;
   }

   bool DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf)
      override
   {
      mBinaryValues[key] = buf;
      return true;
   }

   void Warn() override
   {
   }

private:
   wxString mPath;

   std::unordered_map<wxString, wxString> mStringValues;
   std::unordered_map<wxString, long> mLongValues;
   std::unordered_map<wxString, wxMemoryBuffer> mBinaryValues;

};

MockedPrefs::MockedPrefs()
    : mConfig { std::make_unique<MockedFileConfig>() }
{
   // MockedPrefs destructor will delete this object
   gPrefs = mConfig.get();
}

MockedPrefs::~MockedPrefs ()
{
   gPrefs = nullptr;
}
