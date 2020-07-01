/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.h

*******************************************************************/

#ifndef __AUDACITY_AUTORECOVERY__
#define __AUDACITY_AUTORECOVERY__

#include "xml/XMLTagHandler.h"

#include <wx/mstream.h> // member variables

#include <unordered_map>
#include "audacity/Types.h"

///
/// AutoSaveFile
///

using NameMap = std::unordered_map<wxString, short>;
using IdMap = std::unordered_map<short, wxString>;

// This class's overrides do NOT throw AudacityException.
class AUDACITY_DLL_API AutoSaveFile final : public XMLWriter
{
public:

   static TranslatableString FailureMessage( const FilePath &filePath );

   AutoSaveFile(size_t allocSize = 1024 * 1024);
   virtual ~AutoSaveFile();

   void StartTag(const wxString & name) override;
   void EndTag(const wxString & name) override;

   void WriteAttr(const wxString & name, const wxString &value) override;
   void WriteAttr(const wxString & name, const wxChar *value) override;

   void WriteAttr(const wxString & name, int value) override;
   void WriteAttr(const wxString & name, bool value) override;
   void WriteAttr(const wxString & name, long value) override;
   void WriteAttr(const wxString & name, long long value) override;
   void WriteAttr(const wxString & name, size_t value) override;
   void WriteAttr(const wxString & name, float value, int digits = -1) override;
   void WriteAttr(const wxString & name, double value, int digits = -1) override;

   void WriteData(const wxString & value) override;
   void Write(const wxString & data) override;

   // Non-override functions
   void WriteSubTree(const AutoSaveFile & value);

   const wxMemoryBuffer &GetDict() const;
   const wxMemoryBuffer &GetData() const;

   bool IsEmpty() const;
   bool DictChanged() const;

   static wxString Decode(const wxMemoryBuffer &buffer);

private:
   void WriteName(const wxString & name);

private:
   wxMemoryBuffer mBuffer;
   bool mDictChanged;

   static NameMap mNames;
   static wxMemoryBuffer mDict;
};

#endif
