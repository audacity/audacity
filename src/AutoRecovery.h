/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.h

*******************************************************************/

#ifndef __AUDACITY_AUTORECOVERY__
#define __AUDACITY_AUTORECOVERY__

#include "xml/XMLTagHandler.h"
#include "xml/XMLWriter.h"

#include <wx/debug.h>
#include <wx/hashmap.h>
#include <wx/mstream.h>

#include <unordered_map>

class wxFFile;
class AudacityProject;

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Audacity startup.
//
// This function possibly opens NEW project windows while it recovers all
// projects. If so, it will re-use *pproj, if != NULL and set it to NULL.
//
// Returns: True, if the start of Audacity should continue as normal
//          False if Audacity should be quit immediately
//
// The didRecoverAnything param is strictly for a return value.
// Any value passed in is ignored.
//
bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj,
                                    bool *didRecoverAnything);

//
// XML Handler for a <recordingrecovery> tag
//
class RecordingRecoveryHandler final : public XMLTagHandler
{
public:
   RecordingRecoveryHandler(AudacityProject* proj);
   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   void HandleXMLEndTag(const wxChar *tag) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;

   // This class only knows reading tags

private:

   int FindTrack() const;

   AudacityProject* mProject;
   int mChannel;
   int mNumChannels;
   int mAutoSaveIdent;
};

///
/// AutoSaveFile
///

// Should be plain ASCII
#define AutoSaveIdent "<?xml autosave>"

using NameMap = std::unordered_map<wxString, short>;
using IdMap = std::unordered_map<short, wxString>;

// This class's overrides do NOT throw AudacityException.
class AUDACITY_DLL_API AutoSaveFile final : public XMLWriter
{
public:

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

   bool Write(wxFFile & file) const;
   bool Append(wxFFile & file) const;

   bool IsEmpty() const;

   bool Decode(const FilePath & fileName);

private:
   void WriteName(const wxString & name);
   void CheckSpace(wxMemoryOutputStream & buf);

private:
   wxMemoryOutputStream mBuffer;
   wxMemoryOutputStream mDict;
   NameMap mNames;
   size_t mAllocSize;
};


#endif
