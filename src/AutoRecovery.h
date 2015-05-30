/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.h

*******************************************************************/

#ifndef __AUDACITY_AUTORECOVERY__
#define __AUDACITY_AUTORECOVERY__

#include "Project.h"

#include "xml/XMLTagHandler.h"
#include "xml/XMLWriter.h"

#include <wx/debug.h>
#include <wx/dynarray.h>
#include <wx/ffile.h>
#include <wx/hashmap.h>
#include <wx/mstream.h>

//
// Show auto recovery dialog if there are projects to recover. Should be
// called once at Audacity startup.
//
// This function possibly opens new project windows while it recovers all
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
class RecordingRecoveryHandler: public XMLTagHandler
{
public:
   RecordingRecoveryHandler(AudacityProject* proj);
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);

   // This class only knows reading tags
   virtual void WriteXML(XMLWriter & WXUNUSED(xmlFile)) { wxASSERT(false); }

private:
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

WX_DECLARE_STRING_HASH_MAP_WITH_DECL(short, NameMap, class AUDACITY_DLL_API);
WX_DECLARE_HASH_MAP_WITH_DECL(short, wxString, wxIntegerHash, wxIntegerEqual, IdMap, class AUDACITY_DLL_API);
WX_DECLARE_OBJARRAY_WITH_DECL(IdMap, IdMapArray, class AUDACITY_DLL_API);

class AUDACITY_DLL_API AutoSaveFile : public XMLWriter
{
public:

   AutoSaveFile(size_t allocSize = 1024 * 1024);
   virtual ~AutoSaveFile();

   virtual void StartTag(const wxString & name);
   virtual void EndTag(const wxString & name);

   virtual void WriteAttr(const wxString & name, const wxString &value);
   virtual void WriteAttr(const wxString & name, const wxChar *value);

   virtual void WriteAttr(const wxString & name, int value);
   virtual void WriteAttr(const wxString & name, bool value);
   virtual void WriteAttr(const wxString & name, long value);
   virtual void WriteAttr(const wxString & name, long long value);
   virtual void WriteAttr(const wxString & name, size_t value);
   virtual void WriteAttr(const wxString & name, float value, int digits = -1);
   virtual void WriteAttr(const wxString & name, double value, int digits = -1);

   virtual void WriteData(const wxString & value);

   virtual void WriteSubTree(const AutoSaveFile & value);

   virtual void Write(const wxString & data);
   virtual bool Write(wxFFile & file) const;
   virtual bool Append(wxFFile & file) const;

   virtual bool IsEmpty() const;

   virtual bool Decode(const wxString & fileName);

private:
   void WriteName(const wxString & name);
   void CheckSpace(wxMemoryOutputStream & buf);

private:
   wxMemoryOutputStream mBuffer;
   wxMemoryOutputStream mDict;
   NameMap mNames;
   IdMap mIds;
   IdMapArray mIdStack;
   size_t mAllocSize;
};


#endif
