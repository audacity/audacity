/**********************************************************************

  Audacity: A Digital Audio Editor

  FileHistory.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_FILEHISTORY__
#define __AUDACITY_WIDGETS_FILEHISTORY__

#include <vector>
#include <algorithm>
#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/grid.h>
#include <wx/string.h>
#include <wx/window.h>

class AUDACITY_DLL_API FileHistory
{
 public:
   FileHistory(size_t maxfiles = 12, wxWindowID idbase = wxID_FILE);
   virtual ~FileHistory();

   void AddFileToHistory(const wxString & file, bool update = true);
   void RemoveFileFromHistory(size_t i, bool update = true);
   void Clear();
   void UseMenu(wxMenu *menu);
   void RemoveMenu(wxMenu *menu);
   void Load(wxConfigBase& config, const wxString & group);
   void Save(wxConfigBase& config, const wxString & group);

   void AddFilesToMenu();
   void AddFilesToMenu(wxMenu *menu);

   size_t GetCount();
   const wxString &GetHistoryFile(size_t i) const;

 private:
   size_t mMaxFiles;
   wxWindowID mIDBase;

   std::vector<wxMenu*> mMenus;
   wxArrayString mHistory;

};

#endif
