/**********************************************************************

  Audacity: A Digital Audio Editor

  FileHistory.cpp

  Leland Lucius

*******************************************************************//**

\class FileHistory
\brief Similar to FileHistory, but customized to our needs.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/fileconf.h>
#include <wx/menu.h>

#include "FileHistory.h"
#include "../Internat.h"

FileHistory::FileHistory(size_t maxfiles, wxWindowID base)
{
   mMaxFiles = maxfiles;
   mIDBase = base;
}

FileHistory::~FileHistory()
{
}

// File history management
void FileHistory::AddFileToHistory(const wxString & file, bool update)
{
   // Needed to transition from wxFileHistory to FileHistory since there
   // can be empty history "slots".
   if (file.empty()) {
      return;
   }

#if defined(__WXMSW__)
   int i = mHistory.Index(file, false);
#else
   int i = mHistory.Index(file, true);
#endif

   if (i != wxNOT_FOUND) {
      mHistory.erase( mHistory.begin() + i );
   }

   if (mMaxFiles > 0 && mMaxFiles == mHistory.size()) {
      mHistory.erase( mHistory.end() - 1 );
   }

   mHistory.insert(mHistory.begin(), file);

   if (update) {
      AddFilesToMenu();
   }
}

void FileHistory::RemoveFileFromHistory(size_t i, bool update)
{
   wxASSERT(i < mHistory.size());

   if (i < mHistory.size()) {
      mHistory.erase( mHistory.begin() + i );

      if (update) {
         AddFilesToMenu();
      }
   }
}

void FileHistory::Clear()
{
   mHistory.clear();

   AddFilesToMenu();
}

const wxString &FileHistory::GetHistoryFile(size_t i) const
{
   wxASSERT(i < mHistory.size());

   if (i < mHistory.size()) {
      return mHistory[i];
   }

   static const wxString empty;
   return empty;
}

size_t FileHistory::GetCount()
{
   return mHistory.size();
}

void FileHistory::UseMenu(wxMenu *menu)
{
   Compress();

   auto end = mMenus.end();
   auto iter = std::find(mMenus.begin(), end, menu);
   auto found = (iter != end);

   if (!found)
      mMenus.push_back(menu);
   else {
      wxASSERT(false);
   }
}

void FileHistory::Load(wxConfigBase & config, const wxString & group)
{
   mHistory.clear();

   config.SetPath(group);

   wxString file;
   long ndx;
   bool got = config.GetFirstEntry(file, ndx);
   while (got) {
      AddFileToHistory(config.Read(file), false);
      got = config.GetNextEntry(file, ndx);
   }

   config.SetPath(wxT(".."));

   AddFilesToMenu();
}

void FileHistory::Save(wxConfigBase & config, const wxString & group)
{
   config.SetPath(wxT(""));
   config.DeleteGroup(group);
   config.SetPath(group);

   // Stored in reverse order
   int n = mHistory.size() - 1;
   for (size_t i = 1; i <= mHistory.size(); i++) {
      config.Write(wxString::Format(wxT("file%02d"), (int)i), mHistory[n--]);
   }

   config.SetPath(wxT(""));
}

void FileHistory::AddFilesToMenu()
{
   Compress();
   for (auto pMenu : mMenus)
      if (pMenu)
         AddFilesToMenu(pMenu);
}

void FileHistory::AddFilesToMenu(wxMenu *menu)
{
   wxMenuItemList items = menu->GetMenuItems();
   for (auto end = items.end(), iter = items.begin(); iter != end;)
      menu->Destroy(*iter++);

   for (size_t i = 0; i < mHistory.size(); i++) {
      wxString item =  mHistory[i];
      item.Replace( "&", "&&" );
      menu->Append(mIDBase + 1 + i,item);
   }

   if (mHistory.size() > 0) {
      menu->AppendSeparator();
   }
   menu->Append(mIDBase, _("&Clear"));
   menu->Enable(mIDBase, mHistory.size() > 0);
}

void FileHistory::Compress()
{
   // Clear up expired weak pointers
   auto end = mMenus.end();
   mMenus.erase(
     std::remove_if( mMenus.begin(), end,
        [](wxWeakRef<wxMenu> &pMenu){ return !pMenu; } ),
     end
   );
}

