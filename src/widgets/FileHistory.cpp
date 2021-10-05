/**********************************************************************

  Audacity: A Digital Audio Editor

  FileHistory.cpp

  Leland Lucius

*******************************************************************//**

\class FileHistory
\brief Similar to wxFileHistory, but customized to our needs.

*//*******************************************************************/


#include "FileHistory.h"

#include <wx/defs.h>
#include <wx/menu.h>

#include "Internat.h"
#include "Prefs.h"

#include <mutex>

FileHistory::FileHistory(size_t maxfiles, wxWindowID base)
{
   mMaxFiles = maxfiles;
   mIDBase = base;
}

FileHistory::~FileHistory()
{
}

FileHistory &FileHistory::Global()
{
   // TODO - read the number of files to store in history from preferences
   static FileHistory history{
      ID_RECENT_LAST - ID_RECENT_FIRST + 1, ID_RECENT_CLEAR };
   static std::once_flag flag;
   std::call_once( flag, [&]{
      history.Load(*gPrefs, wxT("RecentFiles"));
   });

   return history;
}

// File history management
void FileHistory::AddFileToHistory(const FilePath & file, bool update)
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

   if (update)
      NotifyMenus();
}

void FileHistory::Remove( size_t i )
{
   wxASSERT(i < mHistory.size());

   if (i < mHistory.size()) {
      mHistory.erase( mHistory.begin() + i );

      NotifyMenus();
   }
}

void FileHistory::Clear()
{
   mHistory.clear();

   NotifyMenus();
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

   NotifyMenu( menu );
}

void FileHistory::Load(wxConfigBase & config, const wxString & group)
{
   mHistory.clear();
   mGroup = group.empty()
      ? wxString{ "RecentFiles" }
      : group;

   config.SetPath(mGroup);

   wxString file;
   long ndx;
   bool got = config.GetFirstEntry(file, ndx);
   while (got) {
      AddFileToHistory(config.Read(file), false);
      got = config.GetNextEntry(file, ndx);
   }

   config.SetPath(wxT(".."));

   NotifyMenus();
}

void FileHistory::Save(wxConfigBase & config)
{
   config.SetPath(wxT(""));
   config.DeleteGroup(mGroup);
   config.SetPath(mGroup);

   // Stored in reverse order
   int n = mHistory.size() - 1;
   for (size_t i = 1; i <= mHistory.size(); i++) {
      config.Write(wxString::Format(wxT("file%02d"), (int)i), mHistory[n--]);
   }

   config.SetPath(wxT(""));

   config.Flush();
}

void FileHistory::NotifyMenus()
{
   Compress();
   for (auto pMenu : mMenus)
      if (pMenu)
         NotifyMenu(pMenu);
   Save(*gPrefs);
}

void FileHistory::NotifyMenu(wxMenu *menu)
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

