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

FileHistory::FileHistory(size_t maxfiles)
{
   mMaxFiles = maxfiles;
}

FileHistory::~FileHistory()
{
}

FileHistory &FileHistory::Global()
{
   // TODO - read the number of files to store in history from preferences
   static FileHistory history;
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

void FileHistoryMenus::UseMenu(wxMenu *menu)
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
   Publish({});
   Save(*gPrefs);
}

FileHistoryMenus::FileHistoryMenus()
{
   mSubscription = FileHistory::Global()
      .Subscribe(*this, &FileHistoryMenus::OnChangedHistory);
}

FileHistoryMenus &FileHistoryMenus::Instance()
{
   static FileHistoryMenus instance;
   return instance;
}

void FileHistoryMenus::OnChangedHistory(Observer::Message)
{
   Compress();
   for (auto pMenu : mMenus)
      if (pMenu)
         NotifyMenu(pMenu);
}

void FileHistoryMenus::NotifyMenu(wxMenu *menu)
{
   wxMenuItemList items = menu->GetMenuItems();
   for (auto end = items.end(), iter = items.begin(); iter != end;)
      menu->Destroy(*iter++);

   const auto &history = FileHistory::Global();
   int mIDBase = ID_RECENT_CLEAR;
   int i = 0;
   for (auto item : history) {
      item.Replace( "&", "&&" );
      menu->Append(mIDBase + 1 + i++, item);
   }

   if (history.size() > 0) {
      menu->AppendSeparator();
   }
   menu->Append(mIDBase, _("&Clear"));
   menu->Enable(mIDBase, history.size() > 0);
}

void FileHistoryMenus::Compress()
{
   // Clear up expired weak pointers
   auto end = mMenus.end();
   mMenus.erase(
     std::remove_if( mMenus.begin(), end,
        [](wxWeakRef<wxMenu> &pMenu){ return !pMenu; } ),
     end
   );
}

