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
   if (file.IsEmpty()) {
      return;
   }

#if defined(__WXMSW__)
   int i = mHistory.Index(file, false);
#else
   int i = mHistory.Index(file, true);
#endif

   if (i != wxNOT_FOUND) {
      mHistory.RemoveAt(i);
   }

   if (mMaxFiles == mHistory.GetCount()) {
      mHistory.RemoveAt(mHistory.GetCount() - 1);
   }

   mHistory.Insert(file, 0);

   if (update) {
      AddFilesToMenu();
   }
}

void FileHistory::RemoveFileFromHistory(size_t i, bool update)
{
   wxASSERT(i < mHistory.GetCount());

   if (i < mHistory.GetCount()) {
      mHistory.RemoveAt(i);

      if (update) {
         AddFilesToMenu();
      }
   }
}

void FileHistory::Clear()
{
   mHistory.Clear();

   AddFilesToMenu();
}

const wxString &FileHistory::GetHistoryFile(size_t i) const
{
   wxASSERT(i < mHistory.GetCount());

   if (i < mHistory.GetCount()) {
      return mHistory[i];
   }

   static const wxString empty;
   return empty;
}

size_t FileHistory::GetCount()
{
   return mHistory.GetCount();
}

void FileHistory::UseMenu(wxMenu *menu)
{
   auto end = mMenus.end();
   auto iter = std::find(mMenus.begin(), end, menu);
   auto found = (iter != end);

   if (!found)
      mMenus.push_back(menu);
   else {
      wxASSERT(false);
   }
}

void FileHistory::RemoveMenu(wxMenu *menu)
{
   auto end = mMenus.end();
   auto iter = std::find(mMenus.begin(), end, menu);
   auto found = (iter != end);

   if (found)
      mMenus.erase(iter);
   else {
      wxASSERT(false);
   }
}

void FileHistory::Load(wxConfigBase & config, const wxString & group)
{
   mHistory.Clear();

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
   config.DeleteGroup(group);
   config.SetPath(group);

   // Stored in reverse order
   int n = mHistory.GetCount() - 1;
   for (size_t i = 1; i <= mHistory.GetCount(); i++) {
      config.Write(wxString::Format(wxT("file%02d"), (int)i), mHistory[n--]);
   }

   config.SetPath(wxT(".."));
}

void FileHistory::AddFilesToMenu()
{
   for (auto pMenu : mMenus)
      AddFilesToMenu(pMenu);
}

void FileHistory::AddFilesToMenu(wxMenu *menu)
{
   wxMenuItemList items = menu->GetMenuItems();
   for (auto end = items.end(), iter = items.begin(); iter != end;)
      menu->Destroy(*iter++);

   for (size_t i = 0; i < mHistory.GetCount(); i++) {
      menu->Append(mIDBase + 1 + i, mHistory[i]);
   }

   if (mHistory.GetCount() > 0) {
      menu->AppendSeparator();
   }
   menu->Append(mIDBase, _("&Clear"));
   menu->Enable(mIDBase, mHistory.GetCount() > 0);
}
