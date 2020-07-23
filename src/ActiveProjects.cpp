/**********************************************************************

  Audacity: A Digital Audio Editor

  ActiveProjects.cpp

********************************************************************//**

\class ActiveProjects
\brief Manages a list of active projects

*//********************************************************************/

#include "Audacity.h"
#include "ActiveProjects.h"
#include "Prefs.h"

#include <wx/filename.h>

FilePaths ActiveProjects::GetAll()
{
   FilePaths files;

   wxString key;
   long ndx;

   wxString configPath = gPrefs->GetPath();
   gPrefs->SetPath(wxT("/ActiveProjects"));

   bool more = gPrefs->GetFirstEntry(key, ndx);
   while (more)
   {
      wxFileName path = gPrefs->Read(key, wxT(""));

      files.Add(path.GetFullPath());

      more = gPrefs->GetNextEntry(key, ndx);
   }
   gPrefs->SetPath(configPath);

   return files;
}

void ActiveProjects::Add(const FilePath &path)
{
   wxString key = Find(path);

   if (key.empty())
   {
      int i = 0;
      do
      {
         key.Printf(wxT("/ActiveProjects/%d"), ++i);
      } while (gPrefs->HasEntry(key));

      gPrefs->Write(key, path);
      gPrefs->Flush();
   }
}

void ActiveProjects::Remove(const FilePath &path)
{
   wxString key = Find(path);

   if (!key.empty())
   {
      gPrefs->DeleteEntry(wxT("/ActiveProjects/" + key));
      gPrefs->Flush();
   }
}

wxString ActiveProjects::Find(const FilePath &path)
{
   bool found = false;

   wxString key;
   long ndx;

   wxString configPath = gPrefs->GetPath();
   gPrefs->SetPath(wxT("/ActiveProjects"));

   bool more = gPrefs->GetFirstEntry(key, ndx);
   while (more)
   {
      if (gPrefs->Read(key, wxT("")).IsSameAs(path))
      {
         found = true;
         break;
      }

      more = gPrefs->GetNextEntry(key, ndx);
   }

   gPrefs->SetPath(configPath);

   return found ? key : wxT("");
}

