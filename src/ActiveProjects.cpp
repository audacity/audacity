/**********************************************************************

  Audacity: A Digital Audio Editor

  ActiveProjects.cpp

********************************************************************//**

\class ActiveProjects
\brief Manages a list of active projects

*//********************************************************************/

#include "Audacity.h"
#include "ActiveProjects.h"
#include "FileNames.h"
#include "Prefs.h"

#include <wx/arrstr.h>
#include <wx/file.h>
#include <wx/filename.h>

FilePaths ActiveProjects::GetAll()
{
   FilePaths files;

   wxFileName fn(FileNames::DataDir(), wxT("activeprojects.cfg"));
   wxFile file;
   if (file.Open(fn.GetFullPath(), wxFile::OpenMode::read))
   {
      wxString lines;
      if (file.ReadAll(&lines))
      {
         files = wxSplit(lines, wxT('\n'));
         for (int i = files.size() - 1; i >= 0; --i)
         {
            if (files[i].empty())
            {
               files.RemoveAt(i);
            }
         }
      }
   }

   return files;   
#if 0
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
#endif
   return files;
}

void ActiveProjects::Add(const FilePath &path)
{
   FilePaths files = GetAll();

   if (files.Index(path) != wxNOT_FOUND)
   {
      return;
   }
   files.Add(path);

   wxString lines = wxJoin(files, wxT('\n')) + wxT('\n');

   wxFileName fn(FileNames::DataDir(), wxT("activeprojects.cfg"));
   wxFile file;
   if (file.Create(fn.GetFullPath(), true))
   {
      file.Write(lines);
      file.Close();
   }

#if 0
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
#endif
}

void ActiveProjects::Remove(const FilePath &path)
{
   FilePaths files = GetAll();

   int ndx = files.Index(path);
   if (ndx == wxNOT_FOUND)
   {
      return;
   }
   files.RemoveAt(ndx);

   wxString lines = wxJoin(files, wxT('\n')) + wxT('\n');

   wxFileName fn(FileNames::DataDir(), wxT("activeprojects.cfg"));
   wxFile file;
   if (file.Create(fn.GetFullPath(), true))
   {
      file.Write(lines);
      file.Close();
   }

#if 0
   wxString key = Find(path);

   if (!key.empty())
   {
      gPrefs->DeleteEntry(wxT("/ActiveProjects/" + key));
      gPrefs->Flush();
   }
#endif
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

