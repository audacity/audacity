/**********************************************************************

  Audacity: A Digital Audio Editor

  ActiveProjects.cpp

********************************************************************//**

\class ActiveProjects
\brief Manages a list of active projects

*//********************************************************************/

#include "ActiveProjects.h"
#include "Prefs.h"

#include <wx/filename.h>

FilePaths ActiveProjects::GetAll()
{
    FilePaths files;

    const auto activeProjectsGroup = gPrefs->BeginGroup("/ActiveProjects");
    for (const auto& key : gPrefs->GetChildKeys()) {
        wxFileName path = gPrefs->Read(key, wxT(""));
        files.Add(path.GetFullPath());
    }

    return files;
}

void ActiveProjects::Add(const FilePath& path)
{
    wxString key = Find(path);

    if (key.empty()) {
        int i = 0;
        do{
            key.Printf(wxT("/ActiveProjects/%d"), ++i);
        } while (gPrefs->HasEntry(key));

        gPrefs->Write(key, path);
        gPrefs->Flush();
    }
}

void ActiveProjects::Remove(const FilePath& path)
{
    wxString key = Find(path);

    if (!key.empty()) {
        gPrefs->DeleteEntry(wxT("/ActiveProjects/" + key));
        gPrefs->Flush();
    }
}

wxString ActiveProjects::Find(const FilePath& path)
{
    const auto activeProjectsGroup = gPrefs->BeginGroup("/ActiveProjects");
    for (const auto& key : gPrefs->GetChildKeys()) {
        if (gPrefs->Read(key, wxT("")).IsSameAs(path)) {
            return key;
        }
    }
    return {};
}
