/**********************************************************************

  Audacity: A Digital Audio Editor

  FileDialog.cpp

  Leland Lucius

*******************************************************************//**

\class FileDialog
\brief Dialog used to present platform specific "Save As" dialog with
custom controls.

*//*******************************************************************/

#include "FileDialog.h"

FileDialogBase::FileDialogBase()
{
   m_creator = NULL;
   m_userdata = 0;
}

bool FileDialogBase::HasUserPaneCreator() const
{
   return m_creator != NULL;
}

void FileDialogBase::SetUserPaneCreator(UserPaneCreatorFunction creator, wxUIntPtr userdata)
{
   m_creator = creator;
   m_userdata = userdata;
}

void FileDialogBase::CreateUserPane(wxWindow *parent)
{
   if (m_creator)
   {
      (*m_creator)(parent, m_userdata);
   }
}

//
// Copied from wx 3.0.2 and modified to support additional features
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/common/fldlgcmn.cpp
// Purpose:     wxFileDialog common functions
// Author:      John Labenski
// Modified by: Leland Lucius
// Created:     14.06.03 (extracted from src/*/filedlg.cpp)
// Copyright:   (c) Robert Roebling
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// FileDialog convenience functions
//----------------------------------------------------------------------------

wxString FileSelector(const wxString& title,
                      const wxString& defaultDir,
                      const wxString& defaultFileName,
                      const wxString& defaultExtension,
                      const wxString& filter,
                      int flags,
                      wxWindow *parent,
                      int x, int y)
{
    // The defaultExtension, if non-empty, is
    // appended to the filename if the user fails to type an extension. The new
    // implementation (taken from FileSelectorEx) appends the extension
    // automatically, by looking at the filter specification. In fact this
    // should be better than the native Microsoft implementation because
    // Windows only allows *one* default extension, whereas here we do the
    // right thing depending on the filter the user has chosen.

    // If there's a default extension specified but no filter, we create a
    // suitable filter.

    wxString filter2;
    if ( !defaultExtension.empty() && filter.empty() )
        filter2 = wxString(wxT("*.")) + defaultExtension;
    else if ( !filter.empty() )
        filter2 = filter;

    FileDialog fileDialog(parent, title, defaultDir,
                            defaultFileName, filter2,
                            flags, wxPoint(x, y));

    // if filter is of form "All files (*)|*|..." set correct filter index
    if ( !defaultExtension.empty() && filter2.find(wxT('|')) != wxString::npos )
    {
        int filterIndex = 0;

        wxArrayString descriptions, filters;
        // don't care about errors, handled already by FileDialog
        (void)wxParseCommonDialogsFilter(filter2, descriptions, filters);
        for (size_t n=0; n<filters.GetCount(); n++)
        {
            if (filters[n].Contains(defaultExtension))
            {
                filterIndex = n;
                break;
            }
        }

        if (filterIndex > 0)
            fileDialog.SetFilterIndex(filterIndex);
    }

    wxString filename;
    if ( fileDialog.ShowModal() == wxID_OK )
    {
        filename = fileDialog.GetPath();
    }

    return filename;
}

//----------------------------------------------------------------------------
// FileSelectorEx
//----------------------------------------------------------------------------

wxString FileSelectorEx(const wxString& title,
                        const wxString& defaultDir,
                        const wxString& defaultFileName,
                        int*            defaultFilterIndex,
                        const wxString& filter,
                        int             flags,
                        wxWindow*       parent,
                        int             x,
                        int             y)

{
    FileDialog fileDialog(parent,
                            title,
                            defaultDir,
                            defaultFileName,
                            filter,
                            flags, wxPoint(x, y));

    wxString filename;
    if ( fileDialog.ShowModal() == wxID_OK )
    {
        if ( defaultFilterIndex )
            *defaultFilterIndex = fileDialog.GetFilterIndex();

        filename = fileDialog.GetPath();
    }

    return filename;
}

