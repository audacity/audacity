/**********************************************************************

  Audacity: A Digital Audio Editor

  FileDialog.h

  Leland Lucius

*******************************************************************//**

\class FileDialog
\brief Dialog used to present platform specific "Save As" dialog with
custom controls.

*//*******************************************************************/

#ifndef _FILEDIALOG_H_
#define _FILEDIALOG_H_

#include <wx/filedlg.h> // to inherit

class WX_WRAPPERS_API FileDialogBase : public wxFileDialogBase
{
public:
    FileDialogBase();
    virtual ~FileDialogBase() {}

    // FileDialogBase

    typedef void (* UserPaneCreatorFunction)(wxWindow* parent, wxUIntPtr userdata);

    virtual bool HasUserPaneCreator() const;
    virtual void SetUserPaneCreator(UserPaneCreatorFunction creator, wxUIntPtr userdata);

    virtual void SetFileExtension(const wxString& extension) {}

protected:
    void CreateUserPane(wxWindow* parent);

    UserPaneCreatorFunction m_creator;
    wxUIntPtr m_userdata;
};

#if defined(__WXGTK__)
#include "gtk/FileDialogPrivate.h"
#elif defined(__WXMAC__)
#include "mac/FileDialogPrivate.h"
#elif defined(__WXMSW__)
#include "win/FileDialogPrivate.h"
#else
#error Unknown implementation
#endif

//
// Copied from wx 3.0.2 and modified to support additional features
//
/////////////////////////////////////////////////////////////////////////////
// Name:        wx/filedlg.h
// Purpose:     wxFileDialog base header
// Author:      Robert Roebling
// Modified by: Leland Lucius
// Created:     8/17/99
// Copyright:   (c) Robert Roebling
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// FileDialog convenience functions
//----------------------------------------------------------------------------

WX_WRAPPERS_API wxString
FileSelector(const wxString& message = wxFileSelectorPromptStr, const wxString& default_path = wxEmptyString,
             const wxString& default_filename = wxEmptyString, const wxString& default_extension = wxEmptyString,
             const wxString& wildcard = wxFileSelectorDefaultWildcardStr, int flags = 0, wxWindow* parent = NULL, int x = wxDefaultCoord,
             int y = wxDefaultCoord);

// An extended version of FileSelector
WX_WRAPPERS_API wxString
FileSelectorEx(const wxString& message = wxFileSelectorPromptStr, const wxString& default_path = wxEmptyString,
               const wxString& default_filename = wxEmptyString, int* indexDefaultExtension = NULL,
               const wxString& wildcard = wxFileSelectorDefaultWildcardStr, int flags = 0, wxWindow* parent = NULL, int x = wxDefaultCoord,
               int y = wxDefaultCoord);

#endif
