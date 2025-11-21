//
// Copied from wx 3.0.2 and modified to support additional features
//
/////////////////////////////////////////////////////////////////////////////
// Name:        wx/osx/filedlg.h
// Purpose:     wxFileDialog class
// Author:      Stefan Csomor
// Modified by: Leland Lucius
// Created:     1998-01-01
// Copyright:   (c) Stefan Csomor
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _MAC_FILEDIALOG_H_
#define _MAC_FILEDIALOG_H_

#include "../FileDialog.h"

class wxChoice;

//-------------------------------------------------------------------------
// wxFileDialog
//-------------------------------------------------------------------------

class FileDialog : public FileDialogBase
{
    DECLARE_DYNAMIC_CLASS(FileDialog)
protected:
    wxArrayString m_fileNames;
    wxArrayString m_paths;

public:
    FileDialog();
    FileDialog(wxWindow* parent, const wxString& message = wxFileSelectorPromptStr, const wxString& defaultDir = wxEmptyString,
               const wxString& defaultFile = wxEmptyString, const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
               long style = wxFD_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
               const wxString& name = wxFileDialogNameStr);

    void Create(wxWindow* parent, const wxString& message = wxFileSelectorPromptStr, const wxString& defaultDir = wxEmptyString,
                const wxString& defaultFile = wxEmptyString, const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
                long style = wxFD_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
                const wxString& name = wxFileDialogNameStr);

#if wxOSX_USE_COCOA
    ~FileDialog();
#endif

    virtual void GetPaths(wxArrayString& paths) const { paths = m_paths; }
    virtual void GetFilenames(wxArrayString& files) const { files = m_fileNames; }

    virtual int ShowModal();

#if wxOSX_USE_COCOA
    virtual void ModalFinishedCallback(void* panel, int resultCode);
#endif

    virtual bool SupportsExtraControl() const;

    virtual void SetFileExtension(const wxString& extension);

    // implementation only

#if wxOSX_USE_COCOA
    void DoViewResized(void* object);
    void DoSendFolderChangedEvent(void* panel, const wxString& path);
    void DoSendSelectionChangedEvent(void* panel);
#endif

protected:
    // not supported for file dialog, RR
    virtual void DoSetSize(int WXUNUSED(x), int WXUNUSED(y),
                           int WXUNUSED(width), int WXUNUSED(height),
                           int WXUNUSED(sizeFlags) = wxSIZE_AUTO) {}

    void SetupExtraControls(WXWindow nativeWindow);

#if wxOSX_USE_COCOA
    void DoOnFilterSelected(int index);
    virtual void OnFilterSelected(wxCommandEvent& event);

    wxArrayString m_filterExtensions;
    wxArrayString m_filterNames;
    wxChoice* m_filterChoice;
    wxWindow* m_filterPanel;
    bool m_useFileTypeFilter;
    int m_firstFileTypeFilter;
    wxArrayString m_currentExtensions;
    WX_NSObject m_delegate;
    WX_NSObject m_sheetDelegate;
#endif

private:
    // Common part of all ctors.
    void Init();
};

#endif
