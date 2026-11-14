//
// Copied from wxWidgets 3.0.2 and modified for Audacity
//
/////////////////////////////////////////////////////////////////////////////
// Name:        wx/gtk/filedlg.h
// Purpose:
// Author:      Robert Roebling
// Copyright:   (c) 1998 Robert Roebling
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _GTK_FILEDIALOGPRIVATE_H_
#define _GTK_FILEDIALOGPRIVATE_H_

#include <wx/gtk/filectrl.h>    // for wxGtkFileChooser
#include <wx/panel.h>

//-------------------------------------------------------------------------
// FileDialog
//-------------------------------------------------------------------------

class WXDLLIMPEXP_CORE FileDialog : public FileDialogBase
{
public:
    FileDialog() { }

    FileDialog(wxWindow* parent, const wxString& message = wxFileSelectorPromptStr, const wxString& defaultDir = wxEmptyString,
               const wxString& defaultFile = wxEmptyString, const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
               long style = wxFD_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
               const wxString& name = wxFileDialogNameStr);
    bool Create(wxWindow* parent, const wxString& message = wxFileSelectorPromptStr, const wxString& defaultDir = wxEmptyString,
                const wxString& defaultFile = wxEmptyString, const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
                long style = wxFD_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
                const wxString& name = wxFileDialogNameStr);
    virtual ~FileDialog();

    virtual wxString GetPath() const;
    virtual void GetPaths(wxArrayString& paths) const;
    virtual wxString GetFilename() const;
    virtual void GetFilenames(wxArrayString& files) const;
    virtual int GetFilterIndex() const;

    virtual void SetMessage(const wxString& message);
    virtual void SetPath(const wxString& path);
    virtual void SetDirectory(const wxString& dir);
    virtual void SetFilename(const wxString& name);
    virtual void SetWildcard(const wxString& wildCard);
    virtual void SetFilterIndex(int filterIndex);

    virtual int ShowModal();

    virtual bool SupportsExtraControl() const { return true; }

    virtual void SetFileExtension(const wxString& extension);

    // Implementation only.
    void GTKSelectionChanged(const wxString& filename);
    void GTKFolderChanged();
    void GTKFilterChanged();

protected:
    // override this from wxTLW since the native
    // form doesn't have any m_wxwindow
    virtual void DoSetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);

private:
    void OnFakeOk(wxCommandEvent& event);
    void OnSize(wxSizeEvent&);
    virtual void AddChildGTK(wxWindowGTK* child);

    wxGtkFileChooser m_fc;

    DECLARE_DYNAMIC_CLASS(FileDialog)
    DECLARE_EVENT_TABLE()
};

#endif
