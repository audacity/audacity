//
// Copied from wxWidgets 3.0.2 and modified for Audacity
//
/////////////////////////////////////////////////////////////////////////////
// Name:        wx/msw/filedlg.h
// Purpose:     wxFileDialog class
// Author:      Julian Smart
// Modified by: Leland Lucius
// Created:     01/02/97
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _WIN_FILEDIALOGPRIVATE_H_
#define _WIN_FILEDIALOGPRIVATE_H_

#include <windows.h>

#include <wx/modalhook.h>

//-------------------------------------------------------------------------
// FileDialog
//-------------------------------------------------------------------------

class WX_WRAPPERS_API FileDialog : public FileDialogBase
{
public:
    FileDialog();
    FileDialog(wxWindow* parent, const wxString& message = wxFileSelectorPromptStr, const wxString& defaultDir = wxEmptyString,
               const wxString& defaultFile = wxEmptyString, const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
               long style = wxFD_DEFAULT_STYLE, const wxPoint& pos = wxDefaultPosition, const wxSize& sz = wxDefaultSize,
               const wxString& name = wxFileDialogNameStr);

    virtual void GetPaths(wxArrayString& paths) const;
    virtual void GetFilenames(wxArrayString& files) const;
    virtual int ShowModal();

    virtual void SetFileExtension(const wxString& extension);

protected:
    // -----------------------------------------
    // wxMSW-specific implementation from now on
    // -----------------------------------------

#if !(defined(__SMARTPHONE__) && defined(__WXWINCE__))
    virtual void DoMoveWindow(int x, int y, int width, int height);
    virtual void DoCentre(int dir);
    virtual void DoGetSize(int* width, int* height) const;
    virtual void DoGetPosition(int* x, int* y) const;
#endif // !(__SMARTPHONE__ && __WXWINCE__)

private:
    void Init();

    wxString GetFullPath(HWND hwnd, int itm);
    void FilterFiles(HWND hwnd, bool refresh);
    void ParseFilter(int index);

    // Parent dialog hook
    static UINT_PTR APIENTRY ParentHook(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam);
    virtual UINT_PTR MSWParentHook(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam, OPENFILENAME* pOfn);

    // Message handlers for the parent dialog
    virtual void MSWOnSize(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnGetMinMaxInfo(HWND hwnd, LPOPENFILENAME pOfn, LPMINMAXINFO pMmi);

    // Child dialog hook
    static UINT_PTR APIENTRY DialogHook(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam);
    virtual UINT_PTR MSWDialogHook(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam, OPENFILENAME* pOfn);

    // Message handlers for the child dialog
    virtual void MSWOnInitDialog(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnDestroy(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnInitDone(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnFolderChange(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnSelChange(HWND hwnd, LPOPENFILENAME pOfn);
    virtual void MSWOnTypeChange(HWND hwnd, LPOPENFILENAME pOfn);

private:
    wxArrayString m_fileNames;

    // remember if our SetPosition() or Centre() (which requires special
    // treatment) was called
    bool m_bMovedWindow;
    int m_centreDir;       // nothing to do if 0

    wxArrayString m_FilterGroups;
    wxArrayString m_Filters;

    HWND mParentDlg;
    HWND mChildDlg;
    WNDPROC mParentProc;
    POINT mMinSize;

    wxPanel* mRoot;

    class Disabler : public wxModalDialogHook
    {
    public:
        Disabler();
        void Init(wxWindow* root, HWND hwnd);

    protected:
        int Enter(wxDialog* dialog);
        void Exit(wxDialog* dialog);
        bool IsChild(const wxDialog* dialog) const;

    private:
        wxWindow* mRoot;
        HWND mHwnd;
        int mModalCount;
    } mDisabler;

    DECLARE_DYNAMIC_CLASS(FileDialog)
    DECLARE_NO_COPY_CLASS(FileDialog)
};

#endif
