/////////////////////////////////////////////////////////////////////////////
// Name:        wx/msw/filedlg.h
// Purpose:     wxFileDialog class
// Author:      Julian Smart
// Modified by: Leland Lucius
// Created:     01/02/97
// RCS-ID:      $Id: FileDialogPrivate.h,v 1.6 2009-04-11 05:53:09 llucius Exp $
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////

#ifndef _WIN_FILEDIALOGPRIVATE_H_
#define _WIN_FILEDIALOGPRIVATE_H_

#include <windows.h>

//-------------------------------------------------------------------------
// FileDialog
//-------------------------------------------------------------------------

class FileDialog : public FileDialogBase
{
 public:
    FileDialog();
    FileDialog(wxWindow *parent,
               const wxString& message = wxFileSelectorPromptStr,
               const wxString& defaultDir = wxEmptyString,
               const wxString& defaultFile = wxEmptyString,
               const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
               long style = wxFD_DEFAULT_STYLE,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& sz = wxDefaultSize,
               const wxString& name = wxFileDialogNameStr);

   virtual void GetPaths(wxArrayString& paths) const;
   virtual void GetFilenames(wxArrayString& files) const;
   void OnSize(wxSizeEvent & e);
   virtual int ShowModal();
   
   // -----------------------------------------
   // wxMSW-specific implementation from now on
   // -----------------------------------------

   //
   virtual UINT_PTR MSWDialogHook(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam, OPENFILENAME *pOfn);

   //
   virtual UINT_PTR MSWParentHook(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam, OPENFILENAME *pOfn);

protected:
   
#if !(defined(__SMARTPHONE__) && defined(__WXWINCE__))
    virtual void DoMoveWindow(int x, int y, int width, int height);
    virtual void DoCentre(int dir);
    virtual void DoGetSize(int *width, int *height) const;
    virtual void DoGetPosition(int *x, int *y) const;
#endif // !(__SMARTPHONE__ && __WXWINCE__)

private:
   void Init();

   wxString GetFullPath(HWND hwnd, int itm);
   void FilterFiles(HWND hwnd, bool refresh);
   void ParseFilter(int index);

   // Message handlers for the child dialog
   virtual void MSWOnInitDialog(HWND hwnd, LPOPENFILENAME pOfn);
   virtual void MSWOnDestroy(HWND hwnd, LPOPENFILENAME pOfn);
   virtual void MSWOnInitDone(HWND hwnd, LPOPENFILENAME pOfn);
   virtual void MSWOnFolderChange(HWND hwnd, LPOPENFILENAME pOfn);
   virtual void MSWOnSelChange(HWND hwnd, LPOPENFILENAME pOfn);
   virtual void MSWOnTypeChange(HWND hwnd, LPOPENFILENAME pOfn);

   // Message handlers for the child dialog
   virtual void MSWOnSize(HWND hwnd, LPOPENFILENAME pOfn);

private:
   wxArrayString m_fileNames;

   // remember if our SetPosition() or Centre() (which requires special
   // treatment) was called
   bool m_bMovedWindow;
   int m_centreDir;        // nothing to do if 0

   wxArrayString m_FilterGroups;
   wxArrayString m_Filters;
   wxChar *m_NameBuf;
   int m_NameBufLen;
   
   HWND mParentDlg;
   HWND mChildDlg;
   WNDPROC mParentProc;

   wxPanel *mRoot;

   DECLARE_DYNAMIC_CLASS(FileDialog)
   DECLARE_NO_COPY_CLASS(FileDialog)
};

#endif

