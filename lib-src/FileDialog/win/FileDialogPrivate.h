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

#ifndef _FILEDIALOGMSW_H_
#define _FILEDIALOGMSW_H_

#include <windows.h>
#include <wx/msw/winundef.h>

//-------------------------------------------------------------------------
// wxFileDialog
//-------------------------------------------------------------------------

class FileDialog: public wxFileDialogBase
{
 public:
   FileDialog(wxWindow *parent,
              const wxString& message = wxFileSelectorPromptStr,
              const wxString& defaultDir = wxEmptyString,
              const wxString& defaultFile = wxEmptyString,
              const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
              long style = 0,
              const wxPoint& pos = wxDefaultPosition);
   
   virtual void SetPath(const wxString& path);
   virtual void GetPaths(wxArrayString& paths) const;
   virtual void GetFilenames(wxArrayString& files) const;
   
   virtual int ShowModal();
   
   virtual void EnableButton(wxString label, fdCallback cb, void *cbdata);
   virtual void ClickButton(int index);
   
   virtual void FilterFiles(HWND hDlg, bool refresh);
   virtual void ParseFilter(int index);
   wxString m_buttonlabel;
   
 protected:
   
#if !(defined(__SMARTPHONE__) && defined(__WXWINCE__))
   virtual void DoMoveWindow(int x, int y, int width, int height);
   virtual void DoGetSize( int *width, int *height ) const;
   virtual void DoGetPosition( int *x, int *y ) const;
#endif // !(__SMARTPHONE__ && __WXWINCE__)
   
private:
   wxArrayString m_fileNames;
   bool m_bMovedWindow;
   long m_dialogStyle;
   
   wxArrayString m_FilterGroups;
   wxArrayString m_Filters;
   wxChar *m_NameBuf;
   int m_NameBufLen;
   
   fdCallback m_callback;
   void *m_cbdata;
   
   DECLARE_DYNAMIC_CLASS(FileDialog)
   DECLARE_NO_COPY_CLASS(FileDialog)
};

#endif

