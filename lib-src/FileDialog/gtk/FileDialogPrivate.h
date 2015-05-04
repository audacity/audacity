/////////////////////////////////////////////////////////////////////////////
// Name:        filedlg.h
// Purpose:
// Author:      Robert Roebling
// Id:          $Id: FileDialogPrivate.h,v 1.2 2008-05-24 02:57:39 llucius Exp $
// Copyright:   (c) 1998 Robert Roebling
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////


#ifndef __FILEDIALOGGTKH__
#define __FILEDIALOGGTKH__

#include "wx/defs.h"

#define GENERIC_FILEDIALOG 1
#include "../generic/FileDialogPrivate.h"

//-------------------------------------------------------------------------
// FileDialog
//-------------------------------------------------------------------------

class FileDialog: public GenericFileDialog
{
public:
   FileDialog() { }
   
   FileDialog(wxWindow *parent,
              const wxString& message = wxFileSelectorPromptStr,
              const wxString& defaultDir = wxEmptyString,
              const wxString& defaultFile = wxEmptyString,
              const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
              long style = 0,
              const wxPoint& pos = wxDefaultPosition);
   
   virtual ~FileDialog();
   
   virtual wxString GetPath() const;
   virtual void GetPaths(wxArrayString& paths) const;
   virtual wxString GetDirectory() const;
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
   virtual bool Show( bool show = true );
   
   //private:
   bool m_destroyed_by_delete;
   
   // override this from wxTLW since the native
   // form doesn't have any m_wxwindow
   virtual void DoSetSize(int x, int y,
                          int width, int height,
                          int sizeFlags = wxSIZE_AUTO);
   
   virtual void EnableButton(wxString label, fdCallback cb, void *cbdata);
   virtual void ClickButton(int index);
   
private:
   DECLARE_DYNAMIC_CLASS(FileDialog)
   DECLARE_EVENT_TABLE()
   void OnFakeOk( wxCommandEvent &event );
   
   wxString m_buttonlabel;
   fdCallback m_callback;
   void *m_cbdata;
   wxArrayString m_patterns;
};

#endif
