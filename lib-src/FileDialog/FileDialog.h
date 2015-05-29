/**********************************************************************

  Audacity: A Digital Audio Editor

  FileDialog.h

  Leland Lucius

*******************************************************************//**

\class FileDialog
\brief Dialog used to present platform specific "Save As" dialog with
custom controls.

*//*******************************************************************/

#ifndef _FILE_DIALOG_H_
#define _FILE_DIALOG_H_

#include "wx/defs.h"
#include "wx/filedlg.h"

typedef void (*fdCallback)(void *, int);

#if defined(__WXGTK__)
#include "gtk/FileDialogPrivate.h"
#elif defined(__WXMAC__)
#include "mac/FileDialogPrivate.h"
#elif defined(__WXMSW__)
#include "win/FileDialogPrivate.h"
#else
#error Unknown implementation
#endif

/////////////////////////////////////////////////////////////////////////////
// Name:        filedlg.h
// Purpose:     wxFileDialog base header
// Author:      Robert Roebling
// Modified by: Leland Lucius
// Created:     8/17/99
// Copyright:   (c) Robert Roebling
// RCS-ID:      $Id: FileDialog.h,v 1.9 2008-05-24 02:57:39 llucius Exp $
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////

DECLARE_EVENT_TYPE(EVT_FILEDIALOG_SELECTION_CHANGED, -1);
DECLARE_EVENT_TYPE(EVT_FILEDIALOG_FILTER_CHANGED, -1);
DECLARE_EVENT_TYPE(EVT_FILEDIALOG_ADD_CONTROLS, -1);

#define FD_NO_ADD_EXTENSION 0x0400

//----------------------------------------------------------------------------
// wxFileDialog convenience functions
//----------------------------------------------------------------------------

wxString 
FileSelector(const wxString & message = wxFileSelectorPromptStr,
             const wxString & default_path = wxEmptyString,
             const wxString & default_filename = wxEmptyString,
             const wxString & default_extension = wxEmptyString,
             const wxString & wildcard = wxFileSelectorDefaultWildcardStr,
             int flags = 0,
             wxWindow *parent = NULL);

#endif

