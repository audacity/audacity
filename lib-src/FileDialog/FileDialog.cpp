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

void FileDialog::EnableButton(wxString label, fdCallback cb, void *data)
{
   m_buttonlabel = label;
   m_callback = cb;
   m_cbdata = data;
}

void FileDialog::ClickButton(int index)
{
   if (m_callback)
   {
      m_callback(m_cbdata, index);
   }
}

/////////////////////////////////////////////////////////////////////////////
// Name:        common/fldlgcmn.cpp
// Purpose:     wxFileDialog common functions
// Author:      John Labenski
// Modified by: Leland Lucius
// Created:     14.06.03 (extracted from src/*/filedlg.cpp)
// RCS-ID:      $Id: FileDialog.cpp,v 1.8 2008-10-05 14:48:59 richardash1981 Exp $
// Copyright:   (c) Robert Roebling
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// FileDialog convenience functions
//----------------------------------------------------------------------------

wxString FileSelector(const wxChar *title,
                      const wxChar *defaultDir,
                      const wxChar *defaultFileName,
                      const wxChar *defaultExtension,
                      const wxChar *filter,
                      int flags,
                      wxWindow *parent,
                      wxString label, fdCallback cb, void *cbdata)
{
   // The defaultExtension, if non-NULL, is
   // appended to the filename if the user fails to type an extension. The new
   // implementation (taken from wxFileSelectorEx) appends the extension
   // automatically, by looking at the filter specification. In fact this
   // should be better than the native Microsoft implementation because
   // Windows only allows *one* default extension, whereas here we do the
   // right thing depending on the filter the user has chosen.

   // If there's a default extension specified but no filter, we create a
   // suitable filter.

   wxString filter2;
   if (defaultExtension && !filter)
      filter2 = wxString(wxT("*.")) + defaultExtension;
   else if (filter)
      filter2 = filter;

   wxString defaultDirString;
   if (defaultDir)
      defaultDirString = defaultDir;

   wxString defaultFilenameString;
   if (defaultFileName)
      defaultFilenameString = defaultFileName;

   FileDialog fileDialog(parent, title, defaultDirString,
                         defaultFilenameString, filter2,
                         flags);

   // Enable the extra button if desired
   if ((flags & wxFD_SAVE) && (cb != NULL))
   {
      fileDialog.EnableButton(label, cb, cbdata);
   }

   // if filter is of form "All files (*)|*|..." set correct filter index
   if ((wxStrlen(defaultExtension) != 0) && (filter2.Find(wxT('|')) != wxNOT_FOUND))
   {
      int filterIndex = 0;
      
      wxArrayString descriptions, filters;
      // don't care about errors, handled already by FileDialog
      (void)wxParseCommonDialogsFilter(filter2, descriptions, filters);
      
      for (size_t n=0; n<filters.GetCount(); n++)
      {
         if (filters[n].Contains(defaultExtension))
         {
            filterIndex = (int)n; // Convert to int to avoid compiler warning, because we probably do not need many tens of thousands of filters.
            break;
         }
      }
      
      if (filterIndex > 0)
         fileDialog.SetFilterIndex(filterIndex);
   }
   
   wxString filename;
   if (fileDialog.ShowModal() == wxID_OK)
   {
      filename = fileDialog.GetPath();
   }
   
   return filename;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 94f72c32-970b-4f4e-bbf3-3880fce7b965
