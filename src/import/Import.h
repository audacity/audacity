/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/list.h>
#include <wx/listimpl.cpp>
#include <wx/dialog.h>
#include <wx/listbox.h>

class Tags;
class TrackFactory;
class Track;
class ImportPlugin;
class ImportFileHandle;
class UnusableImportPlugin;
typedef bool (*progress_callback_t)( void *userData, float percent );

class Format {
public:
   wxString formatName;
   wxArrayString formatExtensions;

   Format(wxString _formatName, wxArrayString _formatExtensions):
      formatName(_formatName),
      formatExtensions(_formatExtensions)
   {
   }
};

class ImportPluginList;
class UnusableImportPluginList;

WX_DECLARE_LIST(Format, FormatList);


class Importer {
public:
   Importer();
   ~Importer();

   void GetSupportedImportFormats(FormatList *formatList);

   // returns number of tracks imported
   // if zero, the import failed and errorMessage will be set.
   int Import(wxString fName,
              TrackFactory *trackFactory,
              Track *** tracks,
              Tags *tags,
              wxString &errorMessage);

private:
   ImportPluginList *mImportPluginList;
   UnusableImportPluginList *mUnusableImportPluginList;
};

//----------------------------------------------------------------------------
// ImportStreamDialog
//----------------------------------------------------------------------------

class ImportStreamDialog: public wxDialog
{
public:
   // constructors and destructors
   ImportStreamDialog( ImportFileHandle *_mFile,
      wxWindow *parent, wxWindowID id, const wxString &title,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER );
   virtual ~ImportStreamDialog();

private:
   ImportFileHandle *mFile;
   wxInt32 scount;
   wxListBox *StreamList;

private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );

private:
   DECLARE_EVENT_TABLE()
};

#endif


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 30d8bb1f-1e74-4cec-918c-4b36a9a75c49

