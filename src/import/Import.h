/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

#include "ImportRaw.h" // defines TrackHolders
#include "ImportForwards.h"
#include <vector>
#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/listbox.h>
#include <wx/tokenzr.h>

#include "../widgets/wxPanelWrapper.h"

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

   Format(const wxString &_formatName, const wxArrayString &_formatExtensions):
      formatName(_formatName),
      formatExtensions(_formatExtensions)
   {
   }
};

class ExtImportItem;

using FormatList = std::vector<Format> ;
WX_DEFINE_ARRAY_PTR(ImportPlugin *, ImportPluginPtrArray);
using ExtImportItems = std::vector< movable_ptr<ExtImportItem> >;

class ExtImportItem
{
  public:
  /**
   * Unique string ID exists for each filter, it is not translateable
   * and can be stored in config. This ID is matched internally with a
   * translated name of a filter.
   * Unknown IDs will be presented and saved as-is.
   * Unused filters will not be used for matching files, unless forced.
   */
  wxArrayString filters;

  /**
   * The index of first unused filter in @filters array
   * 0 - divider is at the top of the list (in the list control
   * it will be the highest item), all filters are unused
   * -1 - divider is at the bottom of the list (in the list control
   * it will be the lowest item), all filters are used
   */
  int divider;

  /**
   * Array of pointers to import plugins (members of FormatList)
   */
  ImportPluginPtrArray filter_objects;

  /**
   * File extensions. Each one is a string with simple wildcards,
   * i.e. "*.wav".
   */
  wxArrayString extensions;

  /**
   * Mime-types. Each one is a string in form of "foo/bar-baz" or
   * something like that.
   */
  wxArrayString mime_types;
};

class Importer {
public:
   Importer();
   ~Importer();

   /**
    * Return instance reference
    */
   static Importer & Get();

   /**
    * Initialization/Termination
    */
   bool Initialize();
   bool Terminate();

   /**
    * Fills @formatList with a list of supported import formats
    */
   void GetSupportedImportFormats(FormatList *formatList);

   /**
    * Reads extended import filters from gPrefs into internal
    * list mExtImportItems
    */
   void ReadImportItems();

   /**
    * Writes mExtImportItems into gPrefs
    */
   void WriteImportItems();

   /**
    * Helper function - uses wxStringTokenizer to tokenize
    * @str string and appends string-tokens to a list @list.
    * @mod deifines tokenizer's behaviour.
    */
   void StringToList(wxString &str, wxString &delims, wxArrayString &list, wxStringTokenizerMode mod = wxTOKEN_RET_EMPTY_ALL);

   /**
    * Returns a pointer to internal items array.
    * External objects are allowed to change the array contents.
    */
   ExtImportItems &GetImportItems() { return mExtImportItems; };

   /**
    * Allocates NEW ExtImportItem, fills it with default data
    * and returns a pointer to it.
    */
    movable_ptr<ExtImportItem> CreateDefaultImportItem();

   // if false, the import failed and errorMessage will be set.
   bool Import(const wxString &fName,
              TrackFactory *trackFactory,
              TrackHolders &tracks,
              Tags *tags,
              wxString &errorMessage);

private:
   static Importer mInstance;

   ExtImportItems mExtImportItems;
   ImportPluginList mImportPluginList;
   UnusableImportPluginList mUnusableImportPluginList;
};

//----------------------------------------------------------------------------
// ImportStreamDialog
//----------------------------------------------------------------------------

class ImportStreamDialog final : public wxDialogWrapper
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
