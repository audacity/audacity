/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.h

  Dominic Mazzoni

  This class holds a few informational tags, such as Title, Author,
  etc. that can be associated with a project or other audio file.
  It is modeled after the ID3 format for MP3 files, and it can
  both import ID3 tags from MP3 files, and export them as well.

  It can present the user with a dialog for editing this information.

  It only keeps track of the fields that are standard in ID3v1
  (title, author, artist, track num, year, genre, and comments),
  but it can export both ID3v1 or the newer ID3v2 format.  The primary
  reason you would want to export ID3v2 tags instead of ID3v1,
  since we're not supporting any v2 fields, is that ID3v2 tags are
  inserted at the BEGINNING of an mp3 file, which is far more
  useful for streaming.

  Use of this functionality requires that libid3tag be compiled in
  with Audacity.

**********************************************************************/

#ifndef __AUDACITY_TAGS__
#define __AUDACITY_TAGS__



#include "XMLTagHandler.h"

#include "ClientData.h"
#include <utility>

#include "widgets/wxPanelWrapper.h" // to inherit

#include <memory>
#include <unordered_map>
#include "Identifier.h"

class wxArrayString;
class wxComboBox;
class wxGridCellChoiceEditor;
class wxGridCellStringRenderer;
class wxGridEvent;
class wxTextCtrl;

class AudacityProject;
class Grid;
class ShuttleGui;
class TagsEditorDialog;
class ComboEditor;

using TagMap = std::unordered_map< wxString, wxString >;

#define TAG_TITLE     wxT("TITLE")
#define TAG_ARTIST   wxT("ARTIST")
#define TAG_ALBUM    wxT("ALBUM")
#define TAG_TRACK    wxT("TRACKNUMBER")
#define TAG_YEAR     wxT("YEAR")
#define TAG_GENRE    wxT("GENRE")
#define TAG_COMMENTS wxT("COMMENTS")
#define TAG_SOFTWARE wxT("Software")
#define TAG_COPYRIGHT wxT("Copyright")

class AUDACITY_DLL_API Tags final
   : public XMLTagHandler
   , public std::enable_shared_from_this< Tags >
   , public ClientData::Base
{

 public:

   static Tags &Get( AudacityProject &project );
   static const Tags &Get( const AudacityProject &project );
   // Returns reference to *tags
   static Tags &Set(
      AudacityProject &project, const std::shared_ptr<Tags> &tags );

   Tags();  // constructor
   Tags( const Tags& ) = default;
   //Tags( Tags && ) = default;
   virtual ~Tags();

   std::shared_ptr<Tags> Duplicate() const;

   void Merge( const Tags &other );

   Tags & operator= (const Tags & src );

   bool ShowEditDialog(
      wxWindow *parent, const TranslatableString &title, bool force = false);

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const /* not override */;

   void AllowEditTitle(bool editTitle);
   void AllowEditTrackNumber(bool editTrackNumber);

   void LoadDefaultGenres();
   void LoadGenres();

   void LoadDefaults();

   int GetNumUserGenres();
   wxString GetUserGenre(int value);

   wxString GetGenre(int value);
   int GetGenre(const wxString & name);

   bool HasTag(const wxString & name) const;
   wxString GetTag(const wxString & name) const;

   using Iterators = IteratorRange<TagMap::const_iterator>;
   Iterators GetRange() const;

   void SetTag(const wxString & name, const wxString & value, const bool bSpecialTag=false);
   void SetTag(const wxString & name, const int & value);

   bool IsEmpty();
   void Clear();

   friend bool operator == (const Tags &lhs, const Tags &rhs);

 private:
   TagMap mXref;
   TagMap mMap;

   wxArrayString mGenres;

   bool mEditTitle;
   bool mEditTrackNumber;
};

inline bool operator != (const Tags &lhs, const Tags &rhs)
{ return !(lhs == rhs); }

class TagsEditorDialog final : public wxDialogWrapper
{
 public:
   // constructors and destructors
   TagsEditorDialog(wxWindow * parent,
              const TranslatableString &title,
              Tags * tags,
              bool editTitle,
              bool editTrack);

   virtual ~TagsEditorDialog();

#if !defined(__WXMSW__)
   bool IsEscapeKey(const wxKeyEvent& /*event*/) override { return false; }
#endif

   void PopulateOrExchange(ShuttleGui & S);

   void OnDontShow( wxCommandEvent & Evt);
   void OnHelp(wxCommandEvent & Evt);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

 private:
   void PopulateGenres();
   void SetEditors();

   void OnChange(wxGridEvent & event);

   void OnEdit(wxCommandEvent & event);
   void OnReset(wxCommandEvent & event);

   void OnClear(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   void OnAdd(wxCommandEvent & event);
   void OnRemove(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void DoCancel(bool escKey);
   void OnCancel(wxCommandEvent & event);

   void OnKeyDown(wxKeyEvent &event);

   bool IsWindowRectValid(const wxRect *windowRect) const;

 private:
   Tags *mTags;
   bool mEditTitle;
   bool mEditTrack;

   Tags mLocal;

   Grid *mGrid;
   ComboEditor *mComboEditor;
   wxGridCellStringRenderer *mStringRenderer;

   DECLARE_EVENT_TABLE()
};

#endif
