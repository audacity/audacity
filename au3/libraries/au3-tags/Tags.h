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
#include "UndoManager.h" // To inherit UndoStateExtension
#include <utility>

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
class ComboEditor;

using TagMap = std::unordered_map< wxString, wxString >;

#define TAG_TITLE       wxT("TITLE")
#define TAG_ARTIST      wxT("ARTIST")
#define TAG_ALBUM       wxT("ALBUM")
#define TAG_TRACK       wxT("TRACKNUMBER")
#define TAG_YEAR        wxT("YEAR")
#define TAG_GENRE       wxT("GENRE")
#define TAG_COMMENTS    wxT("COMMENTS")
#define TAG_SOFTWARE    wxT("Software")
#define TAG_COPYRIGHT   wxT("Copyright")

class TAGS_API Tags final : public XMLTagHandler, public std::enable_shared_from_this< Tags >, public ClientData::Base,
    public UndoStateExtension
{
public:

    static Tags& Get(AudacityProject& project);
    static const Tags& Get(const AudacityProject& project);
    // Returns reference to *tags
    static Tags& Set(
        AudacityProject& project, const std::shared_ptr<Tags>& tags);

    Tags(); // constructor
    Tags(const Tags&) = default;
    //Tags( Tags && ) = default;
    virtual ~Tags();

    std::shared_ptr<Tags> Duplicate() const;

    void Merge(const Tags& other);

    Tags& operator=(const Tags& src);

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXML(XMLWriter& xmlFile) const /* not override */;

    void LoadDefaultGenres();
    void LoadGenres();

    void LoadDefaults();

    int GetNumUserGenres();
    wxString GetUserGenre(int value);

    wxString GetGenre(int value);
    int GetGenre(const wxString& name);

    bool HasTag(const wxString& name) const;
    wxString GetTag(const wxString& name) const;

    using Iterators = IteratorRange<TagMap::const_iterator>;
    Iterators GetRange() const;

    void SetTag(const wxString& name, const wxString& value, const bool bSpecialTag=false);
    void SetTag(const wxString& name, const int& value);

    bool IsEmpty();
    void Clear();

    size_t Count() const;

    // UndoStateExtension implementation
    void RestoreUndoRedoState(AudacityProject&) override;

    friend TAGS_API bool operator ==(const Tags& lhs, const Tags& rhs);

private:
    TagMap mXref;
    TagMap mMap;

    wxArrayString mGenres;
};

inline bool operator !=(const Tags& lhs, const Tags& rhs)
{ return !(lhs == rhs); }

#endif
