/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Tags
\brief ID3 Tags (for MP3)

  This class started as an ID3 tag

  This class holds a few informational tags, such as Title, Author,
  etc. that can be associated with a project or other audio file.
  It is modeled after the ID3 format for MP3 files, and it can
  both import and export ID3 tags from/to MP2, MP3, and AIFF files.

  It can present the user with a dialog for editing this information.

  Use of this functionality requires that libid3tag be compiled in
  with Audacity.

*//****************************************************************//**

\class TagsEditorDialog
\brief Derived from ExpandingToolBar, this dialog allows editing of Tags.

*//*******************************************************************/

#include "Tags.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/log.h>

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#endif

#include "FileNames.h"
#include "Prefs.h"
#include "Project.h"

static const wxChar* DefaultGenres[] =
{
    wxT("Blues"),
    wxT("Classic Rock"),
    wxT("Country"),
    wxT("Dance"),
    wxT("Disco"),
    wxT("Funk"),
    wxT("Grunge"),
    wxT("Hip-Hop"),
    wxT("Jazz"),
    wxT("Metal"),
    wxT("New Age"),
    wxT("Oldies"),
    wxT("Other"),
    wxT("Pop"),
    wxT("R&B"),
    wxT("Rap"),
    wxT("Reggae"),
    wxT("Rock"),
    wxT("Techno"),
    wxT("Industrial"),
    wxT("Alternative"),
    wxT("Ska"),
    wxT("Death Metal"),
    wxT("Pranks"),
    wxT("Soundtrack"),
    wxT("Euro-Techno"),
    wxT("Ambient"),
    wxT("Trip-Hop"),
    wxT("Vocal"),
    wxT("Jazz+Funk"),
    wxT("Fusion"),
    wxT("Trance"),
    wxT("Classical"),
    wxT("Instrumental"),
    wxT("Acid"),
    wxT("House"),
    wxT("Game"),
    wxT("Sound Clip"),
    wxT("Gospel"),
    wxT("Noise"),
    wxT("Alt. Rock"),
    wxT("Bass"),
    wxT("Soul"),
    wxT("Punk"),
    wxT("Space"),
    wxT("Meditative"),
    wxT("Instrumental Pop"),
    wxT("Instrumental Rock"),
    wxT("Ethnic"),
    wxT("Gothic"),
    wxT("Darkwave"),
    wxT("Techno-Industrial"),
    wxT("Electronic"),
    wxT("Pop-Folk"),
    wxT("Eurodance"),
    wxT("Dream"),
    wxT("Southern Rock"),
    wxT("Comedy"),
    wxT("Cult"),
    wxT("Gangsta Rap"),
    wxT("Top 40"),
    wxT("Christian Rap"),
    wxT("Pop/Funk"),
    wxT("Jungle"),
    wxT("Native American"),
    wxT("Cabaret"),
    wxT("New Wave"),
    wxT("Psychedelic"),
    wxT("Rave"),
    wxT("Showtunes"),
    wxT("Trailer"),
    wxT("Lo-Fi"),
    wxT("Tribal"),
    wxT("Acid Punk"),
    wxT("Acid Jazz"),
    wxT("Polka"),
    wxT("Retro"),
    wxT("Musical"),
    wxT("Rock & Roll"),
    wxT("Hard Rock"),
    wxT("Folk"),
    wxT("Folk/Rock"),
    wxT("National Folk"),
    wxT("Swing"),
    wxT("Fast-Fusion"),
    wxT("Bebob"),
    wxT("Latin"),
    wxT("Revival"),
    wxT("Celtic"),
    wxT("Bluegrass"),
    wxT("Avantgarde"),
    wxT("Gothic Rock"),
    wxT("Progressive Rock"),
    wxT("Psychedelic Rock"),
    wxT("Symphonic Rock"),
    wxT("Slow Rock"),
    wxT("Big Band"),
    wxT("Chorus"),
    wxT("Easy Listening"),
    wxT("Acoustic"),
    wxT("Humour"),
    wxT("Speech"),
    wxT("Chanson"),
    wxT("Opera"),
    wxT("Chamber Music"),
    wxT("Sonata"),
    wxT("Symphony"),
    wxT("Booty Bass"),
    wxT("Primus"),
    wxT("Porn Groove"),
    wxT("Satire"),
    wxT("Slow Jam"),
    wxT("Club"),
    wxT("Tango"),
    wxT("Samba"),
    wxT("Folklore"),
    wxT("Ballad"),
    wxT("Power Ballad"),
    wxT("Rhythmic Soul"),
    wxT("Freestyle"),
    wxT("Duet"),
    wxT("Punk Rock"),
    wxT("Drum Solo"),
    wxT("A Cappella"),
    wxT("Euro-House"),
    wxT("Dance Hall"),
    wxT("Goa"),
    wxT("Drum & Bass"),
    wxT("Club-House"),
    wxT("Hardcore"),
    wxT("Terror"),
    wxT("Indie"),
    wxT("BritPop"),

    // Standard name is offensive (see "http://www.audacityteam.org/forum/viewtopic.php?f=11&t=3924").
    wxT("Offensive"), // wxT("Negerpunk"),

    wxT("Polsk Punk"),
    wxT("Beat"),
    wxT("Christian Gangsta Rap"),
    wxT("Heavy Metal"),
    wxT("Black Metal"),
    wxT("Crossover"),
    wxT("Contemporary Christian"),
    wxT("Christian Rock"),
    wxT("Merengue"),
    wxT("Salsa"),
    wxT("Thrash Metal"),
    wxT("Anime"),
    wxT("JPop"),
    wxT("Synthpop")
};

static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
    "tags",
    []( AudacityProject& project ){ return &Tags::Get(project); }
};

static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](AudacityProject&){ return std::make_shared< Tags >(); }
};

Tags& Tags::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get< Tags >(key);
}

const Tags& Tags::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

Tags& Tags::Set(AudacityProject& project, const std::shared_ptr< Tags >& tags)
{
    auto& result = *tags;
    project.AttachedObjects::Assign(key, tags);
    return result;
}

Tags::Tags()
{
    LoadDefaults();
    LoadGenres();
}

Tags::~Tags()
{
}

std::shared_ptr<Tags> Tags::Duplicate() const
{
    return std::make_shared<Tags>(*this);
}

void Tags::Merge(const Tags& other)
{
    for ( auto& pair : other.mMap ) {
        SetTag(pair.first, pair.second);
    }
}

Tags& Tags::operator=(const Tags& src)
{
    mXref.clear();
    mXref = src.mXref;
    mMap.clear();
    mMap = src.mMap;

    mGenres.clear();
    mGenres = src.mGenres;

    return *this;
}

void Tags::LoadDefaults()
{
    wxString value;
    auto tagsGroup = gPrefs->BeginGroup("/Tags");
    for (const auto& key : gPrefs->GetChildKeys()) {
        gPrefs->Read(key, &value, {});
        if (key == wxT("ID3V2")) {
            // LLL:  This is obsolute, but it must be handled and ignored.
        } else {
            SetTag(key, value);
        }
    }
}

bool Tags::IsEmpty()
{
    // At least one of these should be filled in, otherwise
    // it's assumed that the tags have not been set...
    if (HasTag(TAG_TITLE) || HasTag(TAG_ARTIST) || HasTag(TAG_ALBUM)) {
        return false;
    }

    return true;
}

void Tags::Clear()
{
    mXref.clear();
    mMap.clear();
}

size_t Tags::Count() const
{
    return mMap.size();
}

namespace {
bool EqualMaps(const TagMap& map1, const TagMap& map2)
{
    // Maps are unordered, hash maps; can't just iterate in tandem and
    // compare.
    if (map1.size() != map2.size()) {
        return false;
    }

    for (const auto& pair : map2) {
        auto iter = map1.find(pair.first);
        if (iter == map1.end() || iter->second != pair.second) {
            return false;
        }
    }

    return true;
}
}

bool operator==(const Tags& lhs, const Tags& rhs)
{
    if (!EqualMaps(lhs.mXref, rhs.mXref)) {
        return false;
    }

    if (!EqualMaps(lhs.mMap, rhs.mMap)) {
        return false;
    }

    return lhs.mGenres == rhs.mGenres;
}

int Tags::GetNumUserGenres()
{
    return mGenres.size();
}

void Tags::LoadDefaultGenres()
{
    mGenres.clear();
    for (size_t i = 0; i < WXSIZEOF(DefaultGenres); i++) {
        mGenres.push_back(DefaultGenres[i]);
    }
}

void Tags::LoadGenres()
{
    wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
    wxTextFile tf(fn.GetFullPath());

    if (!tf.Exists() || !tf.Open()) {
        LoadDefaultGenres();
        return;
    }

    mGenres.clear();

    int cnt = tf.GetLineCount();
    for (int i = 0; i < cnt; i++) {
        mGenres.push_back(tf.GetLine(i));
    }
}

wxString Tags::GetUserGenre(int i)
{
    if (i >= 0 && i < GetNumUserGenres()) {
        return mGenres[i];
    }

    return wxT("");
}

wxString Tags::GetGenre(int i)
{
    int cnt = WXSIZEOF(DefaultGenres);

    if (i >= 0 && i < cnt) {
        return DefaultGenres[i];
    }

    return wxT("");
}

int Tags::GetGenre(const wxString& name)
{
    int cnt = WXSIZEOF(DefaultGenres);

    for (int i = 0; i < cnt; i++) {
        if (name.CmpNoCase(DefaultGenres[i])) {
            return i;
        }
    }

    return 255;
}

bool Tags::HasTag(const wxString& name) const
{
    wxString key = name;
    key.UpperCase();

    auto iter = mXref.find(key);
    return iter != mXref.end();
}

wxString Tags::GetTag(const wxString& name) const
{
    wxString key = name;
    key.UpperCase();

    auto iter = mXref.find(key);

    if (iter == mXref.end()) {
        return wxEmptyString;
    }

    auto iter2 = mMap.find(iter->second);
    if (iter2 == mMap.end()) {
        wxASSERT(false);
        return wxEmptyString;
    } else {
        return iter2->second;
    }
}

Tags::Iterators Tags::GetRange() const
{
    return { mMap.begin(), mMap.end() };
}

void Tags::SetTag(const wxString& name, const wxString& value, const bool bSpecialTag)
{
    // We don't like empty names
    if (name.empty()) {
        return;
    }

    // Tag name must be ascii
    if (!name.IsAscii()) {
        wxLogError("Tag rejected (Non-ascii character in name)");
        return;
    }

    // All keys are uppercase
    wxString key = name;
    key.UpperCase();

    // Look it up
    TagMap::iterator iter = mXref.find(key);

    // The special tags, if empty, should not exist.
    // However it is allowable for a custom tag to be empty.
    // See Bug 440 and Bug 1382
    if (value.empty() && bSpecialTag) {
        // Erase the tag
        if (iter == mXref.end()) {
            // nothing to do
        } else {
            mMap.erase(iter->second);
            mXref.erase(iter);
        }
    } else {
        if (iter == mXref.end()) {
            // Didn't find the tag

            // Add a NEW tag
            mXref[key] = name;
            mMap[name] = value;
        } else if (iter->second != name) {
            // Watch out for case differences!
            mMap[name] = value;
            mMap.erase(iter->second);
            iter->second = name;
        } else {
            // Update the value
            mMap[iter->second] = value;
        }
    }
}

void Tags::SetTag(const wxString& name, const int& value)
{
    SetTag(name, wxString::Format(wxT("%d"), value));
}

bool Tags::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    if (tag == "tags") {
        return true;
    }

    if (tag == "tag") {
        wxString n, v;

        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            if (attr == "name") {
                n = value.ToWString();
            } else if (attr == "value") {
                v = value.ToWString();
            }
        }

        if (n == wxT("id3v2")) {
            // LLL:  This is obsolete, but it must be handled and ignored.
        } else {
            SetTag(n, v);
        }

        return true;
    }

    return false;
}

XMLTagHandler* Tags::HandleXMLChild(const std::string_view& tag)
{
    if (tag == "tags") {
        return this;
    }

    if (tag == "tag") {
        return this;
    }

    return NULL;
}

void Tags::WriteXML(XMLWriter& xmlFile) const
// may throw
{
    xmlFile.StartTag(wxT("tags"));

    for (const auto& pair : GetRange()) {
        const auto& n = pair.first;
        const auto& v = pair.second;
        xmlFile.StartTag(wxT("tag"));
        xmlFile.WriteAttr(wxT("name"), n);
        xmlFile.WriteAttr(wxT("value"), v);
        xmlFile.EndTag(wxT("tag"));
    }

    xmlFile.EndTag(wxT("tags"));
}

static ProjectFileIORegistry::ObjectWriterEntry entry {
    [](const AudacityProject& project, XMLWriter& xmlFile){
        Tags::Get(project).WriteXML(xmlFile);
    }
};

// Undo/redo handling
static UndoRedoExtensionRegistry::Entry sEntry {
    [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension> {
        return Tags::Get(project).shared_from_this();
    }
};

void Tags::RestoreUndoRedoState(AudacityProject& project)
{
    // Restore tags
    Tags::Set(project, shared_from_this());
}
