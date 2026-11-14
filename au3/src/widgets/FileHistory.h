/**********************************************************************

  Audacity: A Digital Audio Editor

  FileHistory.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_FILEHISTORY__
#define __AUDACITY_WIDGETS_FILEHISTORY__

#include <vector>
#include <algorithm>
#include <wx/defs.h>
#include <wx/weakref.h> // member variable

#include "Identifier.h"
#include "wxArrayStringEx.h"

namespace audacity {
class BasicSettings;
}

class wxMenu;

class AUDACITY_DLL_API FileHistory
{
public:
    FileHistory(size_t maxfiles = 12, wxWindowID idbase = wxID_FILE);
    virtual ~FileHistory();
    FileHistory(const FileHistory&) = delete;
    FileHistory& operator =(const FileHistory&) = delete;

    // These constants define the range of IDs reserved by the global file history
    enum {
        ID_RECENT_CLEAR = 6100,
        ID_RECENT_FIRST = 6101,
        ID_RECENT_LAST  = 6112
    };

    static FileHistory& Global();

    void Append(const FilePath& file)
    { AddFileToHistory(file, true); }
    void Remove(size_t i);
    void Clear();

    // Causes this menu to reflect the contents of this FileHistory, now and
    // also whenever the history changes.
    void UseMenu(wxMenu* menu);

    void Load(audacity::BasicSettings& config, const wxString& group = wxEmptyString);
    void Save(audacity::BasicSettings& config);

    // stl-style accessors
    using const_iterator = FilePaths::const_iterator;
    const_iterator begin() const { return mHistory.begin(); }
    const_iterator end() const { return mHistory.end(); }
    const FilePath& operator[](size_t ii) const { return mHistory[ ii ]; }
    bool empty() const { return mHistory.empty(); }

private:
    void AddFileToHistory(const FilePath& file, bool update);
    void NotifyMenus();
    void NotifyMenu(wxMenu* menu);

    void Compress();

    size_t mMaxFiles;
    wxWindowID mIDBase;

    std::vector< wxWeakRef< wxMenu > > mMenus;
    FilePaths mHistory;

    wxString mGroup;
};

#endif
