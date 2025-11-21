/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

#include "ImportForwards.h"
#include "Identifier.h"
#include <vector>
#include <wx/tokenzr.h> // for enum wxStringTokenizerMode

#include "FileNames.h" // for FileType

#include "Registry.h"

class wxArrayString;
class AudacityProject;
class Tags;
class WaveTrackFactory;
class Track;
class TrackList;
class ImportPlugin;
class ImportProgressListener;
class UnusableImportPlugin;
typedef bool (* progress_callback_t)(void* userData, float percent);

class ExtImportItem;
class WaveTrack;

namespace LibFileFormats {
struct AcidizerTags;
}

using ExtImportItems = std::vector<std::unique_ptr<ExtImportItem> >;
using TrackHolders = std::vector<std::shared_ptr<Track> >;

class ExtImportItem
{
public:
    /**
     * Unique string ID exists for each filter, it is not translatable
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
    std::vector<ImportPlugin*> filter_objects;

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

class IMPORT_EXPORT_API Importer
{
    struct ImporterItem;
public:

    // Objects of this type are statically constructed in files implementing
    // subclasses of ImportPlugin
    struct IMPORT_EXPORT_API RegisteredImportPlugin final : Registry::RegisteredItem<ImporterItem>
    {
        RegisteredImportPlugin(
            const Identifier& id, // an internal string naming the plug-in
            std::unique_ptr<ImportPlugin>, const Registry::Placement& placement = { wxEmptyString, {} });
    };

    // Objects of this type are statically constructed in files, to identify
    // unsupported import formats; typically in a conditional compilation
    struct RegisteredUnusableImportPlugin {
        RegisteredUnusableImportPlugin(std::unique_ptr<UnusableImportPlugin>);
    };

    Importer();
    ~Importer();

    Importer(const Importer&) = delete;
    Importer& operator=(Importer&) = delete;

    /**
     * Return instance reference
     */
    static Importer& Get();

    /**
     * Initialization/Termination
     */
    bool Initialize();
    bool Terminate();

    /**
     * Constructs a list of types, for use by file opening dialogs, that includes
     * all supported file types
     */
    FileNames::FileTypes
    GetFileTypes(const FileNames::FileType& extraType = {});

    /**
     * Remember a file type in preferences
     */
    static void
    SetLastOpenType(const FileNames::FileType& type);

    /**
     * Remember a file type in preferences
     */
    static void
    SetDefaultOpenType(const FileNames::FileType& type);

    /**
     * Choose index of preferred type
     */
    static size_t
    SelectDefaultOpenType(const FileNames::FileTypes& fileTypes);

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
     * @mod defines tokenizer's behaviour.
     */
    void StringToList(wxString& str, wxString& delims, wxArrayString& list, wxStringTokenizerMode mod = wxTOKEN_RET_EMPTY_ALL);

    /**
     * Returns a pointer to internal items array.
     * External objects are allowed to change the array contents.
     */
    ExtImportItems& GetImportItems() { return mExtImportItems; }

    /**
     * Allocates NEW ExtImportItem, fills it with default data
     * and returns a pointer to it.
     */
    std::unique_ptr<ExtImportItem> CreateDefaultImportItem();

    // if false, the import failed and errorMessage will be set.
    bool Import(
        AudacityProject& project, const FilePath& fName, ImportProgressListener* importProgressListener, WaveTrackFactory* trackFactory,
        TrackHolders& tracks, Tags* tags, std::optional<LibFileFormats::AcidizerTags>& outAcidTags, TranslatableString& errorMessage);

private:
    struct Traits : Registry::DefaultTraits
    {
        using LeafTypes = List<ImporterItem>;
    };
    struct IMPORT_EXPORT_API ImporterItem final : Registry::SingleItem {
        static Registry::GroupItem<Traits>& Registry();

        ImporterItem(const Identifier& id, std::unique_ptr<ImportPlugin> pPlugin);
        ~ImporterItem();
        std::unique_ptr<ImportPlugin> mpPlugin;
    };

    static Importer mInstance;

    ExtImportItems mExtImportItems;
    static ImportPluginList& sImportPluginList();
    static UnusableImportPluginList& sUnusableImportPluginList();
};

extern IMPORT_EXPORT_API BoolSetting NewImportingSession;

#endif
