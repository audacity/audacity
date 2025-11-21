/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO__
#define __AUDACITY_PROJECT_FILE_IO__

#include <memory>
#include <optional>
#include <unordered_set>

#include <wx/event.h>

#include "ClientData.h" // to inherit
#include "Observer.h"
#include "Prefs.h" // to inherit
#include "XMLTagHandler.h" // to inherit

struct sqlite3;
struct sqlite3_context;
struct sqlite3_stmt;
struct sqlite3_value;

class AudacityProject;
class DBConnection;
struct DBConnectionErrors;
class ProjectSerializer;
class SqliteSampleBlock;
class TrackList;
class WaveTrack;

namespace BasicUI {
class WindowPlacement;
}

using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;

// From SampleBlock.h
using SampleBlockID = long long;

using Connection = std::unique_ptr<DBConnection>;

using BlockIDs = std::unordered_set<SampleBlockID>;

//! Subscribe to ProjectFileIO to receive messages; always in idle time
enum class ProjectFileIOMessage : int {
    CheckpointFailure,  //!< Failure happened in a worker thread
    ReconnectionFailure, /*!< Failure to reconnect to the database,
      after temporary close and attempted file movement */
    ProjectTitleChange, //!< A normal occurrence
    ProjectFilePathChange, //!< A normal occurrence
};

///\brief Object associated with a project that manages reading and writing
/// of Audacity project file formats, and autosave
class PROJECT_FILE_IO_API ProjectFileIO final : public ClientData::Base, public XMLTagHandler, private PrefsListener,
    public std::enable_shared_from_this<ProjectFileIO>, public Observer::Publisher<ProjectFileIOMessage>
{
public:
    //! Represents a change in the association between in-memory project and
    //! project file, which may be committed or abandoned
    struct PROJECT_FILE_IO_API TentativeConnection {
        TentativeConnection(ProjectFileIO& projectFileIO);
        TentativeConnection(const TentativeConnection& other) = delete;
        TentativeConnection(TentativeConnection&& other);
        ~TentativeConnection();
        void SetFileName(const FilePath& fileName);
        void Commit();
    private:
        ProjectFileIO& mProjectFileIO;
        FilePath mFileName;
        bool mCommitted{ false };
    };

    // Call this static function once before constructing any instances of this
    // class.  Reinvocations have no effect.  Return value is true for success.
    static bool InitializeSQL();

    static ProjectFileIO& Get(AudacityProject& project);
    static const ProjectFileIO& Get(const AudacityProject& project);

    explicit ProjectFileIO(AudacityProject& project);

    ProjectFileIO(const ProjectFileIO&) = delete;
    ProjectFileIO& operator=(const ProjectFileIO&) = delete;
    ~ProjectFileIO();

    const wxString& GetProjectTitle() const { return mTitle; }

    // It seems odd to put this method in this class, but the results do depend
    // on what is discovered while opening the file, such as whether it is a
    // recovery file
    void SetProjectTitle(int number = -1);

    // Should be empty or a fully qualified file name
    const FilePath& GetFileName() const;
    void SetFileName(const FilePath& fileName);

    bool IsModified() const;
    bool IsTemporary() const;
    bool IsRecovered() const;

    void MarkTemporary();

    bool AutoSave(bool recording = false);
    bool AutoSaveDelete(sqlite3* db = nullptr);

    bool OpenProject();
    void CloseProject();
    bool ReopenProject();

    //! If successful, return non-empty; the caller must commit to keep the
    //! association of the opened file with the project
    std::optional<TentativeConnection>
    LoadProject(const FilePath& fileName, bool ignoreAutosave);

    bool UpdateSaved(const TrackList* tracks = nullptr);
    bool SaveProject(const FilePath& fileName, const TrackList* lastSaved);
    bool SaveCopy(const FilePath& fileName);

    wxLongLong GetFreeDiskSpace() const;

    // Returns the bytes used for the given sample block
    int64_t GetBlockUsage(SampleBlockID blockid);

    // Returns the bytes used for all blocks owned by the given track list
    int64_t GetCurrentUsage(
        const std::vector<const TrackList*>& trackLists) const;

    // Return the bytes used by all sample blocks in the project file, whether
    // they are attached to the active tracks or held by the Undo manager.
    int64_t GetTotalUsage();

    // Return the bytes used for the given block using the connection to a
    // specific database. This is the workhorse for the above 3 methods.
    static int64_t GetDiskUsage(DBConnection& conn, SampleBlockID blockid);

    // Displays an error dialog with a button that offers help
    void ShowError(const BasicUI::WindowPlacement& placement, const TranslatableString& dlogTitle, const TranslatableString& message,
                   const wxString& helpPage);
    const TranslatableString& GetLastError() const;
    const TranslatableString& GetLibraryError() const;
    int GetLastErrorCode() const;
    const wxString& GetLastLog() const;

    // Provides a means to bypass "DELETE"s at shutdown if the database
    // is just going to be deleted anyway.  This prevents a noticeable
    // delay caused by SampleBlocks being deleted when the Sequences that
    // own them are deleted.
    //
    // This is definitely hackage territory.  While this ability would
    // still be needed, I think handling it in a DB abstraction might be
    // a tad bit cleaner.
    //
    // For it's usage, see:
    //    SqliteSampleBlock::~SqliteSampleBlock()
    //    ProjectManager::OnCloseWindow()
    void SetBypass();

private:
    //! Strings like -wal that may be appended to main project name to get other files created by
    //! the database system
    static const std::vector<wxString>& AuxiliaryFileSuffixes();

    //! Generate a name for short-lived backup project files from an existing project
    static FilePath SafetyFileName(const FilePath& src);

    //! Rename a file or put up appropriate warning message.
    /*! Failure might happen when renaming onto another device, doing copy of contents */
    bool RenameOrWarn(const FilePath& src, const FilePath& dst);

    bool MoveProject(const FilePath& src, const FilePath& dst);

public:
    //! Remove any files associated with a project at given path; return true if successful
    static bool RemoveProject(const FilePath& filename);

    // Object manages the temporary backing-up of project paths while
    // trying to overwrite with new contents, and restoration in case of failure
    class PROJECT_FILE_IO_API BackupProject
    {
    public:
        //! Rename project file at path, and any auxiliary files, to backup path names
        BackupProject(ProjectFileIO& projectFileIO, const FilePath& path);
        //! Returns false if the renaming in the constructor failed
        bool IsOk() { return !mPath.empty(); }
        //! if `!IsOk()` do nothing; else remove backup files
        void Discard();
        //! if `!IsOk()` do nothing; else if `Discard()` was not called, undo the renaming
        ~BackupProject();
    private:
        FilePath mPath, mSafety;
    };

    // Remove all unused space within a project file
    void Compact(
        const std::vector<const TrackList*>& tracks, bool force = false);

    // The last compact check did actually compact the project file if true
    bool WasCompacted();

    // The last compact check found unused blocks in the project file
    bool HadUnused();

    // In one SQL command, delete sample blocks with ids in the given set, or
    // (when complement is true), with ids not in the given set.
    bool DeleteBlocks(const BlockIDs& blockids, bool complement);

    // Type of function that is given the fields of one row and returns
    // 0 for success or non-zero to stop the query
    using ExecCB = std::function<int (int cols, char** vals, char** names)>;

    //! Return true if a connection is now open
    bool HasConnection() const;

    //! Return a reference to a connection, creating it as needed on demand; throw on failure
    DBConnection& GetConnection();

    //! Return a strings representation of the active project XML doc
    wxString GenerateDoc();

private:
    void OnCheckpointFailure();

    void WriteXMLHeader(XMLWriter& xmlFile) const;
    void WriteXML(XMLWriter& xmlFile, bool recording = false, const TrackList* tracks = nullptr) /* not override */;

    // XMLTagHandler callback methods
    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    void UpdatePrefs() override;

    int Exec(const char* query, const ExecCB& callback, bool silent = false);

    // The opening of the database may be delayed until demanded.
    // Returns a non-null pointer to an open database, or throws an exception
    // if opening fails.
    sqlite3* DB();

    bool OpenConnection(FilePath fileName = {});
    bool CloseConnection();

    // Put the current database connection aside, keeping it open, so that
    // another may be opened with OpenDB()
    void SaveConnection();

    // Close any set-aside connection
    void DiscardConnection();

    // Close any current connection and switch back to using the saved
    void RestoreConnection();

    // Use a connection that is already open rather than invoke OpenConnection
    void UseConnection(Connection&& conn, const FilePath& filePath);

    bool Query(const char* sql, const ExecCB& callback, bool silent = false);

    bool GetValue(const char* sql, wxString& value, bool silent = false);
    bool GetValue(const char* sql, int64_t& value, bool silent = false);

    bool CheckVersion();
    bool InstallSchema(sqlite3* db, const char* schema = "main");

    // Write project or autosave XML (binary) documents
    bool WriteDoc(const char* table, const ProjectSerializer& autosave, const char* schema = "main");

    // Application defined function to verify blockid exists is in set of blockids
    static void InSet(sqlite3_context* context, int argc, sqlite3_value** argv);

    // Return a database connection if successful, which caller must close
    bool CopyTo(const FilePath& destpath, const TranslatableString& msg, bool isTemporary, bool prune = false, const std::vector<const TrackList*>& tracks = {} /*!<
         First track list (or if none, then the project's track list) are tracks to write into document blob;
         That list, plus any others, contain tracks whose sample blocks must be kept
      */
                );

    //! Just set stored errors
    void SetError(const TranslatableString& msg, const TranslatableString& libraryError = {}, int errorCode = {});

    //! Set stored errors and write to log; and default libraryError to what database library reports
    void SetDBError(const TranslatableString& msg, const TranslatableString& libraryError = {}, int errorCode = -1);

    bool ShouldCompact(const std::vector<const TrackList*>& tracks);

private:
    Connection& CurrConn();

    // non-static data members
    AudacityProject& mProject;

    wxString mTitle;

    std::shared_ptr<DBConnectionErrors> mpErrors;

    // The project's file path
    FilePath mFileName;

    // Has this project been recovered from an auto-saved version
    bool mRecovered;

    // Has this project been modified
    bool mModified;

    // Is this project still a temporary/unsaved project
    bool mTemporary;

    // Project was compacted last time Compact() ran
    bool mWasCompacted;

    // Project had unused blocks during last Compact()
    bool mHadUnused;

    Connection mPrevConn;
    FilePath mPrevFileName;
    bool mPrevTemporary;
};

//! Makes a temporary project that doesn't display on the screen
class PROJECT_FILE_IO_API InvisibleTemporaryProject
{
public:
    InvisibleTemporaryProject();
    ~InvisibleTemporaryProject();
    AudacityProject& Project()
    {
        return *mpProject;
    }

private:
    std::shared_ptr<AudacityProject> mpProject;
};

#endif
