/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

#include <atomic>
#include <sqlite3.h>
#include <optional>
#include <cstring>

#include <wx/crt.h>
#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/utils.h>

#include "ActiveProjects.h"
#include "CodeConversions.h"
#include "DBConnection.h"
#include "FileNames.h"
#include "PendingTracks.h"
#include "Project.h"
#include "ProjectFileIOExtension.h"
#include "ProjectHistory.h"
#include "ProjectSerializer.h"
#include "FileNames.h"
#include "SampleBlock.h"
#include "TempDirectory.h"
#include "TransactionScope.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include "BasicUI.h"
#include "wxFileNameWrapper.h"
#include "XMLFileReader.h"
#include "SentryHelper.h"
#include "MemoryX.h"

#include "ProjectFileIOExtension.h"
#include "ProjectFormatVersion.h"

#include "BufferedStreamReader.h"
#include "FromChars.h"

#include "sqlite/SQLiteUtils.h"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.3.0"

#undef NO_SHM
#if !defined(__WXMSW__)
   #define NO_SHM
#endif

// Used to convert 4 byte-sized values into an integer for use in SQLite
// PRAGMA statements. These values will be store in the database header.
//
// Note that endianness is not an issue here since SQLite integers are
// architecture independent.
#define PACK(b1, b2, b3, b4) ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4)

// The ProjectFileID is stored in the SQLite database header to identify the file
// as an Audacity project file. It can be used by applications that identify file
// types, such as the Linux "file" command.
static const int ProjectFileID = PACK('A', 'U', 'D', 'Y');

// The "ProjectFileVersion" represents the version of Audacity at which a specific
// database schema was used. It is assumed that any changes to the database schema
// will require a new Audacity version so if schema changes are required set this
// to the new release being produced.
//
// This version is checked before accessing any tables in the database since there's
// no guarantee what tables exist. If it's found that the database is newer than the
// currently running Audacity, an error dialog will be displayed informing the user
// that they need a newer version of Audacity.
//
// Note that this is NOT the "schema_version" that SQLite maintains. The value
// specified here is stored in the "user_version" field of the SQLite database
// header.
// DV: ProjectFileVersion is now evaluated at runtime
// static const int ProjectFileVersion = PACK(3, 0, 0, 0);

// Navigation:
//
// Bindings are marked out in the code by, e.g.
// BIND SQL sampleblocks
// A search for "BIND SQL" will find all bindings.
// A search for "SQL sampleblocks" will find all SQL related
// to sampleblocks.

static const char* ProjectFileSchema
    =// These are persistent and not connection based
     //
     // See the CMakeList.txt for the SQLite lib for more
     // settings.
      "PRAGMA <schema>.application_id = %d;"
      "PRAGMA <schema>.user_version = %u;"
      ""
      // project is a binary representation of an XML file.
      // it's in binary for speed.
      // One instance only.  id is always 1.
      // dict is a dictionary of fieldnames.
      // doc is the binary representation of the XML
      // in the doc, fieldnames are replaced by 2 byte dictionary
      // index numbers.
      // This is all opaque to SQLite.  It just sees two
      // big binary blobs.
      // There is no limit to document blob size.
      // dict will be smallish, with an entry for each
      // kind of field.
      "CREATE TABLE IF NOT EXISTS <schema>.project"
      "("
      "  id                   INTEGER PRIMARY KEY,"
      "  dict                 BLOB,"
      "  doc                  BLOB"
      ");"
      ""
      // CREATE SQL autosave
      // autosave is a binary representation of an XML file.
      // it's in binary for speed.
      // One instance only.  id is always 1.
      // dict is a dictionary of fieldnames.
      // doc is the binary representation of the XML
      // in the doc, fieldnames are replaced by 2 byte dictionary
      // index numbers.
      // This is all opaque to SQLite.  It just sees two
      // big binary blobs.
      // There is no limit to document blob size.
      // dict will be smallish, with an entry for each
      // kind of field.
      "CREATE TABLE IF NOT EXISTS <schema>.autosave"
      "("
      "  id                   INTEGER PRIMARY KEY,"
      "  dict                 BLOB,"
      "  doc                  BLOB"
      ");"
      ""
      // CREATE SQL sampleblocks
      // 'samples' are fixed size blocks of int16, int32 or float32 numbers.
      // The blocks may be partially empty.
      // The quantity of valid data in the blocks is
      // provided in the project blob.
      //
      // sampleformat specifies the format of the samples stored.
      //
      // blockID is a 64 bit number.
      //
      // Rows are immutable -- never updated after addition, but may be
      // deleted.
      //
      // summin to summary64K are summaries at 3 distance scales.
      "CREATE TABLE IF NOT EXISTS <schema>.sampleblocks"
      "("
      "  blockid              INTEGER PRIMARY KEY AUTOINCREMENT,"
      "  sampleformat         INTEGER,"
      "  summin               REAL,"
      "  summax               REAL,"
      "  sumrms               REAL,"
      "  summary256           BLOB,"
      "  summary64k           BLOB,"
      "  samples              BLOB"
      ");";

class SQLiteBlobStream final
{
public:
    static std::optional<SQLiteBlobStream> Open(
        sqlite3* db, const char* schema, const char* table, const char* column,
        int64_t rowID, bool readOnly) noexcept
    {
        if (db == nullptr) {
            return {};
        }

        sqlite3_blob* blob = nullptr;

        const int rc = sqlite3_blob_open(
            db, schema, table, column, rowID, readOnly ? 0 : 1, &blob);

        if (rc != SQLITE_OK) {
            return {};
        }

        return std::make_optional<SQLiteBlobStream>(blob, readOnly);
    }

    SQLiteBlobStream(sqlite3_blob* blob, bool readOnly) noexcept
        : mBlob(blob)
        , mIsReadOnly(readOnly)
    {
        mBlobSize = sqlite3_blob_bytes(blob);
    }

    SQLiteBlobStream(SQLiteBlobStream&& rhs) noexcept
    {
        *this = std::move(rhs);
    }

    SQLiteBlobStream& operator =(SQLiteBlobStream&& rhs) noexcept
    {
        std::swap(mBlob, rhs.mBlob);
        std::swap(mBlobSize, rhs.mBlobSize);
        std::swap(mOffset, rhs.mOffset);
        std::swap(mIsReadOnly, rhs.mIsReadOnly);

        return *this;
    }

    ~SQLiteBlobStream() noexcept
    {
        // Destructor should not throw and there is no
        // way to handle the error otherwise
        (void)Close();
    }

    bool IsOpen() const noexcept
    {
        return mBlob != nullptr;
    }

    int Close() noexcept
    {
        if (mBlob == nullptr) {
            return SQLITE_OK;
        }

        const int rc = sqlite3_blob_close(mBlob);

        mBlob = nullptr;

        return rc;
    }

    int Write(const void* ptr, int size) noexcept
    {
        // Stream APIs usually return the number of bytes written.
        // sqlite3_blob_write is all-or-nothing function,
        // so Write will return the result of the call
        if (!IsOpen() || mIsReadOnly || ptr == nullptr) {
            return SQLITE_MISUSE;
        }

        const int rc = sqlite3_blob_write(mBlob, ptr, size, mOffset);

        if (rc == SQLITE_OK) {
            mOffset += size;
        }

        return rc;
    }

    int Read(void* ptr, int& size) noexcept
    {
        if (!IsOpen() || ptr == nullptr) {
            return SQLITE_MISUSE;
        }

        const int availableBytes = mBlobSize - mOffset;

        if (availableBytes == 0) {
            size = 0;
            return SQLITE_OK;
        } else if (availableBytes < size) {
            size = availableBytes;
        }

        const int rc = sqlite3_blob_read(mBlob, ptr, size, mOffset);

        if (rc == SQLITE_OK) {
            mOffset += size;
        }

        return rc;
    }

    bool IsEof() const noexcept
    {
        return mOffset == mBlobSize;
    }

private:
    sqlite3_blob* mBlob { nullptr };
    size_t mBlobSize { 0 };

    int mOffset { 0 };

    bool mIsReadOnly { false };
};

class BufferedProjectBlobStream : public BufferedStreamReader
{
public:
    static constexpr std::array<const char*, 2> Columns = { "dict", "doc" };

    BufferedProjectBlobStream(
        sqlite3* db, const char* schema, const char* table,
        int64_t rowID)
    // Despite we use 64k pages in SQLite - it is impossible to guarantee
    // that read is satisfied from a single page.
    // Reading 64k proved to be slower, (64k - 8) gives no measurable difference
    // to reading 32k.
    // Reading 4k is slower than reading 32k.
        : BufferedStreamReader(32 * 1024)
        , mDB(db)
        , mSchema(schema)
        , mTable(table)
        , mRowID(rowID)
    {
    }

private:
    bool OpenBlob(size_t index)
    {
        if (index >= Columns.size()) {
            mBlobStream.reset();
            return false;
        }

        mBlobStream = SQLiteBlobStream::Open(
            mDB, mSchema, mTable, Columns[index], mRowID, true);

        return mBlobStream.has_value();
    }

    std::optional<SQLiteBlobStream> mBlobStream;
    size_t mNextBlobIndex { 0 };

    sqlite3* mDB;
    const char* mSchema;
    const char* mTable;
    const int64_t mRowID;

protected:
    bool HasMoreData() const override
    {
        return mBlobStream.has_value() || mNextBlobIndex < Columns.size();
    }

    size_t ReadData(void* buffer, size_t maxBytes) override
    {
        if (!mBlobStream || mBlobStream->IsEof()) {
            if (!OpenBlob(mNextBlobIndex++)) {
                return {};
            }
        }

        // Do not allow reading more then 2GB at a time (O_o)
        maxBytes = std::min<size_t>(maxBytes, std::numeric_limits<int>::max());
        auto bytesRead = static_cast<int>(maxBytes);

        if (SQLITE_OK != mBlobStream->Read(buffer, bytesRead)) {
            // Reading has failed, close the stream and do not allow opening
            // the next one
            mBlobStream = {};
            mNextBlobIndex = Columns.size();

            return 0;
        } else if (bytesRead == 0) {
            mBlobStream = {};
        }

        return static_cast<size_t>(bytesRead);
    }
};

constexpr std::array<const char*, 2> BufferedProjectBlobStream::Columns;

bool ProjectFileIO::InitializeSQL()
{
    if (audacity::sqlite::Initialize().IsError()) {
        return false;
    }

    audacity::sqlite::SetLogCallback(
        [](int code, std::string_view message) {
        // message is forwarded from SQLite, so it is null-terminated
        wxLogMessage("SQLite error (%d): %s", code, message.data());
    });

    return true;
}

static const AudacityProject::AttachedObjects::RegisteredFactory sFileIOKey{
    []( AudacityProject& parent ){
        auto result = std::make_shared< ProjectFileIO >(parent);
        return result;
    }
};

ProjectFileIO& ProjectFileIO::Get(AudacityProject& project)
{
    auto& result = project.AttachedObjects::Get< ProjectFileIO >(sFileIOKey);
    return result;
}

const ProjectFileIO& ProjectFileIO::Get(const AudacityProject& project)
{
    return Get(const_cast< AudacityProject& >(project));
}

ProjectFileIO::ProjectFileIO(AudacityProject& project)
    : mProject{project}
    , mpErrors{std::make_shared<DBConnectionErrors>()}
{
    mPrevConn = nullptr;

    mRecovered = false;
    mModified = false;
    mTemporary = true;

    SetProjectTitle();

    // Make sure there is plenty of space for Sqlite files
    wxLongLong freeSpace = 0;

    auto path = TempDirectory::TempDir();
    if (wxGetDiskSpace(path, NULL, &freeSpace)) {
        if (freeSpace < wxLongLong(wxLL(100 * 1048576))) {
            auto volume = FileNames::AbbreviatePath(path);
            /* i18n-hint: %s will be replaced by the drive letter (on Windows) */
            BasicUI::ShowErrorDialog({},
                                     XO("Warning"),
                                     XO("There is very little free disk space left on %s\n"
                                        "Please select a bigger temporary directory location in\n"
                                        "Directories Preferences.").Format(volume),
                                     "Error:_Disk_full_or_not_writable"
                                     );
        }
    }
}

ProjectFileIO::~ProjectFileIO()
{
}

bool ProjectFileIO::HasConnection() const
{
    auto& connectionPtr = ConnectionPtr::Get(mProject);
    return connectionPtr.mpConnection != nullptr;
}

DBConnection& ProjectFileIO::GetConnection()
{
    auto& curConn = CurrConn();
    if (!curConn) {
        if (!OpenConnection()) {
            throw SimpleMessageBoxException
                  {
                      ExceptionType::Internal,
                      XO("Failed to open the project's database"),
                      XO("Warning"),
                      "Error:_Disk_full_or_not_writable"
                  };
        }
    }

    return *curConn;
}

wxString ProjectFileIO::GenerateDoc()
{
    auto& trackList = TrackList::Get(mProject);

    XMLStringWriter doc;
    WriteXMLHeader(doc);
    WriteXML(doc, false, trackList.empty() ? nullptr : &trackList);
    return doc;
}

sqlite3* ProjectFileIO::DB()
{
    return GetConnection().DB();
}

/*!
 @pre *CurConn() does not exist
 @post *CurConn() exists or return value is false
 */
bool ProjectFileIO::OpenConnection(FilePath fileName /* = {}  */)
{
    auto& curConn = CurrConn();
    wxASSERT(!curConn);
    bool isTemp = false;

    if (fileName.empty()) {
        fileName = GetFileName();
        if (fileName.empty()) {
            fileName = TempDirectory::UnsavedProjectFileName();
            isTemp = true;
        }
    } else {
        // If this project resides in the temporary directory, then we'll mark it
        // as temporary.
        wxFileName temp(TempDirectory::TempDir(), wxT(""));
        wxFileName file(fileName);
        file.SetFullName(wxT(""));
        if (file == temp) {
            isTemp = true;
        }
    }

    // Pass weak_ptr to project into DBConnection constructor
    curConn = std::make_unique<DBConnection>(
        mProject.shared_from_this(), mpErrors, [this]{ OnCheckpointFailure(); });
    auto rc = curConn->Open(fileName);
    if (rc != SQLITE_OK) {
        // Must use SetError() here since we do not have an active DB
        SetError(
            XO("Failed to open database file:\n\n%s").Format(fileName),
            {},
            rc
            );
        curConn.reset();
        return false;
    }

    if (!CheckVersion()) {
        CloseConnection();
        curConn.reset();
        return false;
    }

    mTemporary = isTemp;

    SetFileName(fileName);

    return true;
}

bool ProjectFileIO::CloseConnection()
{
    auto& curConn = CurrConn();
    if (!curConn) {
        return false;
    }

    if (!curConn->Close()) {
        return false;
    }
    curConn.reset();

    SetFileName({});

    return true;
}

// Put the current database connection aside, keeping it open, so that
// another may be opened with OpenConnection()
void ProjectFileIO::SaveConnection()
{
    // Should do nothing in proper usage, but be sure not to leak a connection:
    DiscardConnection();

    mPrevConn = std::move(CurrConn());
    mPrevFileName = mFileName;
    mPrevTemporary = mTemporary;

    SetFileName({});
}

// Close any set-aside connection
void ProjectFileIO::DiscardConnection()
{
    if (mPrevConn) {
        if (!mPrevConn->Close()) {
            // Store an error message
            SetDBError(
                XO("Failed to discard connection")
                );
        }

        // If this is a temporary project, we no longer want to keep the
        // project file.
        if (mPrevTemporary) {
            // This is just a safety check.
            wxFileName temp(TempDirectory::TempDir(), wxT(""));
            wxFileName file(mPrevFileName);
            file.SetFullName(wxT(""));
            if (file == temp) {
                if (!RemoveProject(mPrevFileName)) {
                    wxLogMessage("Failed to remove temporary project %s", mPrevFileName);
                }
            }
        }
        mPrevConn = nullptr;
        mPrevFileName.clear();
    }
}

// Close any current connection and switch back to using the saved
void ProjectFileIO::RestoreConnection()
{
    auto& curConn = CurrConn();
    if (curConn) {
        if (!curConn->Close()) {
            // Store an error message
            SetDBError(
                XO("Failed to restore connection")
                );
        }
    }

    curConn = std::move(mPrevConn);
    SetFileName(mPrevFileName);
    mTemporary = mPrevTemporary;

    mPrevFileName.clear();
}

void ProjectFileIO::UseConnection(Connection&& conn, const FilePath& filePath)
{
    auto& curConn = CurrConn();
    wxASSERT(!curConn);

    curConn = std::move(conn);
    SetFileName(filePath);
}

static int ExecCallback(void* data, int cols, char** vals, char** names)
{
    auto& cb = *static_cast<const ProjectFileIO::ExecCB*>(data);
    // Be careful not to throw anything across sqlite3's stack frames.
    return GuardedCall<int>(
        [&]{ return cb(cols, vals, names); },
        MakeSimpleGuard(1)
        );
}

int ProjectFileIO::Exec(const char* query, const ExecCB& callback, bool silent)
{
    char* errmsg = nullptr;

    const void* ptr = &callback;
    int rc = sqlite3_exec(DB(), query, ExecCallback,
                          const_cast<void*>(ptr), &errmsg);

    if (rc != SQLITE_ABORT && errmsg && !silent) {
        ADD_EXCEPTION_CONTEXT("sqlite3.query", query);
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));

        SetDBError(
            XO("Failed to execute a project file command:\n\n%s").Format(query),
            Verbatim(errmsg),
            rc
            );
    }
    if (errmsg) {
        sqlite3_free(errmsg);
    }

    return rc;
}

bool ProjectFileIO::Query(const char* sql, const ExecCB& callback, bool silent)
{
    int rc = Exec(sql, callback, silent);
    // SQLITE_ABORT is a non-error return only meaning the callback
    // stopped the iteration of rows early
    if (!(rc == SQLITE_OK || rc == SQLITE_ABORT)) {
        return false;
    }

    return true;
}

bool ProjectFileIO::GetValue(const char* sql, wxString& result, bool silent)
{
    // Retrieve the first column in the first row, if any
    result.clear();
    auto cb = [&result](int cols, char** vals, char**){
        if (cols > 0) {
            result = vals[0];
        }
        // Stop after one row
        return 1;
    };

    return Query(sql, cb, silent);
}

bool ProjectFileIO::GetValue(const char* sql, int64_t& value, bool silent)
{
    bool success = false;
    auto cb = [&value, &success](int cols, char** vals, char**)
    {
        if (cols > 0) {
            const std::string_view valueString = vals[0];

            success = std::errc()
                      == FromChars(
                valueString.data(), valueString.data() + valueString.length(),
                value)
                      .ec;
        }
        // Stop after one row
        return 1;
    };

    return Query(sql, cb, silent) && success;
}

bool ProjectFileIO::CheckVersion()
{
    auto db = DB();
    int rc;

    // Install our schema if this is an empty DB
    wxString result;
    if (!GetValue("SELECT Count(*) FROM sqlite_master WHERE type='table';", result)) {
        // Bug 2718 workaround for a better error message:
        // If at this point we get SQLITE_CANTOPEN, then the directory is read-only
        if (GetLastErrorCode() == SQLITE_CANTOPEN) {
            SetError(
                /* i18n-hint: An error message. */
                XO("Project is in a read only directory\n(Unable to create the required temporary files)"),
                GetLibraryError()
                );
        }

        return false;
    }

    // If the return count is zero, then there are no tables defined, so this
    // must be a new project file.
    if (wxStrtol<char**>(result, nullptr, 10) == 0) {
        return InstallSchema(db);
    }

    // Check for our application ID
    if (!GetValue("PRAGMA application_ID;", result)) {
        return false;
    }

    // It's a database that SQLite recognizes, but it's not one of ours
    if (wxStrtoul<char**>(result, nullptr, 10) != ProjectFileID) {
        SetError(XO("This is not an Audacity project file"));
        return false;
    }

    // Get the project file version
    if (!GetValue("PRAGMA user_version;", result)) {
        return false;
    }

    const ProjectFormatVersion version
        =ProjectFormatVersion::FromPacked(wxStrtoul<char**>(result, nullptr, 10));

    // Project file version is higher than ours. We will refuse to
    // process it since we can't trust anything about it.
    if (SupportedProjectFormatVersion < version) {
        SetError(
            XO("This project was created with a newer version of Audacity.\n\nYou will need to upgrade to open it.")
            );
        return false;
    }

    return true;
}

bool ProjectFileIO::InstallSchema(sqlite3* db, const char* schema /* = "main" */)
{
    int rc;

    wxString sql;
    sql.Printf(ProjectFileSchema, ProjectFileID, BaseProjectFormatVersion.GetPacked());
    sql.Replace("<schema>", schema);

    rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        SetDBError(
            XO("Unable to initialize the project file")
            );
        return false;
    }

    return true;
}

// The orphan block handling should be removed once autosave and related
// blocks become part of the same transaction.

// An SQLite function that takes a blockid and looks it up in a set of
// blockids captured during project load.  If the blockid isn't found
// in the set, it will be deleted.
namespace {
struct ContextData final
{
    const AudacityProject& project;
    const BlockIDs& blockids;
};
}

void ProjectFileIO::InSet(sqlite3_context* context, int argc, sqlite3_value** argv)
{
    auto contextData = reinterpret_cast<ContextData*>(sqlite3_user_data(context));
    SampleBlockID blockid = sqlite3_value_int64(argv[0]);

    sqlite3_result_int(
        context,
        contextData->blockids.find(blockid) != contextData->blockids.end()
        || ProjectFileIOExtensionRegistry::IsBlockLocked(
            contextData->project, blockid));
}

bool ProjectFileIO::DeleteBlocks(const BlockIDs& blockids, bool complement)
{
    auto db = DB();
    int rc;

    ContextData contextData{ mProject, blockids };

    auto cleanup = finally([&]
    {
        // Remove our function, whether it was successfully defined or not.
        sqlite3_create_function(db, "inset", 1, SQLITE_UTF8 | SQLITE_DETERMINISTIC, nullptr, nullptr, nullptr, nullptr);
    });

    // Add the function used to verify each row's blockid against the set of active blockids
    rc = sqlite3_create_function(db, "inset", 1, SQLITE_UTF8 | SQLITE_DETERMINISTIC, &contextData, InSet, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::DeleteBlocks::create_function");

        /* i18n-hint: An error message.  Don't translate inset or blockids.*/
        SetDBError(XO("Unable to add 'inset' function (can't verify blockids)"));
        return false;
    }

    // Delete all rows in the set, or not in it
    // This is the first command that writes to the database, and so we
    // do more informative error reporting than usual, if it fails.
    auto sql = wxString::Format(
        "DELETE FROM sampleblocks WHERE %sinset(blockid);",
        complement ? "NOT " : "");
    rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        ADD_EXCEPTION_CONTEXT("sqlite3.query", sql.ToStdString());
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::GetBlob");

        if (rc == SQLITE_READONLY) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Project is read only\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_LOCKED) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Project is locked\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_BUSY) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Project is busy\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_CORRUPT) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Project is corrupt\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_PERM) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Some permissions issue\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_IOERR) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("A disk I/O error\n(Unable to work with the blockfiles)"));
        } else if (rc == SQLITE_AUTH) {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Not authorized\n(Unable to work with the blockfiles)"));
        } else {
            /* i18n-hint: An error message.  Don't translate blockfiles.*/
            SetDBError(XO("Unable to work with the blockfiles"));
        }

        return false;
    }

    // Mark the project recovered if we deleted any rows
    int changes = sqlite3_changes(db);
    if (changes > 0) {
        wxLogInfo(XO("Total orphan blocks deleted %d").Translation(), changes);
        mRecovered = true;
    }

    return true;
}

bool ProjectFileIO::CopyTo(const FilePath& destpath,
                           const TranslatableString& msg,
                           bool isTemporary,
                           bool prune /* = false */,
                           const std::vector<const TrackList*>& tracks /* = {} */)
{
    using namespace BasicUI;

    auto pConn = CurrConn().get();
    if (!pConn) {
        return false;
    }

    // Get access to the active tracklist
    auto pProject = &mProject;

    WaveTrackUtilities::SampleBlockIDSet blockids;

    // Collect all active blockids
    if (prune) {
        for (auto trackList : tracks) {
            if (trackList) {
                WaveTrackUtilities::InspectBlocks(*trackList, {}, &blockids);
            }
        }
    }
    // Collect ALL blockids
    else {
        auto cb = [&blockids](int cols, char** vals, char**){
            SampleBlockID blockid;
            wxString { vals[0] }.ToLongLong(&blockid);
            blockids.insert(blockid);
            return 0;
        };

        if (!Query("SELECT blockid FROM sampleblocks;", cb)) {
            // Error message already captured.
            return false;
        }
    }

    // Create the project doc
    ProjectSerializer doc;
    WriteXMLHeader(doc);
    WriteXML(doc, false, tracks.empty() ? nullptr : tracks[0]);

    auto db = DB();
    Connection destConn = nullptr;
    bool success = false;
    int rc = SQLITE_OK;
    ProgressResult res = ProgressResult::Success;

    // Cleanup in case things go awry
    auto cleanup = finally([&]
    {
        if (!success) {
            if (destConn) {
                destConn->Close();
                destConn = nullptr;
            }

            // Rollback transaction in case one was active.
            // If this fails (probably due to memory or disk space), the transaction will
            // (presumably) still be active, so further updates to the project file will
            // fail as well. Not really much we can do about it except tell the user.
            auto result = sqlite3_exec(db, "ROLLBACK;", nullptr, nullptr, nullptr);

            // Only capture the error if there wasn't a previous error
            if (result != SQLITE_OK && (rc == SQLITE_DONE || rc == SQLITE_OK)) {
                ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
                ADD_EXCEPTION_CONTEXT(
                    "sqlite3.context", "ProjectGileIO::CopyTo.cleanup");

                SetDBError(
                    XO("Failed to rollback transaction during import")
                    );
            }

            // And detach the outbound DB in case (if it's attached). Don't check for
            // errors since it may not be attached. But, if it is and the DETACH fails,
            // subsequent CopyTo() actions will fail until Audacity is relaunched.
            sqlite3_exec(db, "DETACH DATABASE outbound;", nullptr, nullptr, nullptr);

            // RemoveProject not necessary to clean up attached database
            wxRemoveFile(destpath);
        }
    });

    // Attach the destination database
    wxString sql;
    wxString dbName = destpath;
    // Bug 2793: Quotes in name need escaping for sqlite3.
    dbName.Replace("'", "''");
    sql.Printf("ATTACH DATABASE '%s' AS outbound;", dbName.ToUTF8());

    rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        SetDBError(
            XO("Unable to attach destination database")
            );
        return false;
    }

    // Ensure attached DB connection gets configured
    //
    // NOTE:  Between the above attach and setting the mode here, a normal DELETE
    //        mode journal will be used and will briefly appear in the filesystem.
    if (pConn->FastMode("outbound") != SQLITE_OK) {
        SetDBError(
            XO("Unable to switch to fast journaling mode")
            );

        return false;
    }

    // Install our schema into the new database
    if (!InstallSchema(db, "outbound")) {
        // Message already set
        return false;
    }

    {
        // Ensure statement gets cleaned up
        sqlite3_stmt* stmt = nullptr;
        auto cleanup = finally([&]
        {
            if (stmt) {
                // No need to check return code
                sqlite3_finalize(stmt);
            }
        });

        // Prepare the statement only once
        rc = sqlite3_prepare_v2(db,
                                "INSERT INTO outbound.sampleblocks"
                                "  SELECT * FROM main.sampleblocks"
                                "  WHERE blockid = ?;",
                                -1,
                                &stmt,
                                nullptr);
        if (rc != SQLITE_OK) {
            ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
            ADD_EXCEPTION_CONTEXT(
                "sqlite3.context", "ProjectGileIO::CopyTo.prepare");

            SetDBError(
                XO("Unable to prepare project file command:\n\n%s").Format(sql)
                );
            return false;
        }

        /* i18n-hint: This title appears on a dialog that indicates the progress
           in doing something.*/
        auto progress
            =BasicUI::MakeProgress(XO("Progress"), msg, ProgressShowCancel);
        ProgressResult result = ProgressResult::Success;

        wxLongLong_t count = 0;
        wxLongLong_t total = blockids.size();

        // Start a transaction.  Since we're running without a journal,
        // this really doesn't provide rollback.  It just prevents SQLite
        // from auto committing after each step through the loop.
        //
        // Also note that we will have an open transaction if we fail
        // while copying the blocks. This is fine since we're just going
        // to delete the database anyway.
        sqlite3_exec(db, "BEGIN;", nullptr, nullptr, nullptr);

        // Copy sample blocks from the main DB to the outbound DB
        for (auto blockid : blockids) {
            // Bind statement parameters
            rc = sqlite3_bind_int64(stmt, 1, blockid);
            if (rc != SQLITE_OK) {
                ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
                ADD_EXCEPTION_CONTEXT(
                    "sqlite3.context", "ProjectGileIO::CopyTo.bind");

                SetDBError(
                    XO("Failed to bind SQL parameter")
                    );

                return false;
            }

            // Process it
            rc = sqlite3_step(stmt);
            if (rc != SQLITE_DONE) {
                ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
                ADD_EXCEPTION_CONTEXT(
                    "sqlite3.context", "ProjectGileIO::CopyTo.step");

                SetDBError(
                    XO("Failed to update the project file.\nThe following command failed:\n\n%s").Format(sql)
                    );
                return false;
            }

            // Reset statement to beginning
            if (sqlite3_reset(stmt) != SQLITE_OK) {
                ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
                ADD_EXCEPTION_CONTEXT(
                    "sqlite3.context", "ProjectGileIO::CopyTo.reset");

                THROW_INCONSISTENCY_EXCEPTION;
            }

            result = progress->Poll(++count, total);
            if (result != ProgressResult::Success) {
                // Note that we're not setting success, so the finally
                // block above will take care of cleaning up
                return false;
            }
        }

        // Write the doc.
        //
        // If we're compacting a temporary project (user initiated from the File
        // menu), then write the doc to the "autosave" table since temporary
        // projects do not have a "project" doc.
        if (!WriteDoc(isTemporary ? "autosave" : "project", doc, "outbound")) {
            return false;
        }

        // See BEGIN above...
        sqlite3_exec(db, "COMMIT;", nullptr, nullptr, nullptr);
    }

    // Detach the destination database
    rc = sqlite3_exec(db, "DETACH DATABASE outbound;", nullptr, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::CopyTo::detach");

        SetDBError(
            XO("Destination project could not be detached")
            );

        return false;
    }

    // Tell cleanup everything is good to go
    success = true;

    return true;
}

bool ProjectFileIO::ShouldCompact(const std::vector<const TrackList*>& tracks)
{
    WaveTrackUtilities::SampleBlockIDSet active;
    unsigned long long current = 0;

    {
        auto fn = BlockSpaceUsageAccumulator(current);
        for (auto pTracks : tracks) {
            if (pTracks) {
                WaveTrackUtilities::InspectBlocks(*pTracks, fn,
                                                  &active // Visit unique blocks only
                                                  );
            }
        }
    }

    // Get the number of blocks and total length from the project file.
    unsigned long long total = GetTotalUsage();
    unsigned long long blockcount = 0;

    auto cb = [&blockcount](int cols, char** vals, char**)
    {
        // Convert
        wxString(vals[0]).ToULongLong(&blockcount);
        return 0;
    };

    if (!Query("SELECT Count(*) FROM sampleblocks;", cb) || blockcount == 0) {
        // Shouldn't compact since we don't have the full picture
        return false;
    }

    // Remember if we had unused blocks in the project file
    mHadUnused = (blockcount > active.size());

    // Let's make a percentage...should be plenty of head room
    current *= 100;

    wxLogDebug(wxT("used = %lld total = %lld %lld"), current, total, total ? current / total : 0);
    if (!total || current / total > 80) {
        wxLogDebug(wxT("not compacting"));
        return false;
    }
    wxLogDebug(wxT("compacting"));

    return true;
}

Connection& ProjectFileIO::CurrConn()
{
    auto& connectionPtr = ConnectionPtr::Get(mProject);
    return connectionPtr.mpConnection;
}

const std::vector<wxString>& ProjectFileIO::AuxiliaryFileSuffixes()
{
    static const std::vector<wxString> strings {
        "-wal",
#ifndef NO_SHM
        "-shm",
#endif
    };
    return strings;
}

FilePath ProjectFileIO::SafetyFileName(const FilePath& src)
{
    wxFileNameWrapper fn{ src };

    // Extra characters inserted into filename before extension
    wxString extra =
#ifdef __WXGTK__
        wxT("~")
#else
        wxT(".bak")
#endif
    ;

    int nn = 1;
    auto numberString = [](int num) -> wxString {
        return num == 1 ? wxString{} : wxString::Format(".%d", num);
    };

    auto suffixes = AuxiliaryFileSuffixes();
    suffixes.push_back({});

    // Find backup paths not already occupied; check all auxiliary suffixes
    const auto name = fn.GetName();
    FilePath result;
    do {
        fn.SetName(name + numberString(nn++) + extra);
        result = fn.GetFullPath();
    }while (std::any_of(suffixes.begin(), suffixes.end(), [&](auto& suffix){
        return wxFileExists(result + suffix);
    }));

    return result;
}

bool ProjectFileIO::RenameOrWarn(const FilePath& src, const FilePath& dst)
{
    std::atomic_bool done = { false };
    bool success = false;
    auto thread = std::thread([&]
    {
        success = wxRenameFile(src, dst);
        done = true;
    });

    // Provides a progress dialog with indeterminate mode
    using namespace BasicUI;
    auto pd = MakeGenericProgress(*ProjectFramePlacement(&mProject),
                                  XO("Copying Project"), XO("This may take several seconds"));
    wxASSERT(pd);

    // Wait for the checkpoints to end
    while (!done)
    {
        using namespace std::chrono;
        std::this_thread::sleep_for(50ms);
        pd->Pulse();
    }
    thread.join();

    if (!success) {
        ShowError(*ProjectFramePlacement(&mProject),
                  XO("Error Writing to File"),
                  XO("Audacity failed to write file %s.\n"
                     "Perhaps disk is full or not writable.\n"
                     "For tips on freeing up space, click the help button.")
                  .Format(dst),
                  "Error:_Disk_full_or_not_writable"
                  );
        return false;
    }

    return true;
}

bool ProjectFileIO::MoveProject(const FilePath& src, const FilePath& dst)
{
    // Assume the src database file is not busy.
    if (!RenameOrWarn(src, dst)) {
        return false;
    }

    // So far so good, but the separate -wal and -shm files might yet exist,
    // as when checkpointing failed for limited space on the drive.
    // If so move them too or else lose data.

    std::vector< std::pair<FilePath, FilePath> > pairs{ { src, dst } };
    bool success = false;
    auto cleanup = finally([&]{
        if (!success) {
            // If any one of the renames failed, back out the previous ones.
            // This should be a no-fail recovery!  Not clear what to do if any
            // of these renames fails.
            for (auto& pair : pairs) {
                if (!(pair.first.empty() && pair.second.empty())) {
                    wxRenameFile(pair.second, pair.first);
                }
            }
        }
    });

    for (const auto& suffix : AuxiliaryFileSuffixes()) {
        auto srcName = src + suffix;
        if (wxFileExists(srcName)) {
            auto dstName = dst + suffix;
            if (!RenameOrWarn(srcName, dstName)) {
                return false;
            }
            pairs.push_back({ srcName, dstName });
        }
    }

    return success = true;
}

bool ProjectFileIO::RemoveProject(const FilePath& filename)
{
    if (!wxFileExists(filename)) {
        return false;
    }

    bool success = wxRemoveFile(filename);
    auto& suffixes = AuxiliaryFileSuffixes();
    for (const auto& suffix : suffixes) {
        auto file = filename + suffix;
        if (wxFileExists(file)) {
            success = wxRemoveFile(file) && success;
        }
    }
    return success;
}

ProjectFileIO::BackupProject::BackupProject(
    ProjectFileIO& projectFileIO, const FilePath& path)
{
    auto safety = SafetyFileName(path);
    if (!projectFileIO.MoveProject(path, safety)) {
        return;
    }

    mPath = path;
    mSafety = safety;
}

void ProjectFileIO::BackupProject::Discard()
{
    if (!mPath.empty()) {
        // Succeeded; don't need the safety files
        RemoveProject(mSafety);
        mSafety.clear();
    }
}

ProjectFileIO::BackupProject::~BackupProject()
{
    if (!mPath.empty()) {
        if (!mSafety.empty()) {
            // Failed; restore from safety files
            auto suffixes = AuxiliaryFileSuffixes();
            suffixes.push_back({});
            for (const auto& suffix : suffixes) {
                auto path = mPath + suffix;
                if (wxFileExists(path)) {
                    wxRemoveFile(path);
                }
                wxRenameFile(mSafety + suffix, mPath + suffix);
            }
        }
    }
}

void ProjectFileIO::Compact(
    const std::vector<const TrackList*>& tracks, bool force)
{
    // Haven't compacted yet
    mWasCompacted = false;

    // Assume we have unused blocks until we find out otherwise. That way cleanup
    // at project close time will still occur.
    mHadUnused = true;

    // If forcing compaction, bypass inspection.
    if (!force) {
        // Don't compact if this is a temporary project or if it's determined there are not
        // enough unused blocks to make it worthwhile.
        if (IsTemporary() || !ShouldCompact(tracks)) {
            // Delete the AutoSave doc it if exists
            if (IsModified()) {
                // PRL:  not clear what to do if the following fails, but the worst should
                // be, the project may reopen in its present state as a recovery file, not
                // at the last saved state.
                // REVIEW: Could the autosave file be corrupt though at that point, and so
                // prevent recovery?
                // LLL: I believe Paul is correct since it's deleted with a single SQLite
                // transaction. The next time the file opens will just invoke recovery.
                (void)AutoSaveDelete();
            }

            return;
        }
    }

    wxString origName = mFileName;
    wxString backName = origName + "_compact_back";
    wxString tempName = origName + "_compact_temp";

    // Copy the original database to a new database. Only prune sample blocks if
    // we have a tracklist.
    // REVIEW: Compact can fail on the CopyTo with no error messages.  That's OK?
    // LLL: We could display an error message or just ignore the failure and allow
    // the file to be compacted the next time it's saved.
    if (CopyTo(tempName, XO("Compacting project"), IsTemporary(), !tracks.empty(), tracks)) {
        // Must close the database to rename it
        if (CloseConnection()) {
            // Only use the new file if it is actually smaller than the original.
            //
            // If the original file doesn't have anything to compact (original and new
            // are basically identical), the file could grow by a few pages because of
            // differences in how SQLite constructs the b-tree.
            //
            // In this case, just toss the new file and continue to use the original.
            //
            // Also, do this after closing the connection so that the -wal file
            // gets cleaned up.
            if (wxFileName::GetSize(tempName) < wxFileName::GetSize(origName)) {
                // Rename the original to backup
                if (wxRenameFile(origName, backName)) {
                    // Rename the temporary to original
                    if (wxRenameFile(tempName, origName)) {
                        // Open the newly compacted original file
                        if (OpenConnection(origName)) {
                            // Remove the old original file
                            if (!wxRemoveFile(backName)) {
                                // Just log the error, nothing can be done to correct it
                                // and WX should have logged another message showing the
                                // system error code.
                                wxLogWarning(wxT("Compaction failed to delete backup %s"), backName);
                            }

                            // Remember that we compacted
                            mWasCompacted = true;

                            return;
                        } else {
                            wxLogWarning(wxT("Compaction failed to open new project %s"), origName);
                        }

                        if (!wxRenameFile(origName, tempName)) {
                            wxLogWarning(wxT("Compaction failed to rename original %s to temp %s"),
                                         origName, tempName);
                        }
                    } else {
                        wxLogWarning(wxT("Compaction failed to rename temp %s to orig %s"),
                                     origName, tempName);
                    }

                    if (!wxRenameFile(backName, origName)) {
                        wxLogWarning(wxT("Compaction failed to rename back %s to orig %s"),
                                     backName, origName);
                    }
                } else {
                    wxLogWarning(wxT("Compaction failed to rename orig %s to back %s"),
                                 backName, origName);
                }
            }

            if (!OpenConnection(origName)) {
                wxLogWarning(wxT("Compaction failed to reopen %s"), origName);
            }
        }

        // Did not achieve any real compaction
        // RemoveProject not needed for what was an attached database
        if (!wxRemoveFile(tempName)) {
            // Just log the error, nothing can be done to correct it
            // and WX should have logged another message showing the
            // system error code.
            wxLogWarning(wxT("Failed to delete temporary file...ignoring"));
        }
    }

    return;
}

bool ProjectFileIO::WasCompacted()
{
    return mWasCompacted;
}

bool ProjectFileIO::HadUnused()
{
    return mHadUnused;
}

void ProjectFileIO::UpdatePrefs()
{
    SetProjectTitle();
}

// Pass a number in to show project number, or -1 not to.
void ProjectFileIO::SetProjectTitle(int number)
{
    auto& project = mProject;
    wxString name = project.GetProjectName();

    // If we are showing project numbers, then we also explicitly show "<untitled>" if there
    // is none.
    if (number >= 0) {
        name
            =/* i18n-hint: The %02i is the project number, the %s is the project name.*/
              XO("[Project %02i] Audacity \"%s\"")
              .Format(number + 1,
                      name.empty() ? XO("<untitled>") : Verbatim((const char*)name))
              .Translation();
    }
    // If we are not showing numbers, then <untitled> shows as 'Audacity'.
    else if (name.empty()) {
        name = _TS("Audacity");
    }

    if (mRecovered) {
        name += wxT(" ");
        /* i18n-hint: E.g this is recovered audio that had been lost.*/
        name += _("(Recovered)");
    }

    if (name != mTitle) {
        mTitle = name;
        BasicUI::CallAfter([wThis = weak_from_this()]{
            if (auto pThis = wThis.lock()) {
                pThis->Publish(ProjectFileIOMessage::ProjectTitleChange);
            }
        });
    }
}

const FilePath& ProjectFileIO::GetFileName() const
{
    return mFileName;
}

void ProjectFileIO::SetFileName(const FilePath& fileName)
{
    auto& project = mProject;

    if (!fileName.empty() && fileName != mFileName) {
        BasicUI::CallAfter(
            [wThis = weak_from_this()]
        {
            if (auto pThis = wThis.lock()) {
                pThis->Publish(ProjectFileIOMessage::ProjectFilePathChange);
            }
        });
    }

    if (!mFileName.empty()) {
        ActiveProjects::Remove(mFileName);
    }

    mFileName = fileName;

    if (!mFileName.empty()) {
        ActiveProjects::Add(mFileName);
    }

    if (IsTemporary()) {
        project.SetProjectName({});
    } else {
        project.SetProjectName(wxFileName(mFileName).GetName());
    }

    SetProjectTitle();
}

bool ProjectFileIO::HandleXMLTag(const std::string_view& tag, const AttributesList& attrs)
{
    auto& project = mProject;

    wxString fileVersion;
    wxString audacityVersion;
    int requiredTags = 0;

    // loop through attrs, which is a null-terminated list of
    // attribute-value pairs
    for (auto pair : attrs) {
        auto attr = pair.first;
        auto value = pair.second;

        if (ProjectFileIORegistry::Get()
            .CallAttributeHandler(attr, project, value)) {
            continue;
        } else if (attr == "version") {
            fileVersion = value.ToWString();
            requiredTags++;
        } else if (attr == "audacityversion") {
            audacityVersion = value.ToWString();
            requiredTags++;
        }
    } // while

    if (requiredTags < 2) {
        return false;
    }

    // Parse the file version from the project
    int fver;
    int frel;
    int frev;
    if (!wxSscanf(fileVersion, wxT("%i.%i.%i"), &fver, &frel, &frev)) {
        return false;
    }

    // Parse the file version Audacity was build with
    int cver;
    int crel;
    int crev;
    wxSscanf(wxT(AUDACITY_FILE_FORMAT_VERSION), wxT("%i.%i.%i"), &cver, &crel, &crev);

    int fileVer = ((fver * 100) + frel) * 100 + frev;
    int codeVer = ((cver * 100) + crel) * 100 + crev;

    if (codeVer < fileVer) {
        /* i18n-hint: %s will be replaced by the version number.*/
        auto msg = XO(
            "This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file.")
                   .Format(audacityVersion, AUDACITY_VERSION_STRING);

        ShowError(*ProjectFramePlacement(&project),
                  XO("Can't open project file"),
                  msg,
                  "FAQ:Errors_opening_an_Audacity_project"
                  );

        return false;
    }

    if (tag != "project") {
        return false;
    }

    // All other tests passed, so we succeed
    return true;
}

XMLTagHandler* ProjectFileIO::HandleXMLChild(const std::string_view& tag)
{
    auto& project = mProject;
    return ProjectFileIORegistry::Get().CallObjectAccessor(tag, project);
}

void ProjectFileIO::OnCheckpointFailure()
{
    // DBConnection promises to invoke this in main thread idle time
    // So we don't need a redundant CallAfter to satisfy our own promise
    Publish(ProjectFileIOMessage::CheckpointFailure);
}

void ProjectFileIO::WriteXMLHeader(XMLWriter& xmlFile) const
{
    xmlFile.Write(wxT("<?xml "));
    xmlFile.Write(wxT("version=\"1.0\" "));
    xmlFile.Write(wxT("standalone=\"no\" "));
    xmlFile.Write(wxT("?>\n"));

    xmlFile.Write(wxT("<!DOCTYPE "));
    xmlFile.Write(wxT("project "));
    xmlFile.Write(wxT("PUBLIC "));
    xmlFile.Write(wxT("\"-//audacityproject-1.3.0//DTD//EN\" "));
    xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\" "));
    xmlFile.Write(wxT(">\n"));
}

void ProjectFileIO::WriteXML(XMLWriter& xmlFile,
                             bool recording /* = false */,
                             const TrackList* tracks /* = nullptr */)
// may throw
{
    auto& proj = mProject;
    auto& tracklist = tracks ? *tracks : TrackList::Get(proj);

    //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );

    xmlFile.StartTag(wxT("project"));
    xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

    xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
    xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

    ProjectFileIORegistry::Get().CallWriters(proj, xmlFile);

    auto& pendingTracks = PendingTracks::Get(proj);
    tracklist.Any().Visit([&](const Track& t) {
        auto useTrack = &t;
        if (recording) {
            // When append-recording, there is a temporary "shadow" track accumulating
            // changes and displayed on the screen but it is not yet part of the
            // regular track list.  That is the one that we want to back up.
            // SubstitutePendingChangedTrack() fetches the shadow, if the track has
            // one, else it gives the same track back.
            useTrack = &pendingTracks.SubstitutePendingChangedTrack(t);
        } else if (useTrack->GetId() == TrackId {}) {
            // This is a track added during a non-appending recording that is
            // not yet in the undo history.  The UndoManager skips backing it up
            // when pushing.  Don't auto-save it.
            return;
        }
        useTrack->WriteXML(xmlFile);
    });

    xmlFile.EndTag(wxT("project"));

    //TIMER_STOP( xml_writer_timer );
}

bool ProjectFileIO::AutoSave(bool recording)
{
    ProjectSerializer autosave;
    WriteXMLHeader(autosave);
    WriteXML(autosave, recording);

    if (WriteDoc("autosave", autosave)) {
        mModified = true;
        return true;
    }

    return false;
}

bool ProjectFileIO::AutoSaveDelete(sqlite3* db /* = nullptr */)
{
    int rc;

    if (!db) {
        db = DB();
    }

    rc = sqlite3_exec(db, "DELETE FROM autosave;", nullptr, nullptr, nullptr);
    if (rc != SQLITE_OK) {
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::AutoSaveDelete");

        SetDBError(
            XO("Failed to remove the autosave information from the project file.")
            );
        return false;
    }

    mModified = false;

    return true;
}

bool ProjectFileIO::WriteDoc(const char* table,
                             const ProjectSerializer& autosave,
                             const char* schema /* = "main" */)
{
    auto db = DB();

    TransactionScope transaction(mProject, "UpdateProject");

    int rc;

    // For now, we always use an ID of 1. This will replace the previously
    // written row every time.
    char sql[256];
    sqlite3_snprintf(
        sizeof(sql), sql,
        "INSERT INTO %s.%s(id, dict, doc) VALUES(1, ?1, ?2)"
        "       ON CONFLICT(id) DO UPDATE SET dict = ?1, doc = ?2;",
        schema, table);

    sqlite3_stmt* stmt = nullptr;
    auto cleanup = finally([&]
    {
        if (stmt) {
            sqlite3_finalize(stmt);
        }
    });

    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, nullptr);
    if (rc != SQLITE_OK) {
        ADD_EXCEPTION_CONTEXT("sqlite3.query", sql);
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::prepare");

        SetDBError(
            XO("Unable to prepare project file command:\n\n%s").Format(sql)
            );
        return false;
    }

    const MemoryStream& dict = autosave.GetDict();
    const MemoryStream& data = autosave.GetData();

    // Bind statement parameters
    // Might return SQL_MISUSE which means it's our mistake that we violated
    // preconditions; should return SQL_OK which is 0
    if (
        sqlite3_bind_zeroblob(stmt, 1, dict.GetSize())
        || sqlite3_bind_zeroblob(stmt, 2, data.GetSize())) {
        ADD_EXCEPTION_CONTEXT("sqlite3.query", sql);
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::bind");

        SetDBError(XO("Unable to bind to blob"));
        return false;
    }

    const auto reportError = [this](auto sql) {
        SetDBError(
            XO("Failed to update the project file.\nThe following command failed:\n\n%s")
            .Format(sql));
    };

    rc = sqlite3_step(stmt);

    if (rc != SQLITE_DONE) {
        ADD_EXCEPTION_CONTEXT("sqlite3.query", sql);
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::step");

        reportError(sql);
        return false;
    }

    // Finalize the statement before committing the transaction
    sqlite3_finalize(stmt);
    stmt = nullptr;

    // Get rowid

    int64_t rowID = 0;

    const wxString rowIDSql
        =wxString::Format("SELECT ROWID FROM %s.%s WHERE id = 1;", schema, table);

    if (!GetValue(rowIDSql, rowID, true)) {
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(sqlite3_errcode(db)));
        ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::rowid");

        reportError(rowIDSql);
        return false;
    }

    const auto writeStream = [db, schema, table, rowID, this](const char* column, const MemoryStream& stream) {
        auto blobStream
            =SQLiteBlobStream::Open(db, schema, table, column, rowID, false);

        if (!blobStream) {
            ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(sqlite3_errcode(db)));
            ADD_EXCEPTION_CONTEXT("sqlite3.col", column);
            ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::openBlobStream");

            SetDBError(XO("Unable to bind to blob"));
            return false;
        }

        for (auto chunk : stream) {
            if (SQLITE_OK != blobStream->Write(chunk.first, chunk.second)) {
                ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(sqlite3_errcode(db)));
                ADD_EXCEPTION_CONTEXT("sqlite3.col", column);
                ADD_EXCEPTION_CONTEXT("sqlite3.context", "ProjectGileIO::WriteDoc::writeBlobStream");
                // The user visible message is not changed, so there is no need for new strings
                SetDBError(XO("Unable to bind to blob"));
                return false;
            }
        }

        if (blobStream->Close() != SQLITE_OK) {
            ADD_EXCEPTION_CONTEXT(
                "sqlite3.rc", std::to_string(sqlite3_errcode(db)));
            ADD_EXCEPTION_CONTEXT("sqlite3.col", column);
            ADD_EXCEPTION_CONTEXT(
                "sqlite3.context", "ProjectGileIO::WriteDoc::writeBlobStream");
            // The user visible message is not changed, so there is no need for new
            // strings
            SetDBError(XO("Unable to bind to blob"));
            return false;
        }

        return true;
    };

    if (!writeStream("dict", dict)) {
        return false;
    }

    if (!writeStream("doc", data)) {
        return false;
    }

    const wxString setVersionSql
        =wxString::Format("PRAGMA user_version = %u", BaseProjectFormatVersion.GetPacked());

    if (!Query(setVersionSql.c_str(), [](auto...) { return 0; })) {
        // DV: Very unlikely case.
        // Since we need to improve the error messages in the future, let's use
        // the generic message for now, so no new strings are needed
        reportError(setVersionSql);
        return false;
    }

    return transaction.Commit();
}

ProjectFileIO::
TentativeConnection::TentativeConnection(ProjectFileIO& projectFileIO)
    : mProjectFileIO{projectFileIO}
{
    mProjectFileIO.SaveConnection();
}

ProjectFileIO::
TentativeConnection::TentativeConnection(TentativeConnection&& other)
    : mProjectFileIO{other.mProjectFileIO}
    , mFileName{other.mFileName}
    , mCommitted{other.mCommitted}
{
    other.mCommitted = true;
}

ProjectFileIO::TentativeConnection::~TentativeConnection()
{
    if (!mCommitted) {
        mProjectFileIO.RestoreConnection();
    }
}

void ProjectFileIO::TentativeConnection::SetFileName(const FilePath& fileName)
{
    mFileName = fileName;
}

void ProjectFileIO::TentativeConnection::Commit()
{
    if (!mCommitted && !mFileName.empty()) {
        mProjectFileIO.SetFileName(mFileName);
        mProjectFileIO.DiscardConnection();
        mCommitted = true;
    }
}

auto ProjectFileIO::LoadProject(const FilePath& fileName, bool ignoreAutosave)
-> std::optional<TentativeConnection>
{
    auto now = std::chrono::high_resolution_clock::now();

    std::optional<TentativeConnection> result{ *this };

    bool success = false;

    // Open the project file
    if (!OpenConnection(fileName)) {
        return {};
    }

    int64_t rowId = -1;

    bool useAutosave
        =!ignoreAutosave
          && GetValue("SELECT ROWID FROM main.autosave WHERE id = 1;", rowId, true);

    int64_t rowsCount = 0;
    // If we didn't have an autosave doc, load the project doc instead
    if (
        !useAutosave
        && (!GetValue("SELECT COUNT(1) FROM main.project;", rowsCount, true) || rowsCount == 0)) {
        // Missing both the autosave and project docs. This can happen if the
        // system were to crash before the first autosave into a temporary file.
        // This should be a recoverable scenario.
        mRecovered = true;
        mModified = true;

        return result;
    }

    if (!useAutosave && !GetValue("SELECT ROWID FROM main.project WHERE id = 1;", rowId, false)) {
        return {};
    } else {
        // Load 'er up
        BufferedProjectBlobStream stream(
            DB(), "main", useAutosave ? "autosave" : "project", rowId);

        success = ProjectSerializer::Decode(stream, this);

        if (!success) {
            SetError(
                XO("Unable to parse project information.")
                );
            return {};
        }

        // Check for orphans blocks...sets mRecovered if any were deleted

        auto blockids = WaveTrackFactory::Get(mProject)
                        .GetSampleBlockFactory()
                        ->GetActiveBlockIDs();
        if (blockids.size() > 0) {
            success = DeleteBlocks(blockids, true);
            if (!success) {
                return {};
            }
        }

        // Remember if we used autosave or not
        if (useAutosave) {
            mRecovered = true;
        }
    }

    // Mark the project modified if we recovered it
    if (mRecovered) {
        mModified = true;
    }

    // A previously saved project will have a document in the project table, so
    // we use that knowledge to determine if this file is an unsaved/temporary
    // file or a permanent project file
    wxString queryResult;
    success = GetValue("SELECT Count(*) FROM project;", queryResult);
    if (!success) {
        return {};
    }

    mTemporary = !queryResult.IsSameAs(wxT("1"));

    result->SetFileName(fileName);

    auto duration = std::chrono::high_resolution_clock::now() - now;

    wxLogInfo(
        "Project loaded in %lld ms",
        std::chrono::duration_cast<std::chrono::milliseconds>(duration).count());

    return result;
}

bool ProjectFileIO::UpdateSaved(const TrackList* tracks)
{
    ProjectSerializer doc;
    WriteXMLHeader(doc);
    WriteXML(doc, false, tracks);

    if (!WriteDoc("project", doc)) {
        return false;
    }

    // Autosave no longer needed
    if (!AutoSaveDelete()) {
        return false;
    }

    ProjectFileIOExtensionRegistry::OnUpdateSaved(mProject, doc);

    return true;
}

// REVIEW: This function is believed to report an error to the user in all cases
// of failure.  Callers are believed not to need to do so if they receive 'false'.
// LLL: All failures checks should now be displaying an error.
bool ProjectFileIO::SaveProject(
    const FilePath& fileName, const TrackList* lastSaved)
{
    // In the case where we're saving a temporary project to a permanent project,
    // we'll try to simply rename the project to save a bit of time. We then fall
    // through to the normal Save (not SaveAs) processing.
    if (IsTemporary() && mFileName != fileName) {
        FilePath savedName = mFileName;
        if (CloseConnection()) {
            bool reopened = false;
            bool moved = false;
            if (true == (moved = MoveProject(savedName, fileName))) {
                if (OpenConnection(fileName)) {
                    reopened = true;
                } else {
                    MoveProject(fileName, savedName);
                    moved = false; // No longer moved

                    reopened = OpenConnection(savedName);
                }
            } else {
                // Rename can fail -- if it's to a different device, requiring
                // real copy of contents, which might exhaust space
                reopened = OpenConnection(savedName);
            }

            // Warning issued in MoveProject()
            if (reopened && !moved) {
                return false;
            }

            if (!reopened) {
                BasicUI::CallAfter([this]{
                    ShowError({},
                              XO("Warning"),
                              XO(
                                  "The project's database failed to reopen, "
                                  "possibly because of limited space on the storage device."),
                              "Error:_Disk_full_or_not_writable"
                              );
                    Publish(ProjectFileIOMessage::ReconnectionFailure);
                });

                return false;
            }
        }
    }

    // If we're saving to a different file than the current one, then copy the
    // current to the new file and make it the active file.
    if (mFileName != fileName) {
        // Do NOT prune here since we need to retain the Undo history
        // after we switch to the new file.
        if (!CopyTo(fileName, XO("Saving project"), false)) {
            ShowError({},
                      XO("Error Saving Project"),
                      FileException::WriteFailureMessage(fileName),
                      "Error:_Disk_full_or_not_writable"
                      );
            return false;
        }

        // Open the newly created database
        Connection newConn = std::make_unique<DBConnection>(
            mProject.shared_from_this(), mpErrors,
            [this]{ OnCheckpointFailure(); });

        // NOTE: There is a noticeable delay here when dealing with large multi-hour
        //       projects that we just created. The delay occurs in Open() when it
        //       calls SafeMode() and is due to the switch from the NONE journal mode
        //       to the WAL journal mode.
        //
        //       So, we do the Open() in a thread and display a progress dialog. Since
        //       this is currently the only known instance where this occurs, we do the
        //       threading here. If more instances are identified, then the threading
        //       should be moved to DBConnection::Open(), wrapping the SafeMode() call
        //       there.
        {
            std::atomic_bool done = { false };
            bool success = true;
            auto thread = std::thread([&]
            {
                auto rc =  newConn->Open(fileName);
                if (rc != SQLITE_OK) {
                    // Capture the error string
                    SetError(Verbatim(sqlite3_errstr(rc)));
                    success = false;
                }
                done = true;
            });

            // Provides a progress dialog with indeterminate mode
            using namespace BasicUI;
            auto pd = MakeGenericProgress({},
                                          XO("Syncing"), XO("This may take several seconds"));
            wxASSERT(pd);

            // Wait for the checkpoints to end
            while (!done)
            {
                using namespace std::chrono;
                std::this_thread::sleep_for(50ms);
                pd->Pulse();
            }
            thread.join();

            if (!success) {
                // Additional help via a Help button links to the manual.
                ShowError({},
                          XO("Error Saving Project"),
                          XO("The project failed to open, possibly due to limited space\n"
                             "on the storage device.\n\n%s").Format(GetLastError()),
                          "Error:_Disk_full_or_not_writable");

                newConn = nullptr;

                // Clean up the destination project
                if (!wxRemoveFile(fileName)) {
                    wxLogMessage("Failed to remove destination project after open failure: %s", fileName);
                }

                return false;
            }
        }

        // Autosave no longer needed in original project file.
        if (!AutoSaveDelete()) {
            // Additional help via a Help button links to the manual.
            ShowError({},
                      XO("Error Saving Project"),
                      XO("Unable to remove autosave information, possibly due to limited space\n"
                         "on the storage device.\n\n%s").Format(GetLastError()),
                      "Error:_Disk_full_or_not_writable");

            newConn = nullptr;

            // Clean up the destination project
            if (!wxRemoveFile(fileName)) {
                wxLogMessage("Failed to remove destination project after AutoSaveDelete failure: %s", fileName);
            }

            return false;
        }

        if (lastSaved) {
            using namespace WaveTrackUtilities;
            // Bug2605: Be sure not to save orphan blocks
            bool recovered = mRecovered;
            SampleBlockIDSet blockids;
            InspectBlocks(*lastSaved, {}, &blockids);
            // TODO: Not sure what to do if the deletion fails
            DeleteBlocks(blockids, true);
            // Don't set mRecovered if any were deleted
            mRecovered = recovered;
        }

        // Try to compact the original project file.
        auto empty = TrackList::Create(&mProject);
        Compact({ lastSaved ? lastSaved : empty.get() }, true);

        // Safe to close the original project file now. Not much we can do if this fails,
        // but we should still be in good shape since we'll be switching to the newly
        // saved database below.
        CloseProject();

        // And make it the active project file
        UseConnection(std::move(newConn), fileName);
    }

    if (!UpdateSaved()) {
        ShowError(
            {}, XO("Error Saving Project"),
            FileException::WriteFailureMessage(fileName),
            "Error:_Disk_full_or_not_writable");
        return false;
    }

    // Reaching this point defines success and all the rest are no-fail
    // operations:

    // No longer modified
    mModified = false;

    // No longer recovered
    mRecovered = false;

    // No longer a temporary project
    mTemporary = false;

    // Adjust the title
    SetProjectTitle();

    return true;
}

bool ProjectFileIO::SaveCopy(const FilePath& fileName)
{
    return CopyTo(fileName, XO("Backing up project"), false, true,
                  { &TrackList::Get(mProject) });
}

bool ProjectFileIO::OpenProject()
{
    return OpenConnection();
}

void ProjectFileIO::CloseProject()
{
    auto& currConn = CurrConn();
    if (!currConn) {
        wxLogDebug("Closing project with no database connection");
        return;
    }

    // Save the filename since CloseConnection() will clear it
    wxString filename = mFileName;

    // Not much we can do if this fails.  The user will simply get
    // the recovery dialog upon next restart.
    if (CloseConnection()) {
        // If this is a temporary project, we no longer want to keep the
        // project file.
        if (IsTemporary()) {
            // This is just a safety check.
            wxFileName temp(TempDirectory::TempDir(), wxT(""));
            wxFileName file(filename);
            file.SetFullName(wxT(""));
            if (file == temp) {
                RemoveProject(filename);
            }
        }
    }
}

bool ProjectFileIO::ReopenProject()
{
    FilePath fileName = mFileName;
    if (!CloseConnection()) {
        return false;
    }

    return OpenConnection(fileName);
}

bool ProjectFileIO::IsModified() const
{
    return mModified;
}

bool ProjectFileIO::IsTemporary() const
{
    return mTemporary;
}

bool ProjectFileIO::IsRecovered() const
{
    return mRecovered;
}

void ProjectFileIO::MarkTemporary()
{
    mTemporary = true;
}

wxLongLong ProjectFileIO::GetFreeDiskSpace() const
{
    wxLongLong freeSpace;
    if (wxGetDiskSpace(wxPathOnly(mFileName), NULL, &freeSpace)) {
        if (FileNames::IsOnFATFileSystem(mFileName)) {
            // 4 GiB per-file maximum
            constexpr auto limit = 1ll << 32;

            // Opening a file only to find its length looks wasteful but
            // seems to be necessary at least on Windows with FAT filesystems.
            // I don't know if that is only a wxWidgets bug.
            auto length = wxFile{ mFileName }.Length();
            // auto length = wxFileName::GetSize(mFileName);

            if (length == wxInvalidSize) {
                length = 0;
            }
            auto free = std::max<wxLongLong>(0, limit - length);
            freeSpace = std::min(freeSpace, free);
        }
        return freeSpace;
    }

    return -1;
}

/// Displays an error dialog with a button that offers help
void ProjectFileIO::ShowError(const BasicUI::WindowPlacement& placement,
                              const TranslatableString& dlogTitle,
                              const TranslatableString& message,
                              const wxString& helpPage)
{
    using namespace audacity;
    using namespace BasicUI;
    ShowErrorDialog(placement, dlogTitle, message, helpPage,
                    ErrorDialogOptions { ErrorDialogType::ModalErrorReport }
                    .Log(ToWString(GetLastLog())));
}

const TranslatableString& ProjectFileIO::GetLastError() const
{
    return mpErrors->mLastError;
}

const TranslatableString& ProjectFileIO::GetLibraryError() const
{
    return mpErrors->mLibraryError;
}

int ProjectFileIO::GetLastErrorCode() const
{
    return mpErrors->mErrorCode;
}

const wxString& ProjectFileIO::GetLastLog() const
{
    return mpErrors->mLog;
}

void ProjectFileIO::SetError(
    const TranslatableString& msg, const TranslatableString& libraryError, int errorCode)
{
    auto& currConn = CurrConn();
    if (currConn) {
        currConn->SetError(msg, libraryError, errorCode);
    }
}

void ProjectFileIO::SetDBError(
    const TranslatableString& msg, const TranslatableString& libraryError, int errorCode)
{
    auto& currConn = CurrConn();
    if (currConn) {
        currConn->SetDBError(msg, libraryError, errorCode);
    }
}

void ProjectFileIO::SetBypass()
{
    auto& currConn = CurrConn();
    if (!currConn) {
        return;
    }

    // Determine if we can bypass sample block deletes during shutdown.
    //
    // IMPORTANT:
    // If the project was compacted, then we MUST bypass further
    // deletions since the new file doesn't have the blocks that the
    // Sequences expect to be there.

    currConn->SetBypass(true);

    // Only permanent project files need cleaning at shutdown
    if (!IsTemporary() && !WasCompacted()) {
        // If we still have unused blocks, then we must not bypass deletions
        // during shutdown.  Otherwise, we would have orphaned blocks the next time
        // the project is opened.
        //
        // An example of when dead blocks will exist is when a user opens a permanent
        // project, adds a track (with samples) to it, and chooses not to save the
        // changes.
        if (HadUnused()) {
            currConn->SetBypass(false);
        }
    }

    return;
}

int64_t ProjectFileIO::GetBlockUsage(SampleBlockID blockid)
{
    auto pConn = CurrConn().get();
    if (!pConn) {
        return 0;
    }
    return GetDiskUsage(*pConn, blockid);
}

int64_t ProjectFileIO::GetCurrentUsage(
    const std::vector<const TrackList*>& trackLists) const
{
    using namespace WaveTrackUtilities;
    unsigned long long current = 0;
    const auto fn = BlockSpaceUsageAccumulator(current);

    // Must pass address of this set, even if not otherwise used, to avoid
    // possible multiple count of shared blocks
    SampleBlockIDSet seen;
    for (auto pTracks: trackLists) {
        if (pTracks) {
            InspectBlocks(*pTracks, fn, &seen);
        }
    }

    return current;
}

int64_t ProjectFileIO::GetTotalUsage()
{
    auto pConn = CurrConn().get();
    if (!pConn) {
        return 0;
    }
    return GetDiskUsage(*pConn, 0);
}

//
// Returns the estimation of disk space used by the specified sample blockid or all
// of the sample blocks if the blockid is 0. This does not include small overhead
// of the internal SQLite structures, only the size used by the data
//
int64_t ProjectFileIO::GetDiskUsage(DBConnection& conn, SampleBlockID blockid /* = 0 */)
{
    sqlite3_stmt* stmt = nullptr;

    if (blockid == 0) {
        static const char* statement
            =
                R"(SELECT
	sum(length(blockid) + length(sampleformat) +
	length(summin) + length(summax) + length(sumrms) +
	length(summary256) + length(summary64k) +
	length(samples))
FROM sampleblocks;)";

        stmt = conn.Prepare(DBConnection::GetAllSampleBlocksSize, statement);
    } else {
        static const char* statement
            =
                R"(SELECT
	length(blockid) + length(sampleformat) +
	length(summin) + length(summax) + length(sumrms) +
	length(summary256) + length(summary64k) +
	length(samples)
FROM sampleblocks WHERE blockid = ?1;)";

        stmt = conn.Prepare(DBConnection::GetSampleBlockSize, statement);
    }

    auto cleanup = finally(
        [stmt]() {
        // Clear statement bindings and rewind statement
        if (stmt != nullptr) {
            sqlite3_clear_bindings(stmt);
            sqlite3_reset(stmt);
        }
    });

    if (blockid != 0) {
        int rc = sqlite3_bind_int64(stmt, 1, blockid);

        if (rc != SQLITE_OK) {
            ADD_EXCEPTION_CONTEXT(
                "sqlite3.rc", std::to_string(rc));

            ADD_EXCEPTION_CONTEXT(
                "sqlite3.context", "ProjectFileIO::GetDiskUsage::bind");

            conn.ThrowException(false);
        }
    }

    int rc = sqlite3_step(stmt);

    if (rc != SQLITE_ROW) {
        ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));

        ADD_EXCEPTION_CONTEXT(
            "sqlite3.context", "ProjectFileIO::GetDiskUsage::step");

        conn.ThrowException(false);
    }

    const int64_t size = sqlite3_column_int64(stmt, 0);

    return size;
}

InvisibleTemporaryProject::InvisibleTemporaryProject()
    : mpProject{AudacityProject::Create()}
{
}

InvisibleTemporaryProject::~InvisibleTemporaryProject()
{
    auto& projectFileIO = ProjectFileIO::Get(Project());
    projectFileIO.SetBypass();
    auto& tracks = TrackList::Get(Project());
    tracks.Clear();

    // Consume some delayed track list related events before destroying the
    // temporary project
    try {
        BasicUI::Yield();
    } catch (...) {}

    // Destroy the project and yield again to let delayed window deletions happen
    projectFileIO.CloseProject();
    mpProject.reset();
    try {
        BasicUI::Yield();
    } catch (...) {}
}

//! Install the callback from undo manager
static ProjectHistory::AutoSave::Scope scope {
    [](AudacityProject& project) {
        auto& projectFileIO = ProjectFileIO::Get(project);
        if (!projectFileIO.AutoSave()) {
            throw SimpleMessageBoxException{
                      ExceptionType::Internal,
                      XO("Automatic database backup failed."),
                      XO("Warning"),
                      "Error:_Disk_full_or_not_writable"
            };
        }
    } };
