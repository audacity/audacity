/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

#include <sqlite3.h>
#include <wx/crt.h>
#include <wx/frame.h>

#include "AutoRecovery.h"
#include "FileNames.h"
#include "Project.h"
#include "ProjectFileIORegistry.h"
#include "ProjectSettings.h"
#include "Tags.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/NumericTextCtrl.h"
#include "widgets/ProgressDialog.h"
#include "xml/XMLFileReader.h"

wxDEFINE_EVENT(EVT_PROJECT_TITLE_CHANGE, wxCommandEvent);

static const int ProjectFileID = ('A' << 24 | 'U' << 16 | 'D' << 8 | 'Y');
static const int ProjectFileVersion = 1;

// Navigation:
//
// Bindings are marked out in the code by, e.g. 
// BIND SQL sampleblocks
// A search for "BIND SQL" will find all bindings.
// A search for "SQL sampleblocks" will find all SQL related 
// to sampleblocks.

static const char *ProjectFileSchema =
   "PRAGMA application_id = %d;"
   "PRAGMA user_version = %d;"
   "PRAGMA journal_mode = WAL;"
   "PRAGMA locking_mode = EXCLUSIVE;"
   ""
   // CREATE SQL project
   // doc is a variable sized XML text string.
   // it is the former Audacity .aup file 
   // One instance only.
   "CREATE TABLE IF NOT EXISTS project"
   "("
   "  id                   INTEGER PRIMARY KEY,"
   "  doc                  TEXT"
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
   "CREATE TABLE IF NOT EXISTS autosave"
   "("
   "  id                   INTEGER PRIMARY KEY,"
   "  dict                 BLOB,"
   "  doc                  BLOB"
   ");"
   ""
   // CREATE SQL tags
   // tags is not used (yet)
   "CREATE TABLE IF NOT EXISTS tags"
   "("
   "  name                 TEXT,"
   "  value                BLOB"
   ");"
   ""
   // CREATE SQL sampleblocks
   // 'samples' are fixed size blocks of int16, int32 or float32 numbers.
   // The blocks may be partially empty.
   // The quantity of valid data in the blocks is
   // provided in the project XML.
   // 
   // sampleformat specifies the format of the samples stored.
   //
   // blockID is a 64 bit number.
   //
   // summin to summary64K are summaries at 3 distance scales.
   "CREATE TABLE IF NOT EXISTS sampleblocks"
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

// This singleton handles initialization/shutdown of the SQLite library.
// It is needed because our local SQLite is built with SQLITE_OMIT_AUTOINIT
// defined.
//
// It's safe to use even if a system version of SQLite is used that didn't
// have SQLITE_OMIT_AUTOINIT defined.
class SQLiteIniter
{
public:
   SQLiteIniter()
   {
      mRc = sqlite3_initialize();

#if !defined(__WXMSW__)
      if (mRc == SQLITE_OK)
      {
         // Use the "unix-excl" VFS to make access to the DB exclusive.  This gets
         // rid of the "<dbname>-shm" shared memory file.
         //
         // Though it shouldn't, it doesn't matter if this fails.
         auto vfs = sqlite3_vfs_find("unix-excl");
         if (vfs)
         {
            sqlite3_vfs_register(vfs, 1);
         }
      }
#endif

   }
   ~SQLiteIniter()
   {
      // This function must be called single-threaded only
      // It returns a value, but there's nothing we can do with it
      (void) sqlite3_shutdown();
   }
   int mRc;
};

bool ProjectFileIO::InitializeSQL()
{
   static SQLiteIniter sqliteIniter;
   return sqliteIniter.mRc == SQLITE_OK;
}

static void RefreshAllTitles(bool bShowProjectNumbers )
{
   for ( auto pProject : AllProjects{} ) {
      if ( !GetProjectFrame( *pProject ).IsIconized() ) {
         ProjectFileIO::Get( *pProject ).SetProjectTitle(
            bShowProjectNumbers ? pProject->GetProjectNumber() : -1 );
      }
   }
}

TitleRestorer::TitleRestorer(
   wxTopLevelWindow &window, AudacityProject &project )
{
   if( window.IsIconized() )
      window.Restore();
   window.Raise(); // May help identifying the window on Mac

   // Construct this project's name and number.
   sProjName = project.GetProjectName();
   if ( sProjName.empty() ) {
      sProjName = _("<untitled>");
      UnnamedCount = std::count_if(
         AllProjects{}.begin(), AllProjects{}.end(),
         []( const AllProjects::value_type &ptr ){
            return ptr->GetProjectName().empty();
         }
      );
      if ( UnnamedCount > 1 ) {
         sProjNumber.Printf(
            _("[Project %02i] "), project.GetProjectNumber() + 1 );
         RefreshAllTitles( true );
      } 
   }
   else
      UnnamedCount = 0;
}

TitleRestorer::~TitleRestorer() {
   if( UnnamedCount > 1 )
      RefreshAllTitles( false );
}

static const AudacityProject::AttachedObjects::RegisteredFactory sFileIOKey{
   []( AudacityProject &parent ){
      auto result = std::make_shared< ProjectFileIO >( parent );
      return result;
   }
};

ProjectFileIO &ProjectFileIO::Get( AudacityProject &project )
{
   auto &result = project.AttachedObjects::Get< ProjectFileIO >( sFileIOKey );
   return result;
}

const ProjectFileIO &ProjectFileIO::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectFileIO::ProjectFileIO(AudacityProject &)
{
   mPrevDB = nullptr;
   mDB = nullptr;

   mRecovered = false;
   mModified = false;
   mTemporary = true;

   mBypass = false;

   UpdatePrefs();
}

void ProjectFileIO::Init( AudacityProject &project )
{
   // This step can't happen in the ctor of ProjectFileIO because ctor of
   // AudacityProject wasn't complete
   mpProject = project.shared_from_this();
}

ProjectFileIO::~ProjectFileIO()
{
   if (mDB)
   {
      // Save the filename since CloseDB() will clear it
      wxString filename = mFileName;

      // Not much we can do if this fails.  The user will simply get
      // the recovery dialog upon next restart.
      if (CloseDB())
      {
         // At this point, we are shutting down cleanly and if the project file is
         // still in the temp directory it means that the user has chosen not to
         // save it.  So, delete it.
         if (mTemporary)
         {
            wxFileName temp(FileNames::TempDir());
            if (temp == wxPathOnly(filename))
            {
               wxRemoveFile(filename);
            }
         }
      }
   }
}

sqlite3 *ProjectFileIO::DB()
{
   if (!mDB)
   {
      OpenDB();
      if (!mDB)
         throw SimpleMessageBoxException{
            XO("Failed to open the project's database") };
   }

   return mDB;
}

// Put the current database connection aside, keeping it open, so that
// another may be opened with OpenDB()
void ProjectFileIO::SaveConnection()
{
   // Should do nothing in proper usage, but be sure not to leak a connection:
   DiscardConnection();

   mPrevDB = mDB;
   mDB = nullptr;
}

// Close any set-aside connection
void ProjectFileIO::DiscardConnection()
{
   if ( mPrevDB )
   {
      auto rc = sqlite3_close( mPrevDB );
      if ( rc != SQLITE_OK )
      {
         // Store an error message
         SetDBError(
            XO("Failed to successfully close the source project file")
         );
      }
      mPrevDB = nullptr;
   }
}

// Close any current connection and switch back to using the saved
void ProjectFileIO::RestoreConnection()
{
   if ( mDB )
   {
      auto rc = sqlite3_close( mDB );
      if ( rc != SQLITE_OK )
      {
         // Store an error message
         SetDBError(
            XO("Failed to successfully close the destination project file")
         );
      }
   }
   mDB = mPrevDB;
}

void ProjectFileIO::UseConnection( sqlite3 *db )
{
   wxASSERT(mDB == nullptr);
   mDB = db;
}

sqlite3 *ProjectFileIO::OpenDB(FilePath fileName)
{
   wxASSERT(mDB == nullptr);
   bool temp = false;

   if (fileName.empty())
   {
      fileName = GetFileName();
      if (fileName.empty())
      {
         fileName = FileNames::UnsavedProjectFileName();
         temp = true;
      }
      else
      {
         temp = false;
      }
   }

   int rc = sqlite3_open(fileName, &mDB);
   if (rc != SQLITE_OK)
   {
      SetDBError(XO("Failed to open project file"));
      // sqlite3 docs say you should close anyway to avoid leaks
      sqlite3_close( mDB );
      mDB = nullptr;
      return nullptr;
   }

   if (!CheckVersion())
   {
      CloseDB();
      return nullptr;
   }

   mTemporary = temp;
   SetFileName(fileName);

   return mDB;
}

bool ProjectFileIO::CloseDB()
{
   int rc;

   if (mDB)
   {
      if (!mTemporary)
      {
         rc = sqlite3_exec(mDB, "VACUUM;", nullptr, nullptr, nullptr);
         if (rc != SQLITE_OK)
         {
            wxLogError(XO("Vacuuming failed while closing project file").Translation());
         }
      }

      rc = sqlite3_close(mDB);
      if (rc != SQLITE_OK)
      {
         SetDBError(XO("Failed to close the project file"));
      }

      mDB = nullptr;
      SetFileName({});
   }

   return true;
}

bool ProjectFileIO::DeleteDB()
{
   wxASSERT(mDB == nullptr);

   if (mTemporary && !mFileName.empty())
   {
      wxFileName temp(FileNames::TempDir());
      if (temp == wxPathOnly(mFileName))
      {
         if (!wxRemoveFile(mFileName))
         {
            SetError(XO("Failed to close the project file"));

            return false;
         }
      }
   }

   return true;
}

bool ProjectFileIO::TransactionStart(const wxString &name)
{
   char* errmsg = nullptr;

   int rc = sqlite3_exec(DB(),
                         wxT("SAVEPOINT ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      SetDBError(
         XO("Failed to create savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

bool ProjectFileIO::TransactionCommit(const wxString &name)
{
   char* errmsg = nullptr;

   int rc = sqlite3_exec(DB(),
                         wxT("SAVEPOINT ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      SetDBError(
         XO("Failed to release savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

bool ProjectFileIO::TransactionRollback(const wxString &name)
{
   char* errmsg = nullptr;

   int rc = sqlite3_exec(DB(),
                         wxT("RELEASE ") + name + wxT(";"),
                         nullptr,
                         nullptr,
                         &errmsg);

   if (errmsg)
   {
      SetDBError(
         XO("Failed to release savepoint:\n\n%s").Format(name)
      );
      sqlite3_free(errmsg);
   }

   return rc == SQLITE_OK;
}

/* static */
int ProjectFileIO::ExecCallback(void *data, int cols, char **vals, char **names)
{
   ExecParm *parms = static_cast<ExecParm *>(data);
   return parms->func(parms->result, cols, vals, names);
}

int ProjectFileIO::Exec(const char *query, ExecCB callback, wxString *result)
{
   char *errmsg = nullptr;
   ExecParm ep = {callback, result};

   int rc = sqlite3_exec(DB(), query, ExecCallback, &ep, &errmsg);

   if (errmsg)
   {
      SetDBError(
         XO("Failed to execute a project file command:\n\n%s").Format(query)
      );
      mLibraryError = Verbatim(errmsg);
      sqlite3_free(errmsg);
   }

   return rc;
}

bool ProjectFileIO::GetValue(const char *sql, wxString &result)
{
   auto getresult = [](wxString *result, int cols, char **vals, char **names)
   {
      if (cols == 1 && vals[0])
      {
         result->assign(vals[0]);
         return SQLITE_OK;
      }

      return SQLITE_ABORT;
   };

   int rc = Exec(sql, getresult, &result);
   if (rc != SQLITE_OK)
   {
      return false;
   }

   return true;
}

bool ProjectFileIO::GetBlob(const char *sql, wxMemoryBuffer &buffer)
{
   auto db = DB();
   int rc;

   sqlite3_stmt *stmt = nullptr;
   auto cleanup = finally([&]
   {
      if (stmt)
      {
         sqlite3_finalize(stmt);
      }
   });

   rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to prepare project file command:\n\n%s").Format(sql)
      );
      return false;
   }

   rc = sqlite3_step(stmt);

   // A row wasn't found...not an error
   if (rc == SQLITE_DONE)
   {
      return true;
   }

   if (rc != SQLITE_ROW)
   {
      SetDBError(
         XO("Failed to retrieve data from the project file.\nThe following command failed:\n\n%s").Format(sql)
      );
      // AUD TODO handle error
      return false;
   }

   const void *blob = sqlite3_column_blob(stmt, 0);
   int size = sqlite3_column_bytes(stmt, 0);

   buffer.Clear();
   buffer.AppendData(blob, size);

   return true;
}

bool ProjectFileIO::CheckVersion()
{
   auto db = DB();
   int rc;

   // Install our schema if this is an empty DB
   wxString result;
   if (!GetValue("SELECT Count(*) FROM sqlite_master WHERE type='table';", result))
   {
      return false;
   }

   // If the return count is zero, then there are no tables defined, so this
   // must be a new project file.
   if (wxStrtol<char **>(result, nullptr, 10) == 0)
   {
      return InstallSchema();
   }

   // Check for our application ID
   if (!GetValue("PRAGMA application_ID;", result))
   {
      return false;
   }

   // It's a database that SQLite recognizes, but it's not one of ours
   if (wxStrtoul<char **>(result, nullptr, 10) != ProjectFileID)
   {
      SetError(XO("This is not an Audacity project file"));
      return false;
   }

   // Get the project file version
   if (!GetValue("PRAGMA user_version;", result))
   {
      return false;
   }

   long version = wxStrtol<char **>(result, nullptr, 10);

   // Project file version is higher than ours. We will refuse to
   // process it since we can't trust anything about it.
   if (version > ProjectFileVersion)
   {
      SetError(
         XO("This project was created with a newer version of Audacity:\n\nYou will need to upgrade to process it")
      );
      return false;
   }
   
   // Project file is older than ours, ask the user if it's okay to
   // upgrade.
   if (version < ProjectFileVersion)
   {
      return UpgradeSchema();
   }

   return true;
}

bool ProjectFileIO::InstallSchema()
{
   auto db = DB();
   int rc;

   char sql[1024];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    ProjectFileSchema,
                    ProjectFileID,
                    ProjectFileVersion);

   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to initialize the project file")
      );
      return false;
   }

   return true;
}

bool ProjectFileIO::UpgradeSchema()
{
   return true;
}

void ProjectFileIO::LoadedBlock(int64_t sbid)
{
   if (sbid > mHighestBlockID)
   {
      mHighestBlockID = sbid;
   }
}

bool ProjectFileIO::CheckForOrphans()
{
   auto db = DB();

   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "DELETE FROM sampleblocks WHERE blockid > %lld",
                    mHighestBlockID);

   char *errmsg = nullptr;

   int rc = sqlite3_exec(db, sql, nullptr, nullptr, &errmsg);

   if (errmsg)
   {
      mLibraryError = Verbatim(wxString(errmsg));
      sqlite3_free(errmsg);
   }

   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Failed to delete orphaned blocks")
      );

      return false;
   }

   int changes = sqlite3_changes(db);
   if (changes > 0)
   {
      wxLogInfo(XO("Total orphan blocks deleted %d").Translation(), changes);
      mRecovered = true;
   }

   return true;
}

sqlite3 *ProjectFileIO::CopyTo(const FilePath &destpath)
{
   auto db = DB();
   int rc;
   ProgressResult res = ProgressResult::Success;

   sqlite3 *destdb = nullptr;

   /* Open the database file identified by zFilename. */
   rc = sqlite3_open(destpath, &destdb);
   if (rc == SQLITE_OK)
   {
      bool success = true;
      sqlite3_backup *backup = sqlite3_backup_init(destdb, "main", db, "main");
      if (backup)
      {
         /* i18n-hint: This title appears on a dialog that indicates the progress
            in doing something.*/
         ProgressDialog progress(XO("Progress"), XO("Saving project"));

         do
         {
            // These two calls always return zero before any sqlite3_backup_step
            // but that doesn't matter
            int remaining = sqlite3_backup_remaining(backup);
            int total = sqlite3_backup_pagecount(backup);

            if (progress.Update(total - remaining, total) != ProgressResult::Success)
            {
               SetError(
                  XO("Copy process cancelled.")
               );
               success = false;
               break;
            }

            rc = sqlite3_backup_step(backup, 12);
         } while (rc == SQLITE_OK || rc == SQLITE_BUSY || rc == SQLITE_LOCKED);

         // The return code from finish() will reflect errors from step()
         rc = sqlite3_backup_finish(backup);
         if (rc != SQLITE_OK)
         {
            SetDBError(
               XO("The copy process failed for:\n\n%s").Format(destpath)
            );
            success = false;
         }
      }
      else
      {
         SetDBError(
            XO("Unable to initiate the backup process.")
         );
         success = false;
      }

      if (!success)
      {
         // Don't give this DB connection back to the caller
         rc = sqlite3_close(destdb);
         if (rc != SQLITE_OK)
         {
            SetDBError(
               XO("Failed to successfully close the destination project file:\n\n%s")
            );
         }
         wxRemoveFile(destpath);
         return nullptr;
      }
   }
   else
   {
      // sqlite3 docs say you should close anyway to avoid leaks
      sqlite3_close( destdb );
      SetDBError(
         XO("Unable to open the destination project file:\n\n%s").Format(destpath)
      );
      return nullptr;
   }

   // Let the caller use this connection and close it later
   return destdb;
}

void ProjectFileIO::UpdatePrefs()
{
   SetProjectTitle();
}

// Pass a number in to show project number, or -1 not to.
void ProjectFileIO::SetProjectTitle(int number)
{
   auto pProject = mpProject.lock();
   if (! pProject )
      return;

   auto &project = *pProject;
   auto pWindow = project.GetFrame();
   if (!pWindow)
   {
      return;
   }
   auto &window = *pWindow;
   wxString name = project.GetProjectName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if (number >= 0)
   {
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      name = wxString::Format(_("[Project %02i] Audacity \"%s\""), number + 1,
         name.empty() ? "<untitled>" : (const char *)name);
   }
   // If we are not showing numbers, then <untitled> shows as 'Audacity'.
   else if (name.empty())
   {
      name = _TS("Audacity");
   }

   if (mRecovered)
   {
      name += wxT(" ");
      /* i18n-hint: E.g this is recovered audio that had been lost.*/
      name += _("(Recovered)");
   }

   if (name != window.GetTitle())
   {
      window.SetTitle( name );
      window.SetName(name);       // to make the nvda screen reader read the correct title

      project.QueueEvent(
         safenew wxCommandEvent{ EVT_PROJECT_TITLE_CHANGE } );
   }
}

const FilePath &ProjectFileIO::GetFileName() const
{
   return mFileName;
}

void ProjectFileIO::SetFileName(const FilePath &fileName)
{
   auto pProject = mpProject.lock();
   if (! pProject )
      return;
   auto &project = *pProject;

   mFileName = fileName;

   if (mTemporary)
   {
      project.SetProjectName({});
   }
   else
   {
      project.SetProjectName(wxFileName(mFileName).GetName());
   }

   SetProjectTitle();
}

bool ProjectFileIO::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto pProject = mpProject.lock();
   if (! pProject )
      return false;
   auto &project = *pProject;
   auto &window = GetProjectFrame(project);
   auto &viewInfo = ViewInfo::Get(project);
   auto &settings = ProjectSettings::Get(project);

   wxString fileVersion;
   wxString audacityVersion;
   int requiredTags = 0;
   long longVpos = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while (*attrs)
   {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
      {
         break;
      }

      if (viewInfo.ReadXMLAttribute(attr, value))
      {
         // We need to save vpos now and restore it below
         longVpos = std::max(longVpos, long(viewInfo.vpos));
         continue;
      }

      else if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("audacityversion")))
      {
         audacityVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("rate")))
      {
         double rate;
         Internat::CompatibleToDouble(value, &rate);
         settings.SetRate( rate );
      }

      else if (!wxStrcmp(attr, wxT("snapto")))
      {
         settings.SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
      {
         settings.SetSelectionFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );
      }

      else if (!wxStrcmp(attr, wxT("audiotimeformat")))
      {
         settings.SetAudioTimeFormat(
            NumericConverter::LookupFormat( NumericConverter::TIME, value) );
      }

      else if (!wxStrcmp(attr, wxT("frequencyformat")))
      {
         settings.SetFrequencySelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::FREQUENCY, value ) );
      }

      else if (!wxStrcmp(attr, wxT("bandwidthformat")))
      {
         settings.SetBandwidthSelectionFormatName(
            NumericConverter::LookupFormat( NumericConverter::BANDWIDTH, value ) );
      }
   } // while

   if (longVpos != 0)
   {
      // PRL: It seems this must happen after SetSnapTo
      viewInfo.vpos = longVpos;
   }

   if (requiredTags < 2)
   {
      return false;
   }

   // Parse the file version from the project
   int fver;
   int frel;
   int frev;
   if (!wxSscanf(fileVersion, wxT("%i.%i.%i"), &fver, &frel, &frev))
   {
      return false;
   }

   // Parse the file version Audacity was build with
   int cver;
   int crel;
   int crev;
   wxSscanf(wxT(AUDACITY_FILE_FORMAT_VERSION), wxT("%i.%i.%i"), &cver, &crel, &crev);

   if (cver < fver || crel < frel || crev < frev)
   {
      /* i18n-hint: %s will be replaced by the version number.*/
      auto msg = XO("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file.")
         .Format(audacityVersion, AUDACITY_VERSION_STRING);

      AudacityMessageBox(
         msg,
         XO("Can't open project file"),
         wxOK | wxICON_EXCLAMATION | wxCENTRE,
         &window);

      return false;
   }

   if (wxStrcmp(tag, wxT("project")))
   {
      return false;
   }

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *ProjectFileIO::HandleXMLChild(const wxChar *tag)
{
   auto pProject = mpProject.lock();
   if (! pProject )
      return nullptr;
   auto &project = *pProject;
   auto fn = ProjectFileIORegistry::Lookup(tag);
   if (fn)
   {
      return fn(project);
   }

   return nullptr;
}

void ProjectFileIO::WriteXMLHeader(XMLWriter &xmlFile) const
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

void ProjectFileIO::WriteXML(XMLWriter &xmlFile, const WaveTrackArray *tracks)
// may throw
{
   auto pProject = mpProject.lock();
   if (! pProject )
      THROW_INCONSISTENCY_EXCEPTION;
   auto &proj = *pProject;
   auto &tracklist = TrackList::Get(proj);
   auto &viewInfo = ViewInfo::Get(proj);
   auto &tags = Tags::Get(proj);
   const auto &settings = ProjectSettings::Get(proj);

   //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );

   xmlFile.StartTag(wxT("project"));
   xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

   xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   viewInfo.WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), settings.GetRate());
   xmlFile.WriteAttr(wxT("snapto"), settings.GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"),
                     settings.GetSelectionFormat().Internal());
   xmlFile.WriteAttr(wxT("frequencyformat"),
                     settings.GetFrequencySelectionFormatName().Internal());
   xmlFile.WriteAttr(wxT("bandwidthformat"),
                     settings.GetBandwidthSelectionFormatName().Internal());

   tags.WriteXML(xmlFile);

   unsigned int ndx = 0;
   if (tracks)
   {
      for (auto track : *tracks)
      {
         track->WriteXML(xmlFile);
      }
   }
   else
   {
      tracklist.Any().Visit([&](Track *t)
      {
         t->WriteXML(xmlFile);
      });
   }

   xmlFile.EndTag(wxT("project"));

   //TIMER_STOP( xml_writer_timer );
}

bool ProjectFileIO::AutoSave(const WaveTrackArray *tracks)
{
   AutoSaveFile autosave;
   WriteXMLHeader(autosave);
   WriteXML(autosave, tracks);

   return AutoSave(autosave);
}

bool ProjectFileIO::AutoSave(const AutoSaveFile &autosave)
{
   auto db = DB();
   int rc;

   mModified = true;

   // For now, we always use an ID of 1. This will replace the previously
   // writen row every time.
   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "INSERT INTO autosave(id, dict, doc) VALUES(1, ?1, ?2)"
                    "       ON CONFLICT(id) DO UPDATE SET %sdoc = ?2;",
                    autosave.DictChanged() ? "dict = ?1, " : "");

   sqlite3_stmt *stmt = nullptr;
   auto cleanup = finally([&]
   {
      if (stmt)
      {
         sqlite3_finalize(stmt);
      }
   });

   rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Unable to prepare project file command:\n\n%s").Format(sql)
      );
      return false;
   }

   const wxMemoryBuffer &dict = autosave.GetDict();
   const wxMemoryBuffer &data = autosave.GetData();

   // BIND SQL autosave
   // Might return SQL_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (
      sqlite3_bind_blob(stmt, 1, dict.GetData(), dict.GetDataLen(), SQLITE_STATIC) ||
      sqlite3_bind_blob(stmt, 2, data.GetData(), data.GetDataLen(), SQLITE_STATIC)
   )
      THROW_INCONSISTENCY_EXCEPTION;

   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      SetDBError(
         XO("Failed to update the project file.\nThe following command failed:\n\n%s").Format(sql)
      );
      return false;
   }

   return true;
}

bool ProjectFileIO::AutoSaveDelete()
{
   auto db = DB();
   int rc;

   rc = sqlite3_exec(db, "DELETE FROM autosave;", nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Failed to remove the autosave information from the project file.")
      );
      return false;
   }

   return true;
}

bool ProjectFileIO::LoadProject(const FilePath &fileName)
{
   bool success = false;

   // OpenDB() will change mFileName if the file opens/verifies successfully,
   // so we must set it back to what it was if any errors are encountered here.
   wxString oldFilename = mFileName;

   auto cleanup = finally([&]
   {
      if (!success)
      {
         CloseDB();
         SetFileName(oldFilename);
      }
   });

   // Open the project file
   if (!OpenDB(fileName))
   {
      return false;
   }

   // Get the XML document...either from the project or autosave
   wxString doc;

   // Get the autosave doc, if any
   bool wasAutosave = false;
   wxMemoryBuffer buffer;
   if (!GetBlob("SELECT dict || doc FROM autosave WHERE id = 1;", buffer))
   {
      // Error already set
      return false;
   }

   if (buffer.GetDataLen() > 0)
   {
      doc = AutoSaveFile::Decode(buffer);
      wasAutosave = true;
   }
   // Otherwise, get the project doc
   else if (!GetValue("SELECT doc FROM project;", doc))
   {
      // Error already set
      return false;
   }

   if (doc.empty())
   {
      return false;
   }

   XMLFileReader xmlFile;

   // Reset the highest blockid
   mHighestBlockID = 0;

   success = xmlFile.ParseString(this, doc);
   if (!success)
   {
      SetError(
         XO("Unable to parse project information.")
      );
      mLibraryError = xmlFile.GetErrorStr();
      return false;
   }

   // Remember if it was recovered or not
   mRecovered = wasAutosave;

   // Check for orphans blocks...set mRecovered if any deleted
   if (mHighestBlockID > 0)
   {
      if (!CheckForOrphans())
      {
         return false;
      }
   }

   if (mRecovered)
   {
      mModified = true;
   }

   // A previously saved project will have a document in the project table, so
   // we use that knowledge to determine if this file is an unsaved/temporary
   // file or not
   wxString result;
   if (!GetValue("SELECT Count(*) FROM project;", result))
   {
      return false;
   }

   mTemporary = (wxStrtol<char **>(result, nullptr, 10) != 1);

   SetFileName(fileName);

   return true;
}

bool ProjectFileIO::SaveProject(const FilePath &fileName)
{
   wxString origName;
   bool wasTemp = false;
   bool success = false;

   // Should probably simplify all of the following by using renames. But, one
   // benefit of using CopyTo() for new file saves, is that it will be VACUUMED
   // at the same time.

   auto restore = finally([&]
   {
      if (!origName.empty())
      {
         if (success)
         {
            // The Save was successful, so now it is safe to abandon the
            // original connection
            DiscardConnection();
         
            // And also remove the original file if it was a temporary file
            if (wasTemp)
            {
               wxRemoveFile(origName);
            }

         }
         else
         {
            // Close the new database and go back to using the original
            // connection
            RestoreConnection();

            // And delete the new database
            wxRemoveFile(fileName);
         }
      }
   });

   // If we're saving to a different file than the current one, then copy the
   // current to the new file and make it the active file.
   if (mFileName != fileName)
   {
      auto newDB = CopyTo(fileName);
      if (!newDB)
      {
         return false;
      }

      // Remember the original project filename and temporary status.  Only do
      // this after a successful copy so the "finally" block above doesn't monkey
      // with the files.
      origName = mFileName;
      wasTemp = mTemporary;

      // Save the original database connection and try to switch to a new one
      // (also ensuring closing of one of the connections, with the cooperation
      // of the finally above)
      SaveConnection();
      UseConnection( newDB );
   }

   auto db = DB();
   int rc;

   XMLStringWriter doc;
   WriteXMLHeader(doc);
   WriteXML(doc);

   // Always use an ID of 1. This will replace any existing row.
   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "INSERT INTO project(id, doc) VALUES(1, ?1)"
                    "       ON CONFLICT(id) DO UPDATE SET doc = ?1;");


   {
      sqlite3_stmt* stmt = nullptr;

      auto finalize = finally([&]
      {
         if (stmt)
         {
            // This will free the statement
            sqlite3_finalize(stmt);
         }
      });

      rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
      if (rc != SQLITE_OK)
      {
         SetDBError(
            XO("Unable to prepare project file command:\n\n%s").Format(sql)
         );
         return false;
      }

      // BIND SQL project
      rc = sqlite3_bind_text(stmt, 1, doc, -1, SQLITE_STATIC);
      if (rc != SQLITE_OK)
      {
         SetDBError(
            XO("Unable to bind to project file document.")
         );
         return false;
      }
 
      rc = sqlite3_step(stmt);
      if (rc != SQLITE_DONE)
      {
         SetDBError(
            XO("Failed to save project file information.")
         );
         return false;
      }
   }

   // We need to remove the autosave info from the file since it is now
   // clean and unmodified.  Otherwise, it would be considered "recovered"
   // when next opened.

   if ( !AutoSaveDelete() )
      return false;

   // Reaching this point defines success and all the rest are no-fail
   // operations:

   // Tell the finally block to behave
   success = true;

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
   auto db = CopyTo(fileName);
   if (!db)
   {
      return false;
   }

   int rc;
   bool success = false;

   auto cleanup = finally([&]
   {
      if (db)
      {
         (void) sqlite3_close(db);
      }

      if (!success)
      {
         wxRemoveFile(fileName);
      }
   });

   XMLStringWriter doc;
   WriteXMLHeader(doc);
   WriteXML(doc);

   // Always use an ID of 1. This will replace any existing row.
   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "INSERT INTO project(id, doc) VALUES(1, ?1)"
                    "       ON CONFLICT(id) DO UPDATE SET doc = ?1;");

   {
      sqlite3_stmt* stmt = nullptr;

      auto finalize = finally([&]
      {
         if (stmt)
         {
            // This will free the statement
            sqlite3_finalize(stmt);
         }
      });

      rc = sqlite3_prepare_v2(db, sql, -1, &stmt, 0);
      if (rc != SQLITE_OK)
      {
         SetDBError(
            XO("Unable to prepare project file command:\n\n%s").Format(sql)
         );
         return false;
      }

      // BIND SQL project
      rc = sqlite3_bind_text(stmt, 1, doc, -1, SQLITE_STATIC);
      if (rc != SQLITE_OK)
      {
         SetDBError(
            XO("Unable to bind to project file document.")
         );
         return false;
      }

      rc = sqlite3_step(stmt);
      if (rc != SQLITE_DONE)
      {
         SetDBError(
            XO("Failed to save project file information.")
         );
         return false;
      }
   }

   rc = sqlite3_exec(db, "DELETE FROM autosave;", nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      SetDBError(
         XO("Failed to remove the autosave information from the project file.")
      );
      return false;
   }

   // Tell the finally block to behave
   success = true;

   return true;
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

void ProjectFileIO::Reset()
{
   wxASSERT_MSG(mDB == nullptr, wxT("Resetting project with open project file"));

   mModified = false;
   mRecovered = false;

   SetFileName({});
}

wxLongLong ProjectFileIO::GetFreeDiskSpace()
{
   // make sure it's open and the path is defined
   auto db = DB();

   wxLongLong freeSpace;
   if (wxGetDiskSpace(wxPathOnly(mFileName), NULL, &freeSpace))
   {
      return freeSpace;
   }

   return -1;
}

const TranslatableString & ProjectFileIO::GetLastError() const
{
   return mLastError;
}

const TranslatableString & ProjectFileIO::GetLibraryError() const
{
   return mLibraryError;
}

void ProjectFileIO::SetError(const TranslatableString &msg)
{
   mLastError = msg;
   mLibraryError = {};
}

void ProjectFileIO::SetDBError(const TranslatableString &msg)
{
   mLastError = msg;
   wxLogDebug(wxT("SQLite error: %s"), mLastError.Debug());
   if (mDB)
   {
      mLibraryError = Verbatim(sqlite3_errmsg(mDB));
      wxLogDebug(wxT("   Lib error: %s"), mLibraryError.Debug());
   }
}

void ProjectFileIO::Bypass(bool bypass)
{
   mBypass = bypass;
}

bool ProjectFileIO::ShouldBypass()
{
   return mTemporary && mBypass;
}

