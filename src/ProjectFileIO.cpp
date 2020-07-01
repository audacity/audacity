/**********************************************************************

Audacity: A Digital Audio Editor

ProjectFileIO.cpp

Paul Licameli split from AudacityProject.cpp

**********************************************************************/

#include "ProjectFileIO.h"

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
static const int ProjectFilePageSize = 4096;

static const char *ProjectFileSchema =
   "PRAGMA application_id = %d;"
   "PRAGMA user_version = %d;"
   "PRAGMA page_size = %d;"
   "PRAGMA journal_mode = DELETE;"
   ""
   "CREATE TABLE IF NOT EXISTS project"
   "("
   "  doc                  TEXT"
   ");"
   ""
   "CREATE TABLE IF NOT EXISTS autosave"
   "("
   "  id                   INTEGER PRIMARY KEY,"
   "  dict                 BLOB,"
   "  doc                  BLOB"
   ");"
   ""
   "CREATE TABLE IF NOT EXISTS tags"
   "("
   "  name                 TEXT,"
   "  value                BLOB"
   ");"
   ""
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
      sqlite3_initialize();
   }
   ~SQLiteIniter()
   {
      sqlite3_shutdown();
   }
};
static SQLiteIniter sqliteIniter;

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
   return project.AttachedObjects::Get< ProjectFileIO >( sFileIOKey );
}

const ProjectFileIO &ProjectFileIO::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectFileIO::ProjectFileIO(AudacityProject &project)
:  mProject(project)
{
   mDB = nullptr;

   mRecovered = false;
   mModified = false;
   mTemporary = true;

   UpdatePrefs();
}

ProjectFileIO::~ProjectFileIO()
{
   if (mDB)
   {
      // Not much we can do if this fails.  The user will simply get
      // the recovery dialog upon next restart.
      if (CloseDB())
      {
         // Always remove the journal now that the DB is closed
         wxRemoveFile(mDBPath + wxT("-journal"));

         // At this point, we are shutting down cleanly and if the project file is
         // still in the temp directory it means that the user has chosen not to
         // save it.  So, delete it.
         if (mTemporary && !mDBPath.empty())
         {
            wxFileName temp(FileNames::TempDir());
            if (temp == wxPathOnly(mDBPath))
            {
               wxRemoveFile(mDBPath);
            }
         }
      }
   }
}

sqlite3 *ProjectFileIO::DB()
{
   if (mDB)
   {
      return mDB;
   }

   return OpenDB();
}

sqlite3 *ProjectFileIO::OpenDB(FilePath fileName)
{
   wxASSERT(mDB == nullptr);

   if (fileName.empty())
   {
      fileName = GetFileName();
      if (fileName.empty())
      {
         fileName = FileNames::UnsavedProjectFileName();
         mTemporary = true;
      }
      else
      {
         mTemporary = false;
      }
   }

   int rc = sqlite3_open(fileName, &mDB);
   if (rc != SQLITE_OK)
   {
      // AUD3 TODO COMPLAIN AND THROW - crash in inevitable otherwise
      return nullptr;
   }

 // AUD3 TODO get rid of the mDBPath?!?!?!?!
   mDBPath = fileName;
   mFileName = mDBPath;

   if (!CheckVersion())
   {
      CloseDB();

      // AUD3 TODO COMPLAIN AND THROW - crash in inevitable otherwise
      return nullptr;
   }

   return mDB;
}

bool ProjectFileIO::CloseDB()
{
   int rc;

   if (mDB)
   {
      rc = sqlite3_close(mDB);
      if (rc != SQLITE_OK)
      {
         wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(mDB));
      }
      else
      {
         wxRemoveFile(mDBPath + wxT("-journal"));
      }

      mDB = nullptr;
   }

   return true;
}

bool ProjectFileIO::DeleteDB()
{
   if (!mDBPath.empty() && IsTemporary())
   {
      wxFileName temp(FileNames::TempDir());
      if (temp == wxPathOnly(mDBPath))
      {
         if (!wxRemoveFile(mDBPath))
         {
            return false;
         }

         wxRemoveFile(mDBPath + wxT("-journal"));
      }
   }

   return true;
}

bool ProjectFileIO::CleanDB()
{
   auto db = DB();
   int rc;

   AutoSave();

   wxString destpath = wxFileName::CreateTempFileName(mDBPath + ".xxxxx");

   if (CopyTo(destpath))
   {
      if (CloseDB())
      {
         // This can be removed even if we fail below since the DB is closed
         wxRemoveFile(mDBPath + wxT("-journal"));

         wxString tmppath = wxFileName::CreateTempFileName(mDBPath + ".xxxxx");
         if (wxRename(mDBPath, tmppath) == 0)
         {
            if (wxRename(destpath, mDBPath) == 0)
            {
               wxRemoveFile(tmppath);

               // Success
               return true;
            }

            if (wxRename(tmppath, mDBPath) == 0)
            {
               wxRemoveFile(destpath);
            }
         }
      }
   }

   // AUD3 TODO COMPILAIN
   return false;
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
      mLastError.Format(XO("SQLite Error: %s"), wxString(errmsg));
      sqlite3_free(errmsg);
   }

   return rc;
}

wxString ProjectFileIO::GetValue(const char *sql)
{
   auto getresult = [&](wxString *result, int cols, char **vals, char **names)
   {
      if (cols == 1 && vals[0])
      {
         result->append(vals[0]);
         return SQLITE_OK;
      }

      return SQLITE_ABORT;
   };

   wxString value;

   int rc = Exec(sql, getresult, &value);
   if (rc != SQLITE_OK)
   {
      wxLogDebug(wxT("SQLITE error %s"), mLastError.Translation());
   }

   return value;
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
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   rc = sqlite3_step(stmt);
   if (rc != SQLITE_ROW)
   {
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   const void *blob = sqlite3_column_blob(stmt, 0);
   int size = sqlite3_column_bytes(stmt, 0);

   buffer.AppendData(blob, size);

   return true;
}

bool ProjectFileIO::CheckVersion()
{
   auto db = DB();
   int rc;

   // Install our schema if this is an empty DB
   long count = -1;
   GetValue("SELECT Count(*) FROM sqlite_master WHERE type='table';").ToLong(&count);
   if (count == 0)
   {
      return InstallSchema();
   }

   // Check for our application ID
   long appid = 0;
   GetValue("PRAGMA application_ID;").ToLong(&appid);
   if (appid != ProjectFileID)
   {
      mLastError = XO("This is not an Audacity AUP3 file");
      return false;
   }

   // Get the project file version
   long version;
   GetValue("PRAGMA user_version;").ToLong(&version);

   // Project file version is higher than ours. We will refuse
   // to process it since we can't trust anything about it.
   if (version > ProjectFileVersion)
   {
      // AUD3 - complain about too new schema
      // can't handle it!
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
                    ProjectFileVersion,
                    ProjectFilePageSize);

   rc = sqlite3_exec(db, sql, nullptr, nullptr, nullptr);
   if (rc != SQLITE_OK)
   {
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      return false;
   }

   return true;
}

bool ProjectFileIO::UpgradeSchema()
{
   return true;
}

bool ProjectFileIO::CopyTo(const FilePath &destpath)
{
   auto db = DB();
   int rc;
   bool success = true;
   ProgressResult res = ProgressResult::Success;

   sqlite3 *destdb = nullptr;

   /* Open the database file identified by zFilename. */
   rc = sqlite3_open(destpath, &destdb);
   if (rc == SQLITE_OK)
   {
      sqlite3_backup *backup = sqlite3_backup_init(destdb, "main", db, "main");
      if (backup)
      {
         /* i18n-hint: This title appears on a dialog that indicates the progress
            in doing something.*/
         ProgressDialog progress(XO("Progress"), XO("Saving project"));

         do
         {
            int remaining = sqlite3_backup_remaining(backup);
            int total = sqlite3_backup_pagecount(backup);
            wxLogDebug(wxT("remaining %d total %d"), remaining, total);

            if (progress.Update(total - remaining, total) != ProgressResult::Success)
            {
               success = false;
               break;
            }

            rc = sqlite3_backup_step(backup, 12);
         } while (rc == SQLITE_OK || rc == SQLITE_BUSY || rc == SQLITE_LOCKED);

         // The return code from finish() will reflect errors from step()
         rc = sqlite3_backup_finish(backup);
         if (rc != SQLITE_OK)
         {
            success = false;
         }
      }

      // Close the DB
      rc = sqlite3_close(destdb);
      if (rc != SQLITE_OK)
      {
         success = false;
      }

      // Always delete the journal
      wxRemoveFile(destpath + wxT("-journal"));

      if (!success)
      {
         wxRemoveFile(destpath);
      }
   }
   else
   {
      success = false;
   }

   return success;
}

void ProjectFileIO::UpdatePrefs()
{
   SetProjectTitle();
}

// Pass a number in to show project number, or -1 not to.
void ProjectFileIO::SetProjectTitle(int number)
{
   auto &project = mProject;
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
   wxASSERT(mFileName == mDBPath);
   return mFileName;
}

void ProjectFileIO::SetFileName(const FilePath &fileName)
{
   mFileName = fileName;
   if (mFileName.empty())
   {
      mProject.SetProjectName({});
   }
   else
   {
      mProject.SetProjectName(wxFileName(mFileName).GetName());
   }

   SetProjectTitle();
}

// Most of this string was duplicated 3 places. Made the warning consistent in this global.
// The %s is to be filled with the version string.
// PRL:  Do not statically allocate a string in _() !
static TranslatableString gsLegacyFileWarning() { return
XO("This file was saved by Audacity version %s. The format has changed. \
\n\nAudacity can try to open and save this file, but saving it in this \
\nversion will then prevent any 1.2 or earlier version opening it. \
\n\nAudacity might corrupt the file in opening it, so you should \
back it up first. \
\n\nOpen this file now?");
}

bool ProjectFileIO::WarnOfLegacyFile( )
{
   auto &project = mProject;
   auto &window = GetProjectFrame( project );
   auto msg = gsLegacyFileWarning().Format( XO("1.0 or earlier") );

   // Stop icon, and choose 'NO' by default.
   int action =
      AudacityMessageBox(
         msg,
         XO("Warning - Opening Old Project File"),
         wxYES_NO | wxICON_STOP | wxNO_DEFAULT | wxCENTRE,
         &window);
   return (action != wxNO);
}

bool ProjectFileIO::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   auto &project = mProject;
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

   if (wxStrcmp(tag, wxT("audacityproject")) &&
       wxStrcmp(tag, wxT("project")))
   {
      // If the tag name is not one of these two (the NEW name is
      // "project" with an Audacity namespace, but we don't detect
      // the namespace yet), then we don't know what the error is
      return false;
   }

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *ProjectFileIO::HandleXMLChild(const wxChar *tag)
{
   auto fn = ProjectFileIORegistry::Lookup(tag);
   if (fn)
   {
      return fn(mProject);
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
   auto &proj = mProject;
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
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   const wxMemoryBuffer &dict = autosave.GetDict();
   const wxMemoryBuffer &data = autosave.GetData();

   sqlite3_bind_blob(stmt, 1, dict.GetData(), dict.GetDataLen(), SQLITE_STATIC);
   sqlite3_bind_blob(stmt, 2, data.GetData(), data.GetDataLen(), SQLITE_STATIC);

   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
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
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   return true;
}

bool ProjectFileIO::LoadProject(const FilePath &fileName)
{
   // Open the project file
   OpenDB(fileName);

   // Get the XML document...either from the project or autosave
   wxString doc;

   // Get the autosave doc, if any
   bool wasAutosave = false;
   wxMemoryBuffer buffer;
   if (GetBlob("SELECT dict || doc FROM autosave WHERE id = 1;", buffer))
   {
      doc = AutoSaveFile::Decode(buffer);
      wasAutosave = true;
   }
   else
   {
      // Get the project doc
      doc = GetValue("SELECT doc FROM project;");
   }

   if (doc.empty())
   {
      // AUD3 fixme
      wxLogDebug(wxT("%s"), mLastError.Translation());
      // handle error
      return false;
   }

   XMLFileReader xmlFile;

   bool success = xmlFile.ParseString(this, doc);
   if (!success)
   {
      mLastError = xmlFile.GetErrorStr();
   }
   else
   {
      mRecovered = wasAutosave;
      SetFileName(fileName);
      SetProjectTitle();
   }

   if (mRecovered)
   {
      mModified = true;
   }

   return success;
}

bool ProjectFileIO::SaveProject(const FilePath &fileName)
{
   auto db = DB();
   int rc;

   XMLStringWriter doc;
   WriteXMLHeader(doc);
   WriteXML(doc);

   char sql[256];
   sqlite3_snprintf(sizeof(sql),
                    sql,
                    "REPLACE INTO project (doc) VALUES(?);");

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
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   sqlite3_bind_text(stmt, 1, doc, -1, SQLITE_STATIC);
 
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      wxLogDebug(wxT("SQLITE error %s"), sqlite3_errmsg(db));
      // handle error
      return false;
   }

   sqlite3_finalize(stmt);
   stmt = nullptr;

   if (mDBPath != fileName)
   {
      if (!CopyTo(fileName))
      {
         return false;
      }

      // Remember the original project filename
      wxString origname = mDBPath;

      // Close the original project file
      CloseDB();

      // Set the new filename
      SetFileName(fileName);

      // And open the new one
      OpenDB(fileName);

      // If the original project was temporary, then delete it now
      if (mTemporary)
      {
         wxRemoveFile(origname);
      }
   }

   AutoSaveDelete();

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
   SetProjectTitle();
}

wxLongLong ProjectFileIO::GetFreeDiskSpace()
{
   // make sure it's open and the path is defined
   auto db = DB();

   wxLongLong freeSpace;
   if (wxGetDiskSpace(wxPathOnly(mDBPath), NULL, &freeSpace))
   {
      return freeSpace;
   }

   return -1;
}

const TranslatableString & ProjectFileIO::GetLastError() const
{
   return mLastError;
}

