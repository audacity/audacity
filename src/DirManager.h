/**********************************************************************

  Audacity: A Digital Audio Editor

  DirManager.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _DIRMANAGER_
#define _DIRMANAGER_

#include "MemoryX.h"
#include <wx/list.h>
#include <wx/string.h>
#include <wx/filename.h>
#include <wx/hashmap.h>
#include <wx/utils.h>

#include "audacity/Types.h"
#include "xml/XMLTagHandler.h"
#include "wxFileNameWrapper.h"

class wxHashTable;
class BlockArray;
class BlockFile;
class SequenceTest;

#define FSCKstatus_CLOSE_REQ 0x1
#define FSCKstatus_CHANGED   0x2
#define FSCKstatus_SAVE_AUP  0x4 // used in combination with FSCKstatus_CHANGED

WX_DECLARE_HASH_MAP(int, int, wxIntegerHash, wxIntegerEqual, DirHash);

class BlockFile;
using BlockFilePtr = std::shared_ptr<BlockFile>;

WX_DECLARE_HASH_MAP(wxString, std::weak_ptr<BlockFile>, wxStringHash,
                    wxStringEqual, BlockHash);

wxMemorySize GetFreeMemory();

class PROFILE_DLL_API DirManager final : public XMLTagHandler {
 public:

   // MM: Construct DirManager
   DirManager();

   virtual ~DirManager();

   static void SetTempDir(const wxString &_temp) { globaltemp = _temp; }

   // Returns true on success.
   // If SetProject is told NOT to create the directory
   // but it doesn't already exist, SetProject fails and returns false.
   bool SetProject(wxString& newProjPath, wxString& newProjName, const bool bCreate);

   wxString GetProjectDataDir();
   wxString GetProjectName();

   wxLongLong GetFreeDiskSpace();

   BlockFilePtr
      NewSimpleBlockFile(samplePtr sampleData,
                                 size_t sampleLen,
                                 sampleFormat format,
                                 bool allowDeferredWrite = false);

   BlockFilePtr
      NewAliasBlockFile( const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel);

   BlockFilePtr
      NewODAliasBlockFile( const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel);

   BlockFilePtr
      NewODDecodeBlockFile( const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel, int decodeType);

   /// Returns true if the blockfile pointed to by b is contained by the DirManager
   bool ContainsBlockFile(const BlockFile *b) const;
   /// Check for existing using filename using complete filename
   bool ContainsBlockFile(const wxString &filepath) const;

   // Adds one to the reference count of the block file,
   // UNLESS it is "locked", then it makes a NEW copy of
   // the BlockFile.
   BlockFilePtr CopyBlockFile(const BlockFilePtr &b);

   BlockFile *LoadBlockFile(const wxChar **attrs, sampleFormat format);
   void SaveBlockFile(BlockFile *f, int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   BlockFile *LoadBlockFile(wxTextFile * in, sampleFormat format);
   void SaveBlockFile(BlockFile * f, wxTextFile * out);
#endif

   bool MoveToNewProjectDirectory(BlockFile *f);
   bool CopyToNewProjectDirectory(BlockFile *f);

   bool EnsureSafeFilename(const wxFileName &fName);

   void SetLoadingTarget(BlockArray *pArray, unsigned idx)
   {
      mLoadingTarget = pArray;
      mLoadingTargetIdx = idx;
   }
   void SetLoadingFormat(sampleFormat format) { mLoadingFormat = format; }
   void SetLoadingBlockLength(size_t len) { mLoadingBlockLen = len; }

   // Note: following affects only the loading of block files when opening a project
   void SetLoadingMaxSamples(size_t max) { mMaxSamples = max; }

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag)) { return NULL; }
   void WriteXML(XMLWriter & WXUNUSED(xmlFile)) { wxASSERT(false); } // This class only reads tags.
   bool AssignFile(wxFileNameWrapper &filename, const wxString &value, bool check);

   // Clean the temp dir. Note that now where we have auto recovery the temp
   // dir is not cleaned at start up anymore. But it is cleaned when the
   // program is exited normally.
   static void CleanTempDir();

   // Check the project for errors and possibly prompt user
   // bForceError: Always show log error alert even if no errors are found here.
   //    Important when you know that there are already errors in the log.
   // bAutoRecoverMode: Do not show any option dialogs for how to deal with errors found here.
   //    Too complicated during auto-recover. Just correct problems the "safest" way.
   int ProjectFSCK(const bool bForceError, const bool bAutoRecoverMode);

   void FindMissingAliasedFiles(
         BlockHash& missingAliasedFileAUFHash,     // output: (.auf) AliasBlockFiles whose aliased files are missing
         BlockHash& missingAliasedFilePathHash);   // output: full paths of missing aliased files
   void FindMissingAUFs(
         BlockHash& missingAUFHash);               // output: missing (.auf) AliasBlockFiles
   void FindMissingAUs(
         BlockHash& missingAUHash);                // missing data (.au) blockfiles
   // Find .au and .auf files that are not in the project.
   void FindOrphanBlockFiles(
         const wxArrayString& filePathArray,       // input: all files in project directory
         wxArrayString& orphanFilePathArray);      // output: orphan files


   // Remove all orphaned blockfiles without user interaction. This is
   // generally safe, because orphaned blockfiles are not referenced by the
   // project and thus worthless anyway.
   void RemoveOrphanBlockfiles();

   // Get directory where data files are in. Note that projects are normally
   // not interested in this information, but it is important for the
   // auto-save functionality
   wxString GetDataFilesDir() const;

   // This should only be used by the auto save functionality
   void SetLocalTempDir(const wxString &path);

   // Do not DELETE any temporary files on exit. This is only called if
   // auto recovery is cancelled and should be retried later
   static void SetDontDeleteTempFiles() { dontDeleteTempFiles = true; }

   // Write all write-cached block files to disc, if any
   void WriteCacheToDisk();

   // Fill cache of blockfiles, if caching is enabled (otherwise do nothing)
   void FillBlockfilesCache();

 private:

   wxFileNameWrapper MakeBlockFileName();
   wxFileNameWrapper MakeBlockFilePath(const wxString &value);

   bool MoveOrCopyToNewProjectDirectory(BlockFile *f, bool copy);

   BlockHash mBlockFileHash; // repository for blockfiles

   // Hashes for management of the sub-directory tree of _data
   struct BalanceInfo
   {
      DirHash   dirTopPool;    // available toplevel dirs
      DirHash   dirTopFull;    // full toplevel dirs
      DirHash   dirMidPool;    // available two-level dirs
      DirHash   dirMidFull;    // full two-level dirs
   } mBalanceInfo;

   // Accessor for the balance info, may need to do a delayed update for
   // deletion in case other threads DELETE block files
   BalanceInfo &GetBalanceInfo();

   void BalanceInfoDel(const wxString&);
   void BalanceInfoAdd(const wxString&);
   void BalanceFileAdd(int);
   int BalanceMidAdd(int, int);

   wxString projName;
   wxString projPath;
   wxString projFull;

   wxString lastProject;

   wxArrayString aliasList;

   BlockArray *mLoadingTarget;
   unsigned mLoadingTargetIdx;
   sampleFormat mLoadingFormat;
   size_t mLoadingBlockLen;

   size_t mMaxSamples; // max samples per block

   unsigned long mLastBlockFileDestructionCount { 0 };

   static wxString globaltemp;
   wxString mytemp;
   static int numDirManagers;
   static bool dontDeleteTempFiles;

   friend class SequenceTest;
};

#endif
