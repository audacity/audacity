/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   DirManager.cpp

   Dominic Mazzoni
   Matt Brubeck
   Michael Chinen
   James Crook
   Al Dimond
   Brian Gunlogson
   Josh Haberman
   Vaughan Johnson
   Leland Lucius
   Monty
   Markus Meyer

*******************************************************************//*!

\class DirManager
\brief Creates and manages BlockFile objects.

  This class manages the files that a project uses to store most
  of its data.  It creates NEW BlockFile objects, which can
  be used to store any type of data.  BlockFiles support all of
  the common file operations, but they also support reference
  counting, so two different parts of a project can point to
  the same block of data.

  For example, a track might contain 10 blocks of data representing
  its audio.  If you copy the last 5 blocks and paste at the
  end of the file, no NEW blocks need to be created - we just store
  pointers to NEW ones.  When part of a track is deleted, the
  affected blocks decrement their reference counts, and when they
  reach zero they are deleted.  This same mechanism is also used
  to implement Undo.

  The DirManager, besides mapping filenames to absolute paths,
  also hashes all of the block names used in a project, so that
  when reading a project from disk, multiple copies of the
  same block still get mapped to the same BlockFile object.

  The blockfile/directory scheme is rather complicated with two different schemes.
  The current scheme uses two levels of subdirectories - up to 256 'eXX' and up to
  256 'dYY' directories within each of the 'eXX' dirs, where XX and YY are hex chars.
  In each of the dXX directories there are up to 256 audio files (e.g. .au or .auf).
  They have a filename scheme of 'eXXYYZZZZ', where XX and YY refers to the
  subdirectories as above.  The 'ZZZZ' component is generated randomly for some reason.
  The XX and YY components are sequential.
  DirManager fills up the current dYY subdir until 256 are created, and moves on to the next one.

  So for example, the first blockfile created may be 'e00/d00/e0000a23b.au' and the next
  'e00/d00/e000015e8.au', and the 257th may be 'e00/d01/e0001f02a.au'.
  On close the blockfiles that are no longer referenced by the project (edited or deleted) are removed,
  along with the consequent empty directories.


*//*******************************************************************/


#include "Audacity.h"
#include "DirManager.h"
#include "MemoryX.h"

#include <time.h> // to use time() for srand()

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/dir.h>
#include <wx/log.h>
#include <wx/filefn.h>
#include <wx/hash.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/object.h>

// chmod
#ifdef __UNIX__
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "AudacityApp.h"
#include "BlockFile.h"
#include "blockfile/LegacyBlockFile.h"
#include "blockfile/LegacyAliasBlockFile.h"
#include "blockfile/SimpleBlockFile.h"
#include "blockfile/SilentBlockFile.h"
#include "blockfile/PCMAliasBlockFile.h"
#include "blockfile/ODPCMAliasBlockFile.h"
#include "blockfile/ODDecodeBlockFile.h"
#include "Internat.h"
#include "Project.h"
#include "Prefs.h"
#include "Sequence.h"
#include "widgets/Warning.h"
#include "widgets/MultiDialog.h"

#include "ondemand/ODManager.h"

#include "Track.h"

#if defined(__WXMAC__)
#include <mach/mach.h>
#include <mach/vm_statistics.h>
#endif


wxMemorySize GetFreeMemory()
{
   wxMemorySize avail;

#if defined(__WXMAC__)
   mach_port_t port = mach_host_self();
   mach_msg_type_number_t cnt = HOST_VM_INFO_COUNT;
   vm_statistics_data_t	stats;
   vm_size_t pagesize = 0;

   memset(&stats, 0, sizeof(stats));

   host_page_size(port, &pagesize);
   host_statistics(port, HOST_VM_INFO, (host_info_t) &stats, &cnt);
   avail = stats.free_count * pagesize;
#else
   avail = wxGetFreeMemory();
#endif

   return avail;
}

//
// local helper functions for subdirectory traversal
//

// Behavior of RecursivelyEnumerate is tailored to our uses and not
// entirely straightforward.  It recurs depth-first from the passed-
// in directory into its subdirs according to optional dirspec
// pattern, building a list of directories and (optionally) files
// in the listed order.  The dirspec is not applied to
// subdirs of subdirs. Files in the passed-in directory will not be
// enumerated.  Also, the passed-in directory is the last entry added
// to the list.
static int RecursivelyEnumerate(wxString dirPath,
                                  wxArrayString& filePathArray,  // output: all files in dirPath tree
                                  wxString dirspec,
                                  bool bFiles, bool bDirs,
                                  int progress_count = 0,
                                  int progress_bias = 0,
                                  ProgressDialog* progress = NULL)
{
   int count=0;
   bool cont;

   wxDir dir(dirPath);
   if(dir.IsOpened()){
      wxString name;

      if (bFiles){
         cont= dir.GetFirst(&name, dirspec, wxDIR_FILES);
         while ( cont ){
            wxString filepath = dirPath + wxFILE_SEP_PATH + name;

            count++;
            filePathArray.Add(filepath);

            cont = dir.GetNext(&name);

            if (progress)
               progress->Update(count + progress_bias,
                                progress_count);
         }
      }

      cont= dir.GetFirst(&name, dirspec, wxDIR_DIRS);
      while ( cont ){
         wxString subdirPath = dirPath + wxFILE_SEP_PATH + name;
         count += RecursivelyEnumerate(
                     subdirPath, filePathArray, wxEmptyString,
                     bFiles, bDirs,
                     progress_count, count + progress_bias,
                     progress);
         cont = dir.GetNext(&name);
      }
   }

   if (bDirs) {
      filePathArray.Add(dirPath);
      count++;
   }

   return count;
}

static int RecursivelyEnumerateWithProgress(wxString dirPath,
                                             wxArrayString& filePathArray, // output: all files in dirPath tree
                                             wxString dirspec,
                                             bool bFiles, bool bDirs,
                                             int progress_count,
                                             const wxChar* message)
{
   Maybe<ProgressDialog> progress{};

   if (message)
      progress.create( _("Progress"), message );

   int count = RecursivelyEnumerate(
                  dirPath, filePathArray, dirspec,
                  bFiles, bDirs,
                  progress_count, 0,
                  progress.get());

   return count;
}

static int RecursivelyCountSubdirs(wxString dirPath)
{
   bool bContinue;
   int nCount = 0;
   wxDir dir(dirPath);
   if (dir.IsOpened() && dir.HasSubDirs())
   {
      wxString name;
      bContinue = dir.GetFirst(&name, wxEmptyString, wxDIR_DIRS);
      while (bContinue)
      {
         nCount++;
         wxString subdirPath = dirPath + wxFILE_SEP_PATH + name;
         nCount += RecursivelyCountSubdirs(subdirPath);
         bContinue = dir.GetNext(&name);
      }
   }
   return nCount;
}

static int RecursivelyRemoveEmptyDirs(wxString dirPath,
                                       int nDirCount = 0,
                                       ProgressDialog* pProgress = NULL)
{
   bool bContinue;
   wxDir dir(dirPath);
   int nCount = 0;
   if (dir.IsOpened())
   {
      if (dir.HasSubDirs())
      {
         wxString name;
         bContinue = dir.GetFirst(&name, wxEmptyString, wxDIR_DIRS);
         while (bContinue)
         {
            wxString subdirPath = dirPath + wxFILE_SEP_PATH + name;
            nCount += RecursivelyRemoveEmptyDirs(subdirPath, nDirCount, pProgress);
            bContinue = dir.GetNext(&name);
         }
      }
      // Have to recheck dir.HasSubDirs() again, in case they all were deleted in recursive calls.
      if (!dir.HasSubDirs() && !dir.HasFiles() && (dirPath.Right(5) != wxT("_data")))
      {
         // No subdirs or files. It's empty so DELETE it.
         // Vaughan, 2010-07-07:
         // Note that, per http://src.chromium.org/svn/trunk/src/base/file_util_win.cc, among others,
         // "Some versions of Windows return ERROR_FILE_NOT_FOUND (0x2) when deleting
         // an empty directory..." Supposedly fixed in Vista and up.
         // I discovered this on WinXP. I tried several other Windows SDK functions (e.g., _rmdir
         // and RemoveDirectory), and they all give same results.
         // I noticed dirs get deleted in RecursivelyRemove, maybe because it doesn't
         // consider whether the path is a directory or a file and wxRemoveFile()'s it first.
         // Tried it here on WinXP, but no joy. Leave the code in case it works on other Win systems.
         #ifdef __WXMSW__
            ::wxRemoveFile(dirPath);
         #endif
         ::wxRmdir(dirPath);
      }
      nCount++; // Count dirPath in progress.
      if (pProgress)
         pProgress->Update(nCount, nDirCount);
   }
   return nCount;
}

static void RecursivelyRemove(wxArrayString& filePathArray, int count,
                              bool bFiles, bool bDirs,
                              const wxChar* message = NULL)
{
   Maybe<ProgressDialog> progress{};

   if (message)
      progress.create( _("Progress"), message );

   for (int i = 0; i < count; i++) {
      const wxChar *file = filePathArray[i].c_str();
      if (bFiles)
         ::wxRemoveFile(file);
      if (bDirs)
         ::wxRmdir(file); // See note above about wxRmdir sometimes incorrectly failing on Windows.
      if (progress)
         progress->Update(i, count);
   }
}


//
// DirManager
//

// Static class variables
wxString DirManager::globaltemp;
int DirManager::numDirManagers = 0;
bool DirManager::dontDeleteTempFiles = false;


DirManager::DirManager()
{
   wxLogDebug(wxT("DirManager: Created new instance."));

   mLastBlockFileDestructionCount = BlockFile::gBlockFileDestructionCount;

   // Seed the random number generator.
   // this need not be strictly uniform or random, but it should give
   // unclustered numbers
   srand(time(NULL));

   // Set up local temp subdir
   // Previously, Audacity just named project temp directories "project0",
   // "project1" and so on. But with the advent of recovery code, we need a
   // unique name even after a crash. So we create a random project index
   // and make sure it is not used already. This will not pose any performance
   // penalties as long as the number of open Audacity projects is much
   // lower than RAND_MAX.
   do {
      mytemp = globaltemp + wxFILE_SEP_PATH +
               wxString::Format(wxT("project%d"), rand());
   } while (wxDirExists(mytemp));

   numDirManagers++;

   projPath = wxT("");
   projName = wxT("");

   mLoadingTarget = NULL;
   mLoadingTargetIdx = 0;
   mMaxSamples = ~size_t(0);

   // toplevel pool hash is fully populated to begin
   {
      // We can bypass the accessor function while initializing
      auto &balanceInfo = mBalanceInfo;
      auto &dirTopPool = balanceInfo.dirTopPool;
      for(int i = 0; i < 256; ++i)
         dirTopPool[i] = 0;
   }

   // Make sure there is plenty of space for temp files
   wxLongLong freeSpace = 0;
   if (wxGetDiskSpace(globaltemp, NULL, &freeSpace)) {
      if (freeSpace < wxLongLong(wxLL(100 * 1048576))) {
         ShowWarningDialog(NULL, wxT("DiskSpaceWarning"),
                           _("There is very little free disk space left on this volume.\nPlease select another temporary directory in Preferences."));
      }
   }
}

DirManager::~DirManager()
{
   numDirManagers--;
   if (numDirManagers == 0) {
      CleanTempDir();
      //::wxRmdir(temp);
   }
}


// static
void DirManager::CleanTempDir()
{
   if (dontDeleteTempFiles)
      return; // do nothing

   wxArrayString filePathArray;

   // Subtract 1 because we don't want to DELETE the global temp directory,
   // which this will find and list last.
   int count =
      RecursivelyEnumerate(globaltemp, filePathArray, wxT("project*"), true, true) - 1;
   if (count == 0)
      return;

   RecursivelyRemove(filePathArray, count, true, true, _("Cleaning up temporary files"));
}

bool DirManager::SetProject(wxString& newProjPath, wxString& newProjName, const bool bCreate)
{
   wxString oldPath = this->projPath;
   wxString oldName = this->projName;
   wxString oldFull = projFull;
   wxString oldLoc = projFull;
   if (oldLoc == wxT(""))
      oldLoc = mytemp;

   if (newProjPath == wxT(""))
      newProjPath = ::wxGetCwd();

   this->projPath = newProjPath;
   this->projName = newProjName;
   if (newProjPath.Last() == wxFILE_SEP_PATH)
      this->projFull = newProjPath + newProjName;
   else
      this->projFull = newProjPath + wxFILE_SEP_PATH + newProjName;

   wxString cleanupLoc1=oldLoc;
   wxString cleanupLoc2=projFull;

   if (bCreate) {
      if (!wxDirExists(projFull))
         if (!wxMkdir(projFull))
            return false;

      #ifdef __UNIX__
      chmod(OSFILENAME(projFull), 0775);
      #endif

      #ifdef __WXMAC__
      chmod(OSFILENAME(projFull), 0775);
      #endif

   } else {
      if (!wxDirExists(projFull))
         return false;
   }

   /* Move all files into this NEW directory.  Files which are
      "locked" get copied instead of moved.  (This happens when
      we perform a Save As - the files which belonged to the last
      saved version of the old project must not be moved,
      otherwise the old project would not be safe.) */

   int trueTotal = 0;

   {
      /*i18n-hint: This title appears on a dialog that indicates the progress in doing something.*/
      ProgressDialog progress(_("Progress"),
         _("Saving project data files"));

      int total = mBlockFileHash.size();

      BlockHash::iterator iter = mBlockFileHash.begin();
      bool success = true;
      int count = 0;
      while ((iter != mBlockFileHash.end()) && success)
      {
         BlockFilePtr b = iter->second.lock();
         if (b) {
            if (b->IsLocked())
               success = CopyToNewProjectDirectory( &*b );
            else{
               success = MoveToNewProjectDirectory( &*b );
            }

            progress.Update(count, total);
            count++;
         }
         ++iter;
      }

      // in case there are any nulls
      trueTotal = count;

      if (!success) {
         // If the move failed, we try to move/copy as many files
         // back as possible so that no damage was done.  (No sense
         // in checking for errors this time around - there's nothing
         // that could be done about it.)
         // Likely causes: directory was not writeable, disk was full

         projFull = oldLoc;

         BlockHash::iterator iter = mBlockFileHash.begin();
         while (iter != mBlockFileHash.end())
         {
            BlockFilePtr b = iter->second.lock();
            if (b) {
               MoveToNewProjectDirectory(&*b);

               if (count >= 0)
                  progress.Update(count, total);
               count--;
            }
            ++iter;
         }

         this->projFull = oldFull;
         this->projPath = oldPath;
         this->projName = oldName;

         return false;
      }
   }

   // Some subtlety; SetProject is used both to move a temp project
   // into a permanent home as well as just set up path variables when
   // loading a project; in this latter case, the movement code does
   // nothing because SetProject is called before there are any
   // blockfiles.  Cleanup code trigger is the same
   if (trueTotal > 0) {
      // Clean up after ourselves; look for empty directories in the old
      // and NEW project directories.  The easiest way to do this is to
      // recurse depth-first and rmdir every directory seen in old and
      // NEW; rmdir will fail on non-empty dirs.

      wxArrayString dirlist;
      const int count = RecursivelyEnumerate(cleanupLoc1, dirlist, wxEmptyString, false, true);

      //This destroys the empty dirs of the OD block files, which are yet to come.
      //Dont know if this will make the project dirty, but I doubt it. (mchinen)
      //      count += RecursivelyEnumerate(cleanupLoc2, dirlist, wxEmptyString, false, true);

      if (count > 0)
         RecursivelyRemove(dirlist, count, false, true, _("Cleaning up cache directories"));
   }
   return true;
}

wxString DirManager::GetProjectDataDir()
{
   return projFull;
}

wxString DirManager::GetProjectName()
{
   return projName;
}

wxLongLong DirManager::GetFreeDiskSpace()
{
   wxLongLong freeSpace = -1;
   wxFileName path;

   path.SetPath(projPath.IsEmpty() ? mytemp : projPath);

   // Use the parent directory if the project directory hasn't yet been created
   if (!path.DirExists())
   {
      path.RemoveLastDir();
   }

   if (!wxGetDiskSpace(path.GetFullPath(), NULL, &freeSpace))
   {
      freeSpace = -1;
   }

   return freeSpace;
}

wxString DirManager::GetDataFilesDir() const
{
   return projFull != wxT("")? projFull: mytemp;
}

void DirManager::SetLocalTempDir(const wxString &path)
{
   mytemp = path;
}

wxFileNameWrapper DirManager::MakeBlockFilePath(const wxString &value) {

   wxFileNameWrapper dir;
   dir.AssignDir(GetDataFilesDir());

   if(value.GetChar(0)==wxT('d')){
      // this file is located in a subdirectory tree
      int location=value.Find(wxT('b'));
      wxString subdir=value.Mid(0,location);
      dir.AppendDir(subdir);

      if(!dir.DirExists())
         dir.Mkdir();
   }

   if(value.GetChar(0)==wxT('e')){
      // this file is located in a NEW style two-deep subdirectory tree
      wxString topdir=value.Mid(0,3);
      wxString middir=wxT("d");
      middir.Append(value.Mid(3,2));

      dir.AppendDir(topdir);
      dir.AppendDir(middir);

      if(!dir.DirExists() && !dir.Mkdir(0777,wxPATH_MKDIR_FULL))
      { // need braces to avoid compiler warning about ambiguous else, see the macro
         wxLogSysError(_("mkdir in DirManager::MakeBlockFilePath failed."));
      }
   }
   return dir;
}

bool DirManager::AssignFile(wxFileNameWrapper &fileName,
                            const wxString &value,
                            bool diskcheck)
{
   wxFileNameWrapper dir{ MakeBlockFilePath(value) };

   if(diskcheck){
      // verify that there's no possible collision on disk.  If there
      // is, log the problem and return FALSE so that MakeBlockFileName
      // can try again

      wxDir checkit(dir.GetFullPath());
      if(!checkit.IsOpened()) return FALSE;

      // this code is only valid if 'value' has no extention; that
      // means, effectively, AssignFile may be called with 'diskcheck'
      // set to true only if called from MakeFileBlockName().

      wxString filespec;
      filespec.Printf(wxT("%s.*"),value.c_str());
      if(checkit.HasFiles(filespec)){
         // collision with on-disk state!
         wxString collision;
         checkit.GetFirst(&collision,filespec);

         wxLogWarning(_("Audacity found an orphan block file: %s. \nPlease consider saving and reloading the project to perform a complete project check."),
                      collision.c_str());

         return FALSE;
      }
   }
   fileName.Assign(dir.GetFullPath(),value);
   return fileName.IsOk();
}

static inline unsigned int hexchar_to_int(unsigned int x)
{
   if(x<48U)return 0;
   if(x<58U)return x-48U;
   if(x<65U)return 10U;
   if(x<71U)return x-55U;
   if(x<97U)return 10U;
   if(x<103U)return x-87U;
   return 15U;
}

int DirManager::BalanceMidAdd(int topnum, int midkey)
{
   // enter the midlevel directory if it doesn't exist

   auto &balanceInfo = GetBalanceInfo();
   auto &dirMidPool = balanceInfo.dirMidPool;
   auto &dirMidFull = balanceInfo.dirMidFull;
   auto &dirTopPool = balanceInfo.dirTopPool;
   auto &dirTopFull = balanceInfo.dirTopFull;

   if(dirMidPool.find(midkey) == dirMidPool.end() &&
         dirMidFull.find(midkey) == dirMidFull.end()){
      dirMidPool[midkey]=0;

      // increment toplevel directory fill
      dirTopPool[topnum]++;
      if(dirTopPool[topnum]>=256){
         // this toplevel is now full; move it to the full hash
         dirTopPool.erase(topnum);
         dirTopFull[topnum]=256;
      }
      return 1;
   }
   return 0;
}

void DirManager::BalanceFileAdd(int midkey)
{
   auto &balanceInfo = GetBalanceInfo();
   auto &dirMidPool = balanceInfo.dirMidPool;
   auto &dirMidFull = balanceInfo.dirMidFull;

   // increment the midlevel directory usage information
   if(dirMidPool.find(midkey) != dirMidPool.end()){
      dirMidPool[midkey]++;
      if(dirMidPool[midkey]>=256){
         // this middir is now full; move it to the full hash
         dirMidPool.erase(midkey);
         dirMidFull[midkey]=256;
      }
   }else{
      // this case only triggers in absurdly large projects; we still
      // need to track directory fill even if we're over 256/256/256
      dirMidPool[midkey]++;
   }
}

void DirManager::BalanceInfoAdd(const wxString &file)
{
   const wxChar *s=file.c_str();
   if(s[0]==wxT('e')){
      // this is one of the modern two-deep managed files
      // convert filename to keys
      unsigned int topnum = (hexchar_to_int(s[1]) << 4) |
         hexchar_to_int(s[2]);
      unsigned int midnum = (hexchar_to_int(s[3]) << 4) |
         hexchar_to_int(s[4]);
      unsigned int midkey=topnum<<8|midnum;

      BalanceMidAdd(topnum,midkey);
      BalanceFileAdd(midkey);
   }
}

auto DirManager::GetBalanceInfo() -> BalanceInfo &
{
   // Before returning the map,
   // see whether any block files have disappeared,
   // and if so update

   auto count = BlockFile::gBlockFileDestructionCount;
   if ( mLastBlockFileDestructionCount != count ) {
      auto it = mBlockFileHash.begin(), end = mBlockFileHash.end();
      while (it != end)
      {
         BlockFilePtr ptr { it->second.lock() };
         if (!ptr) {
            auto name = it->first;
            mBlockFileHash.erase( it++ );
            BalanceInfoDel( name );
         }
         else
            ++it;
      }
   }

   mLastBlockFileDestructionCount = count;

   return mBalanceInfo;
}

// Note that this will try to clean up directories out from under even
// locked blockfiles; this is actually harmless as the rmdir will fail
// on non-empty directories.
void DirManager::BalanceInfoDel(const wxString &file)
{
   // do not use GetBalanceInfo(),
   // rather this function will be called from there.
   auto &balanceInfo = mBalanceInfo;
   auto &dirMidPool = balanceInfo.dirMidPool;
   auto &dirMidFull = balanceInfo.dirMidFull;
   auto &dirTopPool = balanceInfo.dirTopPool;
   auto &dirTopFull = balanceInfo.dirTopFull;

   const wxChar *s=file.c_str();
   if(s[0]==wxT('e')){
      // this is one of the modern two-deep managed files

      unsigned int topnum = (hexchar_to_int(s[1]) << 4) |
         hexchar_to_int(s[2]);
      unsigned int midnum = (hexchar_to_int(s[3]) << 4) |
         hexchar_to_int(s[4]);
      unsigned int midkey=topnum<<8|midnum;

      // look for midkey in the mid pool
      if(dirMidFull.find(midkey) != dirMidFull.end()){
         // in the full pool

         if(--dirMidFull[midkey]<256){
            // move out of full into available
            dirMidPool[midkey]=dirMidFull[midkey];
            dirMidFull.erase(midkey);
         }
      }else{
         if(--dirMidPool[midkey]<1){
            // erasing the key here is OK; we have provision to add it
            // back if its needed (unlike the dirTopPool hash)
            dirMidPool.erase(midkey);

            // DELETE the actual directory
            wxString dir=(projFull != wxT("")? projFull: mytemp);
            dir += wxFILE_SEP_PATH;
            dir += file.Mid(0,3);
            dir += wxFILE_SEP_PATH;
            dir += wxT("d");
            dir += file.Mid(3,2);
            wxFileName::Rmdir(dir);

            // also need to remove from toplevel
            if(dirTopFull.find(topnum) != dirTopFull.end()){
               // in the full pool
               if(--dirTopFull[topnum]<256){
                  // move out of full into available
                  dirTopPool[topnum]=dirTopFull[topnum];
                  dirTopFull.erase(topnum);
               }
            }else{
               if(--dirTopPool[topnum]<1){
                  // do *not* erase the hash entry from dirTopPool
                  // *do* DELETE the actual directory
                  wxString dir=(projFull != wxT("")? projFull: mytemp);
                  dir += wxFILE_SEP_PATH;
                  dir += file.Mid(0,3);
                  wxFileName::Rmdir(dir);
               }
            }
         }
      }
   }
}

// only determines appropriate filename and subdir balance; does not
// perform maintainence
wxFileNameWrapper DirManager::MakeBlockFileName()
{
   auto &balanceInfo = GetBalanceInfo();
   auto &dirMidPool = balanceInfo.dirMidPool;
   auto &dirTopPool = balanceInfo.dirTopPool;
   auto &dirTopFull = balanceInfo.dirTopFull;

   wxFileNameWrapper ret;
   wxString baseFileName;

   unsigned int filenum,midnum,topnum,midkey;

   while(1){

      /* blockfiles are divided up into heirarchical directories.
         Each toplevel directory is represented by "e" + two unique
         hexadecimal digits, for a total possible number of 256
         toplevels.  Each toplevel contains up to 256 subdirs named
         "d" + two hex digits.  Each subdir contains 'a number' of
         files.  */

      filenum=0;
      midnum=0;
      topnum=0;

      // first action: if there is no available two-level directory in
      // the available pool, try to make one

      if(dirMidPool.empty()){

         // is there a toplevel directory with space for a NEW subdir?

         if(!dirTopPool.empty()){

            // there's still a toplevel with room for a subdir

            DirHash::iterator iter = dirTopPool.begin();
            int newcount           = 0;
            topnum                 = iter->first;


            // search for unused midlevels; linear search adequate
            // add 32 NEW topnum/midnum dirs full of  prospective filenames to midpool
            for(midnum=0;midnum<256;midnum++){
               midkey=(topnum<<8)+midnum;
               if(BalanceMidAdd(topnum,midkey)){
                  newcount++;
                  if(newcount>=32)break;
               }
            }

            if(dirMidPool.empty()){
               // all the midlevels in this toplevel are in use yet the
               // toplevel claims some are free; this implies multiple
               // internal logic faults, but simply giving up and going
               // into an infinite loop isn't acceptible.  Just in case,
               // for some reason, we get here, dynamite this toplevel so
               // we don't just fail.

               // this is 'wrong', but the best we can do given that
               // something else is also wrong.  It will contain the
               // problem so we can keep going without worry.
               dirTopPool.erase(topnum);
               dirTopFull[topnum]=256;
            }
            continue;
         }
      }

      if(dirMidPool.empty()){
         // still empty, thus an absurdly large project; all dirs are
         // full to 256/256/256; keep working, but fall back to 'big
         // filenames' and randomized placement

         filenum = rand();
         midnum  = (int)(256.*rand()/(RAND_MAX+1.));
         topnum  = (int)(256.*rand()/(RAND_MAX+1.));
         midkey=(topnum<<8)+midnum;


      }else{

         DirHash::iterator iter = dirMidPool.begin();
         midkey                 = iter->first;

         // split the retrieved 16 bit directory key into two 8 bit numbers
         topnum = midkey >> 8;
         midnum = midkey & 0xff;
         filenum = (int)(4096.*rand()/(RAND_MAX+1.));

      }

      baseFileName.Printf(wxT("e%02x%02x%03x"),topnum,midnum,filenum);

      if (!ContainsBlockFile(baseFileName)) {
         // not in the hash, good.
         if (!this->AssignFile(ret, baseFileName, true))
         {
            // this indicates an on-disk collision, likely due to an
            // orphan blockfile.  We should try again, but first
            // alert the balancing info there's a phantom file here;
            // if the directory is nearly full of orphans we neither
            // want performance to suffer nor potentially get into an
            // infinite loop if all possible filenames are taken by
            // orphans (unlikely but possible)
            BalanceFileAdd(midkey);

         }else break;
      }
   }
   // FIXME: Might we get here without midkey having been set?
   //    Seemed like a possible problem in these changes in .aup directory hierarchy.
   BalanceFileAdd(midkey);
   return ret;
}

BlockFilePtr DirManager::NewSimpleBlockFile(
                                 samplePtr sampleData, size_t sampleLen,
                                 sampleFormat format,
                                 bool allowDeferredWrite)
{
   wxFileNameWrapper filePath{ MakeBlockFileName() };
   const wxString fileName{ filePath.GetName() };

   auto newBlockFile = make_blockfile<SimpleBlockFile>
      (std::move(filePath), sampleData, sampleLen, format, allowDeferredWrite);

   mBlockFileHash[fileName] = newBlockFile;

   return newBlockFile;
}

BlockFilePtr DirManager::NewAliasBlockFile(
                                 const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel)
{
   wxFileNameWrapper filePath{ MakeBlockFileName() };
   const wxString fileName = filePath.GetName();

   auto newBlockFile = make_blockfile<PCMAliasBlockFile>
      (std::move(filePath), wxFileNameWrapper{aliasedFile},
       aliasStart, aliasLen, aliasChannel);

   mBlockFileHash[fileName]=newBlockFile;
   aliasList.Add(aliasedFile);

   return newBlockFile;
}

BlockFilePtr DirManager::NewODAliasBlockFile(
                                 const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel)
{
   wxFileNameWrapper filePath{ MakeBlockFileName() };
   const wxString fileName{ filePath.GetName() };

   auto newBlockFile = make_blockfile<ODPCMAliasBlockFile>
      (std::move(filePath), wxFileNameWrapper{aliasedFile},
       aliasStart, aliasLen, aliasChannel);

   mBlockFileHash[fileName]=newBlockFile;
   aliasList.Add(aliasedFile);

   return newBlockFile;
}

BlockFilePtr DirManager::NewODDecodeBlockFile(
                                 const wxString &aliasedFile, sampleCount aliasStart,
                                 size_t aliasLen, int aliasChannel, int decodeType)
{
   wxFileNameWrapper filePath{ MakeBlockFileName() };
   const wxString fileName{ filePath.GetName() };

   auto newBlockFile = make_blockfile<ODDecodeBlockFile>
      (std::move(filePath), wxFileNameWrapper{ aliasedFile },
       aliasStart, aliasLen, aliasChannel, decodeType);

   mBlockFileHash[fileName]=newBlockFile;
   aliasList.Add(aliasedFile); //OD TODO: check to see if we need to remove this when done decoding.
                               //I don't immediately see a place where aliased files remove when a file is closed.

   return newBlockFile;
}

bool DirManager::ContainsBlockFile(const BlockFile *b) const
{
   if (!b)
      return false;
   auto result = b->GetFileName();
   BlockHash::const_iterator it = mBlockFileHash.find(result.name.GetName());
   if (it == mBlockFileHash.end())
      return false;
   BlockFilePtr ptr = it->second.lock();
   return ptr && (b == &*ptr);
}

bool DirManager::ContainsBlockFile(const wxString &filepath) const
{
   // check what the hash returns in case the blockfile is from a different project
   BlockHash::const_iterator it = mBlockFileHash.find(filepath);
   return it != mBlockFileHash.end() &&
      BlockFilePtr{ it->second.lock() };
}

// Adds one to the reference count of the block file,
// UNLESS it is "locked", then it makes a NEW copy of
// the BlockFile.
BlockFilePtr DirManager::CopyBlockFile(const BlockFilePtr &b)
{
   auto result = b->GetFileName();
   const auto &fn = result.name;

   if (!b->IsLocked()) {
      //mchinen:July 13 2009 - not sure about this, but it needs to be added to the hash to be able to save if not locked.
      //note that this shouldn't hurt mBlockFileHash's that already contain the filename, since it should just overwrite.
      //but it's something to watch out for.
      //
      // LLL: Except for silent block files which have uninitialized filename.
      if (fn.IsOk())
         mBlockFileHash[fn.GetName()]=b;
      return b;
   }

   // Copy the blockfile
   BlockFilePtr b2;
   if (!fn.IsOk())
      // Block files with uninitialized filename (i.e. SilentBlockFile)
      // just need an in-memory copy.
      b2 = b->Copy(wxFileNameWrapper{});
   else
   {
      wxFileNameWrapper newFile{ MakeBlockFileName() };
      const wxString newName{newFile.GetName()};
      const wxString newPath{ newFile.GetFullPath() };

      // We assume that the NEW file should have the same extension
      // as the existing file
      newFile.SetExt(fn.GetExt());

      //some block files such as ODPCMAliasBlockFIle don't always have
      //a summary file, so we should check before we copy.
      if(b->IsSummaryAvailable())
      {
         if( !wxCopyFile(fn.GetFullPath(),
                  newFile.GetFullPath()) )
            return {};
      }

      // Done with fn
      result.mLocker.reset();

      b2 = b->Copy(std::move(newFile));

      if (b2 == NULL)
         return {};

      mBlockFileHash[newName]=b2;
      aliasList.Add(newPath);
   }

   return b2;
}

bool DirManager::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if( mLoadingTarget == NULL )
      return false;

   BlockFilePtr pBlockFile {};

   BlockFilePtr &target = mLoadingTarget->at(mLoadingTargetIdx).f;
   
   if (!wxStricmp(tag, wxT("silentblockfile"))) {
      // Silent blocks don't actually have a file associated, so
      // we don't need to worry about the hash table at all
      target = SilentBlockFile::BuildFromXML(*this, attrs);
      return true;
   }
   else if ( !wxStricmp(tag, wxT("simpleblockfile")) )
      pBlockFile = SimpleBlockFile::BuildFromXML(*this, attrs);
   else if( !wxStricmp(tag, wxT("pcmaliasblockfile")) )
      pBlockFile = PCMAliasBlockFile::BuildFromXML(*this, attrs);
   else if( !wxStricmp(tag, wxT("odpcmaliasblockfile")) )
   {
      pBlockFile = ODPCMAliasBlockFile::BuildFromXML(*this, attrs);
      //in the case of loading an OD file, we need to schedule the ODManager to begin OD computing of summary
      //However, because we don't have access to the track or even the Sequence from this call, we mark a flag
      //in the ODMan and check it later.
      ODManager::MarkLoadedODFlag();
   }
   else if( !wxStricmp(tag, wxT("oddecodeblockfile")) )
   {
      pBlockFile = ODDecodeBlockFile::BuildFromXML(*this, attrs);
      ODManager::MarkLoadedODFlag();
   }
   else if( !wxStricmp(tag, wxT("blockfile")) ||
            !wxStricmp(tag, wxT("legacyblockfile")) ) {
      // Support Audacity version 1.1.1 project files

      int i=0;
      bool alias = false;

      while(attrs[i]) {
         if (!wxStricmp(attrs[i], wxT("alias"))) {
            if (wxAtoi(attrs[i+1])==1)
               alias = true;
         }
         i++;
         if (attrs[i])
            i++;
      }

      if (alias)
         pBlockFile = LegacyAliasBlockFile::BuildFromXML(projFull, attrs);
      else
         pBlockFile = LegacyBlockFile::BuildFromXML(projFull, attrs,
                                                         mLoadingBlockLen,
                                                         mLoadingFormat);
   }
   else
      return false;

   if (!pBlockFile)
      // BuildFromXML failed, or we didn't find a valid blockfile tag.
      return false;

   // Check the length here so we don't have to do it in each BuildFromXML method.
   if ((mMaxSamples != ~size_t(0)) && // is initialized
         (pBlockFile->GetLength() > mMaxSamples))
   {
      // See http://bugzilla.audacityteam.org/show_bug.cgi?id=451#c13.
      // Lock pBlockFile so that the ~BlockFile() will not DELETE the file on disk.
      pBlockFile->Lock();
      return false;
   }
   else
      target = pBlockFile;

   //
   // If the block we loaded is already in the hash table, then the
   // object we just loaded is a duplicate, so we DELETE it and
   // return a reference to the existing object instead.
   //

   wxString name = target->GetFileName().name.GetName();
   auto &wRetrieved = mBlockFileHash[name];
   BlockFilePtr retrieved = wRetrieved.lock();
   if (retrieved) {
      // Lock it in order to DELETE it safely, i.e. without having
      // it DELETE the file, too...
      target->Lock();

      target = retrieved;
      return true;
   }

   // This is a NEW object
   wRetrieved = target;
   // MakeBlockFileName wasn't used so we must add the directory
   // balancing information
   BalanceInfoAdd(name);

   return true;
}

bool DirManager::MoveOrCopyToNewProjectDirectory(BlockFile *f, bool copy)
{
   auto result = f->GetFileName();
   const auto &oldFileNameRef = result.name;

   // Check that this BlockFile corresponds to a file on disk
   //ANSWER-ME: Is this checking only for SilentBlockFiles, in which case
   //    (!oldFileName.IsOk()) is a more correct check?
   if (oldFileNameRef.GetName().IsEmpty()) {
      return true;
   }

   wxFileNameWrapper newFileName;
   if (!this->AssignFile(newFileName, oldFileNameRef.GetFullName(), false))
      return false;

   if (newFileName != oldFileNameRef) {
      //check to see that summary exists before we copy.
      bool summaryExisted = f->IsSummaryAvailable();
      auto oldPath = oldFileNameRef.GetFullPath();
      auto newPath = newFileName.GetFullPath();
      if (summaryExisted) {
         auto success = copy
         ? wxCopyFile(oldPath, newPath)
         : wxRenameFile(oldPath, newPath);
         if (!success)
            return false;
      }

      if (!summaryExisted && (f->IsSummaryAvailable() || f->IsSummaryBeingComputed())) {

         // We will need to remember the old file name, so copy it
         wxFileName oldFileName{ oldFileNameRef };

         // Now we can free any lock (and should, if as the comment below says, we need
         // the other threads to progress)
         result.mLocker.reset();

         f->SetFileName(std::move(newFileName));

         //there is a small chance that the summary has begun to be computed on a different thread with the
         //original filename.  we need to catch this case by waiting for it to finish and then copy.

         //block to make sure OD files don't get written while we are changing file names.
         //(It is important that OD files set this lock while computing their summary files.)
         while(f->IsSummaryBeingComputed() && !f->IsSummaryAvailable())
            ::wxMilliSleep(50);

         //check to make sure the oldfile exists.
         //if it doesn't, we can assume it was written to the NEW name, which is fine.
         if (oldFileName.FileExists())
         {
            bool ok = wxCopyFile(oldPath, newPath);
            if(ok && !copy)
               wxRemoveFile(oldPath);
            else if (!ok)
               return false;
         }
      }
      else {
         // Can free this now, and must, because of nonrecursive mutexes
         result.mLocker.reset();
         f->SetFileName(std::move(newFileName));
      }
   }

   return true;
}

bool DirManager::MoveToNewProjectDirectory(BlockFile *f)
{
   return MoveOrCopyToNewProjectDirectory(f, false);
}

bool DirManager::CopyToNewProjectDirectory(BlockFile *f)
{
   return MoveOrCopyToNewProjectDirectory(f, true);
}

bool DirManager::EnsureSafeFilename(const wxFileName &fName)
{
   // Quick check: If it's not even in our alias list,
   // then the file name is A-OK.

   const wxString fullPath{fName.GetFullPath()};
   if (aliasList.Index(fullPath) == wxNOT_FOUND)
      return true;

   /* i18n-hint: 'old' is part of a filename used when a file is renamed. */
   // Figure out what the NEW name for the existing file would be.
   /* i18n-hint: e.g. Try to go from "mysong.wav" to "mysong-old1.wav". */
   // Keep trying until we find a filename that doesn't exist.

   wxFileNameWrapper renamedFileName{ fName };
   int i = 0;
   do {
      i++;
      /* i18n-hint: This is the pattern for filenames that are created
       * when a file needs to be backed up to a different name.  For
       * example, mysong would become mysong-old1, mysong-old2, etc. */
      renamedFileName.SetName(wxString::Format(_("%s-old%d"), fName.GetName().c_str(), i));
   } while (renamedFileName.FileExists());

   // Test creating a file by that name to make sure it will
   // be possible to do the rename

   const wxString renamedFullPath{ renamedFileName.GetFullPath() };
   wxFile testFile(renamedFullPath, wxFile::write);
   if (!testFile.IsOpened()) {
      { // need braces to avoid compiler warning about ambiguous else, see the macro
         wxLogSysError(_("Unable to open/create test file."),
               renamedFullPath.c_str());
      }
      return false;
   }

   // Close the file prior to renaming.
   testFile.Close();

   if (!wxRemoveFile(renamedFullPath)) {
      /* i18n-hint: %s is the name of a file.*/
      { // need braces to avoid compiler warning about ambiguous else, see the macro
         wxLogSysError(_("Unable to remove '%s'."),
            renamedFullPath.c_str());
      }
      return false;
   }

   wxPrintf(_("Renamed file: %s\n"), renamedFullPath.c_str());

   // Go through our block files and see if any indeed point to
   // the file we're concerned about.  If so, point the block file
   // to the renamed file and when we're done, perform the rename.

   bool needToRename = false;
   wxBusyCursor busy;
   BlockHash::iterator iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      BlockFilePtr b = iter->second.lock();
      if (b) {
         // don't worry, we don't rely on this cast unless IsAlias is true
         auto ab = static_cast< AliasBlockFile * > ( &*b );

         // don't worry, we don't rely on this cast unless ISDataAvailable is false
         // which means that it still needs to access the file.
         auto db = static_cast< ODDecodeBlockFile * > ( &*b );

         if (b->IsAlias() && ab->GetAliasedFileName() == fName) {
            needToRename = true;

            //ODBlocks access the aliased file on another thread, so we need to pause them before this continues.
            ab->LockRead();
         }
         //now for encoded OD blocks  (e.g. flac)
         else if (!b->IsDataAvailable() && db->GetEncodedAudioFilename() == fName) {
            needToRename = true;

            //ODBlocks access the aliased file on another thread, so we need to pause them before this continues.
            db->LockRead();
         }
      }
      ++iter;
   }

   if (needToRename) {
      if (!wxRenameFile(fullPath,
                        renamedFullPath))
      {
         // ACK!!! The renaming was unsuccessful!!!
         // (This shouldn't happen, since we tried creating a
         // file of this name and then deleted it just a
         // second earlier.)  But we'll handle this scenario
         // just in case!!!

         // Put things back where they were
         BlockHash::iterator iter = mBlockFileHash.begin();
         while (iter != mBlockFileHash.end())
         {
            BlockFilePtr b = iter->second.lock();
            if (b) {
               auto ab = static_cast< AliasBlockFile * > ( &*b );
               auto db = static_cast< ODDecodeBlockFile * > ( &*b );

               if (b->IsAlias() && (ab->GetAliasedFileName() == fName))
                  ab->UnlockRead();
               if (!b->IsDataAvailable() && (db->GetEncodedAudioFilename() == fName))
                  db->UnlockRead();
            }
            ++iter;
         }

         // Print error message and cancel the export
         wxLogSysError(_("Unable to rename '%s' to '%s'."),
                       fullPath.c_str(),
                       renamedFullPath.c_str());
         return false;
      }
      else
      {
         //point the aliases to the NEW filename.
         BlockHash::iterator iter = mBlockFileHash.begin();
         while (iter != mBlockFileHash.end())
         {
            BlockFilePtr b = iter->second.lock();
            if (b) {
               auto ab = static_cast< AliasBlockFile * > ( &*b );
               auto db = static_cast< ODDecodeBlockFile * > ( &*b );

               if (b->IsAlias() && ab->GetAliasedFileName() == fName)
               {
                  ab->ChangeAliasedFileName(wxFileNameWrapper{ renamedFileName });
                  ab->UnlockRead();
                  wxPrintf(_("Changed block %s to new alias name\n"),
                           b->GetFileName().name.GetFullName().c_str());

               }
               else if (!b->IsDataAvailable() && db->GetEncodedAudioFilename() == fName) {
                  db->ChangeAudioFile(wxFileNameWrapper{ renamedFileName });
                  db->UnlockRead();
               }
            }
            ++iter;
         }

      }

      aliasList.Remove(fullPath);
      aliasList.Add(renamedFullPath);
   }

   // Success!!!  Either we successfully renamed the file,
   // or we didn't need to!
   return true;
}

// Check the BlockFiles against the disk state.
// Missing Blockfile data can be regenerated if possible or replaced with silence.
// Orphan blockfiles can be deleted.
// Note that even BlockFiles not referenced by the current savefile (but locked
// by history) will be reflected in the mBlockFileHash, and that's a
// good thing; this is one reason why we use the hash and not the most
// recent savefile.
int DirManager::ProjectFSCK(const bool bForceError, const bool bAutoRecoverMode)
{
   // In earlier versions of this method, enumerations of errors were
   // all done in sequence, then the user was prompted for each type of error.
   // The enumerations are now interleaved with prompting, because, for example,
   // user choosing to replace missing aliased block files with silence
   // needs to put in SilentBlockFiles and DELETE the corresponding auf files,
   // so those would then not be cumulated in missingAUFHash.
   // We still do the FindX methods outside the conditionals,
   // so the log always shows all found errors.

   int action; // choice of action for each type of error
   int nResult = 0;

   if (bForceError && !bAutoRecoverMode)
   {
      // TODO: Replace with more user friendly error message?
      /* i18n-hint: The audacity project file is XML and has 'tags' in it,
         rather like html tags <something>some stuff</something>.
         This error message is about the tags that hold the sequence information.
         The error message is confusing to users in English, and could just say
         "Found problems with <sequence> when checking project file." */
      wxString msg = _("Project check read faulty Sequence tags.");
      const wxChar *buttons[] =
         {_("Close project immediately with no changes"),
            _("Continue with repairs noted in log, and check for more errors. This will save the project in its current state, unless you \"Close project immediately\" on further error alerts."),
            NULL};
      wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
      action = ShowMultiDialog(msg, _("Warning - Problems Reading Sequence Tags"), buttons);
      if (action == 0)
         nResult = FSCKstatus_CLOSE_REQ;
      else
         nResult = FSCKstatus_CHANGED | FSCKstatus_SAVE_AUP;
   }

   wxArrayString filePathArray; // *all* files in the project directory/subdirectories
   wxString dirPath = (projFull != wxT("") ? projFull : mytemp);
   RecursivelyEnumerateWithProgress(
      dirPath,
      filePathArray,          // output: all files in project directory tree
      wxEmptyString,
      true, false,
      mBlockFileHash.size(),  // rough guess of how many BlockFiles will be found/processed, for progress
      _("Inspecting project file data"));

   //
   // MISSING ALIASED AUDIO FILES
   //
   wxGetApp().SetMissingAliasedFileWarningShouldShow(false);
   BlockHash missingAliasedFileAUFHash;   // (.auf) AliasBlockFiles whose aliased files are missing
   BlockHash missingAliasedFilePathHash;  // full paths of missing aliased files
   this->FindMissingAliasedFiles(missingAliasedFileAUFHash, missingAliasedFilePathHash);

   if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAliasedFileAUFHash.empty())
   {
      // In auto-recover mode, we always create silent blocks, and do not ask user.
      // This makes sure the project is complete next time we open it.
      if (bAutoRecoverMode)
         action = 2;
      else
      {
         wxString msgA =
_("Project check of \"%s\" folder \
\ndetected %lld missing external audio file(s) \
\n('aliased files'). There is no way for Audacity \
\nto recover these files automatically. \
\n\nIf you choose the first or second option below, \
\nyou can try to find and restore the missing files \
\nto their previous location. \
\n\nNote that for the second option, the waveform \
\nmay not show silence. \
\n\nIf you choose the third option, this will save the \
\nproject in its current state, unless you \"Close \
\nproject immediately\" on further error alerts.");
         wxString msg;
         msg.Printf(msgA, this->projName.c_str(), (long long) missingAliasedFilePathHash.size());
         const wxChar *buttons[] =
            {_("Close project immediately with no changes"),
               _("Treat missing audio as silence (this session only)"),
               _("Replace missing audio with silence (permanent immediately)."),
               NULL};
         wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
         action = ShowMultiDialog(msg, _("Warning - Missing Aliased File(s)"), buttons);
      }

      if (action == 0)
         nResult = FSCKstatus_CLOSE_REQ;
      else
      {
         // LL:  A progress dialog should probably be used here
         BlockHash::iterator iter = missingAliasedFileAUFHash.begin();
         while (iter != missingAliasedFileAUFHash.end())
         {
            // This type cast is safe. We checked that it's an alias block file earlier.
            BlockFilePtr b = iter->second.lock();
            wxASSERT(b);
            if (b) {
               auto ab = static_cast< AliasBlockFile * > ( &*b );
               if (action == 1)
                  // Silence error logging for this block in this session.
                  ab->SilenceAliasLog();
               else if (action == 2)
               {
                  // silence the blockfiles by yanking the filename
                  // This is done, eventually, in PCMAliasBlockFile::ReadData()
                  // and ODPCMAliasBlockFile::ReadData, in the stack of b->Recover().
                  // There, if the mAliasedFileName is bad, it zeroes the data.
                  wxFileNameWrapper dummy;
                  dummy.Clear();
                  ab->ChangeAliasedFileName(std::move(dummy));
                  ab->Recover();
                  nResult = FSCKstatus_CHANGED | FSCKstatus_SAVE_AUP;
               }
            }
            ++iter;
         }
         if ((action == 2) && bAutoRecoverMode)
            wxLogWarning(_("   Project check replaced missing aliased file(s) with silence."));
      }
   }

   //
   // MISSING ALIAS (.AUF) AliasBlockFiles
   //
   // Alias summary regeneration must happen after checking missing aliased files.
   //
   BlockHash missingAUFHash;              // missing (.auf) AliasBlockFiles
   this->FindMissingAUFs(missingAUFHash);
   if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAUFHash.empty())
   {
      // In auto-recover mode, we just recreate the alias files, and do not ask user.
      // This makes sure the project is complete next time we open it.
      if (bAutoRecoverMode)
         action = 0;
      else
      {
         wxString msgA =
_("Project check of \"%s\" folder \
\ndetected %lld missing alias (.auf) blockfile(s). \
\nAudacity can fully regenerate these files \
\nfrom the current audio in the project.");
         wxString msg;
         msg.Printf(msgA, this->projName.c_str(), (long long) missingAUFHash.size());
         const wxChar *buttons[] = {_("Regenerate alias summary files (safe and recommended)"),
                                    _("Fill in silence for missing display data (this session only)"),
                                    _("Close project immediately with no further changes"),
                                    NULL};
         wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
         action = ShowMultiDialog(msg, _("Warning - Missing Alias Summary File(s)"), buttons);
      }

      if (action == 2)
         nResult = FSCKstatus_CLOSE_REQ;
      else
      {
         // LL:  A progress dialog should probably be used here
         BlockHash::iterator iter = missingAUFHash.begin();
         while (iter != missingAUFHash.end())
         {
            BlockFilePtr b = iter->second.lock();
            wxASSERT(b);
            if (b) {
               if(action==0){
                  //regenerate from data
                  b->Recover();
                  nResult |= FSCKstatus_CHANGED;
               }else if (action==1){
                  // Silence error logging for this block in this session.
                  b->SilenceLog();
               }
            }
            ++iter;
         }
         if ((action == 0) && bAutoRecoverMode)
            wxLogWarning(_("   Project check regenerated missing alias summary file(s)."));
      }
   }

   //
   // MISSING (.AU) SimpleBlockFiles
   //
   BlockHash missingAUHash;               // missing data (.au) blockfiles
   this->FindMissingAUs(missingAUHash);
   if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAUHash.empty())
   {
      // In auto-recover mode, we just always create silent blocks.
      // This makes sure the project is complete next time we open it.
      if (bAutoRecoverMode)
         action = 2;
      else
      {
         wxString msgA =
_("Project check of \"%s\" folder \
\ndetected %lld missing audio data (.au) blockfile(s), \
\nprobably due to a bug, system crash, or accidental \
\ndeletion. There is no way for Audacity to recover \
\nthese missing files automatically. \
\n\nIf you choose the first or second option below, \
\nyou can try to find and restore the missing files \
\nto their previous location. \
\n\nNote that for the second option, the waveform \
\nmay not show silence.");
         wxString msg;
         msg.Printf(msgA, this->projName.c_str(), (long long) missingAUHash.size());
         const wxChar *buttons[] =
            {_("Close project immediately with no further changes"),
               _("Treat missing audio as silence (this session only)"),
               _("Replace missing audio with silence (permanent immediately)"),
               NULL};
         wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
         action = ShowMultiDialog(msg, _("Warning - Missing Audio Data Block File(s)"), buttons);
      }

      if (action == 0)
         nResult = FSCKstatus_CLOSE_REQ;
      else
      {
         // LL:  A progress dialog should probably be used here
         BlockHash::iterator iter = missingAUHash.begin();
         while (iter != missingAUHash.end())
         {
            BlockFilePtr b = iter->second.lock();
            wxASSERT(b);
            if (b) {
               if (action == 2)
               {
                  //regenerate with zeroes
                  b->Recover();
                  nResult = FSCKstatus_CHANGED;
               }
               else if (action == 1)
                  b->SilenceLog();
            }
            ++iter;
         }
         if ((action == 2) && bAutoRecoverMode)
            wxLogWarning(_("   Project check replaced missing audio data block file(s) with silence."));
      }
   }

   //
   // ORPHAN BLOCKFILES (.au and .auf files that are not in the project.)
   //
   wxArrayString orphanFilePathArray;     // orphan .au and .auf files
   this->FindOrphanBlockFiles(filePathArray, orphanFilePathArray);

   if ((nResult != FSCKstatus_CLOSE_REQ) && !orphanFilePathArray.IsEmpty())
   {
      // In auto-recover mode, leave orphan blockfiles alone.
      // They will be deleted when project is saved the first time.
      if (bAutoRecoverMode)
      {
         wxLogWarning(_("   Project check ignored orphan block file(s). They will be deleted when project is saved."));
         action = 1;
      }
      else
      {
         wxString msgA =
_("Project check of \"%s\" folder \
\nfound %d orphan block file(s). These files are \
\nunused by this project, but might belong to \
other projects. \
\nThey are doing no harm and are small.");
         wxString msg;
         msg.Printf(msgA, this->projName.c_str(), (int)orphanFilePathArray.GetCount());

         const wxChar *buttons[] =
            {_("Continue without deleting; ignore the extra files this session"),
            _("Close project immediately with no further changes"),
            _("Delete orphan files (permanent immediately)"),
            NULL};
         wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
         action = ShowMultiDialog(msg, _("Warning - Orphan Block File(s)"), buttons);
      }

      if (action == 1)
         nResult = FSCKstatus_CLOSE_REQ;
      // Nothing is done if (action == 0).
      else if (action == 2)
      {
         // FSCKstatus_CHANGED was bogus here.
         // The files are deleted, so "Undo Project Repair" could not do anything.
         // Plus they affect none of the valid tracks, so incorrect to mark them changed,
         // and no need for refresh.
         //    nResult |= FSCKstatus_CHANGED;
         for (size_t i = 0; i < orphanFilePathArray.GetCount(); i++)
            wxRemoveFile(orphanFilePathArray[i]);
      }
   }

   if ((nResult != FSCKstatus_CLOSE_REQ) && !ODManager::HasLoadedODFlag())
   {
      // Remove any empty directories.
      ProgressDialog pProgress
         (_("Progress"),
         _("Cleaning up unused directories in project data"));
      // nDirCount is for updating pProgress. +1 because we may DELETE dirPath.
      int nDirCount = RecursivelyCountSubdirs(dirPath) + 1;
      RecursivelyRemoveEmptyDirs(dirPath, nDirCount, &pProgress);
   }

   // Summarize and flush the log.
   if (bForceError ||
         !missingAliasedFileAUFHash.empty() ||
         !missingAUFHash.empty() ||
         !missingAUHash.empty() ||
         !orphanFilePathArray.IsEmpty())
   {
      wxLogWarning(_("Project check found file inconsistencies inspecting the loaded project data."));
      wxLog::FlushActive(); // Flush is modal and will clear the log (both desired).

      // In auto-recover mode, we didn't do any ShowMultiDialog calls above, so put up an alert.
      if (bAutoRecoverMode)
         ::wxMessageBox(
            _("Project check found file inconsistencies during automatic recovery.\n\nSelect 'Show Log...' in the Help menu to see details."),
            _("Warning: Problems in Automatic Recovery"),
            wxOK  | wxICON_EXCLAMATION);
   }

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   return nResult;
}

void DirManager::FindMissingAliasedFiles(
      BlockHash& missingAliasedFileAUFHash,     // output: (.auf) AliasBlockFiles whose aliased files are missing
      BlockHash& missingAliasedFilePathHash)    // output: full paths of missing aliased files
{
   BlockHash::iterator iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      wxString key = iter->first;   // file name and extension
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->IsAlias())
         {
            const wxFileName &aliasedFileName =
            static_cast< AliasBlockFile* > ( &*b )->GetAliasedFileName();
            wxString aliasedFileFullPath = aliasedFileName.GetFullPath();
            // wxEmptyString can happen if user already chose to "replace... with silence".
            if ((aliasedFileFullPath != wxEmptyString) &&
                !aliasedFileName.FileExists())
            {
               missingAliasedFileAUFHash[key] = b;
               if (missingAliasedFilePathHash.find(aliasedFileFullPath) ==
                   missingAliasedFilePathHash.end()) // Add it only once.
                  // Not actually using the block here, just the path,
                  // so set the block to NULL to create the entry.
                  missingAliasedFilePathHash[aliasedFileFullPath] = {};
            }
         }
      }
      ++iter;
   }

   iter = missingAliasedFilePathHash.begin();
   while (iter != missingAliasedFilePathHash.end())
   {
      wxLogWarning(_("Missing aliased audio file: '%s'"), iter->first.c_str());
      ++iter;
   }
}

void DirManager::FindMissingAUFs(
      BlockHash& missingAUFHash)                // output: missing (.auf) AliasBlockFiles
{
   BlockHash::iterator iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      const wxString &key = iter->first;
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->IsAlias() && b->IsSummaryAvailable())
         {
            /* don't look in hash; that might find files the user moved
             that the Blockfile abstraction can't find itself */
            wxFileNameWrapper fileName{ MakeBlockFilePath(key) };
            fileName.SetName(key);
            fileName.SetExt(wxT("auf"));
            if (!fileName.FileExists())
            {
               missingAUFHash[key] = b;
               wxLogWarning(_("Missing alias (.auf) block file: '%s'"),
                            fileName.GetFullPath().c_str());
            }
         }
      }
      ++iter;
   }
}

void DirManager::FindMissingAUs(
      BlockHash& missingAUHash)                 // missing data (.au) blockfiles
{
   BlockHash::iterator iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      const wxString &key = iter->first;
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (!b->IsAlias())
         {
            wxFileNameWrapper fileName{ MakeBlockFilePath(key) };
            fileName.SetName(key);
            fileName.SetExt(wxT("au"));
            if (!fileName.FileExists())
            {
               missingAUHash[key] = b;
               wxLogWarning(_("Missing data block file: '%s'"),
                            fileName.GetFullPath().c_str());
            }
         }
      }
      ++iter;
   }
}

// Find .au and .auf files that are not in the project.
void DirManager::FindOrphanBlockFiles(
      const wxArrayString& filePathArray,       // input: all files in project directory
      wxArrayString& orphanFilePathArray)       // output: orphan files
{
   DirManager *clipboardDM = NULL;

   for (size_t i = 0; i < filePathArray.GetCount(); i++)
   {
      const wxFileName &fullname = filePathArray[i];
      wxString basename = fullname.GetName();
      const wxString ext{fullname.GetExt()};
      if ((mBlockFileHash.find(basename) == mBlockFileHash.end()) && // is orphan
            // Consider only Audacity data files.
            // Specifically, ignore <branding> JPG and <import> OGG ("Save Compressed Copy").
            (ext.IsSameAs(wxT("au")) ||
               ext.IsSameAs(wxT("auf"))))
      {
         if (!clipboardDM) {
            TrackList *clipTracks = AudacityProject::GetClipboardTracks();

            if (clipTracks) {
               TrackListIterator clipIter(clipTracks);
               Track *track = clipIter.First();
               if (track)
                  clipboardDM = track->GetDirManager().get();
            }
         }

         // Ignore it if it exists in the clipboard (from a previously closed project)
         if (!(clipboardDM && clipboardDM->ContainsBlockFile(basename)))
            orphanFilePathArray.Add(fullname.GetFullPath());
      }
   }
   for (size_t i = 0; i < orphanFilePathArray.GetCount(); i++)
      wxLogWarning(_("Orphan block file: '%s'"), orphanFilePathArray[i].c_str());
}


void DirManager::RemoveOrphanBlockfiles()
{
   wxArrayString filePathArray; // *all* files in the project directory/subdirectories
   wxString dirPath = (projFull != wxT("") ? projFull : mytemp);
   RecursivelyEnumerateWithProgress(
      dirPath,
      filePathArray,          // output: all files in project directory tree
      wxEmptyString,
      true, false,
      mBlockFileHash.size(),  // rough guess of how many BlockFiles will be found/processed, for progress
      _("Inspecting project file data"));

   wxArrayString orphanFilePathArray;
   this->FindOrphanBlockFiles(
            filePathArray,          // input: all files in project directory tree
            orphanFilePathArray);   // output: orphan files

   // Remove all orphan blockfiles.
   for (size_t i = 0; i < orphanFilePathArray.GetCount(); i++)
      wxRemoveFile(orphanFilePathArray[i]);
}

void DirManager::FillBlockfilesCache()
{
#ifdef DEPRECATED_AUDIO_CACHE
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=545.
   bool cacheBlockFiles = false;
   gPrefs->Read(wxT("/Directories/CacheBlockFiles"), &cacheBlockFiles);

   if (!cacheBlockFiles)
      return; // user opted not to cache block files

   int lowMem = gPrefs->Read(wxT("/Directories/CacheLowMem"), 16l);
   if (lowMem < 16) {
      lowMem = 16;
   }
   lowMem <<= 20;

   BlockHash::iterator iter;
   int numNeed = 0;

   iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->GetNeedFillCache())
            numNeed++;
      }
      ++iter;
   }

   if (numNeed == 0)
      return;

   ProgressDialog progress(_("Caching audio"),
                           _("Caching audio into memory"));

   iter = mBlockFileHash.begin();
   int current = 0;
   while (iter != mBlockFileHash.end())
   {
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->GetNeedFillCache() && (GetFreeMemory() > lowMem)) {
            b->FillCache();
         }

         if (!progress.Update(current, numNeed))
            break; // user cancelled progress dialog, stop caching
         current++;
      }
      ++iter;
   }
#endif // DEPRECATED_AUDIO_CACHE
}

void DirManager::WriteCacheToDisk()
{
   BlockHash::iterator iter;
   int numNeed = 0;

   iter = mBlockFileHash.begin();
   while (iter != mBlockFileHash.end())
   {
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->GetNeedWriteCacheToDisk())
            numNeed++;
      }
      ++iter;
   }

   if (numNeed == 0)
      return;

   ProgressDialog progress(_("Saving recorded audio"),
                           _("Saving recorded audio to disk"));

   iter = mBlockFileHash.begin();
   int current = 0;
   while (iter != mBlockFileHash.end())
   {
      BlockFilePtr b = iter->second.lock();
      if (b) {
         if (b->GetNeedWriteCacheToDisk())
         {
            b->WriteCacheToDisk();
            progress.Update(current, numNeed);
         }
         current++;
      }
      ++iter;
   }
}

