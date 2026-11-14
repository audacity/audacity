/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFSCK.cpp

  A function that performs consistency checks on the tree of block files

  Paul Licameli split this out of DirManager.cpp

**********************************************************************/

#include "ProjectFSCK.h"

#include <wx/log.h>

#include "BlockFile.h"
#include "DirManager.h"
#include "widgets/AudacityMessageBox.h"
#include "Internat.h"
#include "MemoryX.h"
#include "widgets/MultiDialog.h"
#include "widgets/ProgressDialog.h"

// Check the BlockFiles against the disk state.
// Missing Blockfile data can be regenerated if possible or replaced with silence.
// Orphan blockfiles can be deleted.
// Note that even BlockFiles not referenced by the current savefile (but locked
// by history) will be reflected in the mBlockFileHash, and that's a
// good thing; this is one reason why we use the hash and not the most
// recent savefile.
int ProjectFSCK(
    DirManager& dm, const bool bForceError, const bool bAutoRecoverMode)
{
#pragma message( "====================================================================")
#pragma message( "Don\'t forget to redo ProjectFSCK")
#pragma message( "====================================================================")
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

    if (bForceError && !bAutoRecoverMode) {
        // TODO: Replace with more user friendly error message?
        /* i18n-hint: The audacity project file is XML and has 'tags' in it,
           rather like html tags <something>some stuff</something>.
           This error message is about the tags that hold the sequence information.
           The error message is confusing to users in English, and could just say
           "Found problems with <sequence> when checking project file." */
        auto msg = XO("Project check read faulty Sequence tags.");
        const TranslatableStrings buttons{
            XO("Close project immediately with no changes"),
            XO(
                "Continue with repairs noted in log, and check for more errors. This will save the project in its current state, unless you \"Close project immediately\" on further error alerts.")
        };
        wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
        action = ShowMultiDialog(msg,
                                 XO("Warning - Problems Reading Sequence Tags"),
                                 buttons, "");
        if (action == 0) {
            nResult = FSCKstatus_CLOSE_REQ;
        } else {
            nResult = FSCKstatus_CHANGED | FSCKstatus_SAVE_AUP;
        }
    }
#if 0
    FilePaths filePathArray; // *all* files in the project directory/subdirectories
    auto dirPath = (dm.GetDataFilesDir());
    DirManager::RecursivelyEnumerateWithProgress(
        dirPath,
        filePathArray,        // output: all files in project directory tree
        wxEmptyString,        // All dirs
        wxEmptyString,        // All files
        true, false,
        dm.NumBlockFiles(), // rough guess of how many BlockFiles will be found/processed, for progress
        XO("Inspecting project file data"));

    //
    // MISSING ALIASED AUDIO FILES
    //
    MissingAliasFilesDialog::SetShouldShow(false);
    BlockHash missingAliasFilesAUFHash;  // (.auf) AliasBlockFiles whose aliased files are missing
    BlockHash missingAliasFilesPathHash; // full paths of missing aliased files
    dm.FindMissingAliasFiles(missingAliasFilesAUFHash, missingAliasFilesPathHash);

    if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAliasFilesAUFHash.empty()) {
        // In auto-recover mode, we always create silent blocks, and do not ask user.
        // This makes sure the project is complete next time we open it.
        if (bAutoRecoverMode) {
            action = 2;
        } else {
            auto msg
                =XO(
                      "Project check of \"%s\" folder \
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
\nproject immediately\" on further error alerts.")
                  .Format(
                      dm.GetProjectName(),
                      (long long)missingAliasFilesPathHash.size());
            const TranslatableStrings buttons{
                XO("Close project immediately with no changes"),
                XO("Treat missing audio as silence (this session only)"),
                XO("Replace missing audio with silence (permanent immediately)."),
            };
            wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
            action = ShowMultiDialog(msg,
                                     XO("Warning - Missing Aliased File(s)"),
                                     buttons,
                                     "");
        }

        if (action == 0) {
            nResult = FSCKstatus_CLOSE_REQ;
        } else {
            // LL:  A progress dialog should probably be used here
            BlockHash::iterator iter = missingAliasFilesAUFHash.begin();
            while (iter != missingAliasFilesAUFHash.end())
            {
                // This type cast is safe. We checked that it's an alias block file earlier.
                BlockFilePtr b = iter->second.lock();
                wxASSERT(b);
                if (b) {
                    auto ab = static_cast< AliasBlockFile* >(&*b);

                    if (action == 2) {
                        // silence the blockfiles by yanking the filename
                        // This is done, eventually, in PCMAliasBlockFile::ReadData(),
                        // in the stack of b->Recover().
                        // There, if the mAliasedFileName is bad, it zeroes the data.
                        wxFileNameWrapper dummy;
                        dummy.Clear();
                        ab->ChangeAliasedFileName(std::move(dummy));

                        // If recovery fails for one file, silence it,
                        // and don't try to recover other files but
                        // silence them too.  GuardedCall will cause an appropriate
                        // error message for the user.
                        GuardedCall(
                            [&] { ab->Recover(); },
                            [&] (AudacityException*) { action = 1; }
                            );

                        nResult = FSCKstatus_CHANGED | FSCKstatus_SAVE_AUP;
                    }

                    if (action == 1) {
                        // Silence error logging for this block in this session.
                        ab->SilenceAliasLog();
                    }
                }
                ++iter;
            }
            if ((action == 2) && bAutoRecoverMode) {
                wxLogWarning(wxT("   Project check replaced missing aliased file(s) with silence."));
            }
        }
    }

    //
    // MISSING ALIAS (.AUF) AliasBlockFiles
    //
    // Alias summary regeneration must happen after checking missing aliased files.
    //
    BlockHash missingAUFHash;             // missing (.auf) AliasBlockFiles
    dm.FindMissingAUFs(missingAUFHash);
    if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAUFHash.empty()) {
        // In auto-recover mode, we just recreate the alias files, and do not ask user.
        // This makes sure the project is complete next time we open it.
        if (bAutoRecoverMode) {
            action = 0;
        } else {
            auto msg
                =XO(
                      "Project check of \"%s\" folder \
\ndetected %lld missing alias (.auf) blockfile(s). \
\nAudacity can fully regenerate these files \
\nfrom the current audio in the project.")
                  .Format(
                      dm.GetProjectName(), (long long)missingAUFHash.size());
            const TranslatableStrings buttons{
                XO("Regenerate alias summary files (safe and recommended)"),
                XO("Fill in silence for missing display data (this session only)"),
                XO("Close project immediately with no further changes"),
            };
            wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
            action = ShowMultiDialog(msg,
                                     XO("Warning - Missing Alias Summary File(s)"),
                                     buttons,
                                     "");
        }

        if (action == 2) {
            nResult = FSCKstatus_CLOSE_REQ;
        } else {
            // LL:  A progress dialog should probably be used here
            BlockHash::iterator iter = missingAUFHash.begin();
            while (iter != missingAUFHash.end())
            {
                BlockFilePtr b = iter->second.lock();
                wxASSERT(b);
                if (b) {
                    if (action == 0) {
                        //regenerate from data
                        // If recovery fails for one file, silence it,
                        // and don't try to recover other files but
                        // silence them too.  GuardedCall will cause an appropriate
                        // error message for the user.
                        GuardedCall(
                            [&] {
                            b->Recover();
                            nResult |= FSCKstatus_CHANGED;
                        },
                            [&] (AudacityException*) { action = 1; }
                            );
                    }

                    if (action == 1) {
                        // Silence error logging for this block in this session.
                        b->SilenceLog();
                    }
                }
                ++iter;
            }
            if ((action == 0) && bAutoRecoverMode) {
                wxLogWarning(wxT("   Project check regenerated missing alias summary file(s)."));
            }
        }
    }

    //
    // MISSING (.AU) SimpleBlockFiles
    //
    BlockHash missingAUHash;              // missing data (.au) blockfiles
    dm.FindMissingAUs(missingAUHash);
    if ((nResult != FSCKstatus_CLOSE_REQ) && !missingAUHash.empty()) {
        // In auto-recover mode, we just always create silent blocks.
        // This makes sure the project is complete next time we open it.
        if (bAutoRecoverMode) {
            action = 2;
        } else {
            auto msg
                =XO(
                      "Project check of \"%s\" folder \
\ndetected %lld missing audio data (.au) blockfile(s), \
\nprobably due to a bug, system crash, or accidental \
\ndeletion. There is no way for Audacity to recover \
\nthese missing files automatically. \
\n\nIf you choose the first or second option below, \
\nyou can try to find and restore the missing files \
\nto their previous location. \
\n\nNote that for the second option, the waveform \
\nmay not show silence.")
                  .Format(
                      dm.GetProjectName(), (long long)missingAUHash.size());
            const TranslatableStrings buttons{
                XO("Close project immediately with no further changes"),
                XO("Treat missing audio as silence (this session only)"),
                XO("Replace missing audio with silence (permanent immediately)"),
            };
            wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
            action = ShowMultiDialog(msg,
                                     XO("Warning - Missing Audio Data Block File(s)"),
                                     buttons,
                                     "Warning_-_Missing_Audio_Data_Block_Files");
        }

        if (action == 0) {
            nResult = FSCKstatus_CLOSE_REQ;
        } else {
            // LL:  A progress dialog should probably be used here
            BlockHash::iterator iter = missingAUHash.begin();
            while (iter != missingAUHash.end())
            {
                BlockFilePtr b = iter->second.lock();
                wxASSERT(b);
                if (b) {
                    if (action == 2) {
                        //regenerate from data
                        // If recovery fails for one file, silence it,
                        // and don't try to recover other files but
                        // silence them too.  GuardedCall will cause an appropriate
                        // error message for the user.
                        GuardedCall(
                            [&] {
                            //regenerate with zeroes
                            b->Recover();
                            nResult |= FSCKstatus_CHANGED;
                        },
                            [&] (AudacityException*) { action = 1; }
                            );
                    }

                    if (action == 1) {
                        b->SilenceLog();
                    }
                }
                ++iter;
            }
            if ((action == 2) && bAutoRecoverMode) {
                wxLogWarning(wxT("   Project check replaced missing audio data block file(s) with silence."));
            }
        }
    }

    //
    // ORPHAN BLOCKFILES (.au and .auf files that are not in the project.)
    //
    FilePaths orphanFilePathArray;    // orphan .au and .auf files
    dm.FindOrphanBlockFiles(filePathArray, orphanFilePathArray);

    if ((nResult != FSCKstatus_CLOSE_REQ) && !orphanFilePathArray.empty()) {
        // In auto-recover mode, leave orphan blockfiles alone.
        // They will be deleted when project is saved the first time.
        if (bAutoRecoverMode) {
            wxLogWarning(wxT("   Project check ignored orphan block file(s). They will be deleted when project is saved."));
            action = 1;
        } else {
            auto msg
                =XO(
                      "Project check of \"%s\" folder \
\nfound %d orphan block file(s). These files are \
\nunused by this project, but might belong to \
other projects. \
\nThey are doing no harm and are small.")
                  .Format(dm.GetProjectName(), (int)orphanFilePathArray.size());

            const TranslatableStrings buttons{
                XO("Continue without deleting; ignore the extra files this session"),
                XO("Close project immediately with no further changes"),
                XO("Delete orphan files (permanent immediately)"),
            };
            wxLog::FlushActive(); // MultiDialog has "Show Log..." button, so make sure log is current.
            action = ShowMultiDialog(msg,
                                     XO("Warning - Orphan Block File(s)"),
                                     buttons,
                                     "Warning_-_Orphan_Block_Files"
                                     );
        }

        if (action == 1) {
            nResult = FSCKstatus_CLOSE_REQ;
        }
        // Nothing is done if (action == 0).
        else if (action == 2) {
            // FSCKstatus_CHANGED was bogus here.
            // The files are deleted, so "Undo Project Repair" could not do anything.
            // Plus they affect none of the valid tracks, so incorrect to mark them changed,
            // and no need for refresh.
            //    nResult |= FSCKstatus_CHANGED;
            for ( const auto& orphan : orphanFilePathArray ) {
                wxRemoveFile(orphan);
            }
        }
    }

    if (nResult != FSCKstatus_CLOSE_REQ) {
        // Remove any empty directories.
        ProgressDialog pProgress(
            XO("Progress"),
            XO("Cleaning up unused directories in project data"));
        // nDirCount is for updating pProgress. +1 because we may DELETE dirPath.
        int nDirCount = DirManager::RecursivelyCountSubdirs(dirPath) + 1;
        DirManager::RecursivelyRemoveEmptyDirs(dirPath, nDirCount, &pProgress);
    }

    // Summarize and flush the log.
    if (bForceError
        || !missingAliasFilesAUFHash.empty()
        || !missingAUFHash.empty()
        || !missingAUHash.empty()
        || !orphanFilePathArray.empty()) {
        wxLogWarning(wxT("Project check found file inconsistencies inspecting the loaded project data."));
        wxLog::FlushActive(); // Flush is modal and will clear the log (both desired).

        // In auto-recover mode, we didn't do any ShowMultiDialog calls above, so put up an alert.
        if (bAutoRecoverMode) {
            ::AudacityMessageBox(
                XO(
                    "Project check found file inconsistencies during automatic recovery.\n\nSelect 'Help > Diagnostics > Show Log...' to see details."),
                XO("Warning: Problems in Automatic Recovery"),
                wxOK | wxICON_EXCLAMATION);
        }
    }

    MissingAliasFilesDialog::SetShouldShow(true);
#endif
    return nResult;
}
