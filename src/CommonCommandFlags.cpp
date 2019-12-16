/**********************************************************************

Audacity: A Digital Audio Editor

CommonCommandFlags.cpp

Paul Licameli split from Menus.cpp

**********************************************************************/

#include "Audacity.h"
#include "CommonCommandFlags.h"

#include "Experimental.h"

#include <wx/frame.h>

#include "AudioIO.h"
#include "LabelTrack.h"
#include "Menus.h"
#include "NoteTrack.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectFileIO.h"
#include "ProjectHistory.h"
#include "ProjectSettings.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "commands/CommandManagerWindowClasses.h"

/*

This file registers functions implementing many of the tests for enabling of
menu items.  Sequence of a few of them has minor significance, but for most
there is little reason to keep them in one file.  Flags used only by one
other file might instead be defined only where used.

They are collected here because Menus.cpp is too low level to have the
dependencies implied by the include directives above -- it would make dependency
cycles.

*/

// Really means, some track is selected, that isn't a time track
bool TracksSelectedPred( const AudacityProject &project )
{
   auto range = TrackList::Get( project ).Selected()
     - []( const Track *pTrack ){
        return track_cast<const TimeTrack*>( pTrack ); };
   return !range.empty();
};

// This predicate includes time tracks too.
bool AnyTracksSelectedPred( const AudacityProject &project )
{
   auto range = TrackList::Get( project ).Selected();
   return !range.empty();
};

bool AudioIOBusyPred( const AudacityProject &project )
{
   return AudioIOBase::Get()->IsAudioTokenActive(
      ProjectAudioIO::Get( project ).GetAudioIOToken());
};

bool TimeSelectedPred( const AudacityProject &project )
{
   // This is equivalent to check if there is a valid selection,
   // so it's used for Zoom to Selection too
   return !ViewInfo::Get( project ).selectedRegion.isPoint();
};

const CommandFlagOptions cutCopyOptions{
// In reporting the issue with cut or copy, we don't tell the user they could also select some text in a label.
   []( const TranslatableString &Name ) {
      // PRL:  These strings have hard-coded mention of a certain shortcut key,
      // thus assuming the default shortcuts.  That is questionable.
      TranslatableString format;
#ifdef EXPERIMENTAL_DA
      // i18n-hint: %s will be replaced by the name of an action, such as Normalize, Cut, Fade.
      format = XO("You must first select some audio for '%s' to act on.\n\nCtrl + A selects all audio.");
#else
#ifdef __WXMAC__
      // i18n-hint: %s will be replaced by the name of an action, such as Normalize, Cut, Fade.
      format = XO("Select the audio for %s to use (for example, Cmd + A to Select All) then try again."
      // No need to explain what a help button is for.
      // "\n\nClick the Help button to learn more about selection methods."
      );

#else
      // i18n-hint: %s will be replaced by the name of an action, such as Normalize, Cut, Fade.
      format = XO("Select the audio for %s to use (for example, Ctrl + A to Select All) then try again."
      // No need to explain what a help button is for.
      // "\n\nClick the Help button to learn more about selection methods."
      );
#endif
      return format.Format( Name );
#endif
   },
   "Selecting_Audio_-_the_basics",
   XO("No Audio Selected")
};

// Noise Reduction has a custom error message, when nothing selected.
const CommandFlagOptions noiseReductionOptions{
   []( const TranslatableString &Name ) {
      // i18n-hint: %s will be replaced by the name of an effect, usually 'Noise Reduction'.
      auto format = XO("Select the audio for %s to use.\n\n1. Select audio that represents noise and use %s to get your 'noise profile'.\n\n2. When you have got your noise profile, select the audio you want to change\nand use %s to change that audio.");
      return format.Format( Name, Name, Name );
   },
   "Noise_Reduction",
   XO("No Audio Selected")
};


const ReservedCommandFlag
   // The sequence of these definitions has a minor significance in determining
   // which user error message has precedence if more than one might apply, so
   // they should be kept in this sequence in one .cpp file if it is important
   // to preserve that behavior.  If they are dispersed to more than one file,
   // then the precedence will be unspecified.
   // The ordering of the flags that only disable the default message is not
   // significant.
   AudioIONotBusyFlag{
      [](const AudacityProject &project ){
         return !AudioIOBusyPred( project );
      },
      CommandFlagOptions{ []( const TranslatableString& ) { return
         // This reason will not be shown, because options that require it will be greyed out.
         XO("You can only do this when playing and recording are\nstopped. (Pausing is not sufficient.)");
      } ,"FAQ:Errors:Audio Must Be Stopped"}
      .QuickTest()
      .Priority( 1 )
   }, //lll
   StereoRequiredFlag{
      [](const AudacityProject &project){
         // True iff at least one stereo track is selected, i.e., at least
         // one right channel is selected.
         // TODO: more-than-two-channels
         auto range = TrackList::Get( project ).Selected<const WaveTrack>()
            - &Track::IsLeader;
         return !range.empty();
      },
      { []( const TranslatableString& ) { return
         // This reason will not be shown, because the stereo-to-mono is greyed out if not allowed.
         XO("You must first select some stereo audio to perform this\naction. (You cannot use this with mono.)");
      } ,"Audacity_Selection"}
   },  //lda
   NoiseReductionTimeSelectedFlag{
      TimeSelectedPred,
      noiseReductionOptions
   },
   TimeSelectedFlag{
      TimeSelectedPred,
      cutCopyOptions
   },
   WaveTracksSelectedFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Selected<const WaveTrack>().empty();
      },
      { []( const TranslatableString& ) { return
         XO("You must first select some audio to perform this action.\n(Selecting other kinds of track won't work.)");
      } ,"Audacity_Selection"}
   },
   TracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any().empty();
      },
      CommandFlagOptions{}.DisableDefaultMessage()
   },
   TracksSelectedFlag{
      TracksSelectedPred, // exclude TimeTracks
      { []( const TranslatableString &Name ){ return
         // i18n-hint: %s will be replaced by the name of an action, such as "Remove Tracks".
         XO("\"%s\" requires one or more tracks to be selected.").Format( Name );
      },"Audacity_Selection" }
   },
   AnyTracksSelectedFlag{
      AnyTracksSelectedPred, // Allow TimeTracks
      { []( const TranslatableString &Name ){ return
         // i18n-hint: %s will be replaced by the name of an action, such as "Remove Tracks".
         XO("\"%s\" requires one or more tracks to be selected.").Format( Name );
      },"Audacity_Selection" }
   },
   TrackPanelHasFocus{
      [](const AudacityProject &project){
         for (auto w = wxWindow::FindFocus(); w; w = w->GetParent()) {
            if (dynamic_cast<const NonKeystrokeInterceptingWindow*>(w))
               return true;
         }
         return false;
      },
      CommandFlagOptions{}.DisableDefaultMessage()
   };  //lll

const ReservedCommandFlag
   AudioIOBusyFlag{
      AudioIOBusyPred,
      CommandFlagOptions{}.QuickTest()
   }, //lll
   CaptureNotBusyFlag{
      [](const AudacityProject &){
         auto gAudioIO = AudioIO::Get();
         return !(
            gAudioIO->IsBusy() &&
            gAudioIO->GetNumCaptureChannels() > 0
         );
      }
   };

const ReservedCommandFlag
   LabelTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const LabelTrack>().empty();
      }
   },
   UnsavedChangesFlag{
      [](const AudacityProject &project){
         auto &undoManager = UndoManager::Get( project );
         return
            undoManager.UnsavedChanges()
         ||
            !ProjectFileIO::Get( project ).IsProjectSaved()
         ;
      }
   },
   HasLastEffectFlag{
      [](const AudacityProject &project){
         return !MenuManager::Get( project ).mLastEffect.empty();
      }
   },
   UndoAvailableFlag{
      [](const AudacityProject &project){
         return ProjectHistory::Get( project ).UndoAvailable();
      }
   },
   RedoAvailableFlag{
      [](const AudacityProject &project){
         return ProjectHistory::Get( project ).RedoAvailable();
      }
   },
   ZoomInAvailableFlag{
      [](const AudacityProject &project){
         return
            ViewInfo::Get( project ).ZoomInAvailable()
         &&
            !TrackList::Get( project ).Any().empty()
         ;
      }
   },
   ZoomOutAvailableFlag{
      [](const AudacityProject &project){
         return
            ViewInfo::Get( project ).ZoomOutAvailable()
         &&
            !TrackList::Get( project ).Any().empty()
         ;
      }
   },
   LabelsSelectedFlag{
      [](const AudacityProject &project){
         // At least one label track selected, having at least one label
         // completely within the time selection.
         const auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;
         const auto &test = [&]( const LabelTrack *pTrack ){
            const auto &labels = pTrack->GetLabels();
            return std::any_of( labels.begin(), labels.end(),
               [&](const LabelStruct &label){
                  return
                     label.getT0() >= selectedRegion.t0()
                  &&
                     label.getT1() <= selectedRegion.t1()
                  ;
               }
            );
         };
         auto range = TrackList::Get( project ).Selected<const LabelTrack>()
            + test;
         return !range.empty();
      }
   },
   PlayRegionLockedFlag{
      [](const AudacityProject &project){
         return ViewInfo::Get(project).playRegion.Locked();
      }
   },  //msmeyer
   PlayRegionNotLockedFlag{
      [](const AudacityProject &project){
         const auto &playRegion = ViewInfo::Get(project).playRegion;
         return !playRegion.Locked() && !playRegion.Empty();
      }
   },  //msmeyer
   WaveTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const WaveTrack>().empty();
      }
   },
#ifdef USE_MIDI
   NoteTracksExistFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Any<const NoteTrack>().empty();
      }
   },  //gsw
   NoteTracksSelectedFlag{
      [](const AudacityProject &project){
         return !TrackList::Get( project ).Selected<const NoteTrack>().empty();
      }
   },  //gsw
#endif
   IsNotSyncLockedFlag{
      [](const AudacityProject &project){
         return !ProjectSettings::Get( project ).IsSyncLocked();
      }
   },  //awd
   IsSyncLockedFlag{
      [](const AudacityProject &project){
         return ProjectSettings::Get( project ).IsSyncLocked();
      }
   },  //awd
   NotMinimizedFlag{
      [](const AudacityProject &project){
         const wxWindow *focus = FindProjectFrame( &project );
         if (focus) {
            while (focus && focus->GetParent())
               focus = focus->GetParent();
         }
         return (focus &&
            !static_cast<const wxTopLevelWindow*>(focus)->IsIconized()
         );
      },
      CommandFlagOptions{}.QuickTest()
   }, // prl
   PausedFlag{
      [](const AudacityProject&){
         return AudioIOBase::Get()->IsPaused();
      },
      CommandFlagOptions{}.QuickTest()
   },
   PlayableTracksExistFlag{
      [](const AudacityProject &project){
         auto &tracks = TrackList::Get( project );
         return
#ifdef EXPERIMENTAL_MIDI_OUT
            !tracks.Any<const NoteTrack>().empty()
         ||
#endif
            !tracks.Any<const WaveTrack>().empty()
         ;
      }
   },
   AudioTracksSelectedFlag{
      [](const AudacityProject &project){
         auto &tracks = TrackList::Get( project );
         return
#ifdef USE_MIDI
            !tracks.Selected<const NoteTrack>().empty()
            // even if not EXPERIMENTAL_MIDI_OUT
         ||
#endif
            !tracks.Selected<const WaveTrack>().empty()
         ;
      }
   },
   NoAutoSelect{
     [](const AudacityProject &){ return true; }
   } // jkc
;
