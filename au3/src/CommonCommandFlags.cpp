/**********************************************************************

Audacity: A Digital Audio Editor

CommonCommandFlags.cpp

Paul Licameli split from Menus.cpp

**********************************************************************/

#include "CommonCommandFlags.h"

#include <wx/frame.h>

#include "AudioIO.h"
#include "Clipboard.h"
#include "LabelTrack.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "ProjectWindows.h"
#include "SyncLock.h"
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

// Strong predicate excludes tracks that do not support basic editing.
bool EditableTracksSelectedPred(const AudacityProject& project)
{
    auto range = TrackList::Get(project).Selected()
                 - [] (const Track * pTrack) {
        return !pTrack->SupportsBasicEditing();
    };
    return !range.empty();
}

// Weaker predicate.
bool AnyTracksSelectedPred(const AudacityProject& project)
{
    auto range = TrackList::Get(project).Selected();
    return !range.empty();
}

bool AudioIOBusyPred(const AudacityProject& project)
{
    return AudioIOBase::Get()->IsAudioTokenActive(
        ProjectAudioIO::Get(project).GetAudioIOToken());
}

bool TimeSelectedPred(const AudacityProject& project)
{
    // This is equivalent to check if there is a valid selection,
    // so it's used for Zoom to Selection too
    return !ViewInfo::Get(project).selectedRegion.isPoint();
}

const CommandFlagOptions& cutCopyOptions()
{
    static CommandFlagOptions result{
// In reporting the issue with cut or copy, we don't tell the user they could also select some text in a label.
        []( const TranslatableString& Name ) {
            // PRL:  These strings have hard-coded mention of a certain shortcut key,
            // thus assuming the default shortcuts.  That is questionable.
            TranslatableString format;
#ifdef __WXMAC__
            // i18n-hint: %s will be replaced by the name of an action, such as Normalize, Cut, Fade.
            format = XO("Select the audio for %s to use (for example, Cmd + A to Select All) then try again."
                        );

#else
            // i18n-hint: %s will be replaced by the name of an action, such as Normalize, Cut, Fade.
            format = XO("Select the audio for %s to use (for example, Ctrl + A to Select All) then try again."
                        );
#endif
            return format.Format(Name);
        },
        "Selecting_Audio_-_the_basics",
        XO("No Audio Selected")
    };
    return result;
}

// Noise Reduction has a custom error message, when nothing selected.
const CommandFlagOptions noiseReductionOptions{
    []( const TranslatableString& Name ) {
        // i18n-hint: %s will be replaced by the name of an effect, usually 'Noise Reduction'.
        auto format = XO(
            "Select the audio for %s to use.\n\n1. Select audio that represents noise and use %s to get your 'noise profile'.\n\n2. When you have got your noise profile, select the audio you want to change\nand use %s to change that audio.");
        return format.Format(Name, Name, Name);
    },
    "Noise_Reduction",
    XO("No Audio Selected")
};

// The sequence of these definitions has a minor significance in determining
// which user error message has precedence if more than one might apply, so
// they should be kept in this sequence in one .cpp file if it is important
// to preserve that behavior.  If they are dispersed to more than one file,
// then the precedence will be unspecified.
// The ordering of the flags that only disable the default message is not
// significant.
const ReservedCommandFlag&
AudioIONotBusyFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project ){
            return !AudioIOBusyPred(project);
        },
        CommandFlagOptions{ []( const TranslatableString& ) {
                return
                    // This reason will not be shown, because options that require it will be greyed out.
                    XO("You can only do this when playing and recording are\nstopped. (Pausing is not sufficient.)");
            }, "FAQ:Errors:Audio Must Be Stopped" }
        .QuickTest()
        .Priority(1)
    };
    return flag;
}                   //lll

const ReservedCommandFlag&
StereoRequiredFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            // TODO: more-than-two-channels
            auto range
                =TrackList::Get(project).Selected<const WaveTrack>();
            return std::any_of(range.begin(), range.end(),
                               [](auto pTrack){ return pTrack->NChannels() > 1; });
        },
        { []( const TranslatableString& ) {
                return
                    // This reason will not be shown, because the stereo-to-mono is greyed out if not allowed.
                    XO("You must first select some stereo audio to perform this\naction. (You cannot use this with mono.)");
            }, "Audacity_Selection" }
    };
    return flag;
}                     //lda

const ReservedCommandFlag&
NoiseReductionTimeSelectedFlag()
{
    static ReservedCommandFlag flag{
        TimeSelectedPred,
        noiseReductionOptions
    };
    return flag;
}

const ReservedCommandFlag&
TimeSelectedFlag()
{
    static ReservedCommandFlag flag{
        TimeSelectedPred,
        cutCopyOptions()
    };
    return flag;
}

const ReservedCommandFlag&
WaveTracksSelectedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return
                !TrackList::Get(project).Selected<const WaveTrack>().empty();
        },
        { [](const TranslatableString&) {
                return
                    XO("You must first select some audio to perform this action.\n(Selecting other kinds of track won't work.)");
            }, "Audacity_Selection" }
    };
    return flag;
}

const ReservedCommandFlag&
TracksExistFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !TrackList::Get(project).Any().empty();
        },
        CommandFlagOptions{}.DisableDefaultMessage()
    };
    return flag;
}

const ReservedCommandFlag&
EditableTracksSelectedFlag()
{
    static ReservedCommandFlag flag{
        EditableTracksSelectedPred,
        { []( const TranslatableString& Name ){
                return
                    // i18n-hint: %s will be replaced by the name of an action, such as "Remove Tracks".
                    XO("\"%s\" requires one or more tracks to be selected.").Format(Name);
            }, "Audacity_Selection" }
    };
    return flag;
}

const ReservedCommandFlag&
AnyTracksSelectedFlag()
{
    static ReservedCommandFlag flag{
        AnyTracksSelectedPred,
        { []( const TranslatableString& Name ){
                return
                    // i18n-hint: %s will be replaced by the name of an action, such as "Remove Tracks".
                    XO("\"%s\" requires one or more tracks to be selected.").Format(Name);
            }, "Audacity_Selection" }
    };
    return flag;
}

const ReservedCommandFlag&
TrackPanelHasFocus()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            for (auto w = wxWindow::FindFocus(); w; w = w->GetParent()) {
                if (dynamic_cast<const NonKeystrokeInterceptingWindow*>(w)) {
                    return true;
                }
            }
            return false;
        },
        CommandFlagOptions{}.DisableDefaultMessage()
    };
    return flag;
}                      //lll

const ReservedCommandFlag&
AudioIOBusyFlag()
{
    static ReservedCommandFlag flag{
        AudioIOBusyPred,
        CommandFlagOptions{}.QuickTest()
    };
    return flag;
}                    //lll

const ReservedCommandFlag&
CaptureNotBusyFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject&){
            auto gAudioIO = AudioIO::Get();
            return !(
                gAudioIO->IsBusy()
                && gAudioIO->GetNumCaptureChannels() > 0
                );
        }
    };
    return flag;
}

const ReservedCommandFlag&
LabelTracksExistFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !TrackList::Get(project)
                   .Selected<const LabelTrack>().empty();
        }
    };
    return flag;
}

const ReservedCommandFlag&
UndoAvailableFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return ProjectHistory::Get(project).UndoAvailable();
        }
    };
    return flag;
}

const ReservedCommandFlag&
RedoAvailableFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return ProjectHistory::Get(project).RedoAvailable();
        }
    };
    return flag;
}

const ReservedCommandFlag&
ZoomInAvailableFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return
                ViewInfo::Get(project).ZoomInAvailable()
                &&
                !TrackList::Get(project).Any().empty()
            ;
        }
    };
    return flag;
}

const ReservedCommandFlag&
ZoomOutAvailableFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return
                ViewInfo::Get(project).ZoomOutAvailable()
                &&
                !TrackList::Get(project).Any().empty()
            ;
        }
    };
    return flag;
}

const ReservedCommandFlag&
WaveTracksExistFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !TrackList::Get(project).Any<const WaveTrack>().empty();
        }
    };
    return flag;
}

const ReservedCommandFlag&
IsNotSyncLockedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !SyncLockState::Get(project).IsSyncLocked();
        }
    };
    return flag;
}                     //awd

const ReservedCommandFlag&
IsSyncLockedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return SyncLockState::Get(project).IsSyncLocked();
        }
    };
    return flag;
}                     //awd

const ReservedCommandFlag&
NotMinimizedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            const wxWindow* focus = FindProjectFrame(&project);
            if (focus) {
                while (focus && focus->GetParent()) {
                    focus = focus->GetParent();
                }
            }
            return focus
                   && !static_cast<const wxTopLevelWindow*>(focus)->IsIconized()
            ;
        },
        CommandFlagOptions{}.QuickTest()
    };
    return flag;
}                    // prl

const ReservedCommandFlag&
PausedFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject&){
            return AudioIOBase::Get()->IsPaused();
        },
        CommandFlagOptions{}.QuickTest()
    };
    return flag;
}

const ReservedCommandFlag&
NoAutoSelect()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject&){ return true; }
    };
    return flag;
}                    // jkc
