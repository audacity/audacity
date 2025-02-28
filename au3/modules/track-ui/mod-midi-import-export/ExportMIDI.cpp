/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ExportMIDI.cpp

  Paul Licameli split from FileMenus.cpp

**********************************************************************/

#include <wx/frame.h>

#if defined(USE_MIDI)

//#include "strparse.h"
//#include "mfmidi.h"

#include "FileNames.h"
#include "NoteTrack.h"
#include "Project.h"
#include "ProjectWindows.h"
#include "SelectFile.h"
#include "AudacityMessageBox.h"
#include "FileDialog/FileDialog.h"

#include "ShuttleGui.h"
#include "prefs/ImportExportPrefs.h"

namespace {
void AddControls(ShuttleGui& S)
{
    S.StartStatic(XO("Exported Allegro (.gro) files save time as:"));
    {
#if defined(__WXMAC__)
        // Bug 2692: Place button group in panel so tabbing will work and,
        // on the Mac, VoiceOver will announce as radio buttons.
        S.StartPanel();
#endif
        {
            S.StartRadioButtonGroup(NoteTrack::AllegroStyleSetting);
            {
                S.TieRadioButton();
                S.TieRadioButton();
            }
            S.EndRadioButtonGroup();
        }
#if defined(__WXMAC__)
        S.EndPanel();
#endif
    }
    S.EndStatic();
}

ImportExportPrefs::RegisteredControls reg{
    wxT("AllegroTimeOption"), AddControls };
}

// Insert a menu item
#include "CommandContext.h"
#include "MenuRegistry.h"
#include "CommonCommandFlags.h"

namespace {
const ReservedCommandFlag&
NoteTracksExistFlag()
{
    static ReservedCommandFlag flag{
        [](const AudacityProject& project){
            return !TrackList::Get(project).Any<const NoteTrack>().empty();
        }
    };
    return flag;
}                     //gsw

using namespace MenuRegistry;

void OnExportMIDI(const CommandContext& context)
{
    auto& project = context.project;
    auto& tracks = TrackList::Get(project);
    auto& window = GetProjectFrame(project);

    // Make sure that there is
    // exactly one NoteTrack selected.
    const auto range = tracks.Selected<const NoteTrack>();
    const auto numNoteTracksSelected = range.size();

    if (numNoteTracksSelected > 1) {
        AudacityMessageBox(
            XO("Please select only one Note Track at a time."));
        return;
    } else if (numNoteTracksSelected < 1) {
        AudacityMessageBox(
            XO("Please select a Note Track."));
        return;
    }

    wxASSERT(numNoteTracksSelected);
    if (!numNoteTracksSelected) {
        return;
    }

    const auto nt = *range.begin();

    while (true) {
        wxString fName;

        fName = SelectFile(FileNames::Operation::Export,
                           XO("Export MIDI As:"),
                           wxEmptyString,
                           fName,
                           wxT("mid"),
            {
                { XO("MIDI file"),    { wxT("mid") }, true },
                { XO("Allegro file"), { wxT("gro") }, true },
            },
                           wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                           &window);

        if (fName.empty()) {
            return;
        }

        if (!fName.Contains(wxT("."))) {
            fName = fName + wxT(".mid");
        }

        // Move existing files out of the way.  Otherwise wxTextFile will
        // append to (rather than replace) the current file.

        if (wxFileExists(fName)) {
#ifdef __WXGTK__
            wxString safetyFileName = fName + wxT("~");
#else
            wxString safetyFileName = fName + wxT(".bak");
#endif

            if (wxFileExists(safetyFileName)) {
                wxRemoveFile(safetyFileName);
            }

            wxRename(fName, safetyFileName);
        }

        if (fName.EndsWith(wxT(".mid")) || fName.EndsWith(wxT(".midi"))) {
            nt->ExportMIDI(fName);
        } else if (fName.EndsWith(wxT(".gro"))) {
            nt->ExportAllegro(fName);
        } else {
            auto msg = XO(
                "You have selected a filename with an unrecognized file extension.\nDo you want to continue?");
            auto title = XO("Export MIDI");
            int id = AudacityMessageBox(msg, title, wxYES_NO);
            if (id == wxNO) {
                continue;
            } else if (id == wxYES) {
                nt->ExportMIDI(fName);
            }
        }
        break;
    }
}

AttachedItem sAttachment{
    Command(wxT("ExportMIDI"), XXO("Export MI&DI..."), OnExportMIDI,
            AudioIONotBusyFlag() | NoteTracksExistFlag()),
    { wxT("File/Import-Export/ExportOther"),
      { OrderingHint::After, { "ExportLabels" } } }
};
}

#endif
