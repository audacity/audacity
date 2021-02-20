/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/


#include "ImportMIDI.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/frame.h>
#include <wx/intl.h>

#if defined(USE_MIDI)


#include "../lib-src/header-substitutes/allegro.h"

//#include "strparse.h"
//#include "mfmidi.h"

#include "../NoteTrack.h"
#include "Project.h"
#include "../ProjectFileIO.h"
#include "../ProjectHistory.h"
#include "../ProjectWindow.h"
#include "../SelectUtilities.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/FileHistory.h"

// Given an existing project, try to import into it, return true on success
bool DoImportMIDI( AudacityProject &project, const FilePath &fileName )
{
   auto &projectFileIO = ProjectFileIO::Get( project );
   auto &tracks = TrackList::Get( project );
   auto newTrack =  std::make_shared<NoteTrack>();
   bool initiallyEmpty = tracks.empty();
   
   if (::ImportMIDI(fileName, newTrack.get())) {
      
      SelectUtilities::SelectNone( project );
      auto pTrack = tracks.Add( newTrack );
      pTrack->SetSelected(true);
      
      // Fix the bug 2109.
      // In case the project had soloed tracks before importing,
      // the newly imported track is muted.
      const bool projectHasSolo =
         !(tracks.Any<PlayableTrack>() + &PlayableTrack::GetSolo).empty();
#ifdef EXPERIMENTAL_MIDI_OUT
      if (projectHasSolo)
         pTrack->SetMute(true);
#endif

      ProjectHistory::Get( project )
         .PushState(
            XO("Imported MIDI from '%s'").Format( fileName ),
            XO("Import MIDI")
         );
      
      ProjectWindow::Get( project ).ZoomAfterImport(pTrack);
      FileHistory::Global().Append(fileName);

      // If the project was clean and temporary (not permanently saved), then set
      // the filename to the just imported path.
      if (initiallyEmpty && projectFileIO.IsTemporary()) {
         wxFileName fn(fileName);
         project.SetProjectName(fn.GetName());
         project.SetInitialImportPath(fn.GetPath());
         projectFileIO.SetProjectTitle();
      }
      return true;
   }
   else
      return false;
}

bool ImportMIDI(const FilePath &fName, NoteTrack * dest)
{
   if (fName.length() <= 4){
      AudacityMessageBox(
         XO("Could not open file %s: Filename too short.").Format( fName ) );
      return false;
   }

   bool is_midi = false;
   if (fName.Right(4).CmpNoCase(wxT(".mid")) == 0 || fName.Right(5).CmpNoCase(wxT(".midi")) == 0)
      is_midi = true;
   else if(fName.Right(4).CmpNoCase(wxT(".gro")) != 0) {
      AudacityMessageBox(
         XO("Could not open file %s: Incorrect filetype.").Format( fName ) );
      return false;
   }

   wxFFile mf(fName, wxT("rb"));
   if (!mf.IsOpened()) {
      AudacityMessageBox(
         XO("Could not open file %s.").Format( fName ) );
      return false;
   }

   double offset = 0.0;
   auto new_seq = std::make_unique<Alg_seq>(fName.mb_str(), is_midi, &offset);

   //Should we also check if(seq->tracks() == 0) ?
   if(new_seq->get_read_error() == alg_error_open){
      AudacityMessageBox(
         XO("Could not open file %s.").Format( fName ) );
      mf.Close();
      return false;
   }

   dest->SetSequence(std::move(new_seq));
   dest->SetOffset(offset);
   wxString trackNameBase = fName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   dest->SetName(trackNameBase);
   mf.Close();

   dest->ZoomAllNotes();
   return true;
}

#endif
