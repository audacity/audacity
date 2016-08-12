/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"
#include "ImportMIDI.h"

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/ffile.h>
#include <wx/intl.h>

#if defined(USE_MIDI)

//#include "allegro.h"
//#include "strparse.h"
//#include "mfmidi.h"

#include "../Internat.h"
#include "../NoteTrack.h"

bool ImportMIDI(const wxString &fName, NoteTrack * dest)
{
   if (fName.Length() <= 4){
      wxMessageBox( _("Could not open file ") + fName + _(": Filename too short."));
      return false;
   }

   bool is_midi = false;
   if (fName.Right(4).CmpNoCase(wxT(".mid")) == 0 || fName.Right(5).CmpNoCase(wxT(".midi")) == 0)
      is_midi = true;
   else if(fName.Right(4).CmpNoCase(wxT(".gro")) != 0) {
      wxMessageBox( _("Could not open file ") + fName + _(": Incorrect filetype."));
      return false;
   }

   wxFFile mf(fName, wxT("rb"));
   if (!mf.IsOpened()) {
      wxMessageBox( _("Could not open file ") + fName + wxT("."));
      return false;
   }

   double offset = 0.0;
   auto new_seq = std::make_unique<Alg_seq>(fName.mb_str(), is_midi, &offset);

   //Should we also check if(seq->tracks() == 0) ?
   if(new_seq->get_read_error() == alg_error_open){
      wxMessageBox( _("Could not open file ") + fName + wxT("."));
      mf.Close();
      return false;
   }

   dest->SetSequence(std::move(new_seq));
   dest->SetOffset(offset);
   wxString trackNameBase = fName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   dest->SetName(trackNameBase);
   mf.Close();
   // the mean pitch should be somewhere in the middle of the display
   Alg_iterator iterator(dest->GetSequence(), false);
   iterator.begin();
   // for every event
   Alg_event_ptr evt;
   int note_count = 0;
   int pitch_sum = 0;
   while (NULL != (evt = iterator.next())) {
      // if the event is a note
       if (evt->get_type() == 'n') {
           Alg_note_ptr note = (Alg_note_ptr) evt;
           pitch_sum += (int) note->pitch;
           note_count++;
       }
   }
   int mean_pitch = (note_count > 0 ? pitch_sum / note_count : 60);
   // initial track is about 27 half-steps high; if bottom note is C,
   // then middle pitch class is D. Round mean_pitch to the nearest D:
   int mid_pitch = ((mean_pitch - 2 + 6) / 12) * 12 + 2;
   dest->SetBottomNote(mid_pitch - 14);
   return true;
}

#endif
