/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "ImportMIDI.h"

#include <wx/defs.h>
#include <wx/ffile.h>
#include <wx/intl.h>

#if defined(USE_MIDI)

//#include "allegro.h"
//#include "strparse.h"
//#include "mfmidi.h"

#include "../Internat.h"
#include "../NoteTrack.h"
#include "../widgets/ErrorDialog.h"

bool ImportMIDI(const FilePath &fName, NoteTrack * dest)
{
   if (fName.length() <= 4){
      AudacityMessageBox( wxString::Format(
         _("Could not open file %s: Filename too short."), fName
      ) );
      return false;
   }

   bool is_midi = false;
   if (fName.Right(4).CmpNoCase(wxT(".mid")) == 0 || fName.Right(5).CmpNoCase(wxT(".midi")) == 0)
      is_midi = true;
   else if(fName.Right(4).CmpNoCase(wxT(".gro")) != 0) {
      AudacityMessageBox( wxString::Format(
         _("Could not open file %s: Incorrect filetype."), fName
      ) );
      return false;
   }

   wxFFile mf(fName, wxT("rb"));
   if (!mf.IsOpened()) {
      AudacityMessageBox( wxString::Format(
         _("Could not open file %s."), fName
      ) );
      return false;
   }

   double offset = 0.0;
   auto new_seq = std::make_unique<Alg_seq>(fName.mb_str(), is_midi, &offset);

   //Should we also check if(seq->tracks() == 0) ?
   if(new_seq->get_read_error() == alg_error_open){
      AudacityMessageBox( wxString::Format(
         _("Could not open file %s."), fName
      ) );
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
