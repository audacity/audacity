/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/ffile.h>
#include <wx/intl.h>

#include "../Audacity.h"

#if defined(USE_MIDI)

#include "../Internat.h"
#include "../NoteTrack.h"
#include "ImportMIDI.h"

#include "allegro.h"
#include "strparse.h"
#include "mfmidi.h"

bool ImportMIDI(wxString fName, NoteTrack * dest)
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

   Alg_seq_ptr new_seq = new Alg_seq(fName.mb_str(), is_midi);

   //Should we also check if(seq->tracks() == 0) ?
   if(new_seq->get_read_error() == alg_error_open){
      wxMessageBox( _("Could not open file ") + fName + wxT("."));
      mf.Close();
      return false;
   }

   dest->SetSequence(new_seq);
   mf.Close();
   return true;
}

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 391b08e6-61f4-43ea-8431-c835c31ba86d

