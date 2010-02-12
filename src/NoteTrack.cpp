/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class NoteTrack
\brief A Track that is used for Midi notes.  (Somewhat old code).

*//*******************************************************************/


#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/intl.h>

#include "Audacity.h"

#if defined(USE_MIDI)

#include "allegro.h"
/* REQUIRES PORTMIDI */
//#include "portmidi.h"
//#include "porttime.h"

#include "AColor.h"
#include "NoteTrack.h"
#include "DirManager.h"

NoteTrack *TrackFactory::NewNoteTrack()
{
   return new NoteTrack(mDirManager);
}

NoteTrack::NoteTrack(DirManager * projDirManager):
Track(projDirManager)
{
   SetDefaultName(_("Note Track"));
   SetName(GetDefaultName());

   mSeq = NULL;
   mLen = 0.0;
   mSerializationBuffer = NULL;
   mSerializationLength = 0;

   mDirManager = projDirManager;

   mBottomNote = 24;

   mVisibleChannels = 0xFFFF;
   mLastMidiPosition = 0;
}

NoteTrack::~NoteTrack()
{
   if (mSerializationBuffer) {
      delete [] mSerializationBuffer;
   }

   if (mSeq) {
      delete mSeq;
   }
}

Track *NoteTrack::Duplicate()
{
   NoteTrack *duplicate = new NoteTrack(mDirManager);
   // Duplicate on NoteTrack moves data from mSeq to mSerializationBuffer
   // and from mSerializationBuffer to mSeq on alternate calls. Duplicate
   // to the undo stack and Duplicate back to the project should result
   // in serialized blobs on the undo stack and traversable data in the
   // project object.
   if (mSeq) {
      assert(!mSerializationBuffer);
      // serialize from this to duplicate's mSerializationBuffer
      mSeq->serialize((void**)&duplicate->mSerializationBuffer, 
                      &duplicate->mSerializationLength);
   } else if (mSerializationBuffer) {
      assert(!mSeq);
      Alg_track_ptr alg_track = Alg_seq::unserialize(mSerializationBuffer,
                                                      mSerializationLength);
      assert(alg_track->get_type() == 's');
      duplicate->mSeq = (Alg_seq_ptr) alg_track;
   } else assert(false); // bug if neither mSeq nor mSerializationBuffer
   // copy some other fields here
   duplicate->mBottomNote = mBottomNote;
   duplicate->mLastMidiPosition = mLastMidiPosition;
   duplicate->mLen = mLen;
   duplicate->mVisibleChannels = mVisibleChannels;
   return duplicate;
}

void NoteTrack::DrawLabelControls(wxDC & dc, wxRect & r)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4) {
      return;
   }

   int x = r.x + (r.width / 2 - wid * 2) + 2;
   int y = r.y + 5;

   for (int row = 0; row < 4; row++) {
      for (int col = 0; col < 4; col++) {
         int channel = row * 4 + col + 1;

         wxRect box;
         box.x = x + col * wid;
         box.y = y + row * ht;
         box.width = wid;
         box.height = ht;

         if (mVisibleChannels & (1 << (channel - 1))) {
            AColor::MIDIChannel(&dc, channel);
            dc.DrawRectangle(box);

            AColor::LightMIDIChannel(&dc, channel);
            AColor::Line(dc, box.x, box.y, box.x + box.width - 1, box.y);
            AColor::Line(dc, box.x, box.y, box.x, box.y + box.height - 1);

            AColor::DarkMIDIChannel(&dc, channel);
            AColor::Line(dc,
                         box.x + box.width - 1, box.y,
                         box.x + box.width - 1, box.y + box.height - 1);
            AColor::Line(dc,
                         box.x, box.y + box.height - 1,
                         box.x + box.width - 1, box.y + box.height - 1);
         } else {
            AColor::MIDIChannel(&dc, 0);
            dc.DrawRectangle(box);
         }

         wxString t;
         long w;
         long h;

         t.Printf(wxT("%d"), channel);
         dc.GetTextExtent(t, &w, &h);
   
         dc.DrawText(t, box.x + (box.width - w) / 2, box.y + (box.height - h) / 2);
      }
   }
}

bool NoteTrack::LabelClick(wxRect & r, int mx, int my, bool right)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4)
      return false;

   int x = r.x + (r.width / 2 - wid * 2);
   int y = r.y + 1;

   int col = (mx - x) / wid;
   int row = (my - y) / ht;

   if (row < 0 || row >= 4 || col < 0 || col >= 4)
      return false;

   int channel = row * 4 + col;

   if (right) {
      if (mVisibleChannels == (1 << channel))
         mVisibleChannels = 0xFFFF;
      else
         mVisibleChannels = (1 << channel);
   } else
      mVisibleChannels ^= (1 << channel);

   return true;
}

void NoteTrack::SetSequence(Alg_seq_ptr seq)
{
   if (mSeq)
      delete mSeq;

   mSeq = seq;
   mLen = (seq ? seq->get_real_dur() : 0.0);
}

Alg_seq* NoteTrack::GetSequence() 
{
   return mSeq;
}

void NoteTrack::PrintSequence()
{
   FILE *debugOutput;

   debugOutput = fopen("debugOutput.txt", "wt");
   fprintf(debugOutput, "Importing MIDI...\n");

   if (mSeq) {
      int i = 0;

      while(i < mSeq->length()) {
         fprintf(debugOutput, "--\n");
         fprintf(debugOutput, "type: %c\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type());
         fprintf(debugOutput, "time: %f\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->time);
         fprintf(debugOutput, "channel: %li\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->chan);

         if(((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type() == wxT('n'))
         {
            fprintf(debugOutput, "pitch: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->pitch);
            fprintf(debugOutput, "duration: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->dur);
            fprintf(debugOutput, "velocity: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->loud);
         }
         else if(((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type() == wxT('n'))
         {
            fprintf(debugOutput, "key: %li\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->get_identifier());
            fprintf(debugOutput, "attribute type: %c\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type());
            fprintf(debugOutput, "attribute: %s\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_name());

            if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('r'))
            {
               fprintf(debugOutput, "value: %f\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.r);
            }
            else if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('i')) {
               fprintf(debugOutput, "value: %li\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.i);
            }
            else if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('s')) {
               fprintf(debugOutput, "value: %s\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.s);
            }
            else {}
         }
        
         i++;
      }
   }
   else {
      fprintf(debugOutput, "No sequence defined!\n");
   }

   fclose(debugOutput);
}

int NoteTrack::GetVisibleChannels()
{
   return mVisibleChannels;
}

bool NoteTrack::Cut(double t0, double t1, Track **dest){

   //dest goes onto clipboard
   *dest = NULL; // This is redundant
   if (t1 <= t0)
      return false;
   double len = t1-t0;

   NoteTrack *newTrack = new NoteTrack(mDirManager);

   newTrack->Init(*this);

   mSeq->convert_to_seconds();
   newTrack->mSeq = mSeq->cut(t0, len, false);

   mLen = (mLen <= len ? 0.0 : mLen - len);
   newTrack->mLen = len;

   // What should be done with the rest of newTrack's members?
   //(mBottomNote, mDirManager, mLastMidiPosition,
   // mSerializationBuffer, mSerializationLength, mVisibleChannels)

   *dest = newTrack;

   return true;
}
bool NoteTrack::Copy(double t0, double t1, Track **dest){

   //dest goes onto clipboard
   *dest = NULL; // This is redundant and matches WaveTrack::Copy
   if (t1 <= t0)
      return false;
   double len = t1-t0;

   NoteTrack *newTrack = new NoteTrack(mDirManager);

   newTrack->Init(*this);

   mSeq->convert_to_seconds();
   newTrack->mSeq = mSeq->copy(t0, len, false);
   newTrack->mLen = len;

   // What should be done with the rest of newTrack's members?
   //(mBottomNote, mDirManager, mLastMidiPosition,
   // mSerializationBuffer, mSerializationLength, mVisibleChannels)

   *dest = newTrack;

   return true;
}
bool NoteTrack::Clear(double t0, double t1){

   // If t1 = t0, should Clear return true?
   if (t1 <= t0)
      return false;
   double len = t1-t0;

   mSeq->clear(t0, len, false);

   return true;
}
bool NoteTrack::Paste(double t, Track *src){

   //Check that src is a non-NULL NoteTrack
   if (src == NULL || src->GetKind() != Track::Note)
      return false;
   
   NoteTrack* other = (NoteTrack*)src;
   if (other->mSeq == NULL)
      return false;

   if(!mSeq)
      mSeq = new Alg_seq();

   mSeq->paste(t, other->mSeq);
   mLen = mSeq->get_real_dur();

   return true;
}



bool NoteTrack::ExportMIDI(wxString f)
{
   return mSeq->smf_write(f.mb_str());
}

bool NoteTrack::ExportAllegro(wxString f)
{
   return mSeq->write(f.mb_str());
}


bool NoteTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   return false;
}

XMLTagHandler *NoteTrack::HandleXMLChild(const wxChar *tag)
{
   return NULL;
}

void NoteTrack::WriteXML(XMLWriter &xmlFile)
{
}

#endif // USE_MIDI

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ecfdee12-5b16-4f27-84d1-4a3800d07d1e

