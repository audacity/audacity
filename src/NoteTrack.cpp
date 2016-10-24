/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class NoteTrack
\brief A Track that is used for Midi notes.  (Somewhat old code).

*//*******************************************************************/


#include "Audacity.h"
#include "NoteTrack.h"

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/intl.h>

#if defined(USE_MIDI)
#include <sstream>

#define ROUND(x) ((int) ((x) + 0.5))

#include "AColor.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "effects/TimeWarper.h"

#include "Experimental.h"

#ifdef SONIFY
#include "portmidi.h"

#define SON_PROGRAM 0
#define SON_AutoSave 67
#define SON_ModifyState 60
#define SON_NoteBackground 72
#define SON_NoteForeground 74
#define SON_Measures 76 /* "bar line" */
#define SON_Serialize 77
#define SON_Unserialize 79
#define SON_VEL 100


PmStream *sonMidiStream;
bool sonificationStarted = false;

void SonifyBeginSonification()
{
   PmError err = Pm_OpenOutput(&sonMidiStream, Pm_GetDefaultOutputDeviceID(),
                               NULL, 0, NULL, NULL, 0);
   if (err) sonMidiStream = NULL;
   if (sonMidiStream)
      Pm_WriteShort(sonMidiStream, 0, Pm_Message(0xC0, SON_PROGRAM, 0));
   sonificationStarted = true;
}


void SonifyEndSonification()
{
   if (sonMidiStream) Pm_Close(sonMidiStream);
   sonificationStarted = false;
}




void SonifyNoteOnOff(int p, int v)
{
   if (!sonificationStarted)
      SonifyBeginSonification();
   if (sonMidiStream)
      Pm_WriteShort(sonMidiStream, 0, Pm_Message(0x90, p, v));
}

#define SONFNS(name) \
   void SonifyBegin ## name() { SonifyNoteOnOff(SON_ ## name, SON_VEL); } \
   void SonifyEnd ## name() { SonifyNoteOnOff(SON_ ## name, 0); }

SONFNS(NoteBackground)
SONFNS(NoteForeground)
SONFNS(Measures)
SONFNS(Serialize)
SONFNS(Unserialize)
SONFNS(ModifyState)
SONFNS(AutoSave)

#undef SONFNS

#endif



NoteTrack::Holder TrackFactory::NewNoteTrack()
{
   return std::make_unique<NoteTrack>(mDirManager);
}

NoteTrack::NoteTrack(const std::shared_ptr<DirManager> &projDirManager):
Track(projDirManager)
{
   SetDefaultName(_("Note Track"));
   SetName(GetDefaultName());

   mSeq = NULL;
   mSerializationBuffer = NULL;
   mSerializationLength = 0;

#ifdef EXPERIMENTAL_MIDI_OUT
   mGain = 0;
#endif
   mBottomNote = 24;
   mPitchHeight = 5;

   mVisibleChannels = ALL_CHANNELS;
   mLastMidiPosition = 0;
}

NoteTrack::~NoteTrack()
{
   if (mSerializationBuffer) {
      delete [] mSerializationBuffer;
   }
}

Track::Holder NoteTrack::Duplicate() const
{
   auto duplicate = std::make_unique<NoteTrack>(mDirManager);
   duplicate->Init(*this);
   // Duplicate on NoteTrack moves data from mSeq to mSerializationBuffer
   // and from mSerializationBuffer to mSeq on alternate calls. Duplicate
   // to the undo stack and Duplicate back to the project should result
   // in serialized blobs on the undo stack and traversable data in the
   // project object.
   if (mSeq) {
      SonifyBeginSerialize();
      assert(!mSerializationBuffer);
      // serialize from this to duplicate's mSerializationBuffer
      mSeq->serialize((void**)&duplicate->mSerializationBuffer,
                      &duplicate->mSerializationLength);
      SonifyEndSerialize();
   } else if (mSerializationBuffer) {
      SonifyBeginUnserialize();
      assert(!mSeq);
      std::unique_ptr<Alg_track> alg_track{ Alg_seq::unserialize(mSerializationBuffer,
                                                      mSerializationLength) };
      assert(alg_track->get_type() == 's');
      duplicate->mSeq.reset(static_cast<Alg_seq*>(alg_track.release()));
      SonifyEndUnserialize();
   } else assert(false); // bug if neither mSeq nor mSerializationBuffer
   // copy some other fields here
   duplicate->SetBottomNote(mBottomNote);
   duplicate->SetPitchHeight(mPitchHeight);
   duplicate->mLastMidiPosition = mLastMidiPosition;
   duplicate->mVisibleChannels = mVisibleChannels;
   duplicate->SetOffset(GetOffset());
#ifdef EXPERIMENTAL_MIDI_OUT
   duplicate->SetGain(GetGain());
#endif
   // This std::move is needed to "upcast" the pointer type
   return std::move(duplicate);
}


double NoteTrack::GetOffset() const
{
   return mOffset;
}

double NoteTrack::GetStartTime() const
{
   return GetOffset();
}

double NoteTrack::GetEndTime() const
{
   return GetStartTime() + (mSeq ? mSeq->get_real_dur() : 0.0);
}


void NoteTrack::WarpAndTransposeNotes(double t0, double t1,
                                      const TimeWarper &warper,
                                      double semitones)
{
   // Since this is a duplicate and duplicates convert mSeq to
   // a text string for saving as XML, we probably have to
   // duplicate again to get back an mSeq
   double offset = this->GetOffset(); // track is shifted this amount
   if (!mSeq) { // replace saveme with an (unserialized) duplicate
      Track::Holder unt{ Duplicate() };
      const auto nt = static_cast<NoteTrack*>(unt.get());
      wxASSERT(!mSeq && nt->mSeq && !nt->mSerializationBuffer);
      // swap mSeq and Buffer between this and nt
      nt->mSerializationBuffer = mSerializationBuffer;
      nt->mSerializationLength = mSerializationLength;
      mSerializationBuffer = NULL;
      mSerializationLength = 0;
      mSeq = std::move(nt->mSeq);
   }
   mSeq->convert_to_seconds(); // make sure time units are right
   t1 -= offset; // adjust time range to compensate for track offset
   t0 -= offset;
   if (t1 > mSeq->get_dur()) { // make sure t0, t1 are within sequence
      t1 = mSeq->get_dur();
      if (t0 >= t1) return;
   }
   Alg_iterator iter(mSeq.get(), false);
   iter.begin();
   Alg_event_ptr event;
   while (0 != (event = iter.next()) && event->time < t1) {
      if (event->is_note() && event->time >= t0 &&
          // Allegro data structure does not restrict channels to 16.
          // Since there is not way to select more than 16 channels,
          // map all channel numbers mod 16. This will have no effect
          // on MIDI files, but it will allow users to at least select
          // all channels on non-MIDI event sequence data.
          IsVisibleChan(event->chan % 16)) {
         event->set_pitch(event->get_pitch() + semitones);
      }
   }
   iter.end();
   // now, use warper to warp the tempo map
   mSeq->convert_to_beats(); // beats remain the same
   Alg_time_map_ptr map = mSeq->get_time_map();
   map->insert_beat(t0, map->time_to_beat(t0));
   map->insert_beat(t1, map->time_to_beat(t1));
   int i, len = map->length();
   for (i = 0; i < len; i++) {
      Alg_beat &beat = map->beats[i];
      beat.time = warper.Warp(beat.time + offset) - offset;
   }
   // about to redisplay, so might as well convert back to time now
   mSeq->convert_to_seconds();
}



int NoteTrack::DrawLabelControls(wxDC & dc, wxRect & r)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4) {
      return r.y + 5 + ht * 4;
   }

   int x = r.x + (r.width / 2 - wid * 2) + 2;
   int y = r.y + 5;

   wxRect box;
   for (int row = 0; row < 4; row++) {
      for (int col = 0; col < 4; col++) {
         // chanName is the "external" channel number (1-16)
         // used by AColor and button labels
         int chanName = row * 4 + col + 1;

         box.x = x + col * wid;
         box.y = y + row * ht;
         box.width = wid;
         box.height = ht;

         if (IsVisibleChan(chanName - 1)) {
            AColor::MIDIChannel(&dc, chanName);
            dc.DrawRectangle(box);
// two choices: channel is enabled (to see and play) when button is in
// "up" position (original Audacity style) or in "down" position
//
#define CHANNEL_ON_IS_DOWN 1
#if CHANNEL_ON_IS_DOWN
            AColor::DarkMIDIChannel(&dc, chanName);
#else
            AColor::LightMIDIChannel(&dc, chanName);
#endif
            AColor::Line(dc, box.x, box.y, box.x + box.width - 1, box.y);
            AColor::Line(dc, box.x, box.y, box.x, box.y + box.height - 1);

#if CHANNEL_ON_IS_DOWN
            AColor::LightMIDIChannel(&dc, chanName);
#else
            AColor::DarkMIDIChannel(&dc, chanName);
#endif
            AColor::Line(dc,
                         box.x + box.width - 1, box.y,
                         box.x + box.width - 1, box.y + box.height - 1);
            AColor::Line(dc,
                         box.x, box.y + box.height - 1,
                         box.x + box.width - 1, box.y + box.height - 1);
         } else {
            AColor::MIDIChannel(&dc, 0);
            dc.DrawRectangle(box);
#if CHANNEL_ON_IS_DOWN
            AColor::LightMIDIChannel(&dc, 0);
#else
            AColor::DarkMIDIChannel(&dc, 0);
#endif
            AColor::Line(dc, box.x, box.y, box.x + box.width - 1, box.y);
            AColor::Line(dc, box.x, box.y, box.x, box.y + box.height - 1);

#if CHANNEL_ON_IS_DOWN
            AColor::DarkMIDIChannel(&dc, 0);
#else
            AColor::LightMIDIChannel(&dc, 0);
#endif
            AColor::Line(dc,
                         box.x + box.width - 1, box.y,
                         box.x + box.width - 1, box.y + box.height - 1);
            AColor::Line(dc,
                         box.x, box.y + box.height - 1,
                         box.x + box.width - 1, box.y + box.height - 1);

         }

         wxString t;
         wxCoord w;
         wxCoord h;

         t.Printf(wxT("%d"), chanName);
         dc.GetTextExtent(t, &w, &h);

         dc.DrawText(t, box.x + (box.width - w) / 2, box.y + (box.height - h) / 2);
      }
   }
   AColor::MIDIChannel(&dc, 0); // always return with gray color selected
   return box.GetBottom();
}

bool NoteTrack::LabelClick(wxRect & r, int mx, int my, bool right)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4)
      return false;

   int x = r.x + (r.width / 2 - wid * 2);
   int y = r.y + 1;
   // after adding Mute and Solo buttons, mapping is broken, so hack in the offset
   y += 12;

   int col = (mx - x) / wid;
   int row = (my - y) / ht;

   if (row < 0 || row >= 4 || col < 0 || col >= 4)
      return false;

   int channel = row * 4 + col;

   if (right) {
      if (mVisibleChannels == CHANNEL_BIT(channel))
         mVisibleChannels = ALL_CHANNELS;
      else
         mVisibleChannels = CHANNEL_BIT(channel);
   } else
      ToggleVisibleChan(channel);

   return true;
}

void NoteTrack::SetSequence(std::unique_ptr<Alg_seq> &&seq)
{
   mSeq = std::move(seq);
}

Alg_seq* NoteTrack::GetSequence()
{
   return mSeq.get();
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

Track::Holder NoteTrack::Cut(double t0, double t1)
{
   if (t1 <= t0)
      return{};
   double len = t1-t0;

   auto newTrack = std::make_unique<NoteTrack>(mDirManager);

   newTrack->Init(*this);

   mSeq->convert_to_seconds();
   newTrack->mSeq.reset(mSeq->cut(t0 - GetOffset(), len, false));
   newTrack->SetOffset(GetOffset());

   // What should be done with the rest of newTrack's members?
   //(mBottomNote, mDirManager, mLastMidiPosition,
   // mSerializationBuffer, mSerializationLength, mVisibleChannels)

   // This std::move is needed to "upcast" the pointer type
   return std::move(newTrack);
}

Track::Holder NoteTrack::Copy(double t0, double t1) const
{
   if (t1 <= t0)
      return{};
   double len = t1-t0;

   auto newTrack = std::make_unique<NoteTrack>(mDirManager);

   newTrack->Init(*this);

   mSeq->convert_to_seconds();
   newTrack->mSeq.reset(mSeq->copy(t0 - GetOffset(), len, false));
   newTrack->SetOffset(GetOffset());

   // What should be done with the rest of newTrack's members?
   //(mBottomNote, mDirManager, mLastMidiPosition,
   // mSerializationBuffer, mSerializationLength, mVisibleChannels)

   // This std::move is needed to "upcast" the pointer type
   return std::move(newTrack);
}

bool NoteTrack::Trim(double t0, double t1)
{
   if (t1 <= t0)
      return false;
   mSeq->convert_to_seconds();
   // DELETE way beyond duration just in case something is out there:
   mSeq->clear(t1 - GetOffset(), mSeq->get_dur() + 10000.0, false);
   // Now that stuff beyond selection is cleared, clear before selection:
   mSeq->clear(0.0, t0 - GetOffset(), false);
   // want starting time to be t0
   SetOffset(t0);
   return true;
}

bool NoteTrack::Clear(double t0, double t1)
{
   // If t1 = t0, should Clear return true?
   if (t1 <= t0)
      return false;
   double len = t1-t0;

   if (mSeq)
      mSeq->clear(t0 - GetOffset(), len, false);

   return true;
}

bool NoteTrack::Paste(double t, const Track *src)
{
   // Paste inserts src at time t. If src has a positive offset,
   // the offset is treated as silence which is also inserted. If
   // the offset is negative, the offset is ignored and the ENTIRE
   // src is inserted (otherwise, we would either lose data from
   // src by not inserting things at negative times, or inserting
   // things at negative times could overlap things already in
   // the destination track).

   //Check that src is a non-NULL NoteTrack
   if (src == NULL || src->GetKind() != Track::Note)
      return false;

   NoteTrack* other = (NoteTrack*)src;
   if (other->mSeq == NULL)
      return false;

   if(!mSeq)
      mSeq = std::make_unique<Alg_seq>();

   if (other->GetOffset() > 0) {
      mSeq->convert_to_seconds();
      mSeq->insert_silence(t - GetOffset(), other->GetOffset());
      t += other->GetOffset();
   }
   mSeq->paste(t - GetOffset(), other->mSeq.get());

   return true;
}

// Call this function to manipulate the underlying sequence data. This is
// NOT the function that handles horizontal dragging.
bool NoteTrack::Shift(double t) // t is always seconds
{
   if (t > 0) {
      // insert an even number of measures
      mSeq->convert_to_beats();
      // get initial tempo
      double tempo = mSeq->get_tempo(0.0);
      double beats_per_measure = mSeq->get_bar_len(0.0);
      int m = ROUND(t * tempo / beats_per_measure);
      // need at least 1 measure, so if we rounded down to zero, fix it
      if (m == 0) m = 1;
      // compute NEW tempo so that m measures at NEW tempo take t seconds
      tempo = beats_per_measure * m / t; // in beats per second
      mSeq->insert_silence(0.0, beats_per_measure * m);
      mSeq->set_tempo(tempo * 60.0 /* bpm */, 0.0, beats_per_measure * m);
      mSeq->write("afterShift.gro");
   } else if (t < 0) {
      mSeq->convert_to_seconds();
      mSeq->clear(0, t, true);
   } else { // offset is zero, no modifications
      return false;
   }
   return true;
}

double NoteTrack::NearestBeatTime(double time, double *beat)
{
   wxASSERT(mSeq);
   // Alg_seq knows nothing about offset, so remove offset time
   double seq_time = time - GetOffset();
   seq_time = mSeq->nearest_beat_time(seq_time, beat);
   // add the offset back in to get "actual" audacity track time
   return seq_time + GetOffset();
}

bool NoteTrack::StretchRegion(double t0, double t1, double dur)
{
   wxASSERT(mSeq);
   // Alg_seq::stretch_region uses beats, so we translate time
   // to beats first:
   t0 -= GetOffset();
   t1 -= GetOffset();
   double b0 = mSeq->get_time_map()->time_to_beat(t0);
   double b1 = mSeq->get_time_map()->time_to_beat(t1);
   bool result = mSeq->stretch_region(b0, b1, dur);
   if (result) {
      mSeq->convert_to_seconds();
      mSeq->set_dur(mSeq->get_dur() + dur - (t1 - t0));
   }
   return result;
}

namespace
{
   void swap(std::unique_ptr<Alg_seq> &a, std::unique_ptr<Alg_seq> &b)
   {
      std::unique_ptr<Alg_seq> tmp = std::move(a);
      a = std::move(b);
      b = std::move(tmp);
   }
}

Alg_seq *NoteTrack::MakeExportableSeq(std::unique_ptr<Alg_seq> &cleanup)
{
   cleanup.reset();
   double offset = GetOffset();
   if (offset == 0)
      return mSeq.get();
   // make a copy, deleting events that are shifted before time 0
   double start = -offset;
   if (start < 0) start = 0;
   // notes that begin before "start" are not included even if they
   // extend past "start" (because "all" parameter is set to false)
   cleanup.reset( mSeq->copy(start, mSeq->get_dur() - start, false) );
   auto seq = cleanup.get();
   if (offset > 0) {
      // swap cleanup and mSeq so that Shift operates on the NEW copy
      swap(mSeq, cleanup);
      Shift(offset);
      swap(mSeq, cleanup);  // undo the swap
#ifdef OLD_CODE
      // now shift events by offset. This must be done with an integer
      // number of measures, so first, find the beats-per-measure
      double beats_per_measure = 4.0;
      Alg_time_sig_ptr tsp = NULL;
      if (seq->time_sig.length() > 0 && seq->time_sig[0].beat < ALG_EPS) {
         // there is an initial time signature
         tsp = &(seq->time_sig[0]);
         beats_per_measure = (tsp->num * 4) / tsp->den;
      }
      // also need the initial tempo
      double bps = ALG_DEFAULT_BPM / 60;
      Alg_time_map_ptr map = seq->get_time_map();
      Alg_beat_ptr bp = &(map->beats[0]);
      if (bp->time < ALG_EPS) { // tempo change at time 0
         if (map->beats.len > 1) { // compute slope to get tempo
            bps = (map->beats[1].beat - map->beats[0].beat) /
                  (map->beats[1].time - map->beats[0].time);
         } else if (seq->get_time_map()->last_tempo_flag) {
            bps = seq->get_time_map()->last_tempo;
         }
      }
      // find closest number of measures to fit in the gap
      // number of measures is offset / measure_time
      double measure_time = beats_per_measure / bps; // seconds per measure
      int n = ROUND(offset / measure_time);
      if (n == 0) n = 1;
      // we will insert n measures. Compute the desired duration of each.
      measure_time = offset / n;
      bps = beats_per_measure / measure_time;
      // insert integer multiple of measures at beginning
      seq->convert_to_beats();
      seq->insert_silence(0, beats_per_measure * n);
      // make sure time signature at 0 is correct
      if (tsp) {
         seq->set_time_sig(0, tsp->num, tsp->den);
      }
      // adjust tempo to match offset
      seq->set_tempo(bps * 60.0, 0, beats_per_measure * n);
#endif
   } else {
      // if offset is negative, it might not be a multiple of beats, but
      // we want to preserve the relative positions of measures. I.e. we
      // should shift barlines and time signatures as well as notes.
      // Insert a time signature at the first bar-line if necessary.

      // Translate start from seconds to beats and call it beat:
      double beat = mSeq->get_time_map()->time_to_beat(start);
      // Find the time signature in mSeq in effect at start (beat):
      int i = mSeq->time_sig.find_beat(beat);
      // i is where you would insert a NEW time sig at beat,
      // Case 1: beat coincides with a time sig at i. Time signature
      // at beat means that there is a barline at beat, so when beat
      // is shifted to 0, the relative barline positions are preserved
      if (mSeq->time_sig.length() > 0 &&
          within(beat, mSeq->time_sig[i].beat, ALG_EPS)) {
         // beat coincides with time signature change, so offset must
         // be a multiple of beats
         /* do nothing */ ;
      // Case 2: there is no time signature before beat.
      } else if (i == 0 && (mSeq->time_sig.length() == 0 ||
                            mSeq->time_sig[i].beat > beat)) {
         // If beat does not fall on an implied barline, we need to
         // insert a time signature.
         double measures = beat / 4.0;
         double imeasures = ROUND(measures);
         if (!within(measures, imeasures, ALG_EPS)) {
            double bar_offset = ((int)(measures) + 1) * 4.0 - beat;
            seq->set_time_sig(bar_offset, 4, 4);
         }
      // This case should never be true because if i == 0, either there
      // are no time signatures before beat (Case 2),
      // or there is one time signature at beat (Case 1)
      } else if (i == 0) {
         /* do nothing (might be good to assert(false)) */ ;
      // Case 3: i-1 must be the effective time sig position
      } else {
         i -= 1; // index the time signature in effect at beat
         Alg_time_sig_ptr tsp = &(mSeq->time_sig[i]);
         double beats_per_measure = (tsp->num * 4) / tsp->den;
         double measures = (beat - tsp->beat) / beats_per_measure;
         int imeasures = ROUND(measures);
         if (!within(measures, imeasures, ALG_EPS)) {
            // beat is not on a measure, so we need to insert a time sig
            // to force a bar line at the first measure location after
            // beat
            double bar = tsp->beat + beats_per_measure * ((int)(measures) + 1);
            double bar_offset = bar - beat;
            // insert NEW time signature at bar_offset in NEW sequence
            // It will have the same time signature, but the position will
            // force a barline to match the barlines in mSeq
            seq->set_time_sig(bar_offset, tsp->num, tsp->den);
         }
         // else beat coincides with a barline, so no need for an extra
         // time signature to force barline alignment
      }
   }
   return seq;
}


bool NoteTrack::ExportMIDI(const wxString &f)
{
   std::unique_ptr<Alg_seq> cleanup;
   auto seq = MakeExportableSeq(cleanup);
   bool rslt = seq->smf_write(f.mb_str());
   return rslt;
}

bool NoteTrack::ExportAllegro(const wxString &f)
{
   double offset = GetOffset();
   bool in_seconds;
   gPrefs->Read(wxT("/FileFormats/AllegroStyle"), &in_seconds, true);
   if (in_seconds) {
       mSeq->convert_to_seconds();
   } else {
       mSeq->convert_to_beats();
   }
   return mSeq->write(f.mb_str(), offset);
}


bool NoteTrack::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (!wxStrcmp(tag, wxT("notetrack"))) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         if (!value)
            break;
         const wxString strValue = value;
         long nValue;
         double dblValue;
         if (!wxStrcmp(attr, wxT("name")) && XMLValueChecker::IsGoodString(strValue))
            mName = strValue;
         else if (!wxStrcmp(attr, wxT("offset")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            SetOffset(dblValue);
         else if (!wxStrcmp(attr, wxT("visiblechannels"))) {
             if (!XMLValueChecker::IsGoodInt(strValue) ||
                 !strValue.ToLong(&nValue) ||
                 !XMLValueChecker::IsValidVisibleChannels(nValue))
                 return false;
             mVisibleChannels = nValue;
         }
         else if (!wxStrcmp(attr, wxT("height")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mHeight = nValue;
         else if (!wxStrcmp(attr, wxT("minimized")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            mMinimized = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("isSelected")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            this->SetSelected(nValue != 0);
#ifdef EXPERIMENTAL_MIDI_OUT
         else if (!wxStrcmp(attr, wxT("velocity")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            mGain = (float) dblValue;
#endif
         else if (!wxStrcmp(attr, wxT("bottomnote")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetBottomNote(nValue);
         else if (!wxStrcmp(attr, wxT("data"))) {
             std::string s(strValue.mb_str(wxConvUTF8));
             std::istringstream data(s);
             mSeq = std::make_unique<Alg_seq>(data, false);
         }
      } // while
      return true;
   }
   return false;
}

XMLTagHandler *NoteTrack::HandleXMLChild(const wxChar * WXUNUSED(tag))
{
   return NULL;
}

void NoteTrack::WriteXML(XMLWriter &xmlFile)
{
   std::ostringstream data;
   // Normally, Duplicate is called in pairs -- once to put NoteTrack
   // on the Undo stack, and again to move from the Undo stack to an
   // "active" editable state. For efficiency, we do not do a "real"
   // Duplicate followed by serialization into a binary blob. Instead,
   // we combine the Duplicate with serialization or unserialization.
   // Serialization and Unserialization happen on alternate calls to
   // Duplicate and (usually) produce the right results at the right
   // time.
   // It turns out that this optimized Duplicate is a little too
   // clever. There is at least one case where a track can be duplicated
   // and then AutoSave'd. (E.g. do an "Insert Silence" effect on a
   // NoteTrack.) In this case, mSeq will be NULL. To avoid a crash
   // and perform WriteXML, we may need to restore NoteTracks from binary
   // blobs to regular data structures (with an Alg_seq member).
   Track::Holder holder;
   NoteTrack *saveme = this;
   if (!mSeq) { // replace saveme with an (unserialized) duplicate
      holder = Duplicate();
      saveme = static_cast<NoteTrack*>(holder.get());
      assert(saveme->mSeq);
   }
   saveme->mSeq->write(data, true);
   xmlFile.StartTag(wxT("notetrack"));
   xmlFile.WriteAttr(wxT("name"), saveme->mName);
   xmlFile.WriteAttr(wxT("offset"), saveme->GetOffset());
   xmlFile.WriteAttr(wxT("visiblechannels"), saveme->mVisibleChannels);
   xmlFile.WriteAttr(wxT("height"), saveme->GetActualHeight());
   xmlFile.WriteAttr(wxT("minimized"), saveme->GetMinimized());
   xmlFile.WriteAttr(wxT("isSelected"), this->GetSelected());

#ifdef EXPERIMENTAL_MIDI_OUT
   xmlFile.WriteAttr(wxT("velocity"), (double) saveme->mGain);
#endif
   xmlFile.WriteAttr(wxT("bottomnote"), saveme->mBottomNote);
   xmlFile.WriteAttr(wxT("data"), wxString(data.str().c_str(), wxConvUTF8));
   xmlFile.EndTag(wxT("notetrack"));
}

void NoteTrack::StartVScroll()
{
    mStartBottomNote = mBottomNote;
}

void NoteTrack::VScroll(int start, int end)
{
    int ph = GetPitchHeight();
    int delta = ((end - start) + ph / 2) / ph;
    SetBottomNote(mStartBottomNote + delta);
}

// Zoom the note track, centering the pitch at centerY,
// amount is 1 for zoom in, and -1 for zoom out
void NoteTrack::Zoom(int centerY, int amount)
{
   // Construct track rectangle to map pitch to screen coordinates
   // Only y and height are needed:
   wxRect trackRect(0, GetY(), 1, GetHeight());
   PrepareIPitchToY(trackRect);
   int centerPitch = YToIPitch(centerY);
   // zoom out by changing the pitch height -- a small integer
   mPitchHeight += amount;
   if (mPitchHeight <= 0) mPitchHeight = 1;
   PrepareIPitchToY(trackRect); // update because mPitchHeight changed
   int newCenterPitch = YToIPitch(GetY() + GetHeight() / 2);
   // center the pitch that the user clicked on
   SetBottomNote(mBottomNote + (centerPitch - newCenterPitch));
}


void NoteTrack::ZoomTo(int start, int end)
{
   wxRect trackRect(0, GetY(), 1, GetHeight());
   PrepareIPitchToY(trackRect);
   int topPitch = YToIPitch(start);
   int botPitch = YToIPitch(end);
   if (topPitch < botPitch) { // swap
      int temp = topPitch; topPitch = botPitch; botPitch = temp;
   }
   if (topPitch == botPitch) { // can't divide by zero, do something else
      Zoom(start, 1);
      return;
   }
   int trialPitchHeight = trackRect.height / (topPitch - botPitch);
   if (trialPitchHeight > 25) { // keep mPitchHeight in bounds [1...25]
      trialPitchHeight = 25;
   } else if (trialPitchHeight == 0) {
      trialPitchHeight = 1;
   }
   Zoom((start + end) / 2, trialPitchHeight - mPitchHeight);
}

int NoteTrack::YToIPitch(int y)
{
   y = mBottom - y; // pixels above pitch 0
   int octave = (y / GetOctaveHeight());
   y -= octave * GetOctaveHeight();
   // result is approximate because C and G are one pixel taller than
   // mPitchHeight.
   return (y / mPitchHeight) + octave * 12;
}

#endif // USE_MIDI
