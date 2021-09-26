/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class NoteTrack
\brief A Track that is used for Midi notes.  (Somewhat old code).

*//*******************************************************************/



#include "NoteTrack.h"



#include <wx/wxcrtvararg.h>
#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/intl.h>

#if defined(USE_MIDI)
#include "../lib-src/header-substitutes/allegro.h"

#include <sstream>

#define ROUND(x) ((int) ((x) + 0.5))

#include "AColor.h"
#include "Prefs.h"
#include "Project.h"
#include "prefs/ImportExportPrefs.h"

#include "InconsistencyException.h"

#include "effects/TimeWarper.h"
#include "tracks/ui/TrackView.h"
#include "tracks/ui/TrackControls.h"

#include "AllThemeResources.h"
#include "Theme.h"

#ifdef SONIFY
#include <portmidi.h>

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



static ProjectFileIORegistry::ObjectReaderEntry readerEntry{
   wxT( "notetrack" ),
   []( AudacityProject &project ){
      auto &tracks = TrackList::Get( project );
      auto result = tracks.Add( std::make_shared<NoteTrack>());
      TrackView::Get( *result );
      TrackControls::Get( *result );
      return result;
   }
};

NoteTrack::NoteTrack()
   : NoteTrackBase()
{
   SetDefaultName(_("Note Track"));
   SetName(GetDefaultName());

   mSeq = NULL;
   mSerializationLength = 0;

#ifdef EXPERIMENTAL_MIDI_OUT
   mVelocity = 0;
#endif
   mBottomNote = MinPitch;
   mTopNote = MaxPitch;

   mVisibleChannels = ALL_CHANNELS;
}

NoteTrack::~NoteTrack()
{
}

Alg_seq &NoteTrack::GetSeq() const
{
   if (!mSeq) {
      if (!mSerializationBuffer)
         mSeq = std::make_unique<Alg_seq>();
      else {
         std::unique_ptr<Alg_track> alg_track
         { Alg_seq::unserialize
            ( mSerializationBuffer.get(), mSerializationLength ) };
         wxASSERT(alg_track->get_type() == 's');
         mSeq.reset( static_cast<Alg_seq*>(alg_track.release()) );

         // Preserve the invariant that at most one of the representations is
         // valid
         mSerializationBuffer.reset();
         mSerializationLength = 0;
      }
   }
   wxASSERT(mSeq);
   return *mSeq;
}

Track::Holder NoteTrack::Clone() const
{
   auto duplicate = std::make_shared<NoteTrack>();
   duplicate->Init(*this);
   // The duplicate begins life in serialized state.  Often the duplicate is
   // pushed on the Undo stack.  Then we want to un-serialize it (or a further
   // copy) only on demand after an Undo.
   if (mSeq) {
      SonifyBeginSerialize();
      wxASSERT(!mSerializationBuffer);
      // serialize from this to duplicate's mSerializationBuffer
      void *buffer;
      mSeq->serialize(&buffer,
                      &duplicate->mSerializationLength);
      duplicate->mSerializationBuffer.reset( (char*)buffer );
      SonifyEndSerialize();
   }
   else if (mSerializationBuffer) {
      // Copy already serialized data.
      wxASSERT(!mSeq);
      duplicate->mSerializationLength = this->mSerializationLength;
      duplicate->mSerializationBuffer.reset
         ( safenew char[ this->mSerializationLength ] );
      memcpy( duplicate->mSerializationBuffer.get(),
              this->mSerializationBuffer.get(), this->mSerializationLength );
   }
   else {
      // We are duplicating a default-constructed NoteTrack, and that's okay
   }
   // copy some other fields here
   duplicate->SetBottomNote(mBottomNote);
   duplicate->SetTopNote(mTopNote);
   duplicate->mVisibleChannels = mVisibleChannels;
   duplicate->SetOffset(GetOffset());
#ifdef EXPERIMENTAL_MIDI_OUT
   duplicate->SetVelocity(GetVelocity());
#endif
   return duplicate;
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
   return GetStartTime() + GetSeq().get_real_dur();
}

void NoteTrack::WarpAndTransposeNotes(double t0, double t1,
                                      const TimeWarper &warper,
                                      double semitones)
{
   double offset = this->GetOffset(); // track is shifted this amount
   auto &seq = GetSeq();
   seq.convert_to_seconds(); // make sure time units are right
   t1 -= offset; // adjust time range to compensate for track offset
   t0 -= offset;
   if (t1 > seq.get_dur()) { // make sure t0, t1 are within sequence
      t1 = seq.get_dur();
      if (t0 >= t1) return;
   }
   Alg_iterator iter(mSeq.get(), false);
   iter.begin();
   Alg_event_ptr event;
   while (0 != (event = iter.next()) && event->time < t1) {
      if (event->is_note() && event->time >= t0) {
         event->set_pitch(event->get_pitch() + semitones);
      }
   }
   iter.end();
   // now, use warper to warp the tempo map
   seq.convert_to_beats(); // beats remain the same
   Alg_time_map_ptr map = seq.get_time_map();
   map->insert_beat(t0, map->time_to_beat(t0));
   map->insert_beat(t1, map->time_to_beat(t1));
   int i, len = map->length();
   for (i = 0; i < len; i++) {
      Alg_beat &beat = map->beats[i];
      beat.time = warper.Warp(beat.time + offset) - offset;
   }
   // about to redisplay, so might as well convert back to time now
   seq.convert_to_seconds();
}

// Draws the midi channel toggle buttons within the given rect.
// The rect should be evenly divisible by 4 on both axis.
void NoteTrack::DrawLabelControls
( const NoteTrack *pTrack, wxDC & dc, const wxRect &rect, int highlightedChannel )
{
   dc.SetTextForeground(theTheme.Colour(clrLabelTrackText));
   wxASSERT_MSG(rect.width % 4 == 0, "Midi channel control rect width must be divisible by 4");
   wxASSERT_MSG(rect.height % 4 == 0, "Midi channel control rect height must be divisible by 4");

   auto cellWidth = rect.width / 4;
   auto cellHeight = rect.height / 4;

   wxRect box;
   for (int row = 0; row < 4; row++) {
      for (int col = 0; col < 4; col++) {
         // chanName is the "external" channel number (1-16)
         // used by AColor and button labels
         int chanName = row * 4 + col + 1;

         box.x = rect.x + col * cellWidth;
         box.y = rect.y + row * cellHeight;
         box.width = cellWidth;
         box.height = cellHeight;

         bool visible = pTrack ? pTrack->IsVisibleChan(chanName - 1) : true;
         if (visible) {
            // highlightedChannel counts 0 based
            if ( chanName == highlightedChannel + 1 )
               AColor::LightMIDIChannel(&dc, chanName);
            else
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
            if ( chanName == highlightedChannel + 1 )
               AColor::LightMIDIChannel(&dc, chanName);
            else
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

         wxString text;
         wxCoord w;
         wxCoord h;

         text.Printf(wxT("%d"), chanName);
         dc.GetTextExtent(text, &w, &h);

         dc.DrawText(text, box.x + (box.width - w) / 2, box.y + (box.height - h) / 2);
      }
   }
   dc.SetTextForeground(theTheme.Colour(clrTrackPanelText));
   AColor::MIDIChannel(&dc, 0); // always return with gray color selected
}

int NoteTrack::FindChannel(const wxRect &rect, int mx, int my)
{
   wxASSERT_MSG(rect.width % 4 == 0, "Midi channel control rect width must be divisible by 4");
   wxASSERT_MSG(rect.height % 4 == 0, "Midi channel control rect height must be divisible by 4");

   auto cellWidth = rect.width / 4;
   auto cellHeight = rect.height / 4;

   int col = (mx - rect.x) / cellWidth;
   int row = (my - rect.y) / cellHeight;

   return row * 4 + col;
}


// Handles clicking within the midi controls rect (same as DrawLabelControls).
// This is somewhat oddly written, as these aren't real buttons - they act
// when the mouse goes down; you can't hold it pressed and move off of it.
// Left-clicking toggles a single channel; right-clicking turns off all other channels.
bool NoteTrack::LabelClick(const wxRect &rect, int mx, int my, bool right)
{
   auto channel = FindChannel(rect, mx, my);
   if (right)
      SoloVisibleChan(channel);
   else
      ToggleVisibleChan(channel);

   return true;
}

void NoteTrack::SetSequence(std::unique_ptr<Alg_seq> &&seq)
{
   mSeq = std::move(seq);
}

void NoteTrack::PrintSequence()
{
   FILE *debugOutput;

   debugOutput = fopen("debugOutput.txt", "wt");
   wxFprintf(debugOutput, "Importing MIDI...\n");

   // This is called for debugging purposes.  Do not compute mSeq on demand
   // with GetSeq()
   if (mSeq) {
      int i = 0;

      while(i < mSeq->length()) {
         wxFprintf(debugOutput, "--\n");
         wxFprintf(debugOutput, "type: %c\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type());
         wxFprintf(debugOutput, "time: %f\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->time);
         wxFprintf(debugOutput, "channel: %li\n",
            ((Alg_event_ptr)mSeq->track_list.tracks[i])->chan);

         if(((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type() == wxT('n'))
         {
            wxFprintf(debugOutput, "pitch: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->pitch);
            wxFprintf(debugOutput, "duration: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->dur);
            wxFprintf(debugOutput, "velocity: %f\n",
               ((Alg_note_ptr)mSeq->track_list.tracks[i])->loud);
         }
         else if(((Alg_event_ptr)mSeq->track_list.tracks[i])->get_type() == wxT('n'))
         {
            wxFprintf(debugOutput, "key: %li\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->get_identifier());
            wxFprintf(debugOutput, "attribute type: %c\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type());
            wxFprintf(debugOutput, "attribute: %s\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_name());

            if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('r'))
            {
               wxFprintf(debugOutput, "value: %f\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.r);
            }
            else if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('i')) {
               wxFprintf(debugOutput, "value: %li\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.i);
            }
            else if(((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.attr_type() == wxT('s')) {
               wxFprintf(debugOutput, "value: %s\n", ((Alg_update_ptr)mSeq->track_list.tracks[i])->parameter.s);
            }
            else {}
         }

         i++;
      }
   }
   else {
      wxFprintf(debugOutput, "No sequence defined!\n");
   }

   fclose(debugOutput);
}

Track::Holder NoteTrack::Cut(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   double len = t1-t0;
   //auto delta = -(
      //( std::min( t1, GetEndTime() ) ) - ( std::max( t0, GetStartTime() ) )
   //);

   auto newTrack = std::make_shared<NoteTrack>();

   newTrack->Init(*this);

   auto &seq = GetSeq();
   seq.convert_to_seconds();
   newTrack->mSeq.reset(seq.cut(t0 - GetOffset(), len, false));
   newTrack->SetOffset(0);

   // Not needed
   // Alg_seq::cut seems to handle this
   //AddToDuration( delta );

   // What should be done with the rest of newTrack's members?
   //(mBottomNote,
   // mSerializationBuffer, mSerializationLength, mVisibleChannels)

   return newTrack;
}

Track::Holder NoteTrack::Copy(double t0, double t1, bool) const
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   double len = t1-t0;

   auto newTrack = std::make_shared<NoteTrack>();

   newTrack->Init(*this);

   auto &seq = GetSeq();
   seq.convert_to_seconds();
   newTrack->mSeq.reset(seq.copy(t0 - GetOffset(), len, false));
   newTrack->SetOffset(0);

   // What should be done with the rest of newTrack's members?
   // (mBottomNote, mSerializationBuffer,
   // mSerializationLength, mVisibleChannels)

   return newTrack;
}

bool NoteTrack::Trim(double t0, double t1)
{
   if (t1 < t0)
      return false;
   auto &seq = GetSeq();
   //auto delta = -(
      //( GetEndTime() - std::min( GetEndTime(), t1 ) ) +
      //( std::max(t0, GetStartTime()) - GetStartTime() )
   //);
   seq.convert_to_seconds();
   // DELETE way beyond duration just in case something is out there:
   seq.clear(t1 - GetOffset(), seq.get_dur() + 10000.0, false);
   // Now that stuff beyond selection is cleared, clear before selection:
   seq.clear(0.0, t0 - GetOffset(), false);
   // want starting time to be t0
   SetOffset(t0);

   // Not needed
   // Alg_seq::clear seems to handle this
   //AddToDuration( delta );

   return true;
}

void NoteTrack::Clear(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   double len = t1-t0;

   auto &seq = GetSeq();

   auto offset = GetOffset();
   auto start = t0 - offset;
   if (start < 0.0) {
      // AlgSeq::clear will shift the cleared interval, not changing len, if
      // start is negative.  That's not what we want to happen.
      if (len > -start) {
         seq.clear(0, len + start, false);
         SetOffset(t0);
      }
      else
         SetOffset(offset - len);
   }
   else {
      //auto delta = -(
      //( std::min( t1, GetEndTime() ) ) - ( std::max( t0, GetStartTime() ) )
      //);
      seq.clear(start, len, false);

      // Not needed
      // Alg_seq::clear seems to handle this
      // AddToDuration( delta );
   }
}

void NoteTrack::Paste(double t, const Track *src)
{
   // Paste inserts src at time t. If src has a positive offset,
   // the offset is treated as silence which is also inserted. If
   // the offset is negative, the offset is ignored and the ENTIRE
   // src is inserted (otherwise, we would either lose data from
   // src by not inserting things at negative times, or inserting
   // things at negative times could overlap things already in
   // the destination track).

   //Check that src is a non-NULL NoteTrack
   bool bOk = src && src->TypeSwitch< bool >( [&](const NoteTrack *other) {

      auto myOffset = this->GetOffset();
      if (t < myOffset) {
         // workaround strange behavior described at
         // http://bugzilla.audacityteam.org/show_bug.cgi?id=1735#c3
         SetOffset(t);
         InsertSilence(t, myOffset - t);
      }

      double delta = 0.0;
      auto &seq = GetSeq();
      auto offset = other->GetOffset();
      if ( offset > 0 ) {
         seq.convert_to_seconds();
         seq.insert_silence( t - GetOffset(), offset );
         t += offset;
         // Is this needed or does Alg_seq::insert_silence take care of it?
         //delta += offset;
      }

      // This seems to be needed:
      delta += std::max( 0.0, t - GetEndTime() );

      // This, not:
      //delta += other->GetSeq().get_real_dur();

      seq.paste(t - GetOffset(), &other->GetSeq());

      AddToDuration( delta );

      return true;
   });

   if ( !bOk )
      // THROW_INCONSISTENCY_EXCEPTION; // ?
      (void)0;// intentionally do nothing
}

void NoteTrack::Silence(double t0, double t1)
{
   if (t1 < t0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto len = t1 - t0;

   auto &seq = GetSeq();
   seq.convert_to_seconds();
   // XXX: do we want to set the all param?
   // If it's set, then it seems like notes are silenced if they start or end in the range,
   // otherwise only if they start in the range. --Poke
   seq.silence(t0 - GetOffset(), len, false);
}

void NoteTrack::InsertSilence(double t, double len)
{
   if (len < 0)
      THROW_INCONSISTENCY_EXCEPTION;

   auto &seq = GetSeq();
   seq.convert_to_seconds();
   seq.insert_silence(t - GetOffset(), len);

   // is this needed?
   // AddToDuration( len );
}

#ifdef EXPERIMENTAL_MIDI_OUT
void NoteTrack::SetVelocity(float velocity)
{
   if (mVelocity != velocity) {
      mVelocity = velocity;
      Notify();
   }
}
#endif

// Call this function to manipulate the underlying sequence data. This is
// NOT the function that handles horizontal dragging.
bool NoteTrack::Shift(double t) // t is always seconds
{
   if (t > 0) {
      auto &seq = GetSeq();
      // insert an even number of measures
      seq.convert_to_beats();
      // get initial tempo
      double tempo = seq.get_tempo(0.0);
      double beats_per_measure = seq.get_bar_len(0.0);
      int m = ROUND(t * tempo / beats_per_measure);
      // need at least 1 measure, so if we rounded down to zero, fix it
      if (m == 0) m = 1;
      // compute NEW tempo so that m measures at NEW tempo take t seconds
      tempo = beats_per_measure * m / t; // in beats per second
      seq.insert_silence(0.0, beats_per_measure * m);
      seq.set_tempo(tempo * 60.0 /* bpm */, 0.0, beats_per_measure * m);
      seq.write("afterShift.gro");
   } else if (t < 0) {
      auto &seq = GetSeq();
      seq.convert_to_seconds();
      seq.clear(0, t, true);
   } else { // offset is zero, no modifications
      return false;
   }
   return true;
}

QuantizedTimeAndBeat NoteTrack::NearestBeatTime( double time ) const
{
   // Alg_seq knows nothing about offset, so remove offset time
   double seq_time = time - GetOffset();
   double beat;
   auto &seq = GetSeq();
   seq_time = seq.nearest_beat_time(seq_time, &beat);
   // add the offset back in to get "actual" audacity track time
   return { seq_time + GetOffset(), beat };
}

Track::Holder NoteTrack::PasteInto( AudacityProject & ) const
{
   auto pNewTrack = std::make_shared<NoteTrack>();
   pNewTrack->Paste(0.0, this);
   return pNewTrack;
}

auto NoteTrack::GetIntervals() const -> ConstIntervals
{
   ConstIntervals results;
   results.emplace_back( GetStartTime(), GetEndTime() );
   return results;
}

auto NoteTrack::GetIntervals() -> Intervals
{
   Intervals results;
   results.emplace_back( GetStartTime(), GetEndTime() );
   return results;
}

void NoteTrack::AddToDuration( double delta )
{
   auto &seq = GetSeq();
#if 0
   // PRL:  Would this be better ?
   seq.set_real_dur( seq.get_real_dur() + delta );
#else
   seq.convert_to_seconds();
   seq.set_dur( seq.get_dur() + delta );
#endif
}

bool NoteTrack::StretchRegion
   ( QuantizedTimeAndBeat t0, QuantizedTimeAndBeat t1, double newDur )
{
   auto &seq = GetSeq();
   bool result = seq.stretch_region( t0.second, t1.second, newDur );
   if (result) {
      const auto oldDur = t1.first - t0.first;
      AddToDuration( newDur - oldDur );
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

Alg_seq *NoteTrack::MakeExportableSeq(std::unique_ptr<Alg_seq> &cleanup) const
{
   cleanup.reset();
   double offset = GetOffset();
   if (offset == 0)
      return &GetSeq();
   // make a copy, deleting events that are shifted before time 0
   double start = -offset;
   if (start < 0) start = 0;
   // notes that begin before "start" are not included even if they
   // extend past "start" (because "all" parameter is set to false)
   cleanup.reset( GetSeq().copy(start, GetSeq().get_dur() - start, false) );
   auto seq = cleanup.get();
   if (offset > 0) {
      {
         // swap cleanup and mSeq so that Shift operates on the NEW copy
         swap( this->mSeq, cleanup );
         auto cleanup2 = finally( [&] { swap( this->mSeq, cleanup ); } );

         const_cast< NoteTrack *>( this )->Shift(offset);
      }
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
      auto &mySeq = GetSeq();
      // if offset is negative, it might not be a multiple of beats, but
      // we want to preserve the relative positions of measures. I.e. we
      // should shift barlines and time signatures as well as notes.
      // Insert a time signature at the first bar-line if necessary.

      // Translate start from seconds to beats and call it beat:
      double beat = mySeq.get_time_map()->time_to_beat(start);
      // Find the time signature in mySeq in effect at start (beat):
      int i = mySeq.time_sig.find_beat(beat);
      // i is where you would insert a NEW time sig at beat,
      // Case 1: beat coincides with a time sig at i. Time signature
      // at beat means that there is a barline at beat, so when beat
      // is shifted to 0, the relative barline positions are preserved
      if (mySeq.time_sig.length() > 0 &&
          within(beat, mySeq.time_sig[i].beat, ALG_EPS)) {
         // beat coincides with time signature change, so offset must
         // be a multiple of beats
         /* do nothing */ ;
      // Case 2: there is no time signature before beat.
      } else if (i == 0 && (mySeq.time_sig.length() == 0 ||
                            mySeq.time_sig[i].beat > beat)) {
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
         Alg_time_sig_ptr tsp = &(mySeq.time_sig[i]);
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


bool NoteTrack::ExportMIDI(const wxString &f) const
{
   std::unique_ptr<Alg_seq> cleanup;
   auto seq = MakeExportableSeq(cleanup);
   bool rslt = seq->smf_write(f.mb_str());
   return rslt;
}

bool NoteTrack::ExportAllegro(const wxString &f) const
{
   double offset = GetOffset();
   auto in_seconds = ImportExportPrefs::AllegroStyleSetting.ReadEnum();
   auto &seq = GetSeq();
   if (in_seconds) {
       seq.convert_to_seconds();
   } else {
       seq.convert_to_beats();
   }
   return seq.write(f.mb_str(), offset);
}


namespace {
bool IsValidVisibleChannels(const int nValue)
{
    return (nValue >= 0 && nValue < (1 << 16));
}
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
         if (this->Track::HandleCommonXMLAttribute(attr, strValue))
            ;
         else if (this->NoteTrackBase::HandleXMLAttribute(attr, value))
         {}
         else if (!wxStrcmp(attr, wxT("offset")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            SetOffset(dblValue);
         else if (!wxStrcmp(attr, wxT("visiblechannels"))) {
             if (!XMLValueChecker::IsGoodInt(strValue) ||
                 !strValue.ToLong(&nValue) ||
                 !IsValidVisibleChannels(nValue))
                 return false;
             mVisibleChannels = nValue;
         }
#ifdef EXPERIMENTAL_MIDI_OUT
         else if (!wxStrcmp(attr, wxT("velocity")) &&
                  XMLValueChecker::IsGoodString(strValue) &&
                  Internat::CompatibleToDouble(strValue, &dblValue))
            mVelocity = (float) dblValue;
#endif
         else if (!wxStrcmp(attr, wxT("bottomnote")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetBottomNote(nValue);
         else if (!wxStrcmp(attr, wxT("topnote")) &&
                  XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
            SetTopNote(nValue);
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

void NoteTrack::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   std::ostringstream data;
   Track::Holder holder;
   const NoteTrack *saveme = this;
   if (!mSeq) {
      // replace saveme with an (unserialized) duplicate, which is
      // destroyed at end of function.
      holder = Clone();
      saveme = static_cast<NoteTrack*>(holder.get());
   }
   saveme->GetSeq().write(data, true);
   xmlFile.StartTag(wxT("notetrack"));
   saveme->Track::WriteCommonXMLAttributes( xmlFile );
   this->NoteTrackBase::WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("offset"), saveme->GetOffset());
   xmlFile.WriteAttr(wxT("visiblechannels"), saveme->mVisibleChannels);

#ifdef EXPERIMENTAL_MIDI_OUT
   xmlFile.WriteAttr(wxT("velocity"), (double) saveme->mVelocity);
#endif
   xmlFile.WriteAttr(wxT("bottomnote"), saveme->mBottomNote);
   xmlFile.WriteAttr(wxT("topnote"), saveme->mTopNote);
   xmlFile.WriteAttr(wxT("data"), wxString(data.str().c_str(), wxConvUTF8));
   xmlFile.EndTag(wxT("notetrack"));
}

void NoteTrack::SetBottomNote(int note)
{
   if (note < MinPitch)
      note = MinPitch;
   else if (note > 96)
      note = 96;

   wxCHECK(note <= mTopNote, );

   mBottomNote = note;
}

void NoteTrack::SetTopNote(int note)
{
   if (note > MaxPitch)
      note = MaxPitch;

   wxCHECK(note >= mBottomNote, );

   mTopNote = note;
}

void NoteTrack::SetNoteRange(int note1, int note2)
{
   // Bounds check
   if (note1 > MaxPitch)
      note1 = MaxPitch;
   else if (note1 < MinPitch)
      note1 = MinPitch;
   if (note2 > MaxPitch)
      note2 = MaxPitch;
   else if (note2 < MinPitch)
      note2 = MinPitch;
   // Swap to ensure ordering
   if (note2 < note1) { auto tmp = note1; note1 = note2; note2 = tmp; }

   mBottomNote = note1;
   mTopNote = note2;
}

void NoteTrack::ShiftNoteRange(int offset)
{
   // Ensure everything stays in bounds
   if (mBottomNote + offset < MinPitch || mTopNote + offset > MaxPitch)
       return;

   mBottomNote += offset;
   mTopNote += offset;
}

#if 0
void NoteTrack::StartVScroll()
{
    mStartBottomNote = mBottomNote;
}

void NoteTrack::VScroll(int start, int end)
{
    int ph = GetPitchHeight();
    int delta = ((end - start) + ph / 2) / ph;
    ShiftNoteRange(delta);
}
#endif

void NoteTrack::Zoom(const wxRect &rect, int y, float multiplier, bool center)
{
   NoteTrackDisplayData data = NoteTrackDisplayData(this, rect);
   int clickedPitch = data.YToIPitch(y);
   int extent = mTopNote - mBottomNote + 1;
   int newExtent = (int) (extent / multiplier);
   float position;
   if (center) {
      // center the pitch that the user clicked on
      position = .5;
   } else {
      // align to keep the pitch that the user clicked on in the same place
      position = extent / (clickedPitch - mBottomNote);
   }
   int newBottomNote = clickedPitch - (newExtent * position);
   int newTopNote = clickedPitch + (newExtent * (1 - position));
   SetNoteRange(newBottomNote, newTopNote);
}


void NoteTrack::ZoomTo(const wxRect &rect, int start, int end)
{
   wxRect trackRect(0, rect.GetY(), 1, rect.GetHeight());
   NoteTrackDisplayData data = NoteTrackDisplayData(this, trackRect);
   int pitch1 = data.YToIPitch(start);
   int pitch2 = data.YToIPitch(end);
   if (pitch1 == pitch2) {
      // Just zoom in instead of zooming to show only one note
      Zoom(rect, start, 1, true);
      return;
   }
   // It's fine for this to be in either order
   SetNoteRange(pitch1, pitch2);
}

void NoteTrack::ZoomAllNotes()
{
   Alg_iterator iterator( &GetSeq(), false );
   iterator.begin();
   Alg_event_ptr evt;

   // Go through all of the notes, finding the minimum and maximum value pitches.
   bool hasNotes = false;
   int minPitch = MaxPitch;
   int maxPitch = MinPitch;

   while (NULL != (evt = iterator.next())) {
      if (evt->is_note()) {
         int pitch = (int) evt->get_pitch();
         hasNotes = true;
         if (pitch < minPitch)
            minPitch = pitch;
         if (pitch > maxPitch)
            maxPitch = pitch;
      }
   }

   if (!hasNotes) {
      // Semi-arbitrary default values:
      minPitch = 48;
      maxPitch = 72;
   }

   SetNoteRange(minPitch, maxPitch);
}

NoteTrackDisplayData::NoteTrackDisplayData(const NoteTrack* track, const wxRect &r)
{
   auto span = track->GetTopNote() - track->GetBottomNote() + 1; // + 1 to make sure it includes both

   mMargin = std::min((int) (r.height / (float)(span)) / 2, r.height / 4);

   // Count the number of dividers between B/C and E/F
   int numC = 0, numF = 0;
   auto botOctave = track->GetBottomNote() / 12, botNote = track->GetBottomNote() % 12;
   auto topOctave = track->GetTopNote() / 12, topNote = track->GetTopNote() % 12;
   if (topOctave == botOctave)
   {
      if (botNote == 0) numC = 1;
      if (topNote <= 5) numF = 1;
   }
   else
   {
      numC = topOctave - botOctave;
      numF = topOctave - botOctave - 1;
      if (botNote == 0) numC++;
      if (botNote <= 5) numF++;
      if (topOctave <= 5) numF++;
   }
   // Effective space, excluding the margins and the lines between some notes
   auto effectiveHeight = r.height - (2 * (mMargin + 1)) - numC - numF;
   // Guaranteed that both the bottom and top notes will be visible
   // (assuming that the clamping below does not happen)
   mPitchHeight = effectiveHeight / ((float) span);

   if (mPitchHeight < MinPitchHeight)
      mPitchHeight = MinPitchHeight;
   if (mPitchHeight > MaxPitchHeight)
      mPitchHeight = MaxPitchHeight;

   mBottom = r.y + r.height - GetNoteMargin() - 1 - GetPitchHeight(1) +
            botOctave * GetOctaveHeight() + GetNotePos(botNote);
}

int NoteTrackDisplayData::IPitchToY(int p) const
{ return mBottom - (p / 12) * GetOctaveHeight() - GetNotePos(p % 12); }

int NoteTrackDisplayData::YToIPitch(int y) const
{
   y = mBottom - y; // pixels above pitch 0
   int octave = (y / GetOctaveHeight());
   y -= octave * GetOctaveHeight();
   // result is approximate because C and G are one pixel taller than
   // mPitchHeight.
   // Poke 1-13-18: However in practice this seems not to be an issue,
   // as long as we use mPitchHeight and not the rounded version
   return (y / mPitchHeight) + octave * 12;
}

const float NoteTrack::ZoomStep = powf( 2.0f, 0.25f );

#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include "AudioIOBase.h"
#include "portmidi.h"

// FIXME: When EXPERIMENTAL_MIDI_IN is added (eventually) this should also be enabled -- Poke
wxString GetMIDIDeviceInfo()
{
   wxStringOutputStream o;
   wxTextOutputStream s(o, wxEOL_UNIX);

   if (AudioIOBase::Get()->IsStreamActive()) {
      return XO("Stream is active ... unable to gather information.\n")
         .Translation();
   }


   // XXX: May need to trap errors as with the normal device info
   int recDeviceNum = Pm_GetDefaultInputDeviceID();
   int playDeviceNum = Pm_GetDefaultOutputDeviceID();
   int cnt = Pm_CountDevices();

   // PRL:  why only into the log?
   wxLogDebug(wxT("PortMidi reports %d MIDI devices"), cnt);

   s << wxT("==============================\n");
   s << XO("Default recording device number: %d\n").Format( recDeviceNum );
   s << XO("Default playback device number: %d\n").Format( playDeviceNum );

   auto recDevice = MIDIRecordingDevice.Read();
   auto playDevice = MIDIPlaybackDevice.Read();

   // This gets info on all available audio devices (input and output)
   if (cnt <= 0) {
      s << XO("No devices found\n");
      return o.GetString();
   }

   for (int i = 0; i < cnt; i++) {
      s << wxT("==============================\n");

      const PmDeviceInfo* info = Pm_GetDeviceInfo(i);
      if (!info) {
         s << XO("Device info unavailable for: %d\n").Format( i );
         continue;
      }

      wxString name = wxSafeConvertMB2WX(info->name);
      wxString hostName = wxSafeConvertMB2WX(info->interf);

      s << XO("Device ID: %d\n").Format( i );
      s << XO("Device name: %s\n").Format( name );
      s << XO("Host name: %s\n").Format( hostName );
      /* i18n-hint: Supported, meaning made available by the system */
      s << XO("Supports output: %d\n").Format( info->output );
      /* i18n-hint: Supported, meaning made available by the system */
      s << XO("Supports input: %d\n").Format( info->input );
      s << XO("Opened: %d\n").Format( info->opened );

      if (name == playDevice && info->output)
         playDeviceNum = i;

      if (name == recDevice && info->input)
         recDeviceNum = i;

      // XXX: This is only done because the same was applied with PortAudio
      // If PortMidi returns -1 for the default device, use the first one
      if (recDeviceNum < 0 && info->input){
         recDeviceNum = i;
      }
      if (playDeviceNum < 0 && info->output){
         playDeviceNum = i;
      }
   }

   bool haveRecDevice = (recDeviceNum >= 0);
   bool havePlayDevice = (playDeviceNum >= 0);

   s << wxT("==============================\n");
   if (haveRecDevice)
      s << XO("Selected MIDI recording device: %d - %s\n").Format( recDeviceNum, recDevice );
   else
      s << XO("No MIDI recording device found for '%s'.\n").Format( recDevice );

   if (havePlayDevice)
      s << XO("Selected MIDI playback device: %d - %s\n").Format( playDeviceNum, playDevice );
   else
      s << XO("No MIDI playback device found for '%s'.\n").Format( playDevice );

   // Mention our conditional compilation flags for Alpha only
#ifdef IS_ALPHA

   // Not internationalizing these alpha-only messages
   s << wxT("==============================\n");
#ifdef EXPERIMENTAL_MIDI_OUT
   s << wxT("EXPERIMENTAL_MIDI_OUT is enabled\n");
#else
   s << wxT("EXPERIMENTAL_MIDI_OUT is NOT enabled\n");
#endif
#ifdef EXPERIMENTAL_MIDI_IN
   s << wxT("EXPERIMENTAL_MIDI_IN is enabled\n");
#else
   s << wxT("EXPERIMENTAL_MIDI_IN is NOT enabled\n");
#endif

#endif

   return o.GetString();
}

StringSetting MIDIPlaybackDevice{ L"/MidiIO/PlaybackDevice", L"" };
StringSetting MIDIRecordingDevice{ L"/MidiIO/RecordingDevice", L"" };
IntSetting MIDISynthLatency_ms{ L"/MidiIO/SynthLatency", 5 };

#endif // USE_MIDI
