/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__





#include <utility>
#include "Prefs.h"
#include "Track.h"

#if defined(USE_MIDI)

// define this switch to play MIDI during redisplay to sonify run times
// Note that if SONIFY is defined, the default MIDI device will be opened
// and may block normal MIDI playback.
//#define SONIFY 1

#ifdef SONIFY

#define SONFNS(name) \
   void Begin ## name(); \
   void End ## name();

SONFNS(NoteBackground)
SONFNS(NoteForeground)
SONFNS(Measures)
SONFNS(Serialize)
SONFNS(Unserialize)
SONFNS(ModifyState)
SONFNS(AutoSave)

#undef SONFNS

#endif

class wxDC;
class wxRect;

class Alg_seq;   // from "allegro.h"

using NoteTrackBase =
#ifdef EXPERIMENTAL_MIDI_OUT
   PlayableTrack
#else
   AudioTrack
#endif
   ;

using QuantizedTimeAndBeat = std::pair< double, double >;

class StretchHandle;
class TimeWarper;

class AUDACITY_DLL_API NoteTrack final
   : public NoteTrackBase
{
public:
   NoteTrack();
   virtual ~NoteTrack();

   using Holder = std::shared_ptr<NoteTrack>;
   
private:
   Track::Holder Clone() const override;

public:
   double GetOffset() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   Alg_seq &GetSeq() const;

   void WarpAndTransposeNotes(double t0, double t1,
                              const TimeWarper &warper, double semitones);

   static void DrawLabelControls
      ( const NoteTrack *pTrack, wxDC & dc, const wxRect &rect,
        int highlightedChannel = -1 );
   int FindChannel(const wxRect &rect, int mx, int my);
   bool LabelClick(const wxRect &rect, int x, int y, bool right);

   void SetSequence(std::unique_ptr<Alg_seq> &&seq);
   void PrintSequence();

   Alg_seq *MakeExportableSeq(std::unique_ptr<Alg_seq> &cleanup) const;
   bool ExportMIDI(const wxString &f) const;
   bool ExportAllegro(const wxString &f) const;

   // High-level editing
   Track::Holder Cut  (double t0, double t1) override;
   Track::Holder Copy (double t0, double t1, bool forClipboard = true) const override;
   bool Trim (double t0, double t1) /* not override */;
   void Clear(double t0, double t1) override;
   void Paste(double t, const Track *src) override;
   void Silence(double t0, double t1) override;
   void InsertSilence(double t, double len) override;
   bool Shift(double t) /* not override */;

#ifdef EXPERIMENTAL_MIDI_OUT
   float GetVelocity() const { return mVelocity; }
   void SetVelocity(float velocity);
#endif

   QuantizedTimeAndBeat NearestBeatTime( double time ) const;
   bool StretchRegion
      ( QuantizedTimeAndBeat t0, QuantizedTimeAndBeat t1, double newDur );

   /// Gets the current bottom note (a pitch)
   int GetBottomNote() const { return mBottomNote; }
   /// Gets the current top note (a pitch)
   int GetTopNote() const { return mTopNote; }
   /// Sets the bottom note (a pitch), making sure that it is never greater than the top note.
   void SetBottomNote(int note);
   /// Sets the top note (a pitch), making sure that it is never less than the bottom note.
   void SetTopNote(int note);
   /// Sets the top and bottom note (both pitches) automatically, swapping them if needed.
   void SetNoteRange(int note1, int note2);

   /// Zooms so that all notes are visible
   void ZoomAllNotes();
   /// Zooms so that the entire track is visible
   void ZoomMaxExtent() { SetNoteRange(MinPitch, MaxPitch); }
   /// Shifts all notes vertically by the given pitch
   void ShiftNoteRange(int offset);

   /// Zooms out a constant factor (subject to zoom limits)
   void ZoomOut(const wxRect &rect, int y) { Zoom(rect, y, 1.0f / ZoomStep, true); }
   /// Zooms in a constant factor (subject to zoom limits)
   void ZoomIn(const wxRect &rect, int y) { Zoom(rect, y, ZoomStep, true); }
   /// Zoom the note track around y.
   /// If center is true, the result will be centered at y.
   void Zoom(const wxRect &rect, int y, float multiplier, bool center);
   void ZoomTo(const wxRect &rect, int start, int end);

#if 0
   // Vertical scrolling is performed by dragging the keyboard at
   // left of track. Protocol is call StartVScroll, then update by
   // calling VScroll with original and final mouse position.
   // These functions are not used -- instead, zooming/dragging works like
   // audio track zooming/dragging. The vertical scrolling is nice however,
   // so I left these functions here for possible use in the future.
   void StartVScroll();
   void VScroll(int start, int end);
#endif

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) const override;

   // channels are numbered as integers 0-15, visible channels
   // (mVisibleChannels) is a bit set. Channels are displayed as
   // integers 1-16.

   // Allegro's data structure does not restrict channels to 16.
   // Since there is not way to select more than 16 channels,
   // map all channel numbers mod 16. This will have no effect
   // on MIDI files, but it will allow users to at least select
   // all channels on non-MIDI event sequence data.
#define NUM_CHANNELS 16
   // Bitmask with all NUM_CHANNELS bits set
#define ALL_CHANNELS (1 << NUM_CHANNELS) - 1
#define CHANNEL_BIT(c) (1 << (c % NUM_CHANNELS))
   bool IsVisibleChan(int c) const {
      return (mVisibleChannels & CHANNEL_BIT(c)) != 0;
   }
   void SetVisibleChan(int c) { mVisibleChannels |= CHANNEL_BIT(c); }
   void ClearVisibleChan(int c) { mVisibleChannels &= ~CHANNEL_BIT(c); }
   void ToggleVisibleChan(int c) { mVisibleChannels ^= CHANNEL_BIT(c); }
   // Solos the given channel.  If it's the only channel visible, all channels
   // are enabled; otherwise, it is set to the only visible channel.
   void SoloVisibleChan(int c) {
      if (mVisibleChannels == CHANNEL_BIT(c))
         mVisibleChannels = ALL_CHANNELS;
      else
         mVisibleChannels = CHANNEL_BIT(c);
   }

   Track::Holder PasteInto( AudacityProject & ) const override;

   ConstIntervals GetIntervals() const override;
   Intervals GetIntervals() override;

 private:

   TrackKind GetKind() const override { return TrackKind::Note; }

   void AddToDuration( double delta );

   // These are mutable to allow NoteTrack to switch details of representation
   // in logically const methods
   // At most one of the two pointers is not null at any time.
   // Both are null in a newly constructed NoteTrack.
   mutable std::unique_ptr<Alg_seq> mSeq;
   mutable std::unique_ptr<char[]> mSerializationBuffer;
   mutable long mSerializationLength;

#ifdef EXPERIMENTAL_MIDI_OUT
   float mVelocity; // velocity offset
#endif

   int mBottomNote, mTopNote;
#if 0
   // Also unused from vertical scrolling
   int mStartBottomNote;
#endif

   // Remember continuous variation for zooming,
   // but it is rounded off whenever drawing:
   float mPitchHeight;

   enum { MinPitch = 0, MaxPitch = 127 };
   static const float ZoomStep;

   int mVisibleChannels; // bit set of visible channels

   std::weak_ptr<StretchHandle> mStretchHandle;
};

/// Data used to display a note track
class NoteTrackDisplayData {
private:
   float mPitchHeight;
   // mBottom is the Y offset of pitch 0 (normally off screen)
   // Used so that mBottomNote is located at
   // mY + mHeight - (GetNoteMargin() + 1 + GetPitchHeight())
   int mBottom;
   int mMargin;

   enum { MinPitchHeight = 1, MaxPitchHeight = 25 };
public:
   NoteTrackDisplayData(const NoteTrack* track, const wxRect &r);

   int GetPitchHeight(int factor) const
   { return std::max(1, (int)(factor * mPitchHeight)); }
   int GetNoteMargin() const { return mMargin; };
   int GetOctaveHeight() const { return GetPitchHeight(12) + 2; }
   // IPitchToY returns Y coordinate of top of pitch p
   int IPitchToY(int p) const;
   // compute the window coordinate of the bottom of an octave: This is
   // the bottom of the line separating B and C.
   int GetOctaveBottom(int oct) const {
      return IPitchToY(oct * 12) + GetPitchHeight(1) + 1;
   }
   // Y coordinate for given floating point pitch (rounded to int)
   int PitchToY(double p) const {
      return IPitchToY((int) (p + 0.5));
   }
   // Integer pitch corresponding to a Y coordinate
   int YToIPitch(int y) const;
   // map pitch class number (0-11) to pixel offset from bottom of octave
   // (the bottom of the black line between B and C) to the top of the
   // note. Note extra pixel separates B(11)/C(0) and E(4)/F(5).
   int GetNotePos(int p) const
   { return 1 + GetPitchHeight(p + 1) + (p > 4); }
   // get pixel offset to top of ith black key note
   int GetBlackPos(int i) const { return GetNotePos(i * 2 + 1 + (i > 1)); }
   // GetWhitePos tells where to draw lines between keys as an offset from
   // GetOctaveBottom. GetWhitePos(0) returns 1, which matches the location
   // of the line separating B and C
   int GetWhitePos(int i) const { return 1 + (i * GetOctaveHeight()) / 7; }
};

extern AUDACITY_DLL_API StringSetting MIDIPlaybackDevice;
extern AUDACITY_DLL_API StringSetting MIDIRecordingDevice;
extern AUDACITY_DLL_API IntSetting MIDISynthLatency_ms;

#endif // USE_MIDI

#ifndef SONIFY
// no-ops:
#define SonifyBeginSonification()
#define SonifyEndSonification()
#define SonifyBeginNoteBackground()
#define SonifyEndNoteBackground()
#define SonifyBeginNoteForeground()
#define SonifyEndNoteForeground()
#define SonifyBeginMeasures()
#define SonifyEndMeasures()
#define SonifyBeginSerialize()
#define SonifyEndSerialize()
#define SonifyBeginUnserialize()
#define SonifyEndUnserialize()
#define SonifyBeginAutoSave()
#define SonifyEndAutoSave()
#define SonifyBeginModifyState()
#define SonifyEndModifyState()
#endif


AUDACITY_DLL_API wxString GetMIDIDeviceInfo();

#endif
