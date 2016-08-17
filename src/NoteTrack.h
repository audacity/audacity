/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>
#include "Audacity.h"
#include "Experimental.h"
#include "Track.h"
#include "effects/TimeWarper.h"

#if defined(USE_MIDI)

#include "allegro.h"

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

class DirManager;
class Alg_seq;   // from "allegro.h"

class AUDACITY_DLL_API NoteTrack final : public Track {
 public:
   friend class TrackArtist;

   NoteTrack(const std::shared_ptr<DirManager> &projDirManager);
   virtual ~NoteTrack();

   using Holder = std::unique_ptr<NoteTrack>;
   Track::Holder Duplicate() const override;

   int GetKind() const override { return Note; }

   double GetOffset() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   void WarpAndTransposeNotes(double t0, double t1,
                              const TimeWarper &warper, double semitones);

   int DrawLabelControls(wxDC & dc, wxRect & r);
   bool LabelClick(wxRect & r, int x, int y, bool right);

   void SetSequence(std::unique_ptr<Alg_seq> &&seq);
   Alg_seq* GetSequence();
   void PrintSequence();

   int GetVisibleChannels();

   Alg_seq *MakeExportableSeq(std::unique_ptr<Alg_seq> &cleanup);
   bool ExportMIDI(const wxString &f);
   bool ExportAllegro(const wxString &f);

/* REQUIRES PORTMIDI */
//   int GetLastMidiPosition() const { return mLastMidiPosition; }
//   void SetLastMidiPosition( int position )
//   {
//      mLastMidiPosition = position;
//   }

   // High-level editing
   Track::Holder Cut  (double t0, double t1) override;
   Track::Holder Copy (double t0, double t1) const override;
   bool Trim (double t0, double t1) /* not override */;
   bool Clear(double t0, double t1) override;
   bool Paste(double t, const Track *src) override;
   bool Shift(double t) /* not override */;

#ifdef EXPERIMENTAL_MIDI_OUT
   float GetGain() const { return mGain; }
   void SetGain(float gain) { mGain = gain; }
#endif

   double NearestBeatTime(double time, double *beat);
   bool StretchRegion(double b0, double b1, double dur);

   int GetBottomNote() const { return mBottomNote; }
   int GetPitchHeight() const { return mPitchHeight; }
   void SetPitchHeight(int h) { mPitchHeight = h; }
   void ZoomOut(int y) { Zoom(y, -1); }
   void ZoomIn(int y) { Zoom(y, 1); }
   void Zoom(int centerY, int amount);
   void ZoomTo(int start, int end);
   int GetNoteMargin() const { return (mPitchHeight + 1) / 2; }
   int GetOctaveHeight() const { return mPitchHeight * 12 + 2; }
   // call this once before a series of calls to IPitchToY(). It
   // sets mBottom to offset of octave 0 so that mBottomNote
   // is located at r.y + r.height - (GetNoteMargin() + 1 + mPitchHeight)
   void PrepareIPitchToY(const wxRect &r) const {
       mBottom = r.y + r.height - GetNoteMargin() - 1 - mPitchHeight +
          (mBottomNote / 12) * GetOctaveHeight() +
          GetNotePos(mBottomNote % 12);
   }
   // IPitchToY returns Y coordinate of top of pitch p
   int IPitchToY(int p) const {
      return mBottom - (p / 12) * GetOctaveHeight() - GetNotePos(p % 12);
   }
   // compute the window coordinate of the bottom of an octave: This is
   // the bottom of the line separating B and C.
   int GetOctaveBottom(int oct) const {
      return IPitchToY(oct * 12) + mPitchHeight + 1;
   }
   // Y coordinate for given floating point pitch (rounded to int)
   int PitchToY(double p) const {
      return IPitchToY((int) (p + 0.5));
   }
   // Integer pitch corresponding to a Y coordinate
   int YToIPitch(int y);
   // map pitch class number (0-11) to pixel offset from bottom of octave
   // (the bottom of the black line between B and C) to the top of the
   // note. Note extra pixel separates B(11)/C(0) and E(4)/F(5).
   int GetNotePos(int p) const { return 1 + mPitchHeight * (p + 1) + (p > 4); }
   // get pixel offset to top of ith black key note
   int GetBlackPos(int i) const { return GetNotePos(i * 2 + 1 + (i > 1)); }
   // GetWhitePos tells where to draw lines between keys as an offset from
   // GetOctaveBottom. GetWhitePos(0) returns 1, which matches the location
   // of the line separating B and C
   int GetWhitePos(int i) const { return 1 + (i * GetOctaveHeight()) / 7; }
   void SetBottomNote(int note)
   {
      if (note < 0)
         note = 0;
      else if (note > 96)
         note = 96;

      mBottomNote = note;
   }
   // Vertical scrolling is performed by dragging the keyboard at
   // left of track. Protocol is call StartVScroll, then update by
   // calling VScroll with original and final mouse position.
   // These functions are not used -- instead, zooming/dragging works like
   // audio track zooming/dragging. The vertical scrolling is nice however,
   // so I left these functions here for possible use in the future.
   void StartVScroll();
   void VScroll(int start, int end);

#ifdef EXPERIMENTAL_MIDI_OUT
   wxRect GetGainPlacementRect() const { return mGainPlacementRect; }
   void SetGainPlacementRect(const wxRect &r) { mGainPlacementRect = r; }
#endif

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(XMLWriter &xmlFile) override;

   // channels are numbered as integers 0-15, visible channels
   // (mVisibleChannels) is a bit set. Channels are displayed as
   // integers 1-16.
#define CHANNEL_BIT(c) (1 << (c))
#define ALL_CHANNELS 0xFFFF
   bool IsVisibleChan(int c) {
      return (mVisibleChannels & CHANNEL_BIT(c)) != 0;
   }
   void SetVisibleChan(int c) { mVisibleChannels |= CHANNEL_BIT(c); }
   void ClearVisibleChan(int c) { mVisibleChannels &= ~CHANNEL_BIT(c); }
   void ToggleVisibleChan(int c) { mVisibleChannels ^= CHANNEL_BIT(c); }
 private:
   std::unique_ptr<Alg_seq> mSeq; // NULL means no sequence
   // when Duplicate() is called, assume that it is to put a copy
   // of the track into the undo stack or to redo/copy from the
   // stack to the project object. We want copies to the stack
   // to be serialized (therefore compact) representations, so
   // copy will set mSeq to NULL and serialize to the following
   // variables. If this design is correct, the track will be
   // duplicated again (in the event of redo) back to the project
   // at which point we will unserialize the data back to the
   // mSeq variable. (TrackArtist should check to make sure this
   // flip-flop from mSeq to mSerializationBuffer happened an
   // even number of times, otherwise mSeq will be NULL).
   mutable char *mSerializationBuffer; // NULL means no buffer
   long mSerializationLength;

#ifdef EXPERIMENTAL_MIDI_OUT
   float mGain; // velocity offset
#endif

   // mBottom is the Y offset of pitch 0 (normally off screen)
   mutable int mBottom;
   int mBottomNote;
   int mStartBottomNote;
   int mPitchHeight;
   int mVisibleChannels; // bit set of visible channels
   int mLastMidiPosition;
   wxRect mGainPlacementRect;
};

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


#endif
