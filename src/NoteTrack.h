/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include "Audacity.h" // for USE_* macros

#include "Experimental.h"

#include <utility>
#include "Track.h"
#include "effects/TimeWarper.h"

#if defined(USE_MIDI)

#include "../lib-src/header-substitutes/allegro.h"

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

using NoteTrackBase =
#ifdef EXPERIMENTAL_MIDI_OUT
   PlayableTrack
#else
   AudioTrack
#endif
   ;

using QuantizedTimeAndBeat = std::pair< double, double >;

class StretchHandle;

class AUDACITY_DLL_API NoteTrack final
   : public NoteTrackBase
{
 public:
   NoteTrack(const std::shared_ptr<DirManager> &projDirManager);
   virtual ~NoteTrack();

   std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject, int currentTool, bool bMultiTool)
      override;

   using Holder = std::shared_ptr<NoteTrack>;
   Track::Holder Duplicate() const override;

   double GetOffset() const override;
   double GetStartTime() const override;
   double GetEndTime() const override;

   void DoSetHeight(int h) override;

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

   int GetBottomNote() const { return mBottomNote; }
   int GetPitchHeight(int factor) const
   { return std::max(1, (int)(factor * mPitchHeight)); }
   void SetPitchHeight(int rectHeight, float h)
   {
      // Impose certain zoom limits
      auto octavePadding = 2 * 10; // 10 octaves times 2 single-pixel seperations per pixel
      auto availableHeight = rectHeight - octavePadding;
      auto numNotes = 128.f;
      auto minSpacePerNote =
         std::max((float)MinPitchHeight, availableHeight / numNotes);
      mPitchHeight =
         std::max(minSpacePerNote,
                  std::min((float)MaxPitchHeight, h));
   }
   /// Zooms out a constant factor (subject to zoom limits)
   void ZoomOut(const wxRect &rect, int y) { Zoom(rect, y, 1.0f / ZoomStep, true); }
   /// Zooms in a contant factor (subject to zoom limits)
   void ZoomIn(const wxRect &rect, int y) { Zoom(rect, y, ZoomStep, true); }
   /// Zoom the note track around y.
   /// If center is true, the result will be centered at y.
   void Zoom(const wxRect &rect, int y, float multiplier, bool center);
   void ZoomTo(const wxRect &rect, int start, int end);
   int GetNoteMargin(int height) const
   { return std::min(height / 4, (GetPitchHeight(1) + 1) / 2); }
   int GetOctaveHeight() const { return GetPitchHeight(12) + 2; }
   // call this once before a series of calls to IPitchToY(). It
   // sets mBottom to offset of octave 0 so that mBottomNote
   // is located at r.y + r.height - (GetNoteMargin() + 1 + GetPitchHeight())
   void PrepareIPitchToY(const wxRect &r) const {
       mBottom =
         r.y + r.height - GetNoteMargin(r.height) - 1 - GetPitchHeight(1) +
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
      return IPitchToY(oct * 12) + GetPitchHeight(1) + 1;
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
   int GetNotePos(int p) const
   { return 1 + GetPitchHeight(p + 1) + (p > 4); }
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

   // mBottom is the Y offset of pitch 0 (normally off screen)
   mutable int mBottom;
   int mBottomNote;
   int mStartBottomNote;

   // Remember continuous variation for zooming,
   // but it is rounded off whenever drawing:
   float mPitchHeight;

   enum { MinPitchHeight = 1, MaxPitchHeight = 25 };
   static const float ZoomStep;

   int mVisibleChannels; // bit set of visible channels

   std::weak_ptr<StretchHandle> mStretchHandle;

protected:
   std::shared_ptr<TrackControls> DoGetControls() override;
   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;
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
