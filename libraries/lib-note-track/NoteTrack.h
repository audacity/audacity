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

class NoteTrack;
class StretchHandle;
class TimeWarper;

struct NOTE_TRACK_API NoteTrackAttachment
   : ClientData::Cloneable<NoteTrackAttachment, ClientData::UniquePtr>
{
   ~NoteTrackAttachment() override;
   //! Default implementation does nothing
   virtual void WriteXML(XMLWriter &xmlFile) const;
   //! Return whether the attribute was used; default returns false
   virtual bool HandleAttribute(const Attribute &attribute);
};

using NoteTrackAttachments = ClientData::Site<
   NoteTrack,
   NoteTrackAttachment,
   ClientData::DeepCopying
>;

class NOTE_TRACK_API NoteTrack final
   : public NoteTrackBase
   , public NoteTrackAttachments
{
public:
   using Attachments = NoteTrackAttachments;
   static EnumSetting< bool > AllegroStyleSetting;

   // Construct and also build all attachments
   static NoteTrack *New(AudacityProject &project);

   NoteTrack();
   //! Copy construction hasn't been necessary yet
   NoteTrack(const NoteTrack &orig) = delete;
   NoteTrack(const NoteTrack &orig, ProtectedCreationArg &&) = delete;
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
   float GetVelocity() const {
      return mVelocity.load(std::memory_order_relaxed); }
   void SetVelocity(float velocity);
#endif

   QuantizedTimeAndBeat NearestBeatTime( double time ) const;
   bool StretchRegion
      ( QuantizedTimeAndBeat t0, QuantizedTimeAndBeat t1, double newDur );

   bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;
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
   unsigned GetVisibleChannels() const {
      return mVisibleChannels.load(std::memory_order_relaxed);
   }
   void SetVisibleChannels(unsigned value) {
      mVisibleChannels.store(value, std::memory_order_relaxed);
   }
   bool IsVisibleChan(int c) const {
      return (GetVisibleChannels() & CHANNEL_BIT(c)) != 0;
   }
   void SetVisibleChan(int c) {
      mVisibleChannels.fetch_or(CHANNEL_BIT(c), std::memory_order_relaxed); }
   void ClearVisibleChan(int c) {
      mVisibleChannels.fetch_and(~CHANNEL_BIT(c), std::memory_order_relaxed); }
   void ToggleVisibleChan(int c) {
      mVisibleChannels.fetch_xor(CHANNEL_BIT(c), std::memory_order_relaxed); }
   // Solos the given channel.  If it's the only channel visible, all channels
   // are enabled; otherwise, it is set to the only visible channel.
   void SoloVisibleChan(int c) {
      auto visibleChannels = 0u;
      if (GetVisibleChannels() == CHANNEL_BIT(c))
         visibleChannels = ALL_CHANNELS;
      else
         visibleChannels = CHANNEL_BIT(c);
      mVisibleChannels.store(visibleChannels, std::memory_order_relaxed);
   }

   const TypeInfo &GetTypeInfo() const override;
   static const TypeInfo &ClassTypeInfo();

   Track::Holder PasteInto( AudacityProject & ) const override;

   ConstIntervals GetIntervals() const override;
   Intervals GetIntervals() override;

 private:
#ifdef EXPERIMENTAL_MIDI_OUT
   void DoSetVelocity(float velocity);
#endif

   void AddToDuration( double delta );

   // These are mutable to allow NoteTrack to switch details of representation
   // in logically const methods
   // At most one of the two pointers is not null at any time.
   // Both are null in a newly constructed NoteTrack.
   mutable std::unique_ptr<Alg_seq> mSeq;
   mutable std::unique_ptr<char[]> mSerializationBuffer;
   mutable long mSerializationLength;

#ifdef EXPERIMENTAL_MIDI_OUT
   //! Atomic because it may be read by worker threads in playback
   std::atomic<float> mVelocity{ 0.0f }; // velocity offset
#endif

   //! A bit set; atomic because it may be read by worker threads in playback
   std::atomic<unsigned> mVisibleChannels{ ALL_CHANNELS };
};

extern NOTE_TRACK_API StringSetting MIDIPlaybackDevice;
extern NOTE_TRACK_API StringSetting MIDIRecordingDevice;
extern NOTE_TRACK_API IntSetting MIDISynthLatency_ms;

ENUMERATE_TRACK_TYPE(NoteTrack);

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


NOTE_TRACK_API wxString GetMIDIDeviceInfo();

#endif
