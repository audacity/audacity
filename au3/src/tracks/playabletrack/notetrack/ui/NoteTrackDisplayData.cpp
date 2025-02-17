/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NoteTrackDisplayData.cpp
  @brief Implements NoteTrackDisplayData

  Paul Licameli split from NoteTrack.cpp

**********************************************************************/

#include "NoteTrackDisplayData.h"
#include "NoteTrack.h"
#include "WrapAllegro.h"

static NoteTrackAttachments::RegisteredFactory key{
    [](NoteTrack&){ return std::make_unique<NoteTrackRange>(); }
};

NoteTrackRange& NoteTrackRange::Get(const NoteTrack& track)
{
    auto& mutTrack = const_cast<NoteTrack&>(track);
    return static_cast<NoteTrackRange&>(
        mutTrack.NoteTrackAttachments::Get(key));
}

NoteTrackRange::~NoteTrackRange() = default;

std::unique_ptr<NoteTrackAttachment> NoteTrackRange::Clone() const
{
    return std::make_unique<NoteTrackRange>(*this);
}

void NoteTrackRange::WriteXML(XMLWriter& xmlFile) const
{
    xmlFile.WriteAttr(wxT("bottomnote"), mBottomNote);
    xmlFile.WriteAttr(wxT("topnote"), mTopNote);
}

bool NoteTrackRange::HandleAttribute(const Attribute& pair)
{
    auto attr = pair.first;
    auto value = pair.second;
    long nValue{};
    if (attr == "bottomnote" && value.TryGet(nValue)) {
        SetBottomNote(nValue);
        return true;
    }
    if (attr == "topnote" && value.TryGet(nValue)) {
        SetTopNote(nValue);
        return true;
    }
    return false;
}

void NoteTrackRange::SetBottomNote(int note)
{
    if (note < MinPitch) {
        note = MinPitch;
    } else if (note > 96) {
        note = 96;
    }

    wxCHECK(note <= mTopNote, );

    mBottomNote = note;
}

void NoteTrackRange::SetTopNote(int note)
{
    if (note > MaxPitch) {
        note = MaxPitch;
    }

    wxCHECK(note >= mBottomNote, );

    mTopNote = note;
}

void NoteTrackRange::SetNoteRange(int note1, int note2)
{
    // Bounds check
    if (note1 > MaxPitch) {
        note1 = MaxPitch;
    } else if (note1 < MinPitch) {
        note1 = MinPitch;
    }
    if (note2 > MaxPitch) {
        note2 = MaxPitch;
    } else if (note2 < MinPitch) {
        note2 = MinPitch;
    }
    // Swap to ensure ordering
    if (note2 < note1) {
        std::swap(note1, note2);
    }

    mBottomNote = note1;
    mTopNote = note2;
}

void NoteTrackRange::ShiftNoteRange(int offset)
{
    // Ensure everything stays in bounds
    if (mBottomNote + offset < MinPitch || mTopNote + offset > MaxPitch) {
        return;
    }

    mBottomNote += offset;
    mTopNote += offset;
}

#if 0
void NoteTrackRange::StartVScroll()
{
    mStartBottomNote = mBottomNote;
}

void NoteTrackRange::VScroll(int start, int end)
{
    int ph = GetPitchHeight();
    int delta = ((end - start) + ph / 2) / ph;
    ShiftNoteRange(delta);
}

#endif

void NoteTrackRange::ZoomAllNotes(Alg_seq* pSeq)
{
    Alg_iterator iterator(pSeq, false);
    iterator.begin();
    Alg_event_ptr evt;

    // Go through all of the notes, finding the minimum and maximum value pitches.
    bool hasNotes = false;
    int minPitch = MaxPitch;
    int maxPitch = MinPitch;

    while (nullptr != (evt = iterator.next())) {
        if (evt->is_note()) {
            int pitch = (int)evt->get_pitch();
            hasNotes = true;
            if (pitch < minPitch) {
                minPitch = pitch;
            }
            if (pitch > maxPitch) {
                maxPitch = pitch;
            }
        }
    }

    if (!hasNotes) {
        // Semi-arbitrary default values:
        minPitch = 48;
        maxPitch = 72;
    }

    SetNoteRange(minPitch, maxPitch);
}

NoteTrackDisplayData::NoteTrackDisplayData(
    const NoteTrack& track, const wxRect& rect)
    : mTrack{track}
    , mRect{rect}
{
    auto& data = NoteTrackRange::Get(mTrack);
    auto span = data.GetTopNote() - data.GetBottomNote() + 1; // + 1 to make sure it includes both

    mMargin = std::min((int)(rect.height / (float)(span)) / 2, rect.height / 4);

    // Count the number of dividers between B/C and E/F
    int numC = 0, numF = 0;
    auto botOctave = data.GetBottomNote() / 12, botNote = data.GetBottomNote() % 12;
    auto topOctave = data.GetTopNote() / 12, topNote = data.GetTopNote() % 12;
    if (topOctave == botOctave) {
        if (botNote == 0) {
            numC = 1;
        }
        if (topNote <= 5) {
            numF = 1;
        }
    } else {
        numC = topOctave - botOctave;
        numF = topOctave - botOctave - 1;
        if (botNote == 0) {
            numC++;
        }
        if (botNote <= 5) {
            numF++;
        }
        if (topOctave <= 5) {
            numF++;
        }
    }
    // Effective space, excluding the margins and the lines between some notes
    auto effectiveHeight = rect.height - (2 * (mMargin + 1)) - numC - numF;
    // Guaranteed that both the bottom and top notes will be visible
    // (assuming that the clamping below does not happen)
    mPitchHeight = effectiveHeight / ((float)span);

    if (mPitchHeight < MinPitchHeight) {
        mPitchHeight = MinPitchHeight;
    }
    if (mPitchHeight > MaxPitchHeight) {
        mPitchHeight = MaxPitchHeight;
    }

    mBottom = rect.y + rect.height - GetNoteMargin() - 1 - GetPitchHeight(1)
              + botOctave * GetOctaveHeight() + GetNotePos(botNote);
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

void NoteTrackDisplayData::Zoom(int y, float multiplier, bool center)
{
    auto& data = NoteTrackRange::Get(mTrack);
    int clickedPitch = YToIPitch(y);
    int extent = data.GetTopNote() - data.GetBottomNote() + 1;
    int newExtent = (int)(extent / multiplier);
    float position;
    if (center) {
        // center the pitch that the user clicked on
        position = .5;
    } else {
        // align to keep the pitch that the user clicked on in the same place
        position = extent / (clickedPitch - data.GetBottomNote());
    }
    int newBottomNote = clickedPitch - (newExtent * position);
    int newTopNote = clickedPitch + (newExtent * (1 - position));
    data.SetNoteRange(newBottomNote, newTopNote);
}

void NoteTrackDisplayData::ZoomTo(int start, int end)
{
    wxRect trackRect(0, mRect.GetY(), 1, mRect.GetHeight());
    int pitch1 = YToIPitch(start);
    int pitch2 = YToIPitch(end);
    if (pitch1 == pitch2) {
        // Just zoom in instead of zooming to show only one note
        Zoom(start, 1, true);
        return;
    }
    // It's fine for this to be in either order
    NoteTrackRange::Get(mTrack).SetNoteRange(pitch1, pitch2);
}

int NoteTrackDisplayData::GetPitchHeight(int factor) const
{
    return std::max(1, (int)(factor * mPitchHeight));
}

const float NoteTrackDisplayData::ZoomStep = powf(2.0f, 0.25f);
