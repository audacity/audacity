/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#ifdef USE_MIDI
#include "NoteTrackControls.h"

#include "NoteTrackButtonHandle.h"

#include "../../ui/PlayableTrackButtonHandles.h"
#include "NoteTrackSliderHandles.h"

#include "../../../../HitTestResult.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../NoteTrack.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "Project.h"
#include "../../../../ProjectHistory.h"
#include "../../../../ProjectWindows.h"
#include "../../../../RefreshCode.h"
#include "../../../../prefs/ThemePrefs.h"

#include <mutex>
#include <wx/app.h>
#include <wx/frame.h>

///////////////////////////////////////////////////////////////////////////////
NoteTrackControls::~NoteTrackControls()
{
}

std::vector<UIHandlePtr> NoteTrackControls::HitTest
(const TrackPanelMouseState & st,
 const AudacityProject *pProject)
{
   // Hits are mutually exclusive, results single
   std::vector<UIHandlePtr> results;
   const wxMouseState &state = st.state;
   const wxRect &rect = st.rect;
   if (state.ButtonIsDown(wxMOUSE_BTN_ANY)) {
      auto track = std::static_pointer_cast<NoteTrack>(FindTrack());
      auto result = [&]{
         UIHandlePtr result;
         if (NULL != (result = MuteButtonHandle::HitTest(
            mMuteHandle, state, rect, pProject, track)))
            return result;

         if (NULL != (result = SoloButtonHandle::HitTest(
            mSoloHandle, state, rect, pProject, track)))
            return result;
#ifdef EXPERIMENTAL_MIDI_OUT
         if (NULL != (result = VelocitySliderHandle::HitTest(
            mVelocityHandle, state, rect, track)))
            return result;
         if (NULL != (result = NoteTrackButtonHandle::HitTest(
            mClickHandle, state, rect, track)))
            return result;
#endif
         return result;
      }();
      if (result) {
         results.push_back(result);
         return results;
      }
   }

   return NoteTrackControlsBase::HitTest(st, pProject);
}

class NoteTrackMenuTable : public PopupMenuTable
{
   NoteTrackMenuTable()
      : PopupMenuTable{ "NoteTrack" }
   {}
   DECLARE_POPUP_MENU(NoteTrackMenuTable);

public:
   static NoteTrackMenuTable &Instance();

private:
   void InitUserData(void *pUserData) override
   {
      mpData = static_cast<NoteTrackControlsBase::InitMenuData*>(pUserData);
   }

   NoteTrackControlsBase::InitMenuData *mpData{};

   void OnChangeOctave(wxCommandEvent &);
};

NoteTrackMenuTable &NoteTrackMenuTable::Instance()
{
   static NoteTrackMenuTable instance;
   return instance;
}

enum {
   OnUpOctaveID = 30000,
   OnDownOctaveID,
};

/// Scrolls the note track up or down by an octave
void NoteTrackMenuTable::OnChangeOctave(wxCommandEvent &event)
{
   NoteTrack *const pTrack = static_cast<NoteTrack*>(mpData->pTrack);

   wxASSERT(event.GetId() == OnUpOctaveID
      || event.GetId() == OnDownOctaveID);

   const bool bDown = (OnDownOctaveID == event.GetId());
   pTrack->ShiftNoteRange((bDown) ? -12 : 12);

   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project )
      .ModifyState(false);
   mpData->result = RefreshCode::RefreshAll;
}

BEGIN_POPUP_MENU(NoteTrackMenuTable)
   BeginSection( "Basic" );
      AppendItem( "Up", OnUpOctaveID, XXO("Up &Octave"), POPUP_MENU_FN( OnChangeOctave ) );
      AppendItem( "Down", OnDownOctaveID, XXO("Down Octa&ve"), POPUP_MENU_FN( OnChangeOctave ) );
   EndSection();
END_POPUP_MENU()

PopupMenuTable *NoteTrackControls::GetMenuExtension(Track *)
{
#if defined(USE_MIDI)
   return &NoteTrackMenuTable::Instance();
#else
   return NULL;
#endif
}

// drawing related
#include "../../../../widgets/ASlider.h"
#include "../../../../TrackInfo.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "ViewInfo.h"

using TCPLine = TrackInfo::TCPLine;

#ifdef USE_MIDI
enum : int {
   // PRL:  was it correct to include the margin?
   kMidiCellWidth = ( ( kTrackInfoWidth + kLeftMargin ) / 4) - 2,
   kMidiCellHeight = kTrackInfoBtnSize
};
#endif

#include "NoteTrackButtonHandle.h"
#include "NoteTrackSliderHandles.h"

namespace {
void GetMidiControlsHorizontalBounds( const wxRect &rect, wxRect &dest )
{
   dest.x = rect.x + 1; // To center slightly
   // PRL: TODO: kMidiCellWidth is defined in terms of the other constant
   // kTrackInfoWidth but I am trying to avoid use of that constant.
   // Can cell width be computed from dest.width instead?
   dest.width = kMidiCellWidth * 4;
}

void SliderDrawFunction
( LWSlider *(*Selector)
    (const wxRect &sliderRect, const NoteTrack *t, bool captured, wxWindow*),
  wxDC *dc, const wxRect &rect, const Track *pTrack,
  wxWindow *pParent,
  bool captured, bool highlight )
{
   wxRect sliderRect = rect;
   TrackInfo::GetSliderHorizontalBounds( rect.GetTopLeft(), sliderRect );
   auto nt = static_cast<const NoteTrack*>( pTrack );
   Selector( sliderRect, nt, captured, pParent )->OnPaint(*dc, highlight);
}

#ifdef EXPERIMENTAL_MIDI_OUT
void VelocitySliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto target = dynamic_cast<VelocitySliderHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();

   const auto artist = TrackArtist::Get( context );
   auto pParent = FindProjectFrame( artist->parent->GetProject() );

   SliderDrawFunction(
      &NoteTrackControls::VelocitySlider, dc, rect, pTrack,
      pParent, captured, hit);
}
#endif

void MidiControlsDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto target = dynamic_cast<NoteTrackButtonHandle*>( context.target.get() );
   bool hit = target && target->GetTrack().get() == pTrack;
   auto channel = hit ? target->GetChannel() : -1;
   auto &dc = context.dc;
   wxRect midiRect = rect;
   GetMidiControlsHorizontalBounds(rect, midiRect);
   NoteTrack::DrawLabelControls
      ( static_cast<const NoteTrack *>(pTrack), dc, midiRect, channel );
}
}

static const struct NoteTrackTCPLines
   : TCPLines { NoteTrackTCPLines() {
   (TCPLines&)*this =
      NoteTrackControlsBase::StaticTCPLines();
   insert( end(), {
      { TCPLine::kItemMidiControlsRect, kMidiCellHeight * 4, 0,
        MidiControlsDrawFunction },
#ifdef EXPERIMENTAL_MIDI_OUT
      { TCPLine::kItemVelocity, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
        VelocitySliderDrawFunction },
#endif
   } );
} } noteTrackTCPLines;

void NoteTrackControls::GetVelocityRect(const wxPoint &topleft, wxRect & dest)
{
   TrackInfo::GetSliderHorizontalBounds( topleft, dest );
   auto results = CalcItemY( noteTrackTCPLines, TCPLine::kItemVelocity );
   dest.y = topleft.y + results.first;
   dest.height = results.second;
}

void NoteTrackControls::GetMidiControlsRect(const wxRect & rect, wxRect & dest)
{
   GetMidiControlsHorizontalBounds( rect, dest );
   auto results = TrackInfo::CalcItemY(
      noteTrackTCPLines, TCPLine::kItemMidiControlsRect );
   dest.y = rect.y + results.first;
   dest.height = results.second;
}

unsigned NoteTrackControls::DefaultNoteTrackHeight()
{
   return TrackInfo::DefaultTrackHeight( noteTrackTCPLines );
}

const TCPLines &NoteTrackControls::GetTCPLines() const
{
   return noteTrackTCPLines;
};

namespace {

#ifdef EXPERIMENTAL_MIDI_OUT
   std::unique_ptr<LWSlider>
     gVelocityCaptured
   , gVelocity
   ;
#endif

}

#ifdef EXPERIMENTAL_MIDI_OUT
LWSlider * NoteTrackControls::VelocitySlider
(const wxRect &sliderRect, const NoteTrack *t, bool captured, wxWindow *pParent)
{
   static std::once_flag flag;
   std::call_once( flag, [] {
      wxCommandEvent dummy;
      ReCreateVelocitySlider( dummy );
      wxTheApp->Bind(EVT_THEME_CHANGE, ReCreateVelocitySlider);
   } );

   wxPoint pos = sliderRect.GetPosition();
   float velocity = t ? t->GetVelocity() : 0.0;

   gVelocity->Move(pos);
   gVelocity->Set(velocity);
   gVelocityCaptured->Move(pos);
   gVelocityCaptured->Set(velocity);

   auto slider = (captured ? gVelocityCaptured : gVelocity).get();
   slider->SetParent( pParent );
   return slider;
}
#endif

void NoteTrackControls::ReCreateVelocitySlider( wxEvent &evt )
{
   evt.Skip();
#ifdef EXPERIMENTAL_MIDI_OUT
   wxPoint point{ 0, 0 };
   wxRect sliderRect;
   GetVelocityRect(point, sliderRect);

   /* i18n-hint: Title of the Velocity slider, used to adjust the volume of note tracks */
   gVelocity = std::make_unique<LWSlider>(nullptr, XO("Velocity"),
      wxPoint(sliderRect.x, sliderRect.y),
      wxSize(sliderRect.width, sliderRect.height),
      VEL_SLIDER);
   gVelocity->SetDefaultValue(0.0);
   gVelocityCaptured = std::make_unique<LWSlider>(nullptr, XO("Velocity"),
      wxPoint(sliderRect.x, sliderRect.y),
      wxSize(sliderRect.width, sliderRect.height),
      VEL_SLIDER);
   gVelocityCaptured->SetDefaultValue(0.0);
#endif
}

using DoGetNoteTrackControls = DoGetControls::Override< NoteTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetNoteTrackControls) {
   return [](NoteTrack &track) {
      return std::make_shared<NoteTrackControls>( track.SharedPointer() );
   };
}

#include "../../../ui/TrackView.h"

using GetDefaultNoteTrackHeight = GetDefaultTrackHeight::Override< NoteTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetDefaultNoteTrackHeight) {
   return [](NoteTrack &) {
      return NoteTrackControls::DefaultNoteTrackHeight();
   };
}

#endif
