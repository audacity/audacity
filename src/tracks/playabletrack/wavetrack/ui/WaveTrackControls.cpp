/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "WaveTrackControls.h"

#include "../../ui/PlayableTrackButtonHandles.h"
#include "WaveTrackSliderHandles.h"

#include "WaveTrackView.h"
#include "WaveTrackViewConstants.h"
#include "AudioIOBase.h"
#include "../../../../CellularPanel.h"
#include "Project.h"
#include "../../../../ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../../ProjectWindows.h"
#include "../../../../RefreshCode.h"
#include "../../../../ShuttleGui.h"
#include "Theme.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelAx.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"
#include "../../../../effects/RealtimeEffectManager.h"
#include "../../../../prefs/PrefsDialog.h"
#include "../../../../prefs/ThemePrefs.h"
#include "../../../../widgets/AudacityMessageBox.h"
#include "widgets/ProgressDialog.h"
#include "UserException.h"
#include "Identifier.h"

#include <wx/app.h>
#include <wx/combobox.h>
#include <wx/frame.h>
#include <wx/sizer.h>

WaveTrackControls::~WaveTrackControls()
{
}


std::vector<UIHandlePtr> WaveTrackControls::HitTest
(const TrackPanelMouseState & st,
 const AudacityProject *pProject)
{
   // Hits are mutually exclusive, results single
   const wxMouseState &state = st.state;
   const wxRect &rect = st.rect;
   if (state.ButtonIsDown(wxMOUSE_BTN_LEFT)) {
      auto track = FindTrack();
      std::vector<UIHandlePtr> results;
      auto result = [&]{
         UIHandlePtr result;
         if (NULL != (result = MuteButtonHandle::HitTest(
            mMuteHandle, state, rect, pProject, track)))
            return result;

         if (NULL != (result = SoloButtonHandle::HitTest(
            mSoloHandle, state, rect, pProject, track)))
            return result;

         if (NULL != (result = EffectsButtonHandle::HitTest(
            mEffectsHandle, state, rect, pProject, track)))
            return result;

         if (NULL != (result = GainSliderHandle::HitTest(
            mGainHandle, state, rect, track)))
            return result;

         if (NULL != (result = PanSliderHandle::HitTest(
            mPanHandle, state, rect, track)))
            return result;

         return result;
      }();
      if (result) {
         results.push_back(result);
         return results;
      }
   }

   return PlayableTrackControls::HitTest(st, pProject);
}

WaveTrack &WaveTrackPopupMenuTable::FindWaveTrack() const
{
   return *static_cast< WaveTrack* >( mpData->pTrack );
};

enum {
   reserveDisplays = 100,

   OnRate8ID = 30000,      // <---
   OnRate11ID,             //    |
   OnRate16ID,             //    |
   OnRate22ID,             //    |
   OnRate44ID,             //    |
   OnRate48ID,             //    | Leave these in order
   OnRate88ID,             //    |
   OnRate96ID,             //    |
   OnRate176ID,            //    |
   OnRate192ID,            //    |
   OnRate352ID,            //    |
   OnRate384ID,            //    |
   OnRateOtherID,          //    |
   //    |
   On16BitID,              //    |
   On24BitID,              //    |
   OnFloatID,              // <---

   OnMultiViewID,

   OnSetDisplayId, lastDisplayId = (OnSetDisplayId + reserveDisplays - 1),

   OnChannelLeftID,
   OnChannelRightID,
   OnChannelMonoID,

   OnMergeStereoID,

   OnSwapChannelsID,
   OnSplitStereoID,
   OnSplitStereoMonoID,

   ChannelMenuID,

   // Range of ids for registered items -- keep this last!
   FirstAttachedItemId,
};


namespace {
using ValueFinder = std::function< int( WaveTrack& ) >;

// A function that makes functions that decide checkmark states,
// parametrized by ValueFinder, which specificies how you get the relevant
// value from a track's settings
template< typename Table >
PopupMenuTableEntry::StateFunction stateFn( Table &handler,
   ValueFinder findValue, int value )
{
   return [&handler, findValue = move(findValue), value]()
   -> BasicMenu::Item::State {
      auto pData = static_cast<Table&>( handler ).mpData;
      const auto pTrack = static_cast<WaveTrack*>(pData->pTrack);
      auto &project = pData->project;
      bool unsafe = ProjectAudioIO::Get( project ).IsAudioActive();

      return { !unsafe, value == findValue( *pTrack ) };
   };
};
}


//=============================================================================
// Table class for a sub-menu
struct FormatMenuTable :  PopupMenuTable
{
   FormatMenuTable()
      : PopupMenuTable{ "SampleFormat", XO("&Format") }
   {}
   DECLARE_POPUP_MENU(FormatMenuTable);

   static FormatMenuTable &Instance();

   void InitUserData(void *pUserData) override;

   PlayableTrackControls::InitMenuData *mpData{};

   void OnFormatChange(sampleFormat newFormat);
};

FormatMenuTable &FormatMenuTable::Instance()
{
   static FormatMenuTable instance;
   return instance;
}

void FormatMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<PlayableTrackControls::InitMenuData*>(pUserData);
}


BEGIN_POPUP_MENU(FormatMenuTable)
   // A function that makes functions
   static const auto fn = [this]( int format ){
      return stateFn( *this,
         std::mem_fn( &WaveTrack::GetSampleFormat ), format );
   };

   AppendRadioItem( "16Bit", On16BitID,
      GetSampleFormatStr(int16Sample),
      [this]{ OnFormatChange(int16Sample); },
      fn( int16Sample ) );
   AppendRadioItem("24Bit", On24BitID,
      GetSampleFormatStr(int24Sample),
      [this]{ OnFormatChange(int24Sample); },
      fn( int24Sample ) );
   AppendRadioItem( "Float", OnFloatID,
      GetSampleFormatStr(floatSample),
      [this]{ OnFormatChange(floatSample); },
      fn( floatSample ) );

END_POPUP_MENU()

/// Handles the selection from the Format submenu of the
/// track menu.
void FormatMenuTable::OnFormatChange(sampleFormat newFormat)
{
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);

   if (newFormat == pTrack->GetSampleFormat())
      return; // Nothing to do.

   AudacityProject *const project = &mpData->project;

   ProgressDialog progress{ XO("Changing sample format"),
                            XO("Processing...   0%%"),
                            pdlgHideStopButton };

   sampleCount totalSamples{ 0 };
   for (const auto& channel : TrackList::Channels(pTrack))
      // Hidden samples are processed too, they should be counted as well
      totalSamples += channel->GetSequenceSamplesCount();
   sampleCount processedSamples{ 0 };

   // Below is the lambda function that is passed along the call chain to
   // the Sequence::ConvertToSampleFormat. This callback function is used
   // to report the conversion progress and update the progress dialog.
   auto progressUpdate = [&progress, &totalSamples, &processedSamples]
   (size_t newlyProcessedCount)->void
   {
      processedSamples += newlyProcessedCount;
      double d_processed = processedSamples.as_double();
      double d_total = totalSamples.as_double();
      int percentage{ static_cast<int>((d_processed / d_total) * 100) };

      auto progressStatus = progress.Update(d_processed, d_total,
         XO("Processing...   %i%%").Format(percentage));

      if (progressStatus != ProgressResult::Success)
         throw UserException{};
   };

   for (auto channel : TrackList::Channels(pTrack))
      channel->ConvertToSampleFormat(
         newFormat, progressUpdate);
         
   ProjectHistory::Get( *project )
   /* i18n-hint: The strings name a track and a format */
      .PushState(XO("Changed '%s' to %s")
         .Format( pTrack->GetName(), GetSampleFormatStr(newFormat) ),
      XO("Format Change"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}


//=============================================================================
// Table class for a sub-menu
struct RateMenuTable : PopupMenuTable
{
   RateMenuTable()
      : PopupMenuTable{ "SampleRate", XO("Rat&e") }
   {}
   DECLARE_POPUP_MENU(RateMenuTable);

   static RateMenuTable &Instance();

   void InitUserData(void *pUserData) override;

   PlayableTrackControls::InitMenuData *mpData{};

   void SetRate(WaveTrack * pTrack, double rate);

   void OnRateChange(int ii);
   void OnRateOther();
};

RateMenuTable &RateMenuTable::Instance()
{
   static RateMenuTable instance;
   return instance;
}

void RateMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<PlayableTrackControls::InitMenuData*>(pUserData);
}

const int nRates = 12;

static int gRates[nRates] = { 8000, 11025, 16000, 22050, 44100, 48000, 88200, 96000,
176400, 192000, 352800, 384000 };

// Because of Bug 1780 we can't use AppendRadioItem
// If we did, we'd get no message when clicking on Other...
// when it is already selected.
BEGIN_POPUP_MENU(RateMenuTable)
   // A function that makes functions
   static const auto fn = [this](int rate){
      return stateFn( *this, &WaveTrack::GetRate, rate );
   };

   int ii = 0;
   for ( auto rate : gRates ) {
      AppendCheckItem( wxString::Format( "%d", rate ), OnRate8ID + ii,
        XXO("%d Hz").Format( rate ),
        [this, ii]{ OnRateChange(ii); },
         fn( rate ) );
      ++ii;
   }

   AppendCheckItem( "Other", OnRateOtherID, XXO("&Other..."),
      POPUP_MENU_FN( OnRateOther ),
      [this]() -> BasicMenu::Item::State {
         const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);
         auto &project = mpData->project;
         bool unsafe = ProjectAudioIO::Get( project ).IsAudioActive();
         const auto end = std::end(gRates),
            iter = std::find( std::begin(gRates), end, pTrack->GetRate() );
         return { !unsafe, iter == end };
      }
   );

END_POPUP_MENU()

/// Sets the sample rate for a track, and if it is linked to
/// another track, that one as well.
void RateMenuTable::SetRate(WaveTrack * pTrack, double rate)
{
   AudacityProject *const project = &mpData->project;
   for (auto channel : TrackList::Channels(pTrack))
      channel->SetRate(rate);

   // Separate conversion of "rate" enables changing the decimals without affecting i18n
   wxString rateString = wxString::Format(wxT("%.3f"), rate);
   ProjectHistory::Get( *project )
   /* i18n-hint: The string names a track */
      .PushState(XO("Changed '%s' to %s Hz")
         .Format( pTrack->GetName(), rateString),
      XO("Rate Change"));
}

/// This method handles the selection from the Rate
/// submenu of the track menu, except for "Other" (/see OnRateOther).
void RateMenuTable::OnRateChange(int ii)
{
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);

   SetRate(pTrack, gRates[ii]);

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

void RateMenuTable::OnRateOther()
{
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);

   int newRate;

   /// \todo Remove artificial constants!!
   /// \todo Make a real dialog box out of this!!
   while (true)
   {
      wxDialogWrapper dlg(mpData->pParent, wxID_ANY, XO("Set Rate"));
      dlg.SetName();
      ShuttleGui S(&dlg, eIsCreating);
      wxString rate;
      wxComboBox *cb;

      rate.Printf(wxT("%ld"), lrint(pTrack->GetRate()));

      wxArrayStringEx rates{
         wxT("8000") ,
         wxT("11025") ,
         wxT("16000") ,
         wxT("22050") ,
         wxT("44100") ,
         wxT("48000") ,
         wxT("88200") ,
         wxT("96000") ,
         wxT("176400") ,
         wxT("192000") ,
         wxT("352800") ,
         wxT("384000") ,
      };

      S.StartVerticalLay(true);
      {
         S.SetBorder(10);
         S.StartHorizontalLay(wxEXPAND, false);
         {
            cb = S.AddCombo(XXO("New sample rate (Hz):"),
               rate,
               rates);
#if defined(__WXMAC__)
            // As of wxMac-2.8.12, setting manually is required
            // to handle rates not in the list.  See: Bug #427
            cb->SetValue(rate);
#endif
         }
         S.EndHorizontalLay();
         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      dlg.SetClientSize(dlg.GetSizer()->CalcMin());
      dlg.Center();

      if (dlg.ShowModal() != wxID_OK)
      {
         return;  // user cancelled dialog
      }

      long lrate;
      if (cb->GetValue().ToLong(&lrate) && lrate >= 1 && lrate <= 1000000)
      {
         newRate = (int)lrate;
         break;
      }

      AudacityMessageBox(
         XO("The entered value is invalid"),
         XO("Error"),
         wxICON_ERROR,
         mpData->pParent);
   }

   SetRate(pTrack, newRate);

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

static const auto MenuPathStart = wxT("WaveTrackMenu");

//=============================================================================
// Class defining common command handlers for mono and stereo tracks
struct WaveTrackMenuTable
   : ComputedPopupMenuTable< WaveTrackMenuTable, WaveTrackPopupMenuTable >
{
   static WaveTrackMenuTable &Instance();

   WaveTrackMenuTable()
      : ComputedPopupMenuTable< WaveTrackMenuTable, WaveTrackPopupMenuTable >{
         MenuPathStart }
   {
      mNextId = FirstAttachedItemId;
   }

   void InitUserData(void *pUserData) override;

   DECLARE_POPUP_MENU(WaveTrackMenuTable);

   void OnMultiView();
   void OnSetDisplay(int id);

   void OnChannelChange(wxCommandEvent & event);
   void OnMergeStereo();

   // TODO: more-than-two-channels
   // How should we define generalized channel manipulation operations?
   void SplitStereo(bool stereo);

   void OnSwapChannels();
   void OnSplitStereo();
   void OnSplitStereoMono();
};

WaveTrackMenuTable &WaveTrackMenuTable::Instance()
{
   static WaveTrackMenuTable instance;
   return instance;
}

void WaveTrackMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<PlayableTrackControls::InitMenuData*>(pUserData);
}

static std::vector<WaveTrackSubViewType> AllTypes()
{
   auto result = WaveTrackSubViewType::All();
   if ( result.size() > reserveDisplays ) {
      wxASSERT( false );
      result.resize( reserveDisplays );
   }
   return result;
}

BEGIN_POPUP_MENU(WaveTrackMenuTable)
   // Functions usable in callbacks to check and disable items
   static const auto isMono =
   []( WaveTrackMenuTable &handler ) -> bool {
      auto &track = handler.FindWaveTrack();
      return 1 == TrackList::Channels( &track ).size();
   };

   static const auto isUnsafe =
   []( WaveTrackMenuTable &handler ) -> bool {
      auto &project = handler.mpData->project;
      return RealtimeEffectManager::Get(project).IsActive() &&
         ProjectAudioIO::Get( project ).IsAudioActive();
   };


   BeginSection( "SubViews" );
      // Multi-view check mark item, if more than one track sub-view type is
      // known
      Append( [this]( My &table ) -> Registry::BaseItemPtr {
         if ( WaveTrackSubViews::slots() > 1 )
            return std::make_unique<Entry>(
               "MultiView", Entry::CheckItem, OnMultiViewID, XXO("&Multi-view"),
               POPUP_MENU_FN( OnMultiView ),
               table,
               [this]() -> BasicMenu::Item::State {
                  auto &track = FindWaveTrack();
                  const auto &view = WaveTrackView::Get( track );
                  return { true, view.GetMultiView() };
               } );
            else
               return nullptr;
      } );

      // Append either a checkbox or radio item for each sub-view.
      // Radio buttons if in single-view mode, else checkboxes
      int index = 0;
      for ( const auto &type : AllTypes() ) {
         static const auto stateFn = [this]( bool radio, int index ){ return
            [this, radio, index]() -> BasicMenu::Item::State {
               // Find all known sub-view types
               const auto allTypes = AllTypes();

               // How to convert a type to an index
               const auto IndexForType =
               [&allTypes]( const WaveTrackSubViewType &type ) -> int {
                  const auto begin = allTypes.begin();
                  return std::find( begin, allTypes.end(), type ) - begin;
               };

               auto &track = FindWaveTrack();

               const auto &view = WaveTrackView::Get( track );

               const auto displays = view.GetDisplays();
               const auto end = displays.end(),
                  iter = std::find_if( displays.begin(), end,
                     [&]( const WaveTrackSubViewType &type ){
                        return index == IndexForType( type ); } );
               bool check = (iter != end);

               return {
                  // Bug2275 residual
                  // Disable the checking-off of the only sub-view
                  (radio || displays.size() != 1 || !check),
                  check
               };
            };
         };
         Append( [type, index]( My &table ) -> Registry::BaseItemPtr {
            const auto pTrack = &table.FindWaveTrack();
            const auto &view = WaveTrackView::Get( *pTrack );
            const auto itemType =
               view.GetMultiView() ? Entry::CheckItem : Entry::RadioItem;
            return std::make_unique<Entry>( type.name.Internal(), itemType,
               OnSetDisplayId + index, type.name.Msgid(),
               [index]{ WaveTrackMenuTable::Instance().OnSetDisplay(index); },
               table,
               stateFn( !view.GetMultiView(), index ) );
         } );
         ++index;
      }
      BeginSection( "Extra" );
      EndSection();
   EndSection();

   BeginSection( "Channels" );
   // If these are enabled again, choose a hot key for Mono that does not conflict
   // with Multi View
   //   AppendRadioItem(OnChannelMonoID, XXO("&Mono"),
   //      POPUP_MENU_FN( OnChannelChange ),
   //      []( PopupMenuHandler &handler, wxMenu &menu, int id ){
   //         menu.Enable( id, isMono( handler ) );
   //         menu.Check( id, findTrack( handler ).GetChannel() == Track::MonoChannel );
   //      }
   //   );
   //   AppendRadioItem(OnChannelLeftID, XXO("&Left Channel"),
   //      POPUP_MENU_FN( OnChannelChange ),
   //      []( PopupMenuHandler &handler, wxMenu &menu, int id ){
   //         menu.Enable( id, isMono( handler ) );
   //         menu.Check( id, findTrack( handler ).GetChannel() == Track::LeftChannel );
   //      }
   //   );
   //   AppendRadioItem(OnChannelRightID, XXO("R&ight Channel"),
   //      POPUP_MENU_FN( OnChannelChange ),
   //      []( PopupMenuHandler &handler, wxMenu &menu, int id ){
   //         menu.Enable( id, isMono( handler ) );
   //         menu.Check( id, findTrack( handler ).GetChannel() == Track::RightChannel );
   //      }
   //   );
      AppendItem( "MakeStereo", OnMergeStereoID, XXO("Ma&ke Stereo Track"),
         POPUP_MENU_FN( OnMergeStereo ),
         [this]{
            bool canMakeStereo = !isUnsafe( *this ) && isMono( *this );
            if ( canMakeStereo ) {
               AudacityProject &project = mpData->project;
               auto &tracks = TrackList::Get( project );
               auto &track = FindWaveTrack();
               auto next = * ++ tracks.Find(&track);
               canMakeStereo =
                  (next &&
                   TrackList::Channels(next).size() == 1 &&
                   track_cast<WaveTrack*>(next));
            }
            return canMakeStereo;
         }
      );

      AppendItem( "Swap", OnSwapChannelsID, XXO("Swap Stereo &Channels"),
         POPUP_MENU_FN( OnSwapChannels ),
         [this]{
            auto &track = FindWaveTrack();
            bool isStereo =
               2 == TrackList::Channels( &track ).size();
            return isStereo && !isUnsafe( *this );
         }
      );

      static const auto enableSplitStereo =
      [this]{ return !isMono( *this ) && !isUnsafe( *this ); };

      AppendItem( "Split", OnSplitStereoID, XXO("Spl&it Stereo Track"),
         POPUP_MENU_FN( OnSplitStereo ), enableSplitStereo );
   // DA: Uses split stereo track and then drag pan sliders for split-stereo-to-mono
   #ifndef EXPERIMENTAL_DA
      AppendItem( "SplitToMono", OnSplitStereoMonoID,
         XXO("Split Stereo to Mo&no"), POPUP_MENU_FN( OnSplitStereoMono ),
         enableSplitStereo );
   #endif
   EndSection();

   BeginSection( "Format" );
      POPUP_MENU_SUB_MENU( "Format", FormatMenuTable, mpData )
   EndSection();

   BeginSection( "Rate" );
      POPUP_MENU_SUB_MENU( "Rate", RateMenuTable, mpData )
   EndSection();
END_POPUP_MENU()


void WaveTrackMenuTable::OnMultiView()
{
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   const auto &view = WaveTrackView::Get( *pTrack );
   bool multi = !view.GetMultiView();
   const auto &displays = view.GetDisplays();
   const auto display = displays.empty()
      ? WaveTrackViewConstants::Waveform : displays.begin()->id;
   for (const auto channel : TrackList::Channels(pTrack)) {
      auto &channelView = WaveTrackView::Get( *channel );
      channelView.SetMultiView( multi );

      // Whichever sub-view was on top stays on top
      // If going into Multi-view, it will be 1/nth the height.
      // If exiting multi-view, it will be full height.
      channelView.SetDisplay(display, !multi);
   }
}

///  Set the Display mode based on the menu choice in the Track Menu.
void WaveTrackMenuTable::OnSetDisplay(int id)
{
   const auto pTrack = static_cast<WaveTrack*>(mpData->pTrack);

   auto &view = WaveTrackView::Get( *pTrack );
   if ( view.GetMultiView() ) {
      for (auto channel : TrackList::Channels(pTrack)) {
         if ( !WaveTrackView::Get( *channel )
               .ToggleSubView( WaveTrackView::Display{ id } ) ) {
            // Trying to toggle off the last sub-view.  It was refused.
            // Decide what to do here.  Turn off multi-view instead?
            // PRL:  I don't agree that it makes sense
         }
         else
            ProjectHistory::Get( mpData->project ).ModifyState(true);
      }
   }
   else {
      const auto displays = view.GetDisplays();
      const bool wrongType =
         !(displays.size() == 1 && displays[0].id == id);
      if (wrongType) {
         for (auto channel : TrackList::Channels(pTrack)) {
            WaveTrackView::Get( *channel )
               .SetDisplay( WaveTrackView::Display{ id } );
         }

         AudacityProject *const project = &mpData->project;
         ProjectHistory::Get( *project ).ModifyState(true);

         using namespace RefreshCode;
         mpData->result = RefreshAll | UpdateVRuler;
      }
   }
}

#if 0
void WaveTrackMenuTable::OnChannelChange(wxCommandEvent & event)
{
   int id = event.GetId();
   wxASSERT(id >= OnChannelLeftID && id <= OnChannelMonoID);
   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   wxASSERT(pTrack);
   Track::ChannelType channel;
   TranslatableString channelmsg;
   switch (id) {
   default:
   case OnChannelMonoID:
      channel = Track::MonoChannel;
      channelmsg = XO("Mono");
      break;
   case OnChannelLeftID:
      channel = Track::LeftChannel;
      channelmsg = XO("Left Channel");
      break;
   case OnChannelRightID:
      channel = Track::RightChannel;
      channelmsg = XO("Right Channel");
      break;
   }
   pTrack->SetChannel(channel);
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project )
      .PushState(
/* i18n-hint: The strings name a track and a channel choice (mono, left, or right) */
         XO("Changed '%s' to %s").Format( pTrack->GetName(), channelmsg ),
         XO("Channel"));
   mpData->result = RefreshCode::RefreshAll;
}
#endif

/// Merge two tracks into one stereo track ??
void WaveTrackMenuTable::OnMergeStereo()
{
   AudacityProject *const project = &mpData->project;
   auto &tracks = TrackList::Get( *project );

   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   wxASSERT(pTrack);

   auto partner = static_cast< WaveTrack * >
      ( *tracks.Find( pTrack ).advance( 1 ) );

   bool bBothMinimizedp =
      ((TrackView::Get( *pTrack ).GetMinimized()) &&
       (TrackView::Get( *partner ).GetMinimized()));

   tracks.MakeMultiChannelTrack( *pTrack, 2, false );

   // Set partner's parameters to match target.
   partner->Merge(*pTrack);

   pTrack->SetPan( 0.0f );
   partner->SetPan( 0.0f );

   // Set NEW track heights and minimized state
   auto
      &view = WaveTrackView::Get( *pTrack ),
      &partnerView = WaveTrackView::Get( *partner );
   view.SetMinimized(false);
   partnerView.SetMinimized(false);
   int AverageHeight = (view.GetHeight() + partnerView.GetHeight()) / 2;
   view.SetExpandedHeight(AverageHeight);
   partnerView.SetExpandedHeight(AverageHeight);
   view.SetMinimized(bBothMinimizedp);
   partnerView.SetMinimized(bBothMinimizedp);

   partnerView.RestorePlacements( view.SavePlacements() );
   partnerView.SetMultiView( view.GetMultiView() );

   ProjectHistory::Get( *project ).PushState(
      /* i18n-hint: The string names a track */
      XO("Made '%s' a stereo track").Format( pTrack->GetName() ),
      XO("Make Stereo"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

/// Split a stereo track (or more-than-stereo?) into two (or more) tracks...
void WaveTrackMenuTable::SplitStereo(bool stereo)
{
   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   wxASSERT(pTrack);
   AudacityProject *const project = &mpData->project;
   auto channels = TrackList::Channels( pTrack );

   int totalHeight = 0;
   int nChannels = 0;
   for (auto channel : channels) {
      // Keep original stereo track name.
      channel->SetName(pTrack->GetName());
      auto &view = TrackView::Get( *channel );
      if (stereo)
         channel->SetPanFromChannelType();

      //make sure no channel is smaller than its minimum height
      if (view.GetHeight() < view.GetMinimizedHeight())
         view.SetExpandedHeight(view.GetMinimizedHeight());
      totalHeight += view.GetHeight();
      ++nChannels;
   }

   TrackList::Get( *project ).UnlinkChannels( *pTrack );
   int averageHeight = totalHeight / nChannels;

   for (auto channel : channels)
      // Make tracks the same height
      TrackView::Get( *channel ).SetExpandedHeight( averageHeight );
}

/// Swap the left and right channels of a stero track...
void WaveTrackMenuTable::OnSwapChannels()
{
   AudacityProject *const project = &mpData->project;

   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   auto channels = TrackList::Channels( pTrack );
   if (channels.size() != 2)
      return;

   auto &trackFocus = TrackFocus::Get( *project );
   Track *const focused = trackFocus.Get();
   const bool hasFocus = channels.contains( focused );

   auto partner = *channels.rbegin();

   if (TrackList::SwapChannels(*pTrack)) {
      auto &tracks = TrackList::Get( *project );
      if (hasFocus)
         trackFocus.Set(partner);

      ProjectHistory::Get( *project ).PushState(
         /* i18n-hint: The string names a track  */
         XO("Swapped Channels in '%s'").Format( pTrack->GetName() ),
         XO("Swap Channels"));
   }

   mpData->result = RefreshCode::RefreshAll;
}

/// Split a stereo track into two tracks...
void WaveTrackMenuTable::OnSplitStereo()
{
   SplitStereo(true);
   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project ).PushState(
   /* i18n-hint: The string names a track  */
      XO("Split stereo track '%s'").Format( pTrack->GetName() ),
      XO("Split"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

/// Split a stereo track into two mono tracks...
void WaveTrackMenuTable::OnSplitStereoMono()
{
   SplitStereo(false);
   WaveTrack *const pTrack = static_cast<WaveTrack*>(mpData->pTrack);
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project ).PushState(
   /* i18n-hint: The string names a track  */
      XO("Split Stereo to Mono '%s'").Format( pTrack->GetName() ),
      XO("Split to Mono"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

PopupMenuTable *WaveTrackControls::GetMenuExtension(Track * pTrack)
{
   static Registry::OrderingPreferenceInitializer init{
      MenuPathStart,
      {
         {wxT("/SubViews/Extra"), wxT("WaveColor,SpectrogramSettings")},
      }
   };

   WaveTrackMenuTable & result = WaveTrackMenuTable::Instance();
   return &result;
}

WaveTrackPopupMenuTable &GetWaveTrackMenuTable()
{
   return WaveTrackMenuTable::Instance();
}

// drawing related
#include "../../../../widgets/ASlider.h"
#include "../../../../TrackInfo.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "ViewInfo.h"

namespace {

void SliderDrawFunction
( LWSlider *(*Selector)
    (const wxRect &sliderRect, const WaveTrack *t, bool captured, wxWindow*),
  wxDC *dc, const wxRect &rect, const Track *pTrack,
  wxWindow *pParent,
  bool captured, bool highlight )
{
   wxRect sliderRect = rect;
   TrackInfo::GetSliderHorizontalBounds( rect.GetTopLeft(), sliderRect );
   auto wt = static_cast<const WaveTrack*>( pTrack );
   Selector( sliderRect, wt, captured, pParent )->OnPaint(*dc, highlight);
}

void PanSliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto target = dynamic_cast<PanSliderHandle*>( context.target.get() );
   auto dc = &context.dc;
   bool hit = target && target->GetTrack().get() == pTrack;
   bool captured = hit && target->IsClicked();

   const auto artist = TrackArtist::Get( context );
   auto pParent = FindProjectFrame( artist->parent->GetProject() );

   SliderDrawFunction(
      &WaveTrackControls::PanSlider, dc, rect, pTrack,
      pParent, captured, hit);
}

void GainSliderDrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto target = dynamic_cast<GainSliderHandle*>( context.target.get() );
   auto dc = &context.dc;
   bool hit = target && target->GetTrack().get() == pTrack;
   if( hit )
      hit=hit;
   bool captured = hit && target->IsClicked();

   const auto artist = TrackArtist::Get( context );
   auto pParent = FindProjectFrame( artist->parent->GetProject() );

   SliderDrawFunction(
      &WaveTrackControls::GainSlider, dc, rect, pTrack,
      pParent, captured, hit);
}

void StatusDrawFunction
   ( const TranslatableString &string, wxDC *dc, const wxRect &rect )
{
   static const int offset = 3;
   dc->DrawText(string.Translation(), rect.x + offset, rect.y);
}

void Status1DrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto wt = static_cast<const WaveTrack*>(pTrack);

   /// Returns the string to be displayed in the track label
   /// indicating whether the track is mono, left, right, or
   /// stereo and what sample rate it's using.
   auto rate = wt ? wt->GetRate() : 44100.0;
   TranslatableString s;
   if (!pTrack || TrackList::Channels(pTrack).size() > 1)
      // TODO: more-than-two-channels-message
      // more appropriate strings
      s = XO("Stereo, %dHz");
   else {
      if (wt->GetChannel() == Track::MonoChannel)
         s = XO("Mono, %dHz");
      else if (wt->GetChannel() == Track::LeftChannel)
         s = XO("Left, %dHz");
      else if (wt->GetChannel() == Track::RightChannel)
         s = XO("Right, %dHz");
   }
   s.Format( (int) (rate + 0.5) );

   StatusDrawFunction( s, dc, rect );
}

void Status2DrawFunction
( TrackPanelDrawingContext &context,
  const wxRect &rect, const Track *pTrack )
{
   auto dc = &context.dc;
   auto wt = static_cast<const WaveTrack*>(pTrack);
   auto format = wt ? wt->GetSampleFormat() : floatSample;
   auto s = GetSampleFormatStr(format);
   StatusDrawFunction( s, dc, rect );
}

}

using TCPLine = TrackInfo::TCPLine;

static const struct WaveTrackTCPLines
   : TCPLines { WaveTrackTCPLines() {
   (TCPLines&)*this =
      PlayableTrackControls::StaticWaveTCPLines();
   insert( end(), {

      { TCPLine::kItemGain, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
        GainSliderDrawFunction },
      { TCPLine::kItemPan, kTrackInfoSliderHeight, kTrackInfoSliderExtra,
        PanSliderDrawFunction },

#ifdef EXPERIMENTAL_DA
      // DA: Does not have status information for a track.
#else
      { TCPLine::kItemStatusInfo1, 12, 0,
        Status1DrawFunction },
      { TCPLine::kItemStatusInfo2, 12, 0,
        Status2DrawFunction },
#endif

   } );
} } waveTrackTCPLines;

void WaveTrackControls::GetGainRect(const wxPoint &topleft, wxRect & dest)
{
   TrackInfo::GetSliderHorizontalBounds( topleft, dest );
   auto results = CalcItemY( waveTrackTCPLines, TCPLine::kItemGain );
   dest.y = topleft.y + results.first;
   dest.height = results.second;
}

void WaveTrackControls::GetPanRect(const wxPoint &topleft, wxRect & dest)
{
   GetGainRect( topleft, dest );
   auto results = CalcItemY( waveTrackTCPLines, TCPLine::kItemPan );
   dest.y = topleft.y + results.first;
}

unsigned WaveTrackControls::DefaultWaveTrackHeight()
{
   return TrackInfo::DefaultTrackHeight( waveTrackTCPLines );
}

const TCPLines &WaveTrackControls::GetTCPLines() const
{
   return waveTrackTCPLines;
}

namespace
{
std::unique_ptr<LWSlider>
   gGainCaptured
   , gPanCaptured
   , gGain
   , gPan;
}

LWSlider *WaveTrackControls::GainSlider(
   CellularPanel &panel, const WaveTrack &wt )
{
   auto &controls = TrackControls::Get( wt );
   auto rect = panel.FindRect( controls );
   wxRect sliderRect;
   GetGainRect( rect.GetTopLeft(), sliderRect );
   return GainSlider( sliderRect, &wt, false, &panel );
}

LWSlider * WaveTrackControls::GainSlider
(const wxRect &sliderRect, const WaveTrack *t, bool captured, wxWindow *pParent)
{
   static std::once_flag flag;
   std::call_once( flag, []{ ReCreateGainSlider({}); });
   static auto subscription = theTheme.Subscribe(ReCreateGainSlider);

   wxPoint pos = sliderRect.GetPosition();
   float gain = t ? t->GetGain() : 1.0;

   gGain->Move(pos);
   gGain->Set(gain);
   gGainCaptured->Move(pos);
   gGainCaptured->Set(gain);

   auto slider = (captured ? gGainCaptured : gGain).get();
   slider->SetParent( pParent );
   return slider;
}

void WaveTrackControls::ReCreateGainSlider(ThemeChangeMessage message)
{
   if (message.appearance)
      return;
   const wxPoint point{ 0, 0 };
   wxRect sliderRect;
   GetGainRect(point, sliderRect);

   float defPos = 1.0;
   /* i18n-hint: Title of the Gain slider, used to adjust the volume */
   gGain = std::make_unique<LWSlider>(nullptr, XO("Gain"),
                        wxPoint(sliderRect.x, sliderRect.y),
                        wxSize(sliderRect.width, sliderRect.height),
                        DB_SLIDER);
   gGain->SetDefaultValue(defPos);

   gGainCaptured = std::make_unique<LWSlider>(nullptr, XO("Gain"),
                                wxPoint(sliderRect.x, sliderRect.y),
                                wxSize(sliderRect.width, sliderRect.height),
                                DB_SLIDER);
   gGainCaptured->SetDefaultValue(defPos);
}

LWSlider *WaveTrackControls::PanSlider(
   CellularPanel &panel, const WaveTrack &wt )
{
   auto &controls = TrackControls::Get( wt );
   auto rect = panel.FindRect( controls );
   wxRect sliderRect;
   GetPanRect( rect.GetTopLeft(), sliderRect );
   return PanSlider( sliderRect, &wt, false,  &panel );
}

LWSlider * WaveTrackControls::PanSlider
(const wxRect &sliderRect, const WaveTrack *t, bool captured, wxWindow *pParent)
{
   static std::once_flag flag;
   std::call_once( flag, []{ ReCreatePanSlider({}); });
   static auto subscription = theTheme.Subscribe(ReCreatePanSlider);

   wxPoint pos = sliderRect.GetPosition();
   float pan = t ? t->GetPan() : 0.0;

   gPan->Move(pos);
   gPan->Set(pan);
   gPanCaptured->Move(pos);
   gPanCaptured->Set(pan);

   auto slider = (captured ? gPanCaptured : gPan).get();
   slider->SetParent( pParent );
   return slider;
}

void WaveTrackControls::ReCreatePanSlider(ThemeChangeMessage message)
{
   if (message.appearance)
      return;
   const wxPoint point{ 0, 0 };
   wxRect sliderRect;
   GetPanRect(point, sliderRect);

   float defPos = 0.0;
   /* i18n-hint: Title of the Pan slider, used to move the sound left or right */
   gPan = std::make_unique<LWSlider>(nullptr, XO("Pan"),
                       wxPoint(sliderRect.x, sliderRect.y),
                       wxSize(sliderRect.width, sliderRect.height),
                       PAN_SLIDER);
   gPan->SetDefaultValue(defPos);

   gPanCaptured = std::make_unique<LWSlider>(nullptr, XO("Pan"),
                               wxPoint(sliderRect.x, sliderRect.y),
                               wxSize(sliderRect.width, sliderRect.height),
                               PAN_SLIDER);
   gPanCaptured->SetDefaultValue(defPos);
}

using DoGetWaveTrackControls = DoGetControls::Override< WaveTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetWaveTrackControls) {
   return [](WaveTrack &track) {
      return std::make_shared<WaveTrackControls>( track.SharedPointer() );
   };
}

using GetDefaultWaveTrackHeight = GetDefaultTrackHeight::Override< WaveTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetDefaultWaveTrackHeight) {
   return [](WaveTrack &) {
      return WaveTrackControls::DefaultWaveTrackHeight();
   };
}

