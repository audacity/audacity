/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class TracksPrefs
\brief A PrefsPanel for track display and behavior properties.

*//*******************************************************************/


#include "TracksPrefs.h"
#include "MemoryX.h"

//#include <algorithm>
//#include <wx/defs.h>

#include "Prefs.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "UndoManager.h"
#include "Viewport.h"
#include "WaveformSettings.h"
#include "WaveTrack.h"

int TracksPrefs::iPreferencePinned = -1;

BoolSetting TracksPrefs::TracksFitVerticallyZoomed {
   "/GUI/TracksFitVerticallyZoomed", false };

namespace {
   const wxChar *PinnedHeadPreferenceKey()
   {
      return wxT("/AudioIO/PinnedHead");
   }

   bool PinnedHeadPreferenceDefault()
   {
      return false;
   }

   const wxChar *PinnedHeadPositionPreferenceKey()
   {
      return wxT("/AudioIO/PinnedHeadPosition");
   }

   double PinnedHeadPositionPreferenceDefault()
   {
      return 0.5;
   }
}

//////////
// There is a complicated migration history here!
namespace {
   const auto key0 = wxT("/GUI/DefaultViewMode");
   const auto key1 = wxT("/GUI/DefaultViewModeNew");
   const auto key2 = wxT("/GUI/DefaultViewModeChoice");
   const auto key3 = wxT("/GUI/DefaultViewModeChoiceNew");

   const wxString obsoleteValue{ wxT("WaveformDB") };
};

class TracksViewModeEnumSetting
   : public EnumSetting<WaveChannelViewConstants::Display> {
public:
   using EnumSetting<WaveChannelViewConstants::Display>::EnumSetting;

   void Migrate( wxString &value ) override
   {
      // Special logic for this preference which was three times migrated!

      // PRL:  Bugs 1043, 1044
      // 2.1.1 writes a NEW key for this preference, which got NEW values,
      // to avoid confusing version 2.1.0 if it reads the preference file afterwards.
      // Prefer the NEW preference key if it is present

      static const EnumValueSymbol waveformSymbol{ XO("Waveform") };
      static const EnumValueSymbol spectrumSymbol{ XO("Spectrogram") };

      WaveChannelViewConstants::Display viewMode;
      int oldMode;
      wxString newValue;
      auto stringValue =
         [](WaveChannelViewConstants::Display display) {
         switch (display) {
            case WaveChannelViewConstants::Spectrum:
               return spectrumSymbol.Internal();
            case WaveChannelViewConstants::obsoleteWaveformDBDisplay:
               return obsoleteValue;
            default:
               return waveformSymbol.Internal();
         }
      };

      if (gPrefs->Read(key0, // The very old key
         &oldMode,
         (int)(WaveChannelViewConstants::Waveform))) {
         viewMode = WaveChannelViewConstants::ConvertLegacyDisplayValue(oldMode);
         newValue = stringValue(viewMode);
      }
      else if ( gPrefs->Read(key1,
         &oldMode,
         (int)(WaveChannelViewConstants::Waveform))) {
         viewMode = static_cast<WaveChannelViewConstants::Display>(oldMode);
         newValue = stringValue(viewMode);
      }
      else
         gPrefs->Read( key2, &newValue );

      if ( !gPrefs->Read( key3, &value ) ) {
         if (newValue == obsoleteValue) {
            newValue = waveformSymbol.Internal();
            gPrefs->Write(
               WaveformSettings::waveformScaleKey,
               WaveformSettings::dbLogValueString);
         }

         Write( value = newValue );
         gPrefs->Flush();
         return;
      }
   }
};

static TracksViewModeEnumSetting ViewModeSetting()
{
   // Do a delayed computation, so that registration of sub-view types completes
   // first
   const auto &types = WaveChannelSubViewType::All();
   auto symbols = transform_container< EnumValueSymbols >(
      types, std::mem_fn(&WaveChannelSubViewType::name));
   auto ids = transform_container<std::vector<WaveChannelSubViewType::Display>>(
      types, std::mem_fn(&WaveChannelSubViewType::id));

   // Special entry for multi
   symbols.push_back(WaveChannelViewConstants::MultiViewSymbol);
   ids.push_back(WaveChannelViewConstants::MultiView);

   return {
      key3,
      symbols,
      0, // Waveform
      ids
   };
}

WaveChannelViewConstants::Display TracksPrefs::ViewModeChoice()
{
   return ViewModeSetting().ReadEnum();
}

//////////
static EnumSetting<WaveChannelViewConstants::SampleDisplay>
sampleDisplaySetting{
   wxT("/GUI/SampleViewChoice"),
   {
      { wxT("ConnectDots"), XO("Connect dots") },
      { wxT("StemPlot"), XO("Stem plot") }
   },
   1, // StemPlot

   // for migrating old preferences:
   {
      WaveChannelViewConstants::LinearInterpolate,
      WaveChannelViewConstants::StemPlot
   },
   wxT("/GUI/SampleView")
};

WaveChannelViewConstants::SampleDisplay TracksPrefs::SampleViewChoice()
{
   return sampleDisplaySetting.ReadEnum();
}

//////////
static const std::initializer_list<EnumValueSymbol> choicesZoom{
   { wxT("FitToWidth"), XO("Fit to Width") },
   { wxT("ZoomToSelection"), XO("Zoom to Selection") },
   { wxT("ZoomDefault"), XO("Zoom Default") },
   { XO("Minutes") },
   { XO("Seconds") },
   { wxT("FifthsOfSeconds"), XO("5ths of Seconds") },
   { wxT("TenthsOfSeconds"), XO("10ths of Seconds") },
   { wxT("TwentiethsOfSeconds"), XO("20ths of Seconds") },
   { wxT("FiftiethsOfSeconds"), XO("50ths of Seconds") },
   { wxT("HundredthsOfSeconds"), XO("100ths of Seconds") },
   { wxT("FiveHundredthsOfSeconds"), XO("500ths of Seconds") },
   { XO("MilliSeconds") },
   { XO("Samples") },
   { wxT("FourPixelsPerSample"), XO("4 Pixels per Sample") },
   { wxT("MaxZoom"), XO("Max Zoom") },
};
static auto enumChoicesZoom = {
   WaveChannelViewConstants::kZoomToFit,
   WaveChannelViewConstants::kZoomToSelection,
   WaveChannelViewConstants::kZoomDefault,
   WaveChannelViewConstants::kZoomMinutes,
   WaveChannelViewConstants::kZoomSeconds,
   WaveChannelViewConstants::kZoom5ths,
   WaveChannelViewConstants::kZoom10ths,
   WaveChannelViewConstants::kZoom20ths,
   WaveChannelViewConstants::kZoom50ths,
   WaveChannelViewConstants::kZoom100ths,
   WaveChannelViewConstants::kZoom500ths,
   WaveChannelViewConstants::kZoomMilliSeconds,
   WaveChannelViewConstants::kZoomSamples,
   WaveChannelViewConstants::kZoom4To1,
   WaveChannelViewConstants::kMaxZoom,
};

static EnumSetting<WaveChannelViewConstants::ZoomPresets> zoom1Setting{
   wxT("/GUI/ZoomPreset1Choice"),
   choicesZoom,
   2, // kZoomDefault

   // for migrating old preferences:
   enumChoicesZoom,
   wxT("/GUI/ZoomPreset1")
};

static EnumSetting<WaveChannelViewConstants::ZoomPresets> zoom2Setting{
   wxT("/GUI/ZoomPreset2Choice"),
   choicesZoom,
   13, // kZoom4To1

   // for migrating old preferences:
   enumChoicesZoom,
   wxT("/GUI/ZoomPreset2")
};

WaveChannelViewConstants::ZoomPresets TracksPrefs::Zoom1Choice()
{
   return zoom1Setting.ReadEnum();
}

WaveChannelViewConstants::ZoomPresets TracksPrefs::Zoom2Choice()
{
   return zoom2Setting.ReadEnum();
}

//////////
TracksPrefs::TracksPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: "Tracks" include audio recordings but also other collections of
 * data associated with a time line, such as sequences of labels, and musical
 * notes */
:  PrefsPanel(parent, winid, XO("Tracks"))
{
   Populate();
}

TracksPrefs::~TracksPrefs()
{
}

ComponentInterfaceSymbol TracksPrefs::GetSymbol() const
{
   return TRACKS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString TracksPrefs::GetDescription() const
{
   return XO("Preferences for Tracks");
}

ManualPageID TracksPrefs::HelpPageName()
{
   return "Tracks_Preferences";
}

void TracksPrefs::Populate()
{
   // Keep view choices and codes in proper correspondence --
   // we don't display them by increasing integer values.


   // How samples are displayed when zoomed in:


   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TracksPrefs::PopulateOrExchange(ShuttleGui & S)
{
   auto viewModeSetting = ViewModeSetting();

   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("Display"));
   {
      S.TieCheckBox(XXO("Auto-&fit track height"), TracksFitVerticallyZoomed);
      S.TieCheckBox(XXO("Use &half-wave display when collapsed"),
                  {wxT("/GUI/CollapseToHalfWave"),
                   false});
#ifdef SHOW_PINNED_UNPINNED_IN_PREFS
      S.TieCheckBox(XXO("&Pinned Recording/Playback head"),
         {PinnedHeadPreferenceKey(),
          PinnedHeadPreferenceDefault()});
#endif
      S.TieCheckBox(XXO("A&uto-scroll if head unpinned"),
         {wxT("/GUI/AutoScroll"),
          true});

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
#ifdef SHOW_PINNED_POSITION_IN_PREFS
         S.TieNumericTextBox(
            XXO("Pinned &head position"),
            {PinnedHeadPositionPreferenceKey(),
             PinnedHeadPositionPreferenceDefault()},
            30
         );
#endif

         S.TieChoice(XXO("Default &view mode:"),
                     viewModeSetting );

         S.TieChoice(
            XXO("Default Waveform scale:"),
            WaveformSettings::waveformScaleSetting);

         S.TieChoice(XXO("Display &samples:"),
                     sampleDisplaySetting );

         S.TieTextBox(XXO("Default audio track &name:"),
                      AudioTrackNameSetting,
                      30);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(XO("Zoom Toggle"));
   {
      S.StartMultiColumn(4);
      {
         S.TieChoice(XXO("Preset 1:"),
                     zoom1Setting );

         S.TieChoice(XXO("Preset 2:"),
                     zoom2Setting );
      }
   }
   S.EndStatic();
   S.EndScroller();
}

bool TracksPrefs::GetPinnedHeadPreference()
{
   // JKC: Cache this setting as it is read many times during drawing, and otherwise causes screen flicker.
   // Correct solution would be to re-write wxFileConfig to be efficient.
   if( iPreferencePinned >= 0 )
      return iPreferencePinned == 1;
   bool bResult = gPrefs->ReadBool(PinnedHeadPreferenceKey(), PinnedHeadPreferenceDefault());
   iPreferencePinned = bResult ? 1: 0;
   return bResult;
}

void TracksPrefs::SetPinnedHeadPreference(bool value, bool flush)
{
   iPreferencePinned = value ? 1 :0;
   gPrefs->Write(PinnedHeadPreferenceKey(), value);
   if(flush)
      gPrefs->Flush();
}

double TracksPrefs::GetPinnedHeadPositionPreference()
{
   auto value = gPrefs->ReadDouble(
      PinnedHeadPositionPreferenceKey(),
      PinnedHeadPositionPreferenceDefault());
   return std::max(0.0, std::min(1.0, value));
}

void TracksPrefs::SetPinnedHeadPositionPreference(double value, bool flush)
{
   value = std::max(0.0, std::min(1.0, value));
   gPrefs->Write(PinnedHeadPositionPreferenceKey(), value);
   if(flush)
      gPrefs->Flush();
}

bool TracksPrefs::Commit()
{
   // Bug 1583: Clear the caching of the preference pinned state.
   iPreferencePinned = -1;
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // Bug 1661: Don't store the name for new tracks if the name is the
   // default in that language.
   if (WaveTrack::GetDefaultAudioTrackNamePreference() ==
       AudioTrackNameSetting.GetDefault()) {
      AudioTrackNameSetting.Delete();
      gPrefs->Flush();
   }

   AudioTrackNameSetting.Invalidate();
   TracksFitVerticallyZoomed.Invalidate();
   return true;
}

namespace{
PrefsPanel::Registration sAttachment{ "Tracks",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew TracksPrefs(parent, winid);
   }
};

//! Observer attached to each project applies the vertical zoom fit preference
struct Handler : ClientData::Base {
   explicit Handler(AudacityProject &project) : mProject{ project }
      , mUndoSubscription{ UndoManager::Get(mProject)
         .Subscribe([this](const UndoRedoMessage &message){
            if (message.type == UndoRedoMessage::Pushed &&
               TracksPrefs::TracksFitVerticallyZoomed.Read()
            )
               Viewport::Get(mProject).ZoomFitVertically(); })}
   {}

   Handler(const Handler &) = delete;
   Handler &operator=(const Handler &) = delete;

   AudacityProject &mProject;
   const Observer::Subscription mUndoSubscription;
}; // struct Handler

static const AudacityProject::AttachedObjects::RegisteredFactory key{
   Callable::UniqueMaker<Handler, AudacityProject &>()
};
}
