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

#include "../Audacity.h"
#include "TracksPrefs.h"

//#include <algorithm>
//#include <wx/defs.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include "../Experimental.h"
#include "../Internat.h"

int TracksPrefs::iPreferencePinned = -1;

namespace {
   const wxChar *PinnedHeadPreferenceKey()
   {
      return wxT("/AudioIO/PinnedHead");
   }

   bool PinnedHeadPreferenceDefault()
   {
      return false;
   }
}


//////////
static const IdentInterfaceSymbol choicesView[] = {
   { XO("Waveform") },
   { wxT("WaveformDB"), XO("Waveform (dB)") },
   { XO("Spectrogram") }
};
static const int intChoicesView[] = {
   (int)(WaveTrack::Waveform),
   (int)(WaveTrack::obsoleteWaveformDBDisplay),
   (int)(WaveTrack::Spectrum)
};
static const size_t nChoicesView = WXSIZEOF(choicesView);
static_assert( nChoicesView == WXSIZEOF(intChoicesView), "size mismatch" );

static const size_t defaultChoiceView = 0;

class TracksViewModeSetting : public EncodedEnumSetting {
public:
   TracksViewModeSetting(
      const wxString &key,
      const IdentInterfaceSymbol symbols[], size_t nSymbols,
      size_t defaultSymbol,

      const int intValues[],
      const wxString &oldKey
   )
      : EncodedEnumSetting{
         key, symbols, nSymbols, defaultSymbol, intValues, oldKey }
   {}

   void Migrate( wxString &value ) override
   {
      // Special logic for this preference which was twice migrated!

      // First test for the older but not oldest key:
      EncodedEnumSetting::Migrate(value);
      if (!value.empty())
         return;

      // PRL:  Bugs 1043, 1044
      // 2.1.1 writes a NEW key for this preference, which got NEW values,
      // to avoid confusing version 2.1.0 if it reads the preference file afterwards.
      // Prefer the NEW preference key if it is present

      int oldMode;
      gPrefs->Read(wxT("/GUI/DefaultViewMode"), // The very old key
         &oldMode,
         (int)(WaveTrack::Waveform));
      auto viewMode = WaveTrack::ConvertLegacyDisplayValue(oldMode);

      // Now future-proof 2.1.1 against a recurrence of this sort of bug!
      viewMode = WaveTrack::ValidateWaveTrackDisplay(viewMode);

      const_cast<TracksViewModeSetting*>(this)->WriteInt( viewMode );
      gPrefs->Flush();

      value = mSymbols[ FindInt(viewMode) ].Internal();
   }
};

static TracksViewModeSetting viewModeSetting{
   wxT("/GUI/DefaultViewModeChoice"),
   choicesView, nChoicesView, defaultChoiceView,

   intChoicesView,
   wxT("/GUI/DefaultViewModeNew")
};

WaveTrack::WaveTrackDisplay TracksPrefs::ViewModeChoice()
{
   return (WaveTrack::WaveTrackDisplay) viewModeSetting.ReadInt();
}

//////////
static const IdentInterfaceSymbol choicesSampleDisplay[] = {
   { wxT("ConnectDots"), XO("Connect dots") },
   { wxT("StemPlot"), XO("Stem plot") }
};
static const size_t nChoicesSampleDisplay = WXSIZEOF( choicesSampleDisplay );
static const int intChoicesSampleDisplay[] = {
   (int) WaveTrack::LinearInterpolate,
   (int) WaveTrack::StemPlot
};
static_assert(
   nChoicesSampleDisplay == WXSIZEOF(intChoicesSampleDisplay), "size mismatch" );

static const size_t defaultChoiceSampleDisplay = 1;

static EncodedEnumSetting sampleDisplaySetting{
   wxT("/GUI/SampleViewChoice"),
   choicesSampleDisplay, nChoicesSampleDisplay, defaultChoiceSampleDisplay,

   intChoicesSampleDisplay,
   wxT("/GUI/SampleView")
};

WaveTrack::SampleDisplay TracksPrefs::SampleViewChoice()
{
   return (WaveTrack::SampleDisplay) sampleDisplaySetting.ReadInt();
}

//////////
TracksPrefs::TracksPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: "Tracks" include audio recordings but also other collections of
 * data associated with a time line, such as sequences of labels, and musical
 * notes */
:  PrefsPanel(parent, winid, _("Tracks"))
{
   Populate();
}

TracksPrefs::~TracksPrefs()
{
}

void TracksPrefs::Populate()
{
   // Keep view choices and codes in proper correspondence --
   // we don't display them by increasing integer values.


   // How samples are displayed when zoomed in:

   mZoomChoices.Add( _("Fit to Width") );
   mZoomCodes.push_back( WaveTrack::kZoomToFit );
   mZoomChoices.Add( _("Zoom to Selection") );
   mZoomCodes.push_back( WaveTrack::kZoomToSelection );
   mZoomChoices.Add( _("Zoom Default") );
   mZoomCodes.push_back( WaveTrack::kZoomDefault );
   mZoomChoices.Add( _("Minutes") );
   mZoomCodes.push_back( WaveTrack::kZoomMinutes );
   mZoomChoices.Add( _("Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoomSeconds );
   mZoomChoices.Add( _("5ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom5ths );
   mZoomChoices.Add( _("10ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom10ths );
   mZoomChoices.Add( _("20ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom20ths );
   mZoomChoices.Add( _("50ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom50ths );
   mZoomChoices.Add( _("100ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom100ths );
   mZoomChoices.Add( _("500ths of Seconds") );
   mZoomCodes.push_back( WaveTrack::kZoom500ths );
   mZoomChoices.Add( _("MilliSeconds") );
   mZoomCodes.push_back( WaveTrack::kZoomMilliSeconds );
   mZoomChoices.Add( _("Samples") );
   mZoomCodes.push_back( WaveTrack::kZoomSamples );
   mZoomChoices.Add( _("4 Pixels per Sample") );
   mZoomCodes.push_back( WaveTrack::kZoom4To1 );
   mZoomChoices.Add( _("Max Zoom") );
   mZoomCodes.push_back( WaveTrack::kMaxZoom );



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
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Display"));
   {
      S.TieCheckBox(_("&Pinned Recording/Playback head"),
                    PinnedHeadPreferenceKey(),
                    PinnedHeadPreferenceDefault());
      S.TieCheckBox(_("A&uto-scroll if head unpinned"),
                    wxT("/GUI/AutoScroll"),
                    true);
      S.TieCheckBox(_("Auto-&fit track height"),
                    wxT("/GUI/TracksFitVerticallyZoomed"),
                    false);
      S.TieCheckBox(_("Sho&w audio track name as overlay"),
                  wxT("/GUI/ShowTrackNameInWaveform"),
                  false);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Default &view mode:"),
                     viewModeSetting );

         S.TieChoice(_("Display &samples:"),
                     sampleDisplaySetting );

         S.TieTextBox(_("Default audio track &name:"),
                      wxT("/GUI/TrackNames/DefaultTrackName"),
                      _("Audio Track"),
                      30);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Zoom Toggle"));
   {
      S.StartMultiColumn(4);
      {
         S.TieChoice(_("Preset 1:"),
                     wxT("/GUI/ZoomPreset1"),
                     WaveTrack::kZoomDefault,
                     mZoomChoices,
                     mZoomCodes);

         S.TieChoice(_("Preset 2:"),
                     wxT("/GUI/ZoomPreset2"),
                     WaveTrack::kZoom4To1,
                     mZoomChoices,
                     mZoomCodes);
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

wxString TracksPrefs::GetDefaultAudioTrackNamePreference()
{
   const auto name =
      gPrefs->Read(wxT("/GUI/TrackNames/DefaultTrackName"), wxT(""));

   if (name.empty() || ( name == "Audio Track" ))
      // When nothing was specified,
      // the default-default is whatever translation of...
      /* i18n-hint: The default name for an audio track. */
      return _("Audio Track");
   else
      return name;
}

bool TracksPrefs::Commit()
{
   // Bug 1583: Clear the caching of the preference pinned state.
   iPreferencePinned = -1;
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // Bug 1661: Don't store the name for new tracks if the name is the
   // default in that language.
   if (GetDefaultAudioTrackNamePreference() == _("Audio Track")) {
      gPrefs->DeleteEntry(wxT("/GUI/TrackNames/DefaultTrackName"));
      gPrefs->Flush();
   }

   return true;
}

wxString TracksPrefs::HelpPageName()
{
   return "Tracks_Preferences";
}

PrefsPanel *TracksPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew TracksPrefs(parent, winid);
}
