/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackCommand.cpp
\brief Definitions for SetTrackCommand

\class SetTrackCommand
\brief Command that sets track information, name, mute/solo etc.

*//*******************************************************************/

#include "../Audacity.h"
#include "SetTrackInfoCommand.h"
#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../prefs/WaveformSettings.h"
#include "../prefs/SpectrogramSettings.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

SetTrackCommand::SetTrackCommand()
{
}

enum kColours
{
   kColour0,
   kColour1,
   kColour2,
   kColour3,
   nColours
};

static const wxString kColourStrings[nColours] =
{
   XO("Color0"),
   XO("Color1"),
   XO("Color2"),
   XO("Color3"),
};


enum kDisplayTypes
{
   kWaveform,
   kSpectrogram,
   nDisplayTypes
};

static const wxString kDisplayTypeStrings[nDisplayTypes] =
{
   XO("Waveform"),
   XO("Spectrogram"),
};

enum kScaleTypes
{
   kLinear,
   kDb,
   nScaleTypes
};

static const wxString kScaleTypeStrings[nScaleTypes] =
{
   XO("Linear"),
   XO("dB"),
};


enum kZoomTypes
{
   kReset,
   kTimes2,
   kHalfWave,
   nZoomTypes
};

static const wxString kZoomTypeStrings[nZoomTypes] =
{
   XO("Reset"),
   XO("Times2"),
   XO("HalfWave"),
};


bool SetTrackCommand::DefineParams( ShuttleParams & S ){ 
   wxArrayString colours(  nColours,      kColourStrings );
   wxArrayString displays( nDisplayTypes, kDisplayTypeStrings );
   wxArrayString scales(   nScaleTypes,   kScaleTypeStrings );
   wxArrayString vzooms(   nZoomTypes,    kZoomTypeStrings );

   S.OptionalY( bHasTrackIndex     ).Define(     mTrackIndex,     wxT("Track"),      0, 0, 100 );
   S.OptionalN( bHasChannelIndex   ).Define(     mChannelIndex,   wxT("Channel"),    0, 0, 100 );
   S.OptionalN( bHasTrackName      ).Define(     mTrackName,      wxT("Name"),       wxT("Unnamed") );
   S.OptionalN( bHasPan            ).Define(     mPan,            wxT("Pan"),        0.0, -1.0, 1.0);
   S.OptionalN( bHasGain           ).Define(     mGain,           wxT("Gain"),       1.0,  0.0, 10.0);
   S.OptionalN( bHasHeight         ).Define(     mHeight,         wxT("Height"),     120, 44, 700 );
   S.OptionalN( bHasDisplayType    ).DefineEnum( mDisplayType,    wxT("Display"),    kWaveform, displays );
   S.OptionalN( bHasScaleType      ).DefineEnum( mScaleType,      wxT("Scale"),      kLinear,   scales );
   S.OptionalN( bHasColour         ).DefineEnum( mColour,         wxT("Color"),      kColour0,  colours );
   S.OptionalN( bHasUseSpecPrefs   ).Define(     bUseSpecPrefs,   wxT("SpecPrefs"),  false );
   S.OptionalN( bHasVZoom          ).DefineEnum( mVZoom,          wxT("VZoom"),      kReset,    vzooms );

   S.OptionalN( bHasSpectralSelect ).Define(     bSpectralSelect, wxT("SpectralSel"),true );
   S.OptionalN( bHasGrayScale      ).Define(     bGrayScale,      wxT("GrayScale"),  false );
   // There is also a select command.  This is an alternative.
   S.OptionalN( bHasSelected       ).Define(     bSelected,       wxT("Selected"),   false );
   S.OptionalN( bHasFocused        ).Define(     bFocused,        wxT("Focused"),    false );
   S.OptionalN( bHasSolo           ).Define(     bSolo,           wxT("Solo"),       false );
   S.OptionalN( bHasMute           ).Define(     bMute,           wxT("Mute"),       false );
   return true;
};

void SetTrackCommand::PopulateOrExchange(ShuttleGui & S)
{
   wxArrayString colours(  nColours,      kColourStrings );
   wxArrayString displays( nDisplayTypes, kDisplayTypeStrings );
   wxArrayString scales(   nScaleTypes,   kScaleTypeStrings );
   wxArrayString vzooms(   nZoomTypes,    kZoomTypeStrings );

   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxALIGN_CENTER);
   {
      S.Optional( bHasTrackIndex  ).TieNumericTextBox(  _("Track Index:"),   mTrackIndex );
      S.Optional( bHasChannelIndex).TieNumericTextBox(  _("Channel Index:"), mChannelIndex );
      S.Optional( bHasTrackName   ).TieTextBox(         _("Name:"),          mTrackName );
      S.Optional( bHasPan         ).TieSlider(          _("Pan:"),           mPan,  1.0, -1.0);
      S.Optional( bHasGain        ).TieSlider(          _("Gain:"),          mGain, 10.0, 0.0);
      S.Optional( bHasHeight      ).TieNumericTextBox(  _("Height:"),        mHeight );
      S.Optional( bHasColour      ).TieChoice(          _("Colour:"),        mColour,      &colours );
      S.Optional( bHasDisplayType ).TieChoice(          _("Display:"),       mDisplayType, &displays );
      S.Optional( bHasScaleType   ).TieChoice(          _("Scale:"),         mScaleType,   &scales );
      S.Optional( bHasVZoom       ).TieChoice(          _("VZoom:"),         mVZoom,       &vzooms );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxALIGN_CENTER);
   {
      S.Optional( bHasUseSpecPrefs   ).TieCheckBox( _("Use Spectral Prefs:"), bUseSpecPrefs );
      S.Optional( bHasSpectralSelect ).TieCheckBox( _("Spectral Select:"),    bSpectralSelect);
      S.Optional( bHasGrayScale      ).TieCheckBox( _("Gray Scale:"),         bGrayScale );
      S.Optional( bHasSelected       ).TieCheckBox( _("Selected:"),           bSelected );
      S.Optional( bHasFocused        ).TieCheckBox( _("Focused:"),            bFocused);
      S.Optional( bHasSolo           ).TieCheckBox( _("Solo:"),               bSolo);
      S.Optional( bHasMute           ).TieCheckBox( _("Mute:"),               bMute);
   }
   S.EndMultiColumn();
}

bool SetTrackCommand::Apply(const CommandContext & context)
{

   long i = 0;// track counter
   long j = 0;// channel counter
   TrackListIterator iter(context.GetProject()->GetTracks());
   Track *t = iter.First();
   bool bIsSecondChannel = false;
   while (t )
   {
      bool bThisTrack = 
         (bHasTrackIndex && (i==mTrackIndex)) ||
         (bHasChannelIndex && (j==mChannelIndex ) ) ||
         (!bHasTrackIndex && !bHasChannelIndex) ;

      if( bThisTrack ){
         auto wt = dynamic_cast<WaveTrack *>(t);
         auto pt = dynamic_cast<PlayableTrack *>(t);

         // You can get some intriguing effects by setting R and L channels to 
         // different values.
         if( bHasTrackName )
            t->SetName(mTrackName);
         if( wt && bHasPan )
            wt->SetPan(mPan);
         if( wt && bHasGain )
            wt->SetGain(mGain);
         if( wt && bHasColour )
            wt->SetWaveColorIndex( mColour );
         if( t && bHasHeight )
            t->SetHeight( mHeight );

         if( wt && bHasDisplayType  )
            wt->SetDisplay(
               (mDisplayType == kWaveform) ? 
                  WaveTrack::WaveTrackDisplay::Waveform
                  : WaveTrack::WaveTrackDisplay::Spectrum
               );
         if( wt && bHasScaleType )
            wt->GetIndependentWaveformSettings().scaleType = 
               (mScaleType==kLinear) ? 
                  WaveformSettings::stLinear
                  : WaveformSettings::stLogarithmic;

         if( wt && bHasUseSpecPrefs   ){
            wt->UseSpectralPrefs( bUseSpecPrefs );
         }
         if( wt && bHasSpectralSelect )
            wt->GetSpectrogramSettings().spectralSelection = bSpectralSelect;
         if( wt && bHasGrayScale )
            wt->GetSpectrogramSettings().isGrayscale = bGrayScale;
         if( wt && bHasVZoom ){
            switch( mVZoom ){
               default:
               case kReset: wt->SetDisplayBounds(-1,1); break;
               case kTimes2: wt->SetDisplayBounds(-2,2); break;
               case kHalfWave: wt->SetDisplayBounds(0,1); break;
            }
         }
         // These ones don't make sense on the second channel of a stereo track.
         if( !bIsSecondChannel ){
            if( bHasSelected )
               t->SetSelected(bSelected);
            if( bHasFocused && bFocused)
            {
               TrackPanel *panel = context.GetProject()->GetTrackPanel();
               panel->SetFocusedTrack( t );
            }
            if( pt && bHasSolo )
               pt->SetSolo(bSolo);
            if( pt && bHasMute )
               pt->SetMute(bMute);
         }
      }
      bIsSecondChannel = t->GetLinked();
      if( !bIsSecondChannel )
         ++i;
      j++;
      t = iter.Next();
   }
   return true;
}


