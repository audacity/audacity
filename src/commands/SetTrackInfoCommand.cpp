/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackCommand.cpp
\brief Definitions for SetTrackCommand built up from 
SetTrackBase, SetTrackStatusCommand, SetTrackAudioCommand and
SetTrackVisualsCommand

\class SetTrackBase
\brief Base class for the various SetTrackCommand classes.  
Sbclasses provide the settings that are relevant to them.

\class SetTrackStatusCommand
\brief A SetTrackBase that sets name, selected and focus.

\class SetTrackAudioCommand
\brief A SetTrackBase that sets pan, gain, mute and solo.

\class SetTrackVisualsCommand
\brief A SetTrackBase that sets appearance of a track.

\class SetTrackCommand
\brief A SetTrackBase that combines SetTrackStatusCommand,
SetTrackAudioCommand and SetTrackVisualsCommand.

*//*******************************************************************/

#include "../Audacity.h"
#include "SetTrackInfoCommand.h"

#include "../Project.h"
#include "../Track.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../prefs/WaveformSettings.h"
#include "../prefs/SpectrogramSettings.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "CommandContext.h"

SetTrackBase::SetTrackBase(){
   mbPromptForTracks = true;
}

//Define for the old scheme, where SetTrack defines its own track selection.
//rather than using the current selection.
//#define USE_OWN_TRACK_SELECTION


bool SetTrackBase::ApplyInner( const CommandContext &context, Track *t  )
{
      static_cast<void>(&context);
      static_cast<void>(&t);
      return true;
};


bool SetTrackBase::DefineParams( ShuttleParams & S)
{
   static_cast<void>(S);
#ifdef USE_OWN_TRACK_SELECTION
   S.OptionalY( bHasTrackIndex     ).Define(     mTrackIndex,     wxT("Track"),      0, 0, 100 );
   S.OptionalN( bHasChannelIndex   ).Define(     mChannelIndex,   wxT("Channel"),    0, 0, 100 );
#endif
   return true;
}

void SetTrackBase::PopulateOrExchange(ShuttleGui & S)
{
   static_cast<void>(S);
#ifdef USE_OWN_TRACK_SELECTION
   if( !mbPromptForTracks )
      return;
   S.AddSpace(0, 5);
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasTrackIndex  ).TieNumericTextBox(  _("Track Index:"),   mTrackIndex );
      S.Optional( bHasChannelIndex).TieNumericTextBox(  _("Channel Index:"), mChannelIndex );
   }
   S.EndMultiColumn();
#endif
}

bool SetTrackBase::Apply(const CommandContext & context  )
{
   long i = 0;// track counter
   long j = 0;// channel counter
   auto tracks = context.GetProject()->GetTracks();
   for ( auto t : tracks->Leaders() )
   {
      auto channels = TrackList::Channels(t);
      for ( auto channel : channels ) {
         bool bThisTrack =
#ifdef USE_OWN_TRACK_SELECTION
         (bHasTrackIndex && (i==mTrackIndex)) ||
         (bHasChannelIndex && (j==mChannelIndex ) ) ||
         (!bHasTrackIndex && !bHasChannelIndex) ;
#else
         channel->GetSelected();
#endif

         if( bThisTrack ){
            ApplyInner( context, channel );
         }
         ++j; // count all channels
      }
      ++i; // count groups of channels
   }
   return true;
}

bool SetTrackStatusCommand::DefineParams( ShuttleParams & S ){ 
   SetTrackBase::DefineParams( S );
   S.OptionalN( bHasTrackName      ).Define(     mTrackName,      wxT("Name"),       _("Unnamed") );
   // There is also a select command.  This is an alternative.
   S.OptionalN( bHasSelected       ).Define(     bSelected,       wxT("Selected"),   false );
   S.OptionalN( bHasFocused        ).Define(     bFocused,        wxT("Focused"),    false );
   return true;
};

void SetTrackStatusCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasTrackName   ).TieTextBox(         _("Name:"),          mTrackName );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasSelected       ).TieCheckBox( _("Selected"),           bSelected );
      S.Optional( bHasFocused        ).TieCheckBox( _("Focused"),            bFocused);
   }
   S.EndMultiColumn();
}

bool SetTrackStatusCommand::ApplyInner(const CommandContext & context, Track * t )
{
   //auto wt = dynamic_cast<WaveTrack *>(t);
   //auto pt = dynamic_cast<PlayableTrack *>(t);

   // You can get some intriguing effects by setting R and L channels to 
   // different values.
   if( bHasTrackName )
      t->SetName(mTrackName);

   // In stereo tracks, both channels need selecting/deselecting.
   if( bHasSelected )
      t->SetSelected(bSelected);

   // These ones don't make sense on the second channel of a stereo track.
   if( !bIsSecondChannel ){
      if( bHasFocused )
      {
         TrackPanel *panel = context.GetProject()->GetTrackPanel();
         if( bFocused)
            panel->SetFocusedTrack( t );
         else if( t== panel->GetFocusedTrack() )
            panel->SetFocusedTrack( nullptr );
      }
   }
   return true;
}



bool SetTrackAudioCommand::DefineParams( ShuttleParams & S ){ 
   SetTrackBase::DefineParams( S );
   S.OptionalN( bHasMute           ).Define(     bMute,           wxT("Mute"),       false );
   S.OptionalN( bHasSolo           ).Define(     bSolo,           wxT("Solo"),       false );

   S.OptionalN( bHasGain           ).Define(     mGain,           wxT("Gain"),       0.0,  -36.0, 36.0);
   S.OptionalN( bHasPan            ).Define(     mPan,            wxT("Pan"),        0.0, -100.0, 100.0);
   return true;
};

void SetTrackAudioCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasMute           ).TieCheckBox( _("Mute"),               bMute);
      S.Optional( bHasSolo           ).TieCheckBox( _("Solo"),               bSolo);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasGain        ).TieSlider(          _("Gain:"),          mGain, 36.0,-36.0);
      S.Optional( bHasPan         ).TieSlider(          _("Pan:"),           mPan,  100.0, -100.0);
   }
   S.EndMultiColumn();
}

bool SetTrackAudioCommand::ApplyInner(const CommandContext & context, Track * t )
{
   static_cast<void>(context);
   auto wt = dynamic_cast<WaveTrack *>(t);
   auto pt = dynamic_cast<PlayableTrack *>(t);

   if( wt && bHasGain )
      wt->SetGain(DB_TO_LINEAR(mGain));
   if( wt && bHasPan )
      wt->SetPan(mPan/100.0);

   // These ones don't make sense on the second channel of a stereo track.
   if( !bIsSecondChannel ){
      if( pt && bHasSolo )
         pt->SetSolo(bSolo);
      if( pt && bHasMute )
         pt->SetMute(bMute);
   }
   return true;
}



enum kColours
{
   kColour0,
   kColour1,
   kColour2,
   kColour3,
   nColours
};

static const EnumValueSymbol kColourStrings[nColours] =
{
   { wxT("Color0"), XO("Color 0") },
   { wxT("Color1"), XO("Color 1") },
   { wxT("Color2"), XO("Color 2") },
   { wxT("Color3"), XO("Color 3") },
};


enum kDisplayTypes
{
   kWaveform,
   kSpectrogram,
   nDisplayTypes
};

static const EnumValueSymbol kDisplayTypeStrings[nDisplayTypes] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Waveform") },
   { XO("Spectrogram") },
};

enum kScaleTypes
{
   kLinear,
   kDb,
   nScaleTypes
};

static const EnumValueSymbol kScaleTypeStrings[nScaleTypes] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Linear") },
   /* i18n-hint: abbreviates decibels */
   { XO("dB") },
};

enum kZoomTypes
{
   kReset,
   kTimes2,
   kHalfWave,
   nZoomTypes
};

static const EnumValueSymbol kZoomTypeStrings[nZoomTypes] =
{
   { XO("Reset") },
   { wxT("Times2"), XO("Times 2") },
   { XO("HalfWave") },
};

bool SetTrackVisualsCommand::DefineParams( ShuttleParams & S ){ 
   SetTrackBase::DefineParams( S );
   S.OptionalN( bHasHeight         ).Define(     mHeight,         wxT("Height"),     120, 44, 700 );
   S.OptionalN( bHasDisplayType    ).DefineEnum( mDisplayType,    wxT("Display"),    kWaveform, kDisplayTypeStrings, nDisplayTypes );
   S.OptionalN( bHasScaleType      ).DefineEnum( mScaleType,      wxT("Scale"),      kLinear,   kScaleTypeStrings, nScaleTypes );
   S.OptionalN( bHasColour         ).DefineEnum( mColour,         wxT("Color"),      kColour0,  kColourStrings, nColours );
   S.OptionalN( bHasVZoom          ).DefineEnum( mVZoom,          wxT("VZoom"),      kReset,    kZoomTypeStrings, nZoomTypes );
   S.OptionalN( bHasVZoomTop       ).Define(     mVZoomTop,       wxT("VZoomHigh"),  1.0,  -2.0,  2.0 );
   S.OptionalN( bHasVZoomBottom    ).Define(     mVZoomBottom,    wxT("VZoomLow"),   -1.0, -2.0,  2.0 );

   S.OptionalN( bHasUseSpecPrefs   ).Define(     bUseSpecPrefs,   wxT("SpecPrefs"),  false );
   S.OptionalN( bHasSpectralSelect ).Define(     bSpectralSelect, wxT("SpectralSel"),true );
   S.OptionalN( bHasGrayScale      ).Define(     bGrayScale,      wxT("GrayScale"),  false );

   return true;
};

void SetTrackVisualsCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasHeight      ).TieNumericTextBox(  _("Height:"),        mHeight );
      S.Optional( bHasColour      ).TieChoice(          _("Colour:"),        mColour,
         LocalizedStrings(  kColourStrings, nColours ) );
      S.Optional( bHasDisplayType ).TieChoice(          _("Display:"),       mDisplayType,
         LocalizedStrings( kDisplayTypeStrings, nDisplayTypes ) );
      S.Optional( bHasScaleType   ).TieChoice(          _("Scale:"),         mScaleType,
         LocalizedStrings( kScaleTypeStrings, nScaleTypes ) );
      S.Optional( bHasVZoom       ).TieChoice(          _("VZoom:"),         mVZoom,
         LocalizedStrings( kZoomTypeStrings, nZoomTypes ) );
      S.Optional( bHasVZoomTop    ).TieTextBox(         _("VZoom Top:"),     mVZoomTop );
      S.Optional( bHasVZoomBottom ).TieTextBox(         _("VZoom Bottom:"),  mVZoomBottom );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasUseSpecPrefs   ).TieCheckBox( _("Use Spectral Prefs"), bUseSpecPrefs );
      S.Optional( bHasSpectralSelect ).TieCheckBox( _("Spectral Select"),    bSpectralSelect);
      S.Optional( bHasGrayScale      ).TieCheckBox( _("Gray Scale"),         bGrayScale );
   }
   S.EndMultiColumn();
}

bool SetTrackVisualsCommand::ApplyInner(const CommandContext & context, Track * t )
{
   static_cast<void>(context);
   auto wt = dynamic_cast<WaveTrack *>(t);
   //auto pt = dynamic_cast<PlayableTrack *>(t);
   static const double ZOOMLIMIT = 0.001f;

   // You can get some intriguing effects by setting R and L channels to 
   // different values.
   if( wt && bHasColour )
      wt->SetWaveColorIndex( mColour );

   if( t && bHasHeight )
      t->SetHeight( mHeight );

   if( wt && bHasDisplayType  )
      wt->SetDisplay(
         (mDisplayType == kWaveform) ?
            WaveTrack::WaveTrackDisplayValues::Waveform
            : WaveTrack::WaveTrackDisplayValues::Spectrum
         );
   if( wt && bHasScaleType )
      wt->GetIndependentWaveformSettings().scaleType = 
         (mScaleType==kLinear) ? 
            WaveformSettings::stLinear
            : WaveformSettings::stLogarithmic;

   if( wt && bHasVZoom ){
      switch( mVZoom ){
         default:
         case kReset: wt->SetDisplayBounds(-1,1); break;
         case kTimes2: wt->SetDisplayBounds(-2,2); break;
         case kHalfWave: wt->SetDisplayBounds(0,1); break;
      }
   }

   if ( wt && (bHasVZoomTop || bHasVZoomBottom) && !bHasVZoom){
      float vzmin, vzmax;
      wt->GetDisplayBounds(&vzmin, &vzmax);

      if ( !bHasVZoomTop ){
         mVZoomTop = vzmax;
      }
      if ( !bHasVZoomBottom ){
         mVZoomBottom = vzmin;
      }

      // Can't use std::clamp until C++17
      mVZoomTop = std::max(-2.0, std::min(mVZoomTop, 2.0));
      mVZoomBottom = std::max(-2.0, std::min(mVZoomBottom, 2.0));

      if (mVZoomBottom > mVZoomTop){
         std::swap(mVZoomTop, mVZoomBottom);
      }
      if ( mVZoomTop - mVZoomBottom < ZOOMLIMIT ){
         double c = (mVZoomBottom + mVZoomTop) / 2;
         mVZoomBottom = c - ZOOMLIMIT / 2.0;
         mVZoomTop = c + ZOOMLIMIT / 2.0;
      }
      wt->SetDisplayBounds(mVZoomBottom, mVZoomTop);
      TrackPanel *const tp = ::GetActiveProject()->GetTrackPanel();
      tp->UpdateVRulers();
   }

   if( wt && bHasUseSpecPrefs   ){
      wt->UseSpectralPrefs( bUseSpecPrefs );
   }
   if( wt && bHasSpectralSelect ){
      wt->GetSpectrogramSettings().spectralSelection = bSpectralSelect;
   }
   if( wt && bHasGrayScale ){
      wt->GetSpectrogramSettings().isGrayscale = bGrayScale;
   }

   return true;
}


SetTrackCommand::SetTrackCommand()
{
   mSetStatus.mbPromptForTracks = false;
   mSetAudio.mbPromptForTracks = false;
   mSetVisuals.mbPromptForTracks = false;
}

