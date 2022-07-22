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


#include "SetTrackInfoCommand.h"

#include "LoadCommands.h"
#include "Project.h"
#include "../TrackPanelAx.h"
#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../prefs/WaveformSettings.h"
#include "../prefs/SpectrogramSettings.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveTrackView.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveTrackViewConstants.h"
#include "CommandContext.h"

SetTrackBase::SetTrackBase(){
   mbPromptForTracks = true;
   bIsSecondChannel = false;
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

template<bool Const>
bool SetTrackBase::VisitSettings( SettingsVisitorBase<Const> & S)
{
   static_cast<void>(S);
#ifdef USE_OWN_TRACK_SELECTION
   S.OptionalY( bHasTrackIndex     ).Define(     mTrackIndex,     wxT("Track"),      0, 0, 100 );
   S.OptionalN( bHasChannelIndex   ).Define(     mChannelIndex,   wxT("Channel"),    0, 0, 100 );
#endif
   return true;
}

bool SetTrackBase::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SetTrackBase::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

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
      S.Optional( bHasTrackIndex  ).TieNumericTextBox(  XO("Track Index:"),   mTrackIndex );
      S.Optional( bHasChannelIndex).TieNumericTextBox(  XO("Channel Index:"), mChannelIndex );
   }
   S.EndMultiColumn();
#endif
}

bool SetTrackBase::Apply(const CommandContext & context  )
{
   long i = 0;// track counter
   long j = 0;// channel counter
   auto &tracks = TrackList::Get( context.project );
   for ( auto t : tracks.Leaders() )
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

const ComponentInterfaceSymbol SetTrackStatusCommand::Symbol
{ XO("Set Track Status") };

namespace{ BuiltinCommandsModule::Registration< SetTrackStatusCommand > reg; }

template<bool Const>
bool SetTrackStatusCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   SetTrackBase::VisitSettings( S );
   S.OptionalN( bHasTrackName      ).Define(     mTrackName,      wxT("Name"),       _("Unnamed") );
   // There is also a select command.  This is an alternative.
   S.OptionalN( bHasSelected       ).Define(     bSelected,       wxT("Selected"),   false );
   S.OptionalN( bHasFocused        ).Define(     bFocused,        wxT("Focused"),    false );
   return true;
};

bool SetTrackStatusCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SetTrackStatusCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SetTrackStatusCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasTrackName   ).TieTextBox(         XXO("Name:"),          mTrackName );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasSelected       ).TieCheckBox( XXO("Selected"),           bSelected );
      S.Optional( bHasFocused        ).TieCheckBox( XXO("Focused"),            bFocused);
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
         auto &trackFocus = TrackFocus::Get( context.project );
         if( bFocused)
            trackFocus.Set( t );
         else if( t == trackFocus.Get() )
            trackFocus.Set( nullptr );
      }
   }
   return true;
}



const ComponentInterfaceSymbol SetTrackAudioCommand::Symbol
{ XO("Set Track Audio") };

namespace{ BuiltinCommandsModule::Registration< SetTrackAudioCommand > reg2; }

template<bool Const>
bool SetTrackAudioCommand::VisitSettings( SettingsVisitorBase<Const> & S ){
   SetTrackBase::VisitSettings( S );
   S.OptionalN( bHasMute           ).Define(     bMute,           wxT("Mute"),       false );
   S.OptionalN( bHasSolo           ).Define(     bSolo,           wxT("Solo"),       false );

   S.OptionalN( bHasGain           ).Define(     mGain,           wxT("Gain"),       0.0,  -36.0, 36.0);
   S.OptionalN( bHasPan            ).Define(     mPan,            wxT("Pan"),        0.0, -100.0, 100.0);
   return true;
};

bool SetTrackAudioCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SetTrackAudioCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SetTrackAudioCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasMute           ).TieCheckBox( XXO("Mute"),               bMute);
      S.Optional( bHasSolo           ).TieCheckBox( XXO("Solo"),               bSolo);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasGain        ).TieSlider(          XXO("Gain:"),          mGain, 36.0,-36.0);
      S.Optional( bHasPan         ).TieSlider(          XXO("Pan:"),           mPan,  100.0, -100.0);
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



const ComponentInterfaceSymbol SetTrackVisualsCommand::Symbol
{ XO("Set Track Visuals") };

namespace{ BuiltinCommandsModule::Registration< SetTrackVisualsCommand > reg3; }

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


enum kScaleTypes
{
   kLinearAmp,
   kLogarithmicDb,
   kLinearDb,
   nScaleTypes
};

static const EnumValueSymbol kScaleTypeStrings[nScaleTypes] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Linear (amp)") },
   /* i18n-hint: abbreviates decibels */
   { XO("Logarithmic (dB)") },
   /* i18n-hint: abbreviates decibels */
   { XO("Linear (dB)")}
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

static EnumValueSymbols DiscoverSubViewTypes()
{
   const auto &types = WaveTrackSubViewType::All();
   auto result = transform_container< EnumValueSymbols >(
      types, std::mem_fn( &WaveTrackSubView::Type::name ) );
   result.push_back( WaveTrackViewConstants::MultiViewSymbol );
   return result;
}

template<bool Const>
bool SetTrackVisualsCommand::VisitSettings( SettingsVisitorBase<Const> & S ){ 
   SetTrackBase::VisitSettings( S );
   S.OptionalN( bHasHeight         ).Define(     mHeight,         wxT("Height"),     120, 44, 2000 );

   {
      auto symbols = DiscoverSubViewTypes();
      S.OptionalN( bHasDisplayType    ).DefineEnum( mDisplayType,    wxT("Display"),    0, symbols.data(), symbols.size() );
   }

   S.OptionalN( bHasScaleType      ).DefineEnum( mScaleType,      wxT("Scale"),      kLinearAmp,   kScaleTypeStrings, nScaleTypes );
   S.OptionalN( bHasColour         ).DefineEnum( mColour,         wxT("Color"),      kColour0,  kColourStrings, nColours );
   S.OptionalN( bHasVZoom          ).DefineEnum( mVZoom,          wxT("VZoom"),      kReset,    kZoomTypeStrings, nZoomTypes );
   S.OptionalN( bHasVZoomTop       ).Define(     mVZoomTop,       wxT("VZoomHigh"),  1.0,  -2.0,  2.0 );
   S.OptionalN( bHasVZoomBottom    ).Define(     mVZoomBottom,    wxT("VZoomLow"),   -1.0, -2.0,  2.0 );

   S.OptionalN( bHasUseSpecPrefs   ).Define(     bUseSpecPrefs,   wxT("SpecPrefs"),  false );
   S.OptionalN( bHasSpectralSelect ).Define(     bSpectralSelect, wxT("SpectralSel"),true );

   auto schemes = SpectrogramSettings::GetColorSchemeNames();
   S.OptionalN( bHasSpecColorScheme).DefineEnum( mSpecColorScheme,wxT("SpecColor"),  SpectrogramSettings::csColorNew, schemes.data(), schemes.size());

   return true;
};

bool SetTrackVisualsCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }

bool SetTrackVisualsCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

void SetTrackVisualsCommand::PopulateOrExchange(ShuttleGui & S)
{
   SetTrackBase::PopulateOrExchange( S );
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      S.Optional( bHasHeight      ).TieNumericTextBox(  XXO("Height:"),        mHeight );
      S.Optional( bHasColour      ).TieChoice(          XXO("Color:"),         mColour,
         Msgids(  kColourStrings, nColours ) );
      
      {
         auto symbols = DiscoverSubViewTypes();
         auto typeNames = transform_container<TranslatableStrings>(
             symbols, std::mem_fn( &EnumValueSymbol::Stripped ) );
         S.Optional( bHasDisplayType ).TieChoice(          XXO("Display:"),       mDisplayType,
            typeNames );
      }

      S.Optional( bHasScaleType   ).TieChoice(          XXO("Scale:"),         mScaleType,
         Msgids( kScaleTypeStrings, nScaleTypes ) );
      S.Optional( bHasVZoom       ).TieChoice(          XXO("VZoom:"),         mVZoom,
         Msgids( kZoomTypeStrings, nZoomTypes ) );
      S.Optional( bHasVZoomTop    ).TieTextBox(         XXO("VZoom Top:"),     mVZoomTop );
      S.Optional( bHasVZoomBottom ).TieTextBox(         XXO("VZoom Bottom:"),  mVZoomBottom );
   }
   S.EndMultiColumn();
   S.StartMultiColumn(2, wxEXPAND);
   {
      S.SetStretchyCol( 1 );
      S.Optional( bHasUseSpecPrefs   ).TieCheckBox( XXO("Use Spectral Prefs"), bUseSpecPrefs );
      S.Optional( bHasSpectralSelect ).TieCheckBox( XXO("Spectral Select"),    bSpectralSelect);
   }
   S.EndMultiColumn();
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol( 2 );
      auto schemes = SpectrogramSettings::GetColorSchemeNames();
      S.Optional( bHasSpecColorScheme).TieChoice( XC("Sche&me", "spectrum prefs"), mSpecColorScheme,
         Msgids( schemes.data(), schemes.size() ) );
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
      TrackView::Get( *t ).SetExpandedHeight( mHeight );

   if( wt && bHasDisplayType  ) {
      auto &view = WaveTrackView::Get( *wt );
      auto &all = WaveTrackSubViewType::All();
      if (mDisplayType < all.size())
         view.SetDisplay( all[ mDisplayType ].id );
      else {
         view.SetMultiView( true );
         view.SetDisplay( WaveTrackSubViewType::Default(), false );
      }
   }
   if (wt && bHasScaleType) {
      switch (mScaleType) {
      default:
      case kLinearAmp: wt->GetWaveformSettings().scaleType = WaveformSettings::stLinearAmp;
      case kLogarithmicDb: wt->GetWaveformSettings().scaleType = WaveformSettings::stLogarithmicDb;
      case kLinearDb: wt->GetWaveformSettings().scaleType = WaveformSettings::stLinearDb;
      }
   }

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
      auto &tp = TrackPanel::Get( context.project );
      tp.UpdateVRulers();
   }

   if( wt && bHasUseSpecPrefs   ){
      wt->UseSpectralPrefs( bUseSpecPrefs );
   }
   if( wt && bHasSpectralSelect ){
      wt->GetSpectrogramSettings().spectralSelection = bSpectralSelect;
   }
   if (wt && bHasSpecColorScheme) {
      wt->GetSpectrogramSettings().colorScheme = (SpectrogramSettings::ColorScheme)mSpecColorScheme;
   }

   return true;
}


const ComponentInterfaceSymbol SetTrackCommand::Symbol
{ XO("Set Track") };

namespace{ BuiltinCommandsModule::Registration< SetTrackCommand > reg4; }

SetTrackCommand::SetTrackCommand()
{
   mSetStatus.mbPromptForTracks = false;
   mSetAudio.mbPromptForTracks = false;
   mSetVisuals.mbPromptForTracks = false;
}

bool SetTrackCommand::VisitSettings( SettingsVisitor & S )
   { return VisitSettings<false>(S); }
bool SetTrackCommand::VisitSettings( ConstSettingsVisitor & S )
   { return VisitSettings<true>(S); }

