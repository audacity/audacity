/**********************************************************************

   Audacity - A Digital Audio Editor
   Copyright 1999-2018 Audacity Team
   License: wxwidgets

   Dan Horgan
   James Crook

******************************************************************//**

\file SetTrackCommand.h
\brief Declarations of SetTrackCommand and SetTrackCommandType classes

*//*******************************************************************/

#ifndef __SET_TRACK_COMMAND__
#define __SET_TRACK_COMMAND__

#include "Command.h"
#include "CommandType.h"

class Track;

class SetTrackBase : public AudacityCommand
{
public:
   SetTrackBase();
   bool Apply(const CommandContext & context) override;
   virtual bool ApplyInner( const CommandContext &context, Track *t  );
   virtual bool DefineParams( ShuttleParams & S ) override;
   virtual void PopulateOrExchange(ShuttleGui & S) override;

   int mTrackIndex;
   int mChannelIndex;
   bool bHasTrackIndex;
   bool bHasChannelIndex;

   bool bIsSecondChannel;
   bool mbPromptForTracks;
};


class SetTrackStatusCommand : public SetTrackBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   //SetTrackStatusCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a track.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_track_status");};
   bool ApplyInner( const CommandContext & context, Track * t ) override;

public:
   wxString mTrackName;
   bool bSelected;
   bool bFocused;

// For tracking optional parameters.
   bool bHasTrackName;
   bool bHasSelected;
   bool bHasFocused;
};

class SetTrackAudioCommand : public SetTrackBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   //SetTrackAudioCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a track.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_track_audio");};
   bool ApplyInner( const CommandContext & context, Track * t ) override;

public:
   double mPan;
   double mGain;
   bool bSolo;
   bool bMute;

// For tracking optional parameters.
   bool bHasPan;
   bool bHasGain;
   bool bHasSolo;
   bool bHasMute;
};

class SetTrackVisualsCommand : public SetTrackBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   //SetTrackVisualsCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a track.");};
   bool DefineParams( ShuttleParams & S ) override;
   void PopulateOrExchange(ShuttleGui & S) override;

   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_I#set_track_visuals");};
   bool ApplyInner( const CommandContext & context, Track * t ) override;

public:
   int mColour;
   int mHeight;
   int mDisplayType;
   int mScaleType;
   int mVZoom;
   double mVZoomTop;
   double mVZoomBottom;

   bool bUseSpecPrefs;
   bool bSpectralSelect;
   bool bGrayScale;

// For tracking optional parameters.
   bool bHasColour;
   bool bHasHeight;
   bool bHasDisplayType;
   bool bHasScaleType;
   bool bHasVZoom;
   bool bHasVZoomTop;
   bool bHasVZoomBottom;

   bool bHasUseSpecPrefs;
   bool bHasSpectralSelect;
   bool bHasGrayScale;
};

class SetTrackCommand : public SetTrackBase
{
public:
   static const ComponentInterfaceSymbol Symbol;

   SetTrackCommand();
   // ComponentInterface overrides
   ComponentInterfaceSymbol GetSymbol() override {return Symbol;};
   TranslatableString GetDescription() override {return XO("Sets various values for a track.");};
   // AudacityCommand overrides
   wxString ManualPage() override {return wxT("Extra_Menu:_Scriptables_II#set_track");};

public:

   bool DefineParams( ShuttleParams & S ) override { 
      return 
         SetTrackBase::DefineParams(S) &&
         mSetStatus.DefineParams(S) &&  
         mSetAudio.DefineParams(S) &&
         mSetVisuals.DefineParams(S);
   };
   void PopulateOrExchange(ShuttleGui & S) override {
      SetTrackBase::PopulateOrExchange( S );
      mSetStatus.PopulateOrExchange(S);
      mSetAudio.PopulateOrExchange(S);
      mSetVisuals.PopulateOrExchange(S);
   };
   bool ApplyInner(const CommandContext & context, Track * t ) override {
      mSetStatus.bIsSecondChannel = bIsSecondChannel;
      mSetAudio.bIsSecondChannel = bIsSecondChannel;
      mSetVisuals.bIsSecondChannel = bIsSecondChannel;
      return 
         mSetStatus.ApplyInner( context, t ) &&  
         mSetAudio.ApplyInner( context, t )&&
         mSetVisuals.ApplyInner( context, t );
   }

private:
   SetTrackStatusCommand mSetStatus;
   SetTrackAudioCommand mSetAudio;
   SetTrackVisualsCommand mSetVisuals;
};



#endif /* End of include guard: __SETTRACKINFOCOMMAND__ */
