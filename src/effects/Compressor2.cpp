/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor2.cpp

  Max Maisel

*******************************************************************//**

\class EffectCompressor2
\brief An Effect which reduces the dynamic level.

*//*******************************************************************/


#include "../Audacity.h" // for rint from configwin.h
#include "Compressor2.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/valgen.h>

#include "../Internat.h"
#include "../Prefs.h"
#include "../ProjectFileManager.h"
#include "../Shuttle.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"
#include "../widgets/ProgressDialog.h"

#include "LoadEffects.h"

BEGIN_EVENT_TABLE(EffectCompressor2, wxEvtHandler)
END_EVENT_TABLE()

const ComponentInterfaceSymbol EffectCompressor2::Symbol
{ XO("Compressor v2") };

namespace{ BuiltinEffectsModule::Registration< EffectCompressor2 > reg; }

EffectCompressor2::EffectCompressor2()
{
   SetLinearEffectFlag(false);
}

EffectCompressor2::~EffectCompressor2()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectCompressor2::GetSymbol()
{
   return Symbol;
}

TranslatableString EffectCompressor2::GetDescription()
{
   return XO("Reduces the dynamic of one or more tracks");
}

wxString EffectCompressor2::ManualPage()
{
   return wxT("Compressor2");
}

// EffectDefinitionInterface implementation

EffectType EffectCompressor2::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation
bool EffectCompressor2::DefineParams( ShuttleParams & S )
{
   return true;
}

bool EffectCompressor2::GetAutomationParameters(CommandParameters & parms)
{
   return true;
}

bool EffectCompressor2::SetAutomationParameters(CommandParameters & parms)
{
   return true;
}

// Effect implementation

bool EffectCompressor2::CheckWhetherSkipEffect()
{
   return false;
}

bool EffectCompressor2::Startup()
{
   wxString base = wxT("/Effects/Compressor2/");
   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
      SaveUserPreset(GetCurrentSettingsGroup());

      gPrefs->Flush();
   }
   return true;
}

bool EffectCompressor2::Process()
{
   return false;
}

void EffectCompressor2::PopulateOrExchange(ShuttleGui & S)
{
}

bool EffectCompressor2::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateUI();
   return true;
}

bool EffectCompressor2::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }
   return true;
}

// EffectCompressor2 implementation

void EffectCompressor2::OnUpdateUI(wxCommandEvent & WXUNUSED(evt))
{
   UpdateUI();
}

void EffectCompressor2::UpdateUI()
{
}
