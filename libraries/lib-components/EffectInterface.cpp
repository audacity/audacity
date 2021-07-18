/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectInterface.cpp

**********************************************************************/
#include "EffectInterface.h"
#include <wx/tokenzr.h>

Identifier EffectDefinitionInterface::GetSquashedName(const Identifier &ident)
{
   // Get rid of leading and trailing white space
   auto name = ident.GET();
   name.Trim(true).Trim(false);

   if (name.empty())
      return {};

   wxStringTokenizer st(name, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens()) {
      wxString tok = st.GetNextToken();
      id += tok.Left(1).MakeUpper() + tok.Mid(1).MakeLower();
   }

   return id;
}

EffectDefinitionInterface::~EffectDefinitionInterface() = default;

EffectType EffectDefinitionInterface::GetClassification()
{
   return GetType();
}

bool EffectDefinitionInterface::EnablesDebug()
{
   return false;
}

ManualPageID EffectDefinitionInterface::ManualPage()
{
   return {};
}

FilePath EffectDefinitionInterface::HelpPage()
{
   return {};
}

bool EffectDefinitionInterface::IsHiddenFromMenus()
{
   return false;
}

//bool EffectDefinitionInterface::DefineParams(ShuttleParams & S)
//{
//   return false;
//}

EffectProcessor::~EffectProcessor() = default;

EffectUIClientInterface::~EffectUIClientInterface() = default;
