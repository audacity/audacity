/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectInterface.cpp

**********************************************************************/
#include "EffectInterface.h"
#include <wx/tokenzr.h>

const RegistryPath &EffectSettingsExtra::DurationKey()
{
   static wxString key("LastUsedDuration");
   return key;
}

EffectSettingsAccess::~EffectSettingsAccess() = default;

SimpleEffectSettingsAccess::~SimpleEffectSettingsAccess() = default;

const EffectSettings &SimpleEffectSettingsAccess::Get()
{
   return mSettings;
}

void SimpleEffectSettingsAccess::Set(EffectSettings &&settings)
{
   mSettings = std::move(settings);
}

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

EffectType EffectDefinitionInterface::GetClassification() const
{
   return GetType();
}

bool EffectDefinitionInterface::EnablesDebug() const
{
   return false;
}

ManualPageID EffectDefinitionInterface::ManualPage() const
{
   return {};
}

FilePath EffectDefinitionInterface::HelpPage() const
{
   return {};
}

bool EffectDefinitionInterface::IsHiddenFromMenus() const
{
   return false;
}

bool EffectDefinitionInterface::VisitSettings(
   SettingsVisitor &, EffectSettings &)
{
   return false;
}

bool EffectDefinitionInterface::VisitSettings(
   ConstSettingsVisitor &, const EffectSettings &) const
{
   return false;
}

auto EffectDefinitionInterface::MakeSettings() const -> EffectSettings
{
   return {};
}

bool EffectDefinitionInterface::CopySettingsContents(
   const EffectSettings &, EffectSettings &) const
{
   return true;
}

EffectProcessor::~EffectProcessor() = default;

EffectUIValidator::~EffectUIValidator() = default;

bool EffectUIValidator::UpdateUI()
{
   return true;
}

DefaultEffectUIValidator::DefaultEffectUIValidator(
   EffectUIClientInterface &effect, EffectSettingsAccess &access)
   : mEffect{effect}
   , mAccess{access}
{}

DefaultEffectUIValidator::~DefaultEffectUIValidator()
{
   mEffect.CloseUI();
}

bool DefaultEffectUIValidator::ValidateUI()
{
   bool result {};
   mAccess.ModifySettings([&](EffectSettings &settings){
      result = mEffect.ValidateUI(settings);
   });
   return result;
}

EffectUIClientInterface::~EffectUIClientInterface() = default;

const RegistryPath &CurrentSettingsGroup()
{
   static RegistryPath id{ "CurrentSettings" };
   return id;
}

const RegistryPath &FactoryDefaultsGroup()
{
   static RegistryPath id{ "FactoryDefaults" };
   return id;
}

RegistryPath UserPresetsGroup(const RegistryPath & name)
{
   RegistryPath group = wxT("UserPresets");
   if (!name.empty())
      group += wxCONFIG_PATH_SEPARATOR + name;
   return group;
}
