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

void SimpleEffectSettingsAccess::Flush()
{
}

bool SimpleEffectSettingsAccess::IsSameAs(
   const EffectSettingsAccess &other) const
{
   if (auto pOther =
      dynamic_cast<const SimpleEffectSettingsAccess*>(&other))
      return &this->mSettings == &pOther->mSettings;
   return false;
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

EffectSettingsManager::~EffectSettingsManager() = default;

bool EffectSettingsManager::VisitSettings(
   SettingsVisitor &, EffectSettings &)
{
   return false;
}

bool EffectSettingsManager::VisitSettings(
   ConstSettingsVisitor &, const EffectSettings &) const
{
   return false;
}

auto EffectSettingsManager::MakeSettings() const -> EffectSettings
{
   return {};
}

bool EffectSettingsManager::CopySettingsContents(
   const EffectSettings &, EffectSettings &) const
{
   return true;
}

EffectInstance::~EffectInstance() = default;

bool EffectInstance::Init()
{
   return true;
}

bool EffectInstance::RealtimeInitialize(EffectSettings &, double)
{
   return false;
}

bool EffectInstance::RealtimeAddProcessor(EffectSettings &, unsigned, float)
{
   return true;
}

bool EffectInstance::RealtimeSuspend()
{
   return true;
}

bool EffectInstance::RealtimeResume()
{
   return true;
}

bool EffectInstance::RealtimeProcessStart(EffectSettings &)
{
   return true;
}

size_t EffectInstance::RealtimeProcess(size_t, EffectSettings &,
   const float *const *, float *const *, size_t)
{
   return 0;
}

bool EffectInstance::RealtimeProcessEnd(EffectSettings &) noexcept
{
   return true;
}

bool EffectInstance::RealtimeFinalize(EffectSettings &) noexcept
{
   return true;
}

size_t EffectInstance::GetTailSize() const
{
   return 0;
}

EffectInstanceWithBlockSize::~EffectInstanceWithBlockSize() = default;

size_t EffectInstanceWithBlockSize::GetBlockSize() const
{
   return mBlockSize;
}

size_t EffectInstanceWithBlockSize::SetBlockSize(size_t maxBlockSize)
{
   return (mBlockSize = maxBlockSize);
}

EffectInstanceFactory::~EffectInstanceFactory() = default;

int EffectInstanceFactory::GetMidiInCount() const
{
   return 0;
}

int EffectInstanceFactory::GetMidiOutCount() const
{
   return 0;
}

EffectUIValidator::EffectUIValidator(
   EffectUIClientInterface &effect, EffectSettingsAccess &access)
   : mEffect{effect}
   , mAccess{access}
{}

EffectUIValidator::~EffectUIValidator()
{
   mEffect.CloseUI();
}

bool EffectUIValidator::UpdateUI()
{
   return true;
}

DefaultEffectUIValidator::~DefaultEffectUIValidator() = default;

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
