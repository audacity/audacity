/**********************************************************************

  Audacity: A Digital Audio Editor

  Demo.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_DEMO_COMMAND__
#define __AUDACITY_DEMO_COMMAND__

#include "AudacityCommand.h"
#include "SampleFormat.h"

class ShuttleGui;

class DemoCommand final : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    // ComponentInterface overrides
    ComponentInterfaceSymbol GetSymbol() const override { return Symbol; }
    TranslatableString GetDescription() const override { return XO("Does the demo action."); }
    template<bool Const> bool VisitSettings(SettingsVisitorBase<Const>& S);
    bool VisitSettings(SettingsVisitor& S) override;
    bool VisitSettings(ConstSettingsVisitor& S) override;
    void PopulateOrExchange(ShuttleGui& S) override;
    bool Apply(const CommandContext& context) override;

    // AudacityCommand overrides
    ManualPageID ManualPage() override { return L"Extra_Menu:_Scriptables_I"; }

private:
    double delay;
    double decay;
};

#endif // __AUDACITY_DEMO_COMMAND__
