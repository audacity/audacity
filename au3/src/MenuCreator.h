/**********************************************************************

  Audacity: A Digital Audio Editor

  MenuCreator.h

  Dominic Mazzoni

  Paul Licameli split from Menus.h

**********************************************************************/
#ifndef __AUDACITY_MENU_CREATOR__
#define __AUDACITY_MENU_CREATOR__

#include "Callable.h"
#include "CommandManager.h"

class AUDACITY_DLL_API MenuCreator final : public CommandManager
{
public:
    // For manipulating the enclosing menu or sub-menu directly,
    // adding any number of items, not using the CommandManager
    struct SpecialItem final : MenuRegistry::SpecialItem
    {
        using Appender = std::function<void (AudacityProject&, wxMenu&)>;

        explicit SpecialItem(const Identifier& internalName, const Appender& fn_)
            : MenuRegistry::SpecialItem{internalName}
            , fn{fn_}
        {}
        ~SpecialItem() override;

        Appender fn;
    };

    static constexpr auto Special = Callable::UniqueMaker<SpecialItem>();

    // "permit" allows filtering even if the active window isn't a child of the
    // project.
    // Lyrics and MixerTrackCluster classes use it.
    static bool FilterKeyEvent(AudacityProject& project, const wxKeyEvent& evt, bool permit = false);

    static MenuCreator& Get(AudacityProject& project);
    static const MenuCreator& Get(const AudacityProject& project);

    MenuCreator(AudacityProject& project);
    ~MenuCreator() override;
    void CreateMenusAndCommands();
    void RebuildMenuBar();
    static void RebuildAllMenuBars();

    // a temporary hack that should be removed as soon as we
    // get multiple effect preview working
    bool ReallyDoQuickCheck() override;

    void RemoveDuplicateShortcuts();

private:
    void ExecuteCommand(const CommandContext& context, const wxEvent* evt, const CommandListEntry& entry) override;
};

struct NormalizedKeyString;

AUDACITY_DLL_API
NormalizedKeyString KeyEventToKeyString(const wxKeyEvent& keyEvent);

#endif
