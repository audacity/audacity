/**********************************************************************

  Audacity: A Digital Audio Editor

  MenuRegistry.h

  Brian Gunlogson
  Dominic Mazzoni

  Paul Licameli split from CommandManager.h

**********************************************************************/
#ifndef __AUDACITY_MENU_REGISTRY__
#define __AUDACITY_MENU_REGISTRY__

#include "Callable.h"
#include "MemoryX.h"
#include "CommandFunctors.h"
#include "CommandFlag.h"
#include "Registry.h"
#include <functional>

class AudacityProject;
using CommandParameter = CommandID;

// Define items that populate tables that specifically describe menu trees
namespace MenuRegistry {
struct Traits;
template<typename MenuTraits> struct Visitor;

template<typename MenuTraits> struct Visitor;

// type of a function that determines checkmark state
using CheckFn = std::function< bool (AudacityProject&) >;

struct MENUS_API Options
{
    Options() {}
    // Allow implicit construction from an accelerator string, which is
    // a very common case
    Options(const wxChar* accel_)
        : accel{accel_} {}
    // A two-argument constructor for another common case
    Options(
        const wxChar* accel_,
        const TranslatableString& longName_)
        : accel{accel_}, longName{longName_} {}

    Options&& Accel(const wxChar* value)
    && { accel = value; return std::move(*this); }
    Options&& IsEffect(bool value = true)
    && { bIsEffect = value; return std::move(*this); }
    Options&& Parameter(const CommandParameter& value)
    && { parameter = value; return std::move(*this); }
    Options&& LongName(const TranslatableString& value)
    && { longName = value; return std::move(*this); }
    Options&& IsGlobal()
    && { global = true; return std::move(*this); }
    Options&& UseStrictFlags()
    && { useStrictFlags = true; return std::move(*this); }
    Options&& WantKeyUp()
    && { wantKeyUp = true; return std::move(*this); }
    Options&& SkipKeyDown()
    && { skipKeyDown = true; return std::move(*this); }

    // This option affects debugging only:
    Options&& AllowDup()
    && { allowDup = true; return std::move(*this); }

    Options&& AllowInMacros(int value = 1)
    && { allowInMacros = value; return std::move(*this); }

    // CheckTest is overloaded
    // Take arbitrary predicate
    Options&& CheckTest(const CheckFn& fn)
    && { checker = fn; return std::move(*this); }
    // Take a preference path
    Options&& CheckTest(const wxChar* key, bool defaultValue)
    &&
    {
        checker = MakeCheckFn(key, defaultValue);
        return std::move(*this);
    }

    // Take a BoolSetting
    Options&& CheckTest(const BoolSetting& setting)
    &&
    {
        checker = MakeCheckFn(setting);
        return std::move(*this);
    }

    const wxChar* accel{ wxT("") };
    CheckFn checker;   // default value means it's not a check item
    bool bIsEffect{ false };
    CommandParameter parameter{};
    TranslatableString longName{};
    bool global{ false };
    bool useStrictFlags{ false };
    bool wantKeyUp{ false };
    bool skipKeyDown{ false };
    bool allowDup{ false };
    // 0 = never, 1 = always, -1 = deduce from label
    int allowInMacros{ -1 };

private:
    static CheckFn MakeCheckFn(const wxString key, bool defaultValue);
    static CheckFn MakeCheckFn(const BoolSetting& setting);
};

//! A mix-in discovered by dynamic_cast; independent of the Traits
struct MENUS_API ItemProperties {
    enum Properties {
        None,
        Inline,
        Section,
        Whole,
        Extension,
    };
    virtual ~ItemProperties() = default;
    virtual Properties GetProperties() const = 0;
};

namespace detail {
struct MENUS_API VisitorBase {
    std::pair<bool, bool>
    ShouldBeginGroup(const ItemProperties* pProperties);
    void AfterBeginGroup(const ItemProperties* pProperties);
    bool ShouldEndGroup(const ItemProperties* pProperties);
    bool ShouldDoSeparator();

    std::vector<bool> firstItem;
    std::vector<bool> needSeparator;
};
}

using namespace Registry;

//! Wraps the behavior of another VisitorFuntions<MenuTraits>, and also
//! needs a callback for what to do at separator lines
template<typename MenuTraits> struct Visitor : VisitorFunctions<MenuTraits>, detail::VisitorBase
{
    Visitor(VisitorFunctions<MenuTraits> functions, std::function<void()> doSeparator)
        : VisitorFunctions<MenuTraits>{std::tuple {
                [this](const GroupItem<MenuTraits>& item, const Path& path)
            {
                using namespace MenuRegistry;
                const auto pProperties = dynamic_cast<const ItemProperties*>(&item);
                auto [begin, separate] = ShouldBeginGroup(pProperties);
                if (separate) {
                    mDoSeparator();
                }
                if (begin) {
                    mWrapped.BeginGroup(item, path);
                }
                AfterBeginGroup(pProperties);
            },

                [this](const Registry::SingleItem& item, const Path& path)
            {
                if (ShouldDoSeparator()) {
                    mDoSeparator();
                }
                mWrapped.Visit(item, path);
            },

                [this](const GroupItem<MenuTraits>& item, const Path& path)
            {
                using namespace MenuRegistry;
                const auto pProperties = dynamic_cast<const ItemProperties*>(&item);
                if (ShouldEndGroup(pProperties)) {
                    mWrapped.EndGroup(item, path);
                }
            }
            }},
        mWrapped{ move(functions) },
    mDoSeparator{ move(doSeparator) }
    {}

private:
    const VisitorFunctions<MenuTraits> mWrapped;
    const std::function<void()> mDoSeparator;
};

struct CommandItem;
struct CommandGroupItem;
struct ConditionalGroupItem;
struct MenuItem;
struct MenuItems;
struct SpecialItem;
struct MenuPart;

struct Traits : Registry::DefaultTraits {
    using ComputedItemContextType = AudacityProject;
    using LeafTypes = List<
        CommandItem, CommandGroupItem, SpecialItem>;
    using NodeTypes = List<
        ConditionalGroupItem, MenuItem, MenuItems, MenuPart>;
};

template<typename RegistryTraits>
static inline bool IsSection(const GroupItem<RegistryTraits>& item)
{
    auto pProperties = dynamic_cast<const ItemProperties*>(&item);
    return pProperties && pProperties->GetProperties()
           == ItemProperties::Section;
}

struct MenuItemData {
    MenuItemData(TranslatableString title)
        : mTitle{std::move(title)} {}
    const TranslatableString mTitle;
};
}

#ifdef _WIN32
template struct __declspec(dllexport) Composite::Extension<
    Registry::GroupItem<MenuRegistry::Traits>,
    MenuRegistry::MenuItemData, const Identifier&
    >;
#endif

namespace  MenuRegistry {
// Describes a main menu in the toolbar, or a sub-menu
struct MENUS_API MenuItem final : Composite::Extension<
        GroupItem<Traits>, MenuItemData, const Identifier&
        >, ItemProperties
{
    using Extension::Extension;
    ~MenuItem() override;
    const auto& GetTitle() const { return mTitle; }
    Properties GetProperties() const override;
};

using Condition = std::function<bool ()>;
}

#ifdef _WIN32
template struct __declspec(dllexport) Composite::Extension<
    Registry::GroupItem<MenuRegistry::Traits>,
    MenuRegistry::Condition, const Identifier&
    >;
#endif

namespace  MenuRegistry {
// Collects other items that are conditionally shown or hidden, but are
// always available to macro programming
struct MENUS_API ConditionalGroupItem final : Composite::Extension<
        GroupItem<Traits>, Condition, const Identifier&
        >
{
    using Extension::Extension;
    ~ConditionalGroupItem() override;
    using Condition::operator();
};

// usage:
//   auto scope = FinderScope( findCommandHandler );
//   return Items( ... );
//
// or:
//   return ( FinderScope( findCommandHandler ), Items( ... ) );
//
// where findCommandHandler names a function.
// This is used before a sequence of many calls to Command() and
// CommandGroup(), so that the finder argument need not be specified
// in each call.
class MENUS_API FinderScope : ValueRestorer< CommandHandlerFinder >
{
    static CommandHandlerFinder sFinder;

public:
    //! @post result: `result != nullptr`
    static CommandHandlerFinder DefaultFinder() { return sFinder; }

    //! @pre `finder != nullptr`
    explicit
    FinderScope(CommandHandlerFinder finder)
        : ValueRestorer(sFinder, finder)
    { assert(finder); }
};

// Describes one command in a menu
struct MENUS_API CommandItem final : SingleItem {
    CommandItem(const CommandID& name_, const TranslatableString& label_in_, CommandFunctorPointer callback_, CommandFlag flags_,
                const Options& options_, CommandHandlerFinder finder_);

    // Takes a pointer to member function directly, and delegates to the
    // previous constructor; useful within the lifetime of a FinderScope
    /*!
     @pre `finder != nullptr`
     */
    template< typename Handler >
    CommandItem(const CommandID& name,
                const TranslatableString& label_in,
                void(Handler::* pmf)(const CommandContext&),
                CommandFlag flags,
                const Options& options = {},
                CommandHandlerFinder finder = FinderScope::DefaultFinder())
        : CommandItem(name, label_in,
                      CommandFunctorPointer {
            static_cast<CommandFunctorPointer::MemberFn>(pmf) },
                      flags, options, finder)
    { assert(finder); }

    // Takes a pointer to nonmember function and delegates to the first
    // constructor
    CommandItem(const CommandID& name,
                const TranslatableString& label_in,
                CommandFunctorPointer::NonMemberFn callback,
                CommandFlag flags,
                const Options& options = {})
        : CommandItem(name, label_in,
                      CommandFunctorPointer { callback },
                      flags, options, nullptr)
    {}

    ~CommandItem() override;

    const TranslatableString label_in;
    CommandHandlerFinder finder;
    CommandFunctorPointer callback;
    CommandFlag flags;
    Options options;
};

// Describes several successive commands in a menu that are closely related
// and dispatch to one common callback, which will be passed a number
// in the CommandContext identifying the command
struct MENUS_API CommandGroupItem final : SingleItem {
    CommandGroupItem(const Identifier& name_, std::vector<ComponentInterfaceSymbol> items_, CommandFunctorPointer callback_,
                     CommandFlag flags_, bool isEffect_, CommandHandlerFinder finder_);

    // Takes a pointer to member function directly, and delegates to the
    // previous constructor; useful within the lifetime of a FinderScope
    /*!
     @pre `finder != nullptr`
     */
    template< typename Handler >
    CommandGroupItem(const Identifier& name_,
                     std::vector<ComponentInterfaceSymbol> items_,
                     void(Handler::* pmf)(const CommandContext&),
                     CommandFlag flags_,
                     bool isEffect_,
                     CommandHandlerFinder finder = FinderScope::DefaultFinder())
        : CommandGroupItem(name_, move(items_),
                           CommandFunctorPointer {
            static_cast<CommandFunctorPointer::MemberFn>(pmf) },
                           flags_, isEffect_, finder)
    { assert(finder); }

    // Takes a pointer to nonmember function and delegates to the first
    // constructor
    CommandGroupItem(const CommandID& name,
                     std::vector< ComponentInterfaceSymbol > items,
                     CommandFunctorPointer::NonMemberFn fn,
                     CommandFlag flags,
                     bool isEffect = false)
        : CommandGroupItem(name, move(items),
                           CommandFunctorPointer { fn },
                           flags, isEffect, nullptr)
    {}

    ~CommandGroupItem() override;

    const std::vector<ComponentInterfaceSymbol> items;
    CommandHandlerFinder finder;
    CommandFunctorPointer callback;
    CommandFlag flags;
    bool isEffect;
};

// For manipulating the enclosing menu or sub-menu directly,
// adding any number of items, not using the CommandManager
struct MENUS_API SpecialItem : SingleItem
{
    using SingleItem::SingleItem;
    ~SpecialItem() override;
};

//! Groups of this type are inlined in the menu tree organization.  They
//! (but not their contained items) are excluded from visitations
struct MENUS_API MenuItems : Composite::Extension<
        GroupItem<Traits>, void, const Identifier&
        >, ItemProperties
{
    using Extension::Extension;
    ~MenuItems() override;
    //! Anonymous if its name is empty, else weakly ordered
    Ordering GetOrdering() const override;
    Properties GetProperties() const override;
};

struct MENUS_API MenuPart : Composite::Extension<
        GroupItem<Traits>, void, const Identifier&
        >, ItemProperties
{
    using Extension::Extension;
    ~MenuPart() override;
    Properties GetProperties() const override;
};

/*! @name Factories
      The following, and Registry::Indirect(), are the functions to use directly
      to specify elements of menu groupings.
    */
//! @{

//! Variadic constructor from pointers to subordinate items, which are moved
//! into the result.
/*!
    Null pointers are permitted, and ignored when building the menu.
    Items are spliced into the enclosing menu.
    The name is untranslated and may be empty, to make the group transparent
    in identification of items by path.  Otherwise try to keep the name
    stable across Audacity versions.
    */
constexpr auto Items = Callable::UniqueMaker<MenuItems>();

//! Like Items, but insert a menu separator between the menu section and
//! any other items or sections before or after it in the same (innermost,
//! enclosing) menu.
/*!
    It's not necessary that the sisters of sections be other sections, but it
    might clarify the logical groupings.
    */
constexpr auto Section = Callable::UniqueMaker<MenuPart>();

//! Items will appear in a main toolbar menu or in a sub-menu.
/*!
    The name is untranslated.  Try to keep the name stable across Audacity
    versions.
    If the name of a menu is empty, then subordinate items cannot be located
    by path.
    */
constexpr auto Menu = Callable::UniqueMaker<MenuItem>();

//! These items register in the CommandManager but are not shown in menus
//! if the condition evaluates false.
/*!
    The name is untranslated.  Try to keep the name stable across Audacity
    versions.
    Name for conditional group must be non-empty.
    */
constexpr auto ConditionalItems = Callable::UniqueMaker<ConditionalGroupItem>();

constexpr auto Command = Callable::UniqueMaker<CommandItem>();

constexpr auto CommandGroup = Callable::UniqueMaker<CommandGroupItem,
                                                    const Identifier&, std::vector<ComponentInterfaceSymbol> >();

//! @}

struct MENUS_API ItemRegistry {
    static GroupItem<Traits>& Registry();
};

// Typically you make a static object of this type in the .cpp file that
// also defines the added menu actions.
// pItem can be specified by an expression using the inline functions above.
using AttachedItem = RegisteredItem<ItemRegistry>;

MENUS_API void Visit(Visitor<Traits>& visitor, AudacityProject& project);
}

#endif
