/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include "Identifier.h"

#include "Callable.h"
#include "ClientData.h"
#include "CommandFunctors.h"
#include "CommandFlag.h"
#include "GlobalVariable.h"

#include "Keyboard.h"
#include "MenuRegistry.h"

#include "Observer.h"
#include "Prefs.h"
#include "Project.h"

#include <functional>
#include <vector>

#include "XMLTagHandler.h"

#include <unordered_map>

class wxEvent;

class BoolSetting;

using PluginID = wxString;

class AudacityProject;
class CommandContext;

//! Sent when menus update (such as for changing enablement of items)
struct MenuUpdateMessage {};

class MENUS_API CommandManager /* not final */ : public XMLTagHandler, public ClientData::Base,
    public Observer::Publisher<MenuUpdateMessage>, private PrefsListener
{
protected:
    static const TranslatableString COMMAND;

public:
    struct CommandListEntry;

    struct MENUS_API Factory : DefaultedGlobalHook<Factory,
                                                   Callable::SharedPtrFactory<CommandManager, AudacityProject&>::Function
                                                   > {};

    static CommandManager& Get(AudacityProject& project);
    static const CommandManager& Get(const AudacityProject& project);

    // Interception of menu item handling.
    // If it returns true, bypass the usual dispatch of commands.
    struct MENUS_API GlobalMenuHook : GlobalHook<GlobalMenuHook,
                                                 bool(const CommandID&)
                                                 > {};

    explicit CommandManager(AudacityProject& project);
    CommandManager(const CommandManager&) = delete;
    CommandManager& operator=(const CommandManager&) = delete;
    ~CommandManager() override;

    // If quick, skip some needlessly expensive checks.
    CommandFlag GetUpdateFlags(bool quick = false) const;
    void UpdatePrefs() override;

    // Command Handling
    bool ReportIfActionNotAllowed(
        const TranslatableString& Name, CommandFlag& flags, CommandFlag flagsRqd);
    bool TryToMakeActionAllowed(
        CommandFlag& flags, CommandFlag flagsRqd);

    CommandFlag mLastFlags = AlwaysEnabledFlag;

    // Last effect applied to this project
    PluginID mLastGenerator{};
    PluginID mLastEffect{};
    PluginID mLastAnalyzer{};
    int mLastAnalyzerRegistration = repeattypenone;
    int mLastAnalyzerRegisteredId = -1;
    PluginID mLastTool{};
    int mLastToolRegistration = repeattypenone;
    int mLastToolRegisteredId = -1;
    enum {
        repeattypenone = 0,
        repeattypeplugin = 1,
        repeattypeunique = 2,
        repeattypeapplymacro = 3
    };
    unsigned mRepeatGeneratorFlags = 0;
    unsigned mRepeatEffectFlags = 0;
    unsigned mRepeatAnalyzerFlags = 0;
    unsigned mRepeatToolFlags = 0;

    // 0 is grey out, 1 is Autoselect, 2 is Give warnings.
    int mWhatIfNoSelection;

    // not configurable for now, but could be later.
    bool mStopIfWasPaused{ true };

    AudacityProject& GetProject() { return mProject; }
    size_t NCommands() const { return mCommandList.size(); }

    void PurgeData();

    struct MENUS_API Populator : MenuRegistry::Visitor<MenuRegistry::Traits>
    {
        using LeafVisitor = std::function<
            void (const Registry::SingleItem&, const Registry::Path&)>;
        Populator(AudacityProject& project, LeafVisitor leafVisitor, std::function<void()> doSeparator);
        virtual ~Populator();

    protected:
        void DoBeginGroup(
            const MenuRegistry::GroupItem<MenuRegistry::Traits>& item);
        void DoVisit(const Registry::SingleItem& item);
        void DoEndGroup(
            const MenuRegistry::GroupItem<MenuRegistry::Traits>& item);

        //! Called by DoBeginGroup
        //! Default implementation does nothing
        virtual void BeginMenu(const TranslatableString& tName);

        //! Called by DoBeginGroup
        //! Default implementation does nothing
        virtual void BeginOccultCommands();

        //! Called by DoEndGroup
        //! Default implementation does nothing
        virtual void EndMenu();

        //! Called by DoEndGroup
        //! Default implementation does nothing
        virtual void EndOccultCommands();

        //! Called by DoVisit
        //! Override to make entries that carry extra information.
        //! Not called for every visit, because existing items may be reused
        /*!
         @post result: `result != nullptr`
         */
        virtual std::unique_ptr<CommandListEntry>
        AllocateEntry(const MenuRegistry::Options& options);
        //! Called by DoVisit
        //! Override to intercept all visits of items;
        //! default implementation is noop
        /*!
         @param options null if a member of a list of commands
         */
        virtual void VisitEntry(CommandListEntry& entry, const MenuRegistry::Options* options);

        void DoSeparator();

        //! Stack of names of menus that were begun and not yet ended
        const TranslatableStrings& MenuNames() const { return mMenuNames; }

    private:
        void AddItemList(const CommandID& name, const ComponentInterfaceSymbol items[], size_t nItems, CommandHandlerFinder finder,
                         CommandFunctorPointer callback, CommandFlag flags, bool bIsEffect = false);
        void AddItem(const CommandID& name, const TranslatableString& label_in, CommandHandlerFinder finder, CommandFunctorPointer callback,
                     CommandFlag flags, const MenuRegistry::Options& options = {});
        CommandListEntry* NewIdentifier(const CommandID& name, const TranslatableString& label, CommandHandlerFinder finder,
                                        CommandFunctorPointer callback, const CommandID& nameSuffix, int index, int count,
                                        const MenuRegistry::Options& options);
        void AddGlobalCommand(const CommandID& name, const TranslatableString& label, CommandHandlerFinder finder,
                              CommandFunctorPointer callback, const MenuRegistry::Options& options = {});
        void SetMaxList();

    protected:
        AudacityProject& mProject;

        // false at the start of a menu and immediately after a separator.
        bool mbSeparatorAllowed{ false };

    private:
        // mMaxList only holds shortcuts that should not be added (by default)
        // and is sorted.
        std::vector<NormalizedKeyString> mMaxListOnly;
        TranslatableStrings mMenuNames;
        std::vector<bool> mFlags;
        int mCurrentID{ 17000 };
        bool bMakingOccultCommands{ false };
    };

public:
    void SetCommandFlags(const CommandID& name, CommandFlag flags);

    void EnableUsingFlags(
        CommandFlag flags, CommandFlag strictFlags);
    void Enable(const wxString& name, bool enabled);
    void Check(const CommandID& name, bool checked);
    void Modify(const wxString& name, const TranslatableString& newLabel);

    //
    // Modifying accelerators
    //

    void SetKeyFromName(const CommandID& name, const NormalizedKeyString& key);
    //! @pre `0 <= i && i < NCommands()`
    void SetKeyFromIndex(int i, const NormalizedKeyString& key);

    bool HandleMenuID(int id, CommandFlag flags, bool alwaysEnabled);
    void RegisterLastAnalyzer(const CommandContext& context);
    void RegisterLastTool(const CommandContext& context);
    void DoRepeatProcess(const CommandContext& context, int);

    enum TextualCommandResult {
        CommandFailure,
        CommandSuccess,
        CommandNotFound
    };

    /*!
     @pre `&context.project == &GetProject()`
     */
    TextualCommandResult
    HandleTextualCommand(const CommandID& Str, const CommandContext& context, CommandFlag flags, bool alwaysEnabled);

    //
    // Accessing
    //

    TranslatableStrings GetCategories();
    void GetAllCommandNames(CommandIDs& names, bool includeMultis) const;
    void GetAllCommandLabels(
        TranslatableStrings& labels, std::vector<bool>& vExcludeFromMacros, bool includeMultis) const;
    void GetAllCommandData(
        CommandIDs& names, std::vector<NormalizedKeyString>& keys, std::vector<NormalizedKeyString>& default_keys,
        TranslatableStrings& labels, TranslatableStrings& categories, TranslatableStrings& prefixes, bool includeMultis);

    // Each command is assigned a numerical ID for use in wxMenu and wxEvent,
    // which need not be the same across platforms or sessions
    CommandID GetNameFromNumericID(int id) const;

    TranslatableString GetLabelFromName(const CommandID& name) const;
    TranslatableString GetPrefixedLabelFromName(const CommandID& name) const;
    TranslatableString GetCategoryFromName(const CommandID& name) const;
    NormalizedKeyString GetKeyFromName(const CommandID& name) const;
    NormalizedKeyString GetDefaultKeyFromName(const CommandID& name) const;

    bool GetEnabled(const CommandID& name) const;
    int GetNumberOfKeysRead() const;

#if defined(_DEBUG)
    void CheckDups();
#endif
    // May have side-effect on the config file
    TranslatableString ReportDuplicateShortcuts();

    //
    // Loading/Saving
    //

    void WriteXML(XMLWriter& xmlFile) const /* not override */;

    ///
    /// Formatting summaries that include shortcut keys
    ///
    TranslatableString DescribeCommandsAndShortcuts(
        // If a shortcut key is defined for the command, then it is appended,
        // parenthesized, after the translated name.
        const ComponentInterfaceSymbol commands[], size_t nCommands) const;

    // Sorted list of the shortcut keys to be excluded from the standard defaults
    static const std::vector<NormalizedKeyString>& ExcludedList();

private:

    static int NextIdentifier(int ID);

protected:
    bool HandleCommandEntry(
        const CommandListEntry* entry, CommandFlag flags, bool alwaysEnabled, const wxEvent* evt = nullptr,
        const CommandContext* pGivenContext = nullptr);

    virtual void ExecuteCommand(const CommandContext& context, const wxEvent* evt, const CommandListEntry& entry);

    //
    // Modifying
    //

private:
    void Enable(CommandListEntry& entry, bool enabled);

    //
    // Accessing
    //

public:
    void UpdateCheckmarks();

    //! Format a string appropriate for insertion in a menu
    /*!
     @param pLabel if not null, use this instead of the manager's
     stored label
     */
    wxString FormatLabelForMenu(
        const CommandID& id, const TranslatableString* pLabel) const;

    void ModifyUndoMenuItems();

    // checkActive is a temporary hack that should be removed as soon as we
    // get multiple effect preview working
    void UpdateMenus(bool checkActive = true);

protected:
    //! Default implementation returns true
    virtual bool ReallyDoQuickCheck();

private:

    //
    // Loading/Saving
    //

    bool HandleXMLTag(const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;

    void TellUserWhyDisallowed(const TranslatableString& Name, CommandFlag flagsGot, CommandFlag flagsRequired);

    void OnUndoRedo(struct UndoRedoMessage);

protected:
    AudacityProject& mProject;

public:
    struct MENUS_API CommandListEntry
    {
        static wxString FormatLabelForMenu(
            const TranslatableString& translatableLabel, const NormalizedKeyString& keyStr);

        virtual ~CommandListEntry();

        //! Default implementation does nothing
        virtual void UpdateCheckmark(AudacityProject& project);
        //! Default implementation simply assigns `this->label`
        virtual void Modify(const TranslatableString& newLabel);
        //! Default implementation simply returns `this->enabled`
        virtual bool GetEnabled() const;
        //! Default implementation does nothing
        virtual void Check(bool checked);
        //! Default implementation simply assigns `this->enabled`
        virtual void Enable(bool enabled);
        //! Default implementation simply assigns `this->enabled`
        virtual void EnableMultiItem(bool enabled);

        wxString FormatLabelForMenu() const
        {
            return FormatLabelForMenu(label, key);
        }

        int id;
        CommandID name;
        TranslatableString longLabel;
        NormalizedKeyString key;
        NormalizedKeyString defaultKey;
        TranslatableString label;
        TranslatableString labelPrefix;
        TranslatableString labelTop;
        CommandHandlerFinder finder;
        CommandFunctorPointer callback;
        CommandParameter parameter;

        // type of a function that determines checkmark state
        using CheckFn = std::function< bool (AudacityProject&) >;
        CheckFn checkmarkFn;

        bool multi;
        int index;
        int count;
        bool enabled;
        bool skipKeydown;
        bool wantKeyup;
        bool allowDup;
        bool isGlobal;
        bool isOccult;
        bool isEffect;
        bool excludeFromMacros;
        CommandFlag flags;
        bool useStrictFlags{ false };
    };

protected:
    using CommandKeyHash
        =std::unordered_map<NormalizedKeyString, CommandListEntry*>;
    //! @invariant for each [key, value]: for some 0 <= i < NCommands():
    //! `value == mCommandList[i].get()`
    CommandKeyHash mCommandKeyHash;

private:
    // This is an array of pointers, not structures, because the hash maps also
    // point to them,
    // so we don't want the structures to relocate with vector operations.
    using CommandList = std::vector<std::unique_ptr<CommandListEntry> >;
    //! @invariant for all 0 <= i < NCommands(): `mCommandList[i] != nullptr`
    CommandList mCommandList;

    using CommandNameHash = std::unordered_map<CommandID, CommandListEntry*>;
    //! @invariant for each [key, value]: for some 0 <= i < NCommands():
    //! `value == mCommandList[i].get()`
    CommandNameHash mCommandNameHash;

    using CommandNumericIDHash = std::unordered_map<int, CommandListEntry*>;
    //! @invariant for each [key, value]: for some 0 <= i < NCommands():
    //! `value == mCommandList[i].get()`
    CommandNumericIDHash mCommandNumericIDHash;
    int mXMLKeysRead;

    TranslatableString mNiceName;
    int mLastProcessId;

    const Observer::Subscription mUndoSubscription;
};
#endif
