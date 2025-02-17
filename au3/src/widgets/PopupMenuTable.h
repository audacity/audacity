/**********************************************************************

Audacity: A Digital Audio Editor

PopupMenuTable.h

Paul Licameli

This file defines PopupMenuTable, which inherits from wxEventHandler,

associated macros simplifying the population of tables,

and PopupMenuTable::Menu which is buildable from one or more such
tables, and automatically attaches and detaches the event handlers.

**********************************************************************/

#ifndef __AUDACITY_POPUP_MENU_TABLE__
#define __AUDACITY_POPUP_MENU_TABLE__

class wxCommandEvent;

#include <functional>
#include <vector>
#include <wx/menu.h> // to inherit wxMenu
#include <memory>

#include "Internat.h"
#include "CommandManager.h"

class PopupMenuHandler;
struct PopupMenuSection;
class PopupMenuTable;
struct PopupMenuTableEntry;
struct PopupSubMenu;
struct PopupMenuTableTraits : Registry::DefaultTraits {
    using ComputedItemContextType = PopupMenuTable;
    using LeafTypes = List<PopupMenuTableEntry>;
    using NodeTypes = List<PopupMenuSection, PopupSubMenu>;
};
using PopupMenuGroupItem = Registry::GroupItem<PopupMenuTableTraits>;

struct AUDACITY_DLL_API PopupMenuTableEntry : Registry::SingleItem
{
    enum Type {
        Item, RadioItem, CheckItem
    };
    using InitFunction
        =std::function< void (PopupMenuHandler& handler, wxMenu& menu, int id) >;

    Type type;
    int id;
    TranslatableString caption;
    wxCommandEventFunction func;
    PopupMenuHandler& handler;
    InitFunction init;

    //! @pre func is not null
    PopupMenuTableEntry(const Identifier& stringId,
                        Type type_, int id_, const TranslatableString& caption_,
                        wxCommandEventFunction func_, PopupMenuHandler& handler_,
                        InitFunction init_ = {})
        : SingleItem{stringId}
        , type(type_)
        , id(id_)
        , caption(caption_)
        , func(func_)
        , handler(handler_)
        , init(init_)
    {
        wxASSERT(func);
    }

    ~PopupMenuTableEntry() override;
};

struct AUDACITY_DLL_API PopupSubMenu : PopupMenuGroupItem, MenuRegistry::ItemProperties
{
    TranslatableString caption;
    PopupMenuTable& table;

    PopupSubMenu(const Identifier& stringId, const TranslatableString& caption_, PopupMenuTable& table);

    ~PopupSubMenu() override;
    Properties GetProperties() const override;
};

struct PopupMenuSection : PopupMenuGroupItem, MenuRegistry::ItemProperties
{
    using PopupMenuGroupItem::PopupMenuGroupItem;
    ~PopupMenuSection() override;
    Properties GetProperties() const override { return Section; }
};

class PopupMenuHandler : public wxEvtHandler
{
public:
    PopupMenuHandler() = default;
    PopupMenuHandler(const PopupMenuHandler&) = delete;
    PopupMenuHandler& operator=(const PopupMenuHandler&) = delete;

    //! Called before the menu items are appended.
    /*! Store context data, if needed.
       May be called more than once before the menu opens.
       Pointer remains valid for the duration of any callback, if
       PopupMenuTable::BuildMenu() is called and the result's Popup() is called
       before any other menus are built.
     */
    virtual void InitUserData(void* pUserData) = 0;
};

// Opaque structure built by PopupMenuTable::BuildMenu
class AUDACITY_DLL_API PopupMenu
{
public:
    virtual ~PopupMenu();
    virtual void Popup(wxWindow& window, const wxPoint& pos) = 0;
};

class AUDACITY_DLL_API PopupMenuTable : public PopupMenuHandler
{
public:
    using Entry = PopupMenuTableEntry;

    // Supply a nonempty caption for sub-menu tables
    PopupMenuTable(const Identifier& id, const TranslatableString& caption = {})
        : mId{id}
        , mCaption{caption}
        , mRegistry{
                    std::make_unique<PopupMenuGroupItem>(mId)}
    {}

    // Optional pUserData gets passed to the InitUserData routines of tables.
    // No memory management responsibility is assumed by this function.
    static std::unique_ptr<PopupMenu> BuildMenu(
        PopupMenuTable* pTable, void* pUserData = NULL);

    const Identifier& Id() const { return mId; }
    const TranslatableString& Caption() const { return mCaption; }
    const auto* GetRegistry() const { return mRegistry.get(); }

    // Typically statically constructed:
    template<typename Ptr> struct AttachedItem {
        AttachedItem(PopupMenuTable& table,
                     const Registry::Placement& placement, Ptr pItem)
        { table.RegisterItem(placement, std::move(pItem)); }
    };

    // menu must have been built by BuildMenu
    // More items get added to the end of it
    static void ExtendMenu(PopupMenu& menu, PopupMenuTable& otherTable);

    const auto& Get(void* pUserData)
    {
        if (pUserData) {
            this->InitUserData(pUserData);
        }
        if (!mTop) {
            Populate();
        }
        return mTop;
    }

    void Clear()
    {
        mTop.reset();
    }

    // Adapts a factory for a table sub-type
    template<typename Table, typename Factory>
    static auto Adapt(const Factory& factory)
    {
        return [factory](PopupMenuTable& table){
            return std::shared_ptr{ factory(static_cast<Table&>(table)) };
        };
    }

private:
    template<typename Ptr> void RegisterItem(
        const Registry::Placement& placement, Ptr pItem)
    {
        Registry::RegisterItem(*mRegistry, placement, move(pItem));
    }

protected:
    // This convenience function composes a label, with the following optional
    // part put in parentheses if useExtra is true
    static TranslatableString MakeLabel(const TranslatableString& label,
                                        bool useExtra, const TranslatableString& extra)
    {
        return useExtra
               ? XXO("%s (%s)").Format(label, extra)
               : label;
    }

    virtual void Populate() = 0;

    // To be used in implementations of Populate():
    template<typename Ptr>
    void Append(Ptr pItem) { mStack.back()->push_back(std::move(pItem)); }

    void Append(
        const Identifier& stringId, PopupMenuTableEntry::Type type, int id, const TranslatableString& string, wxCommandEventFunction memFn,
        // This callback might check or disable a menu item:
        const PopupMenuTableEntry::InitFunction& init);

    void AppendItem(const Identifier& stringId, int id,
                    const TranslatableString& string, wxCommandEventFunction memFn,
                    // This callback might check or disable a menu item:
                    const PopupMenuTableEntry::InitFunction& init = {})
    { Append(stringId, PopupMenuTableEntry::Item, id, string, memFn, init); }

    void AppendRadioItem(const Identifier& stringId, int id,
                         const TranslatableString& string, wxCommandEventFunction memFn,
                         // This callback might check or disable a menu item:
                         const PopupMenuTableEntry::InitFunction& init = {})
    { Append(stringId, PopupMenuTableEntry::RadioItem, id, string, memFn, init); }

    void AppendCheckItem(const Identifier& stringId, int id,
                         const TranslatableString& string, wxCommandEventFunction memFn,
                         const PopupMenuTableEntry::InitFunction& init = {})
    { Append(stringId, PopupMenuTableEntry::CheckItem, id, string, memFn, init); }

    void BeginSection(const Identifier& name);
    void EndSection();

    std::shared_ptr<PopupSubMenu> mTop;
    std::vector<PopupMenuGroupItem*> mStack;
    Identifier mId;
    TranslatableString mCaption;
    std::unique_ptr<PopupMenuGroupItem> mRegistry;
};

/*
The following macros make it easy to attach a popup menu to a window.

Example of usage:

In class MyTable (maybe in the private section),
which inherits from PopupMenuTable,

DECLARE_POPUP_MENU(MyTable);
virtual void InitUserData(void *pUserData);
virtual void DestroyMenu();

Then in MyTable.cpp,

void MyTable::InitUserData(void *pUserData)
{
   // Remember pData
   auto pData = static_cast<MyData*>(pUserData);
}

void MyTable::DestroyMenu()
{
   // Do cleanup
}

BEGIN_POPUP_MENU(MyTable)
   // This is inside a function and can contain arbitrary code.  But usually
   // you only need a sequence of calls:

   AppendItem("Cut",
      OnCutSelectedTextID, XO("Cu&t"), POPUP_MENU_FN( OnCutSelectedText ),
      // optional argument:
      [](PopupMenuHandler &handler, wxMenu &menu, int id)
      {
         auto data = static_cast<MyTable&>( handler ).pData;
         // maybe enable or disable the menu item
      }
   );
   // etc.

END_POPUP_MENU()

where OnCutSelectedText is a (maybe private) member function of MyTable.

Elsewhere,

MyTable myTable;
MyData data;
auto pMenu = PopupMenuTable::BuildMenu(pParent, &myTable, &data);

// Optionally:
OtherTable otherTable;
PopupMenuTable::ExtendMenu( *pMenu, otherTable );

pMenu->Popup( *pParent, { event.m_x, event.m_y } );

That's all!
*/

#define DECLARE_POPUP_MENU(HandlerClass) \
    void Populate() override;

// begins function
#define BEGIN_POPUP_MENU(HandlerClass) \
    void HandlerClass::Populate() { \
        using My = HandlerClass; \
        mTop = std::make_shared< PopupSubMenu >( \
            Id(), Caption(), *this); \
        mStack.clear(); \
        mStack.push_back(mTop.get());

#define POPUP_MENU_FN(memFn) ((wxCommandEventFunction)(&My::memFn))

#define POPUP_MENU_SUB_MENU(stringId, classname, pUserData) \
    mStack.back()->push_back( \
        Registry::Indirect(classname::Instance().Get(pUserData)));

// ends function
#define END_POPUP_MENU() }

#endif
