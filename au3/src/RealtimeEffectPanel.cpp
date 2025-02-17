/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file RealtimeEffectPanel.cpp

   @author Vitaly Sverchinsky

**********************************************************************/

#include "RealtimeEffectPanel.h"

#include <wx/app.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>
#include <wx/menu.h>
#include <wx/wupdlock.h>
#include <wx/hyperlink.h>

#include <wx/dcbuffer.h>

#include "HelpSystem.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "BasicUI.h"
#include "Observer.h"
#include "PluginManager.h"
#include "Project.h"
#include "ProjectHistory.h"
#include "ProjectWindow.h"
#include "ProjectWindows.h"
#include "TrackFocus.h"
#include "AColor.h"
#include "WaveTrack.h"
#include "effects/EffectUI.h"
#include "EffectManager.h"
#include "RealtimeEffectList.h"
#include "RealtimeEffectState.h"
#include "effects/RealtimeEffectStateUI.h"
#include "UndoManager.h"
#include "PendingTracks.h"
#include "Prefs.h"
#include "BasicUI.h"
#include "ListNavigationEnabled.h"
#include "ListNavigationPanel.h"
#include "MovableControl.h"
#include "menus/MenuHelper.h"
#include "prefs/EffectsPrefs.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

namespace {
using namespace MenuRegistry;
class RealtimeEffectsMenuVisitor final : public Visitor<Traits>
{
    wxMenu& mMenu;
    wxMenu* mMenuPtr { nullptr };
    int mMenuItemIdCounter { wxID_HIGHEST };
    std::vector<Identifier> mIndexedPluginList;
    int mMenuLevelCounter { 0 };
public:
    RealtimeEffectsMenuVisitor(wxMenu& menu)
        : Visitor<Traits>{std::tuple {
                [this](const MenuRegistry::MenuItem& menuItem, const auto&) {
                //Don't create a group item for root
                if (mMenuLevelCounter != 0) {
                    auto submenu = std::make_unique<wxMenu>();
                    mMenuPtr->AppendSubMenu(submenu.get(), menuItem.GetTitle().Translation());
                    mMenuPtr = submenu.release();
                }
                ++mMenuLevelCounter;
            },

                [this](const MenuRegistry::CommandItem& commandItem, const auto&) {
                mMenuPtr->Append(mMenuItemIdCounter, commandItem.label_in.Translation());
                mIndexedPluginList.push_back(commandItem.name);
                ++mMenuItemIdCounter;
            },

                [this](const MenuRegistry::MenuItem&, const auto&) {
                --mMenuLevelCounter;
                if (mMenuLevelCounter != 0) {
                    assert(mMenuPtr->GetParent() != nullptr);
                    mMenuPtr = mMenuPtr->GetParent();
                }
            } },

                          [this]() {
            mMenuPtr->AppendSeparator();
        }},
        mMenu(menu), mMenuPtr(&mMenu)
    {
    }

    Identifier GetPluginID(int menuIndex) const
    {
        assert(menuIndex >= wxID_HIGHEST && menuIndex < (wxID_HIGHEST + mIndexedPluginList.size()));
        return mIndexedPluginList[menuIndex - wxID_HIGHEST];
    }
};

class EffectsMenuHelper final : public PrefsListener
{
    std::shared_ptr<MenuRegistry::MenuItem> mCachedMenu;
    Observer::Subscription mPluginsChangedSubscription;
public:

    static std::optional<wxString> PickEffect(AudacityProject& project, wxWindow* parent, const wxString& selectedEffectID)
    {
        wxMenu menu;
        if (!selectedEffectID.empty()) {
            //no need to handle language change since menu creates its own event loop
            menu.Append(wxID_REMOVE, _("No Effect"));
            menu.AppendSeparator();
        }

        RealtimeEffectsMenuVisitor visitor { menu };

        Get().Populate(project, visitor);

        int commandId = wxID_NONE;

#if defined(__WXMSW__) || defined(__WXMAC__)
        menu.AppendSeparator();
        menu.Append(wxID_MORE, _("Get more effects..."));
#endif
        menu.Bind(wxEVT_MENU, [&](wxCommandEvent evt) { commandId = evt.GetId(); });

        if (parent->PopupMenu(&menu, parent->GetClientRect().GetLeftBottom()) && commandId != wxID_NONE) {
            if (commandId == wxID_REMOVE) {
                return wxString {}
            }
            if (commandId == wxID_MORE) {
                OpenInDefaultBrowser("https://www.musehub.com");
            } else {
                return visitor.GetPluginID(commandId).GET();
            }
        }

        return {};
    }

private:

    static EffectsMenuHelper& Get()
    {
        static EffectsMenuHelper helper;
        return helper;
    }

    void Populate(AudacityProject& project, Visitor<Traits>& visitor)
    {
        auto cachedMenuItem = GetMenuItem();
        if (!cachedMenuItem) {
            return;
        }

        VisitWithFunctions(visitor, cachedMenuItem.get(), {}, project);
    }

    EffectsMenuHelper()
    {
        mPluginsChangedSubscription = PluginManager::Get().Subscribe(
            [this](PluginsChangedMessage)
        {
            mCachedMenu.reset();
        }
            );
    }

    std::shared_ptr<MenuItem> GetMenuItem()
    {
        if (!mCachedMenu) {
            UpdateEffectMenuItems();
        }

        assert(mCachedMenu);
        if (mCachedMenu) {
            return mCachedMenu;
        }
        return {};
    }

    void UpdatePrefs() override
    {
        mCachedMenu.reset();
    }

    void UpdateEffectMenuItems()
    {
        using namespace MenuRegistry;
        auto root = std::shared_ptr{ Menu("", TranslatableString {}) };

        static auto realtimeEffectPredicate = [](const PluginDescriptor& desc)
        {
            return desc.IsEffectRealtime();
        };

        const auto groupby = RealtimeEffectsGroupBy.Read();

        auto analyzeSection = Section("", Menu("", XO("Analyze")));
        auto submenu
            =static_cast<MenuItem*>(analyzeSection->begin()->get());
        MenuHelper::PopulateEffectsMenu(
            *submenu,
            EffectTypeAnalyze,
            {}, groupby, nullptr,
            realtimeEffectPredicate
            );

        if (!submenu->empty()) {
            root->push_back(move(analyzeSection));
        }

        MenuHelper::PopulateEffectsMenu(
            *root,
            EffectTypeProcess,
            {}, groupby, nullptr,
            realtimeEffectPredicate
            );

        mCachedMenu.swap(root);
    }
};

const PluginDescriptor* GetPlugin(const PluginID& ID)
{
    auto desc = PluginManager::Get().GetPlugin(ID);
    return desc;
}

TranslatableString GetEffectName(RealtimeEffectState& state)
{
    const auto& ID = state.GetID();
    const auto desc = GetPlugin(ID);
    return desc
           ? desc->GetSymbol().Msgid()
           : XO("%s (missing)")
           .Format(PluginManager::GetEffectNameFromID(ID).GET());
}

template<typename Visitor>
void VisitRealtimeEffectStateUIs(const RealtimeEffectList& effects, Visitor&& visitor)
{
    effects.Visit(
        [visitor](auto& effectState, bool)
    {
        auto& ui = RealtimeEffectStateUI::Get(effectState);
        visitor(ui);
    });
}

template<typename Visitor>
void VisitRealtimeEffectStateUIs(const Track& track, Visitor&& visitor)
{
    VisitRealtimeEffectStateUIs(RealtimeEffectList::Get(track), std::forward<Visitor>(visitor));
}

void UpdateRealtimeEffectUIData(const Track& track)
{
    const auto& name = track.GetName();
    VisitRealtimeEffectStateUIs(
        RealtimeEffectList::Get(track), [&](auto& ui) { ui.SetTargetName(name); });
}

void UpdateRealtimeEffectUIData(const AudacityProject& project)
{
    const auto& name = project.GetProjectName();
    VisitRealtimeEffectStateUIs(
        RealtimeEffectList::Get(project), [&](auto& ui) { ui.SetTargetName(name); }
        );
}

void ReopenRealtimeEffectUIData(AudacityProject& project, SampleTrack& track)
{
    VisitRealtimeEffectStateUIs(
        RealtimeEffectList::Get(track),
        [&](auto& ui)
    {
        if (ui.IsShown()) {
            ui.Hide(&project);
            ui.Show(project);
        }
    });
}

//fwd
class RealtimeEffectControl;

class DropHintLine : public wxWindow
{
public:
    DropHintLine(wxWindow* parent,
                 wxWindowID id,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize)
        : wxWindow(parent, id, pos, size, wxNO_BORDER, wxEmptyString)
    {
        wxWindow::SetBackgroundStyle(wxBG_STYLE_PAINT);
        Bind(wxEVT_PAINT, &DropHintLine::OnPaint, this);
    }

    bool AcceptsFocus() const override { return false; }

private:
    void OnPaint(wxPaintEvent&)
    {
        wxBufferedPaintDC dc(this);
        const auto rect = wxRect(GetSize());

        dc.SetPen(*wxTRANSPARENT_PEN);
        dc.SetBrush(GetBackgroundColour());
        dc.DrawRectangle(rect);
    }
};

class HyperLinkCtrlWrapper : public ListNavigationEnabled<wxHyperlinkCtrl>
{
public:
    HyperLinkCtrlWrapper(wxWindow* parent,
                         wxWindowID id,
                         const wxString& label,
                         const wxString& url,
                         const wxPoint& pos = wxDefaultPosition,
                         const wxSize& size = wxDefaultSize,
                         long style = wxHL_DEFAULT_STYLE,
                         const wxString& name = wxHyperlinkCtrlNameStr)
    {
        Create(parent, id, label, url, pos, size, style, name);
    }

    void Create(wxWindow* parent,
                wxWindowID id,
                const wxString& label,
                const wxString& url,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxHL_DEFAULT_STYLE,
                const wxString& name = wxHyperlinkCtrlNameStr)
    {
        ListNavigationEnabled<wxHyperlinkCtrl>::Create(parent, id, label, url, pos, size, style, name);
        Bind(wxEVT_PAINT, &HyperLinkCtrlWrapper::OnPaint, this);
    }

    void OnPaint(wxPaintEvent& evt)
    {
        wxPaintDC dc(this);
        dc.SetFont(GetFont());
        dc.SetTextForeground(GetForegroundColour());
        dc.SetTextBackground(GetBackgroundColour());

        auto labelRect = GetLabelRect();

        dc.DrawText(GetLabel(), labelRect.GetTopLeft());
        if (HasFocus()) {
            AColor::DrawFocus(dc, labelRect);
        }
    }
};

#if wxUSE_ACCESSIBILITY
class RealtimeEffectControlAx : public wxAccessible
{
public:
    RealtimeEffectControlAx(wxWindow* win = nullptr)
        : wxAccessible(win) { }

    wxAccStatus GetName(int childId, wxString* name) override
    {
        if (childId != wxACC_SELF) {
            return wxACC_NOT_IMPLEMENTED;
        }

        if (auto movable = wxDynamicCast(GetWindow(), MovableControl)) {
            //i18n-hint: argument - position of the effect in the effect stack
            *name = wxString::Format(_("Effect %d"), movable->FindIndexInParent() + 1);
        }
        return wxACC_OK;
    }

    wxAccStatus GetChildCount(int* childCount) override
    {
        const auto window = GetWindow();
        *childCount = window->GetChildren().size();
        return wxACC_OK;
    }

    wxAccStatus GetChild(int childId, wxAccessible** child) override
    {
        if (childId == wxACC_SELF) {
            *child = this;
        } else {
            const auto window = GetWindow();
            const auto& children = window->GetChildren();
            const auto childIndex = childId - 1;
            if (childIndex < children.size()) {
                *child = children[childIndex]->GetAccessible();
            } else {
                *child = nullptr;
            }
        }
        return wxACC_OK;
    }

    wxAccStatus GetRole(int childId, wxAccRole* role) override
    {
        if (childId != wxACC_SELF) {
            return wxACC_NOT_IMPLEMENTED;
        }

        *role = wxROLE_SYSTEM_PANE;
        return wxACC_OK;
    }

    wxAccStatus GetState(int childId, long* state) override
    {
        if (childId != wxACC_SELF) {
            return wxACC_NOT_IMPLEMENTED;
        }

        const auto window = GetWindow();
        if (!window->IsEnabled()) {
            *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
        } else {
            *state = wxACC_STATE_SYSTEM_FOCUSABLE;
            if (window->HasFocus()) {
                *state |= wxACC_STATE_SYSTEM_FOCUSED;
            }
        }
        return wxACC_OK;
    }
};
#endif

class EffectListUIDelegate
{
public:
    virtual ~EffectListUIDelegate() = default;

    virtual RealtimeEffectList& GetEffectList() = 0;
    virtual wxString GetSourceName() = 0;
    virtual ChannelGroup* GetChannelGroup() = 0;
};

//UI control that represents individual effect from the effect list
class RealtimeEffectControl : public ListNavigationEnabled<MovableControl>
{
    wxWeakRef<AudacityProject> mProject;
    std::shared_ptr<EffectListUIDelegate> mDelegate;
    //std::shared_ptr<SampleTrack> mTrack;
    std::shared_ptr<RealtimeEffectState> mEffectState;
    std::shared_ptr<EffectSettingsAccess> mSettingsAccess;

    ThemedAButtonWrapper<AButton>* mChangeButton{ nullptr };
    AButton* mEnableButton{ nullptr };
    ThemedAButtonWrapper<AButton>* mOptionsButton{};

    Observer::Subscription mSubscription;

public:
    RealtimeEffectControl() = default;

    RealtimeEffectControl(wxWindow* parent,
                          wxWindowID winid,
                          const wxPoint& pos = wxDefaultPosition,
                          const wxSize& size = wxDefaultSize)
    {
        Create(parent, winid, pos, size);
    }

    void Create(wxWindow* parent,
                wxWindowID winid,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize)
    {
        //Prevents flickering and paint order issues
        MovableControl::SetBackgroundStyle(wxBG_STYLE_PAINT);
        MovableControl::Create(parent, winid, pos, size, wxNO_BORDER | wxWANTS_CHARS);

        Bind(wxEVT_PAINT, &RealtimeEffectControl::OnPaint, this);
        Bind(wxEVT_SET_FOCUS, &RealtimeEffectControl::OnFocusChange, this);
        Bind(wxEVT_KILL_FOCUS, &RealtimeEffectControl::OnFocusChange, this);

        auto sizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

        //On/off button
        auto enableButton = safenew ThemedAButtonWrapper<AButton>(this);
        enableButton->SetTranslatableLabel(XO("Power"));
        enableButton->SetImageIndices(0, bmpEffectOff, bmpEffectOff, bmpEffectOn, bmpEffectOn, bmpEffectOff);
        enableButton->SetButtonToggles(true);
        enableButton->SetBackgroundColorIndex(clrEffectListItemBackground);
        mEnableButton = enableButton;

        enableButton->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
            if (mDelegate && mEffectState) {
                mEffectState->SetActive(mEnableButton->IsDown());
                if (mProject) {
                    ProjectHistory::Get(*mProject).ModifyState(false);
                    UndoManager::Get(*mProject).MarkUnsaved();
                }
            }
        });

        //Central button with effect name, show settings
        const auto optionsButton = safenew ThemedAButtonWrapper<AButton>(this, wxID_ANY);
        optionsButton->SetImageIndices(0,
                                       bmpHButtonNormal,
                                       bmpHButtonHover,
                                       bmpHButtonDown,
                                       bmpHButtonHover,
                                       bmpHButtonDisabled);
        optionsButton->SetBackgroundColorIndex(clrEffectListItemBackground);
        optionsButton->SetForegroundColorIndex(clrTrackPanelText);
        optionsButton->SetButtonType(AButton::TextButton);
        optionsButton->Bind(wxEVT_BUTTON, &RealtimeEffectControl::OnOptionsClicked, this);

        //Remove/replace effect
        auto changeButton = safenew ThemedAButtonWrapper<AButton>(this);
        changeButton->SetImageIndices(0, bmpMoreNormal, bmpMoreHover, bmpMoreDown, bmpMoreHover, bmpMoreDisabled);
        changeButton->SetBackgroundColorIndex(clrEffectListItemBackground);
        changeButton->SetTranslatableLabel(XO("Replace effect"));
        changeButton->Bind(wxEVT_BUTTON, &RealtimeEffectControl::OnChangeButtonClicked, this);

        auto dragArea = safenew wxStaticBitmap(this, wxID_ANY, theTheme.Bitmap(bmpDragArea));
        dragArea->Disable();
        sizer->Add(dragArea, 0, wxLEFT | wxCENTER, 5);
        sizer->Add(enableButton, 0, wxLEFT | wxCENTER, 5);
        sizer->Add(optionsButton, 1, wxLEFT | wxCENTER, 5);
        sizer->Add(changeButton, 0, wxLEFT | wxRIGHT | wxCENTER, 5);
        mChangeButton = changeButton;
        mOptionsButton = optionsButton;

        auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        vSizer->Add(sizer.release(), 0, wxUP | wxDOWN | wxEXPAND, 10);

        SetSizer(vSizer.release());

        SetInTabOrder(false);

#if wxUSE_ACCESSIBILITY
        SetAccessible(safenew RealtimeEffectControlAx(this));
#endif
    }

    void SetEffect(AudacityProject& project,
                   const std::shared_ptr<EffectListUIDelegate>& delegate,
                   const std::shared_ptr<RealtimeEffectState>& pState)
    {
        mProject = &project;
        mDelegate = delegate;
        mEffectState = pState;

        mSubscription = mEffectState->Subscribe([this](RealtimeEffectStateChange state) {
            state == RealtimeEffectStateChange::EffectOn
            ? mEnableButton->PushDown()
            : mEnableButton->PopUp();
        });

        TranslatableString label;
        if (pState) {
            label = GetEffectName(*mEffectState);
            mSettingsAccess = pState->GetAccess();
        } else {
            mSettingsAccess.reset();
        }
        if (mEnableButton) {
            mSettingsAccess && mSettingsAccess->Get().extra.GetActive()
            ? mEnableButton->PushDown()
            : mEnableButton->PopUp();
        }
        if (mOptionsButton) {
            mOptionsButton->SetTranslatableLabel(label);
            mOptionsButton->SetEnabled(pState && GetPlugin(pState->GetID()));
        }
    }

    void RemoveFromList()
    {
        if (!mDelegate || !mProject || mEffectState == nullptr) {
            return;
        }

        auto& ui = RealtimeEffectStateUI::Get(*mEffectState);
        // Don't need autosave for the effect that is being removed
        ui.Hide();

        //After AudioIO::RemoveState call this will be destroyed
        auto effectName = GetEffectName(*mEffectState);

        AudioIO::Get()->RemoveState(*mProject, mDelegate->GetChannelGroup(), std::move(mEffectState));
        ProjectHistory::Get(*mProject).PushState(
            /*! i18n-hint: undo history record
             first parameter - realtime effect name
             second parameter - track name
             */
            XO("Removed %s from %s").Format(effectName, mDelegate->GetSourceName()),
            /*! i18n-hint: undo history record
             first parameter - realtime effect name */
            XO("Remove %s").Format(effectName)
            );
    }

    void OnOptionsClicked(wxCommandEvent& event)
    {
        if (!mDelegate || !mEffectState) {
            return;//not initialized
        }
        const auto ID = mEffectState->GetID();
        const auto effectPlugin = EffectManager::Get().GetEffect(ID);

        if (effectPlugin == nullptr) {
            ///TODO: effect is not available
            return;
        }

        if (!mProject) {
            return;
        }
        auto& effectStateUI = RealtimeEffectStateUI::Get(*mEffectState);

        effectStateUI.SetTargetName(mDelegate->GetSourceName());
        effectStateUI.Toggle(*mProject);
    }

    void OnChangeButtonClicked(wxCommandEvent& event)
    {
        if (!mDelegate || mProject == nullptr) {
            return;
        }

        if (mEffectState == nullptr) {
            return;//not initialized
        }
        const auto effectID = EffectsMenuHelper::PickEffect(
            *mProject,
            mChangeButton,
            mEffectState->GetID()
            );
        if (!effectID) {
            return;//nothing
        }
        if (effectID->empty()) {
            RemoveFromList();
        } else {
            auto& project = *mProject;
            auto& em = RealtimeEffectManager::Get(project);
            auto oIndex = em.FindState(mDelegate->GetChannelGroup(), mEffectState);
            if (!oIndex) {
                return;
            }

            auto oldName = GetEffectName(*mEffectState);

            if (auto state = AudioIO::Get()
                             ->ReplaceState(project, mDelegate->GetChannelGroup(), *oIndex, *effectID)
                ) {
                // Message subscription took care of updating the button text
                // and destroyed `this`!
                auto effect = state->GetEffect();
                assert(effect); // postcondition of ReplaceState
                ProjectHistory::Get(project).PushState(
                    /*i18n-hint: undo history,
                     first and second parameters - realtime effect names
                     */
                    XO("Replaced %s with %s")
                    .Format(oldName, effect->GetName()),
                    /*! i18n-hint: undo history record
                     first parameter - realtime effect name */
                    XO("Replace %s").Format(oldName));
            }
        }
    }

    void OnPaint(wxPaintEvent&)
    {
        wxBufferedPaintDC dc(this);
        const auto rect = wxRect(GetSize());

        dc.SetPen(*wxTRANSPARENT_PEN);
        dc.SetBrush(GetBackgroundColour());
        dc.DrawRectangle(rect);

        dc.SetPen(theTheme.Colour(clrEffectListItemBorder));
        dc.SetBrush(theTheme.Colour(clrEffectListItemBorder));
        dc.DrawLine(rect.GetBottomLeft(), rect.GetBottomRight());

        if (HasFocus()) {
            AColor::DrawFocus(dc, GetClientRect().Deflate(3, 3));
        }
    }

    void OnFocusChange(wxFocusEvent& evt)
    {
        Refresh(false);
        evt.Skip();
    }
};

static wxString GetSafeVendor(const PluginDescriptor& descriptor)
{
    if (descriptor.GetVendor().empty()) {
        return XO("Unknown").Translation();
    }

    return descriptor.GetVendor();
}
}

class TrackEffectListUIDelegate final : public EffectListUIDelegate
{
    std::shared_ptr<SampleTrack> mTrack;
public:

    TrackEffectListUIDelegate(std::shared_ptr<SampleTrack> track)
        : mTrack(std::move(track))
    {
        UpdateRealtimeEffectUIData(*mTrack);
    }

    RealtimeEffectList& GetEffectList() override
    {
        return RealtimeEffectList::Get(*mTrack);
    }

    wxString GetSourceName() override
    {
        return mTrack->GetName();
    }

    ChannelGroup* GetChannelGroup() override
    {
        return mTrack.get();
    }
};

class ProjectEffectListDelegate final : public EffectListUIDelegate
{
    std::shared_ptr<RealtimeEffectList> mEffectList;
public:

    ProjectEffectListDelegate(AudacityProject& project)
    {
        mEffectList = RealtimeEffectList::Get(project).shared_from_this();
        UpdateRealtimeEffectUIData(project);
    }

    RealtimeEffectList& GetEffectList() override
    {
        return *mEffectList;
    }

    wxString GetSourceName() override
    {
        //i18n-hint: master channel display name
        return _("Master");
    }

    ChannelGroup* GetChannelGroup() override
    {
        return nullptr;
    }
};

class RealtimeEffectListWindow : public wxScrolledWindow
{
    wxWeakRef<AudacityProject> mProject;
    AButton* mAddEffect{ nullptr };
    wxWindow* mEffectListContainer{ nullptr };
    wxWindow* mFooter{ nullptr };

    std::shared_ptr<EffectListUIDelegate> mDelegate;

    Observer::Subscription mEffectListItemMovedSubscription;

public:
    RealtimeEffectListWindow(wxWindow* parent,
                             wxWindowID winid = wxID_ANY,
                             const wxPoint& pos = wxDefaultPosition,
                             const wxSize& size = wxDefaultSize,
                             long style = wxScrolledWindowStyle,
                             const wxString& name = wxPanelNameStr)
        : wxScrolledWindow(parent, winid, pos, size, style, name)
    {
#ifdef __WXMSW__
        //Fixes flickering on redraw
        wxScrolledWindow::SetDoubleBuffered(true);
#endif
        auto rootSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

        auto effectListContainer = safenew ThemedWindowWrapper<wxPanel>(this, wxID_ANY);
        effectListContainer->SetBackgroundColorIndex(clrMedium);
        effectListContainer->SetSizer(safenew wxBoxSizer(wxVERTICAL));
        effectListContainer->SetDoubleBuffered(true);
        effectListContainer->Hide();
        mEffectListContainer = effectListContainer;

        auto addEffect = safenew ThemedAButtonWrapper<AButton>(this, wxID_ANY);
        addEffect->SetImageIndices(0,
                                   bmpHButtonNormal,
                                   bmpHButtonHover,
                                   bmpHButtonDown,
                                   bmpHButtonHover,
                                   bmpHButtonDisabled);
        addEffect->SetTranslatableLabel(XO("Add effect"));
        addEffect->SetButtonType(AButton::TextButton);
        addEffect->SetBackgroundColorIndex(clrMedium);
        addEffect->SetForegroundColorIndex(clrTrackPanelText);
        addEffect->Bind(wxEVT_BUTTON, &RealtimeEffectListWindow::OnAddEffectClicked, this);
        mAddEffect = addEffect;

        //indicates the insertion position of the item
        auto dropHintLine = safenew ThemedWindowWrapper<DropHintLine>(effectListContainer, wxID_ANY);
        dropHintLine->SetBackgroundColorIndex(clrDropHintHighlight);
        dropHintLine->Hide();

        rootSizer->Add(mEffectListContainer, 0, wxEXPAND | wxBOTTOM, 10);
        rootSizer->Add(addEffect, 0, wxLEFT | wxRIGHT | wxBOTTOM | wxEXPAND, 20);

        SetSizer(rootSizer.release());
        SetMinSize({});

        Bind(EVT_MOVABLE_CONTROL_DRAG_STARTED, [dropHintLine](const MovableControlEvent& event)
        {
            if (auto window = dynamic_cast<wxWindow*>(event.GetEventObject())) {
                window->Raise();
            }
        });
        Bind(EVT_MOVABLE_CONTROL_DRAG_POSITION, [this, dropHintLine](const MovableControlEvent& event)
        {
            constexpr auto DropHintLineHeight { 3 };//px

            auto sizer = mEffectListContainer->GetSizer();
            assert(sizer != nullptr);

            if (event.GetSourceIndex() == event.GetTargetIndex()) {
                //do not display hint line if position didn't change
                dropHintLine->Hide();
                return;
            }

            if (!dropHintLine->IsShown()) {
                dropHintLine->Show();
                dropHintLine->Raise();
                if (auto window = dynamic_cast<wxWindow*>(event.GetEventObject())) {
                    window->Raise();
                }
            }

            auto item = sizer->GetItem(event.GetTargetIndex());
            dropHintLine->SetSize(item->GetSize().x, DropHintLineHeight);

            if (event.GetTargetIndex() > event.GetSourceIndex()) {
                dropHintLine->SetPosition(item->GetRect().GetBottomLeft() - wxPoint(0, DropHintLineHeight));
            } else {
                dropHintLine->SetPosition(item->GetRect().GetTopLeft());
            }
        });

        Bind(EVT_MOVABLE_CONTROL_DRAG_FINISHED, [this, dropHintLine](const MovableControlEvent& event)
        {
            dropHintLine->Hide();

            if (!mDelegate || !mProject) {
                return;
            }

            const auto from = event.GetSourceIndex();
            const auto to = event.GetTargetIndex();
            if (from != to) {
                auto& effectList = mDelegate->GetEffectList();
                auto effectName
                    =effectList.GetStateAt(from)->GetEffect()->GetName();
                bool up = (to < from);
                effectList.MoveEffect(from, to);
                ProjectHistory::Get(*mProject).PushState(
                    (up
                     /*! i18n-hint: undo history record
                      first parameter - realtime effect name
                      second parameter - track name
                      */
                     ? XO("Moved %s up in %s")
                     /*! i18n-hint: undo history record
                      first parameter - realtime effect name
                      second parameter - track name
                      */
                     : XO("Moved %s down in %s"))
                    .Format(effectName, mDelegate->GetSourceName()),
                    XO("Change effect order"), UndoPush::CONSOLIDATE);
            } else {
                wxWindowUpdateLocker freeze(this);
                Layout();
            }
        });

        SetScrollRate(0, 20);
#if defined(__WXMSW__) || defined(__WXMAC__)
#endif
    }

    void SetFooter(wxWindow* footer)
    {
        if (footer == mFooter) {
            return;
        }

        if (mFooter != nullptr) {
            GetSizer()->Detach(mFooter);
            mFooter->Destroy();
        }

        mFooter = footer;
        GetSizer()->Add(mFooter, 0, wxEXPAND);

        Layout();
    }

    void OnEffectListItemChange(const RealtimeEffectListMessage& msg)
    {
        auto sizer = mEffectListContainer->GetSizer();
        const auto insertItem = [this, &msg](){
            auto& effects = mDelegate->GetEffectList();
            InsertEffectRow(msg.srcIndex, effects.GetStateAt(msg.srcIndex));
            if (mFooter != nullptr) {
                mFooter->Hide();
            }
        };
        const auto removeItem = [&](){
            auto& ui = RealtimeEffectStateUI::Get(*msg.affectedState);
            // Don't need to auto-save changed settings of effect that is deleted
            // Undo history push will do it anyway
            ui.Hide();

            auto window = sizer->GetItem(msg.srcIndex)->GetWindow();
            sizer->Remove(msg.srcIndex);
            wxTheApp->CallAfter([ref = wxWeakRef { window }] {
                if (ref) {
                    ref->Destroy();
                }
            });

            if (sizer->IsEmpty()) {
                if (mEffectListContainer->IsDescendant(FindFocus())) {
                    mAddEffect->SetFocus();
                }

                mEffectListContainer->Hide();
                if (mFooter != nullptr) {
                    mFooter->Show();
                }
            }
        };

        wxWindowUpdateLocker freeze(this);
        if (msg.type == RealtimeEffectListMessage::Type::Move) {
            const auto sizer = mEffectListContainer->GetSizer();

            const auto movedItem = sizer->GetItem(msg.srcIndex);

            const auto proportion = movedItem->GetProportion();
            const auto flag = movedItem->GetFlag();
            const auto border = movedItem->GetBorder();
            const auto window = movedItem->GetWindow();

            if (msg.srcIndex < msg.dstIndex) {
                window->MoveAfterInTabOrder(sizer->GetItem(msg.dstIndex)->GetWindow());
            } else {
                window->MoveBeforeInTabOrder(sizer->GetItem(msg.dstIndex)->GetWindow());
            }

            sizer->Remove(msg.srcIndex);
            sizer->Insert(msg.dstIndex, window, proportion, flag, border);
        } else if (msg.type == RealtimeEffectListMessage::Type::Insert) {
            insertItem();
        } else if (msg.type == RealtimeEffectListMessage::Type::WillReplace) {
            removeItem();
        } else if (msg.type == RealtimeEffectListMessage::Type::DidReplace) {
            insertItem();
        } else if (msg.type == RealtimeEffectListMessage::Type::Remove) {
            removeItem();
        }
        SendSizeEventToParent();
    }

    void ResetDelegate()
    {
        mEffectListItemMovedSubscription.Reset();

        mProject.Release();
        mDelegate.reset();
        ReloadEffectsList();
    }

    void SetDelegate(AudacityProject& project, const std::shared_ptr<EffectListUIDelegate>& delegate)
    {
        mEffectListItemMovedSubscription.Reset();

        mProject = &project;
        mDelegate = delegate;
        ReloadEffectsList();

        if (mDelegate) {
            mEffectListItemMovedSubscription = mDelegate->GetEffectList().Subscribe(
                *this, &RealtimeEffectListWindow::OnEffectListItemChange);
        }
    }

    void EnableEffects(bool enable)
    {
        if (mDelegate) {
            mDelegate->GetEffectList().SetActive(enable);
        }
    }

    void ReloadEffectsList()
    {
        wxWindowUpdateLocker freeze(this);

        //delete items that were added to the sizer
        mEffectListContainer->Hide();
        mEffectListContainer->GetSizer()->Clear(true);

        if (!mDelegate || mDelegate->GetEffectList().GetStatesCount() == 0) {
            mEffectListContainer->Hide();
        }

        auto isEmpty{ true };
        if (mDelegate) {
            auto& effects = mDelegate->GetEffectList();
            isEmpty = effects.GetStatesCount() == 0;
            for (size_t i = 0, count = effects.GetStatesCount(); i < count; ++i) {
                InsertEffectRow(i, effects.GetStateAt(i));
            }
        }
        mAddEffect->SetEnabled(!!mDelegate);
        //Workaround for GTK: Underlying GTK widget does not update
        //its size when wxWindow size is set to zero
        mEffectListContainer->Show(!isEmpty);
        if (mFooter != nullptr) {
            mFooter->Show(isEmpty);
        }

        SendSizeEventToParent();
    }

    void OnAddEffectClicked(const wxCommandEvent& event)
    {
        if (!mDelegate || !mProject) {
            return;
        }

        const auto effectId = EffectsMenuHelper::PickEffect(
            *mProject,
            dynamic_cast<wxWindow*>(event.GetEventObject()),
            {}
            );

        if (!effectId || effectId->empty()) {
            return;
        }

        auto plug = PluginManager::Get().GetPlugin(*effectId);
        if (!plug) {
            return;
        }

        if (!PluginManager::IsPluginAvailable(*plug)) {
            BasicUI::ShowMessageBox(
                XO("This plugin could not be loaded.\nIt may have been deleted."),
                BasicUI::MessageBoxOptions()
                .Caption(XO("Plugin Error")));

            return;
        }

        if (const auto state = AudioIO::Get()->AddState(*mProject, mDelegate->GetChannelGroup(), *effectId)) {
            auto effect = state->GetEffect();
            assert(effect); // postcondition of AddState
            const auto effectName = effect->GetName();
            ProjectHistory::Get(*mProject).PushState(
                /*! i18n-hint: undo history record
                 first parameter - realtime effect name
                 second parameter - track name
                 */
                XO("Added %s to %s").Format(effectName, mDelegate->GetSourceName()),
                //i18n-hint: undo history record
                XO("Add %s").Format(effectName));
        }
    }

    void InsertEffectRow(size_t index,
                         const std::shared_ptr<RealtimeEffectState>& pState)
    {
        if (!mDelegate || !mProject) {
            return;
        }

        // See comment in ReloadEffectsList
        if (!mEffectListContainer->IsShown()) {
            mEffectListContainer->Show();
        }

        auto row = safenew ThemedWindowWrapper<RealtimeEffectControl>(mEffectListContainer, wxID_ANY);
        row->SetBackgroundColorIndex(clrEffectListItemBackground);
        row->SetEffect(*mProject, mDelegate, pState);
        mEffectListContainer->GetSizer()->Insert(index, row, 0, wxEXPAND);
    }
};

struct RealtimeEffectPanel::PrefsListenerHelper : PrefsListener
{
    AudacityProject& mProject;

    explicit PrefsListenerHelper(AudacityProject& project)
        : mProject{project}
    {}

    void UpdatePrefs() override
    {
        auto& trackList = TrackList::Get(mProject);
        for (auto waveTrack : trackList.Any<WaveTrack>()) {
            ReopenRealtimeEffectUIData(mProject, *waveTrack);
        }
    }
};

namespace {
AttachedWindows::RegisteredFactory sKey{
    [](AudacityProject& project) -> wxWeakRef<wxWindow> {
        constexpr auto EffectsPanelMinWidth { 255 };

        const auto pProjectWindow = &ProjectWindow::Get(project);
        auto effectsPanel = safenew ThemedWindowWrapper<RealtimeEffectPanel>(
            project,
            pProjectWindow->GetContainerWindow(),
            wxID_ANY,
            wxDefaultPosition,
            wxDefaultSize,
            wxNO_BORDER | wxSP_LIVE_UPDATE | wxSP_THIN_SASH
            );
        effectsPanel->SetMinSize({ EffectsPanelMinWidth, -1 });
        effectsPanel->SetName(_("Realtime effects"));
        effectsPanel->SetBackgroundColorIndex(clrMedium);
        effectsPanel->Hide();//initially hidden
        return effectsPanel;
    }
};
}

RealtimeEffectPanel& RealtimeEffectPanel::Get(AudacityProject& project)
{
    return GetAttachedWindows(project).Get<RealtimeEffectPanel>(sKey);
}

const RealtimeEffectPanel&
RealtimeEffectPanel::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

RealtimeEffectPanel::RealtimeEffectPanel(
    AudacityProject& project, wxWindow* parent, wxWindowID id, const wxPoint& pos,
    const wxSize& size,
    long style, const wxString& name)
    : wxSplitterWindow(parent, id, pos, size, style, name)
    , mProject(project)
    , mPrefsListenerHelper(std::make_unique<PrefsListenerHelper>(project))
{
    SetSashInvisible();//Use custom sash

    SetSashGravity(1.0);
    MakeTrackEffectPane();
    MakeMasterEffectPane();
    {
        RealtimeEffectList::Get(project).IsActive()
        ? mToggleMasterEffects->PushDown()
        : mToggleMasterEffects->PopUp();
        mMasterEffectList->SetDelegate(mProject,
                                       std::make_shared<ProjectEffectListDelegate>(mProject)
                                       );
    }
    SetMinimumPaneSize(mTrackEffectsPanel->GetSizer()->CalcMin().y);
    SplitHorizontally(mTrackEffectsPanel, mProjectEffectsPanel, -267);

    Bind(wxEVT_CHAR_HOOK, &RealtimeEffectPanel::OnCharHook, this);
    mTrackListChanged
        =PendingTracks::Get(mProject).Subscribe([this](const TrackListEvent& evt) {
        auto track = evt.mpTrack.lock();
        auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(track);

        if (waveTrack == nullptr) {
            return;
        }

        switch (evt.mType) {
            case TrackListEvent::TRACK_DATA_CHANGE:
                if (mCurrentTrack.lock() == waveTrack) {
                    mTrackTitle->SetLabel(track->GetName());
                }
                UpdateRealtimeEffectUIData(*waveTrack);
                break;
            case TrackListEvent::DELETION:
                if (evt.mExtra == 0) {
                    mPotentiallyRemovedTracks.push_back(waveTrack);
                }
                break;
            case TrackListEvent::ADDITION:
                // Addition can be fired as a part of "replace" event.
                // Calling UpdateRealtimeEffectUIData is mostly no-op,
                // it will just create a new State and Access for it.
                UpdateRealtimeEffectUIData(*waveTrack);
                break;
            default:
                break;
        }
    });

    mUndoSubscription = UndoManager::Get(mProject).Subscribe(
        [this](UndoRedoMessage message)
    {
        if (
            message.type == UndoRedoMessage::Type::Purge
            || message.type == UndoRedoMessage::Type::BeginPurge
            || message.type == UndoRedoMessage::Type::EndPurge) {
            return;
        }

        auto& trackList = TrackList::Get(mProject);

        // Realtime effect UI is only updated on Undo or Redo
        auto waveTracks = trackList.Any<WaveTrack>();

        if (
            message.type == UndoRedoMessage::Type::UndoOrRedo
            || message.type == UndoRedoMessage::Type::Reset) {
            for (auto waveTrack : waveTracks) {
                UpdateRealtimeEffectUIData(*waveTrack);
            }
        }

        // But mPotentiallyRemovedTracks processing happens as fast as possible.
        // This event is fired right after the track is deleted, so we do not
        // hold the strong reference to the track much longer than need.
        if (mPotentiallyRemovedTracks.empty()) {
            return;
        }

        // Collect RealtimeEffectUIs that are currently shown
        // for the potentially removed tracks
        std::vector<RealtimeEffectStateUI*> shownUIs;

        for (auto track : mPotentiallyRemovedTracks) {
            // By construction, track cannot be null
            assert(track != nullptr);

            VisitRealtimeEffectStateUIs(
                *track,
                [&shownUIs](auto& ui)
            {
                if (ui.IsShown()) {
                    shownUIs.push_back(&ui);
                }
            });
        }

        // For every UI shown - check if the corresponding state
        // is reachable from the current track list.
        for (auto effectUI : shownUIs) {
            bool reachable = false;

            for (auto track : waveTracks) {
                VisitRealtimeEffectStateUIs(
                    *track,
                    [effectUI, &reachable](auto& ui)
                {
                    if (effectUI == &ui) {
                        reachable = true;
                    }
                });

                if (reachable) {
                    break;
                }
            }

            if (!reachable) {
                // Don't need to autosave for an unreachable state
                effectUI->Hide();
            }
        }

        mPotentiallyRemovedTracks.clear();
    });

    mFocusChangeSubscription = TrackFocus::Get(project)
                               .Subscribe([this](const TrackFocusChangeMessage& msg) {
        if (IsShown()) {
            auto& trackFocus = TrackFocus::Get(mProject);
            ShowPanel(dynamic_cast<SampleTrack*>(trackFocus.Get()), false);
        }
    });

    Bind(wxEVT_CLOSE_WINDOW, [this](wxCloseEvent&) {
        HidePanel();
    });
}

RealtimeEffectPanel::~RealtimeEffectPanel()
{
}

void RealtimeEffectPanel::ShowPanel(SampleTrack* track, bool focus)
{
    if (track == nullptr) {
        ResetTrack();
        return;
    }

    wxWindowUpdateLocker freeze(this);

    SetTrack(track->SharedPointer<SampleTrack>());

    auto& projectWindow = ProjectWindow::Get(mProject);
    const auto pContainerWindow = projectWindow.GetContainerWindow();
    if (pContainerWindow->GetWindow1() != this) {
        //Restore previous effects window size
        pContainerWindow->SplitVertically(
            this,
            projectWindow.GetTrackListWindow(),
            this->GetSize().GetWidth());
    }
    if (focus) {
        SetFocus();
    }
    projectWindow.Layout();
}

void RealtimeEffectPanel::HidePanel()
{
    wxWindowUpdateLocker freeze(this);

    auto& projectWindow = ProjectWindow::Get(mProject);
    const auto pContainerWindow = projectWindow.GetContainerWindow();
    const auto pTrackListWindow = projectWindow.GetTrackListWindow();
    if (pContainerWindow->GetWindow2() == nullptr) {
        //only effects panel is present, restore split positions before removing effects panel
        //Workaround: ::Replace and ::Initialize do not work here...
        pContainerWindow->SplitVertically(this, pTrackListWindow);
    }

    pContainerWindow->Unsplit(this);
    pTrackListWindow->SetFocus();
    projectWindow.Layout();
}

void RealtimeEffectPanel::SetTrack(const std::shared_ptr<SampleTrack>& track)
{
    //Avoid creation-on-demand of a useless, empty list in case the track is of non-wave type.
    if (track && dynamic_cast<WaveTrack*>(&*track) != nullptr) {
        mTrackTitle->SetLabel(track->GetName());
        mToggleTrackEffects->Enable();
        track && RealtimeEffectList::Get(*track).IsActive()
        ? mToggleTrackEffects->PushDown()
        : mToggleTrackEffects->PopUp();
        mTrackEffectList->SetDelegate(
            mProject,
            std::make_shared<TrackEffectListUIDelegate>(track)
            );

        mCurrentTrack = track;
        //i18n-hint: argument - track name
        mTrackEffectsHeader->SetName(wxString::Format(_("Realtime effects for %s"), track->GetName()));
    } else {
        ResetTrack();
    }
}

void RealtimeEffectPanel::ResetTrack()
{
    mTrackTitle->SetLabel(wxEmptyString);
    mToggleTrackEffects->Disable();
    mTrackEffectList->ResetDelegate();
    mCurrentTrack.reset();
    mTrackEffectsHeader->SetName(wxEmptyString);
}

void RealtimeEffectPanel::SetFocus()
{
    mTrackEffectsHeader->SetFocus();
}

void RealtimeEffectPanel::MakeTrackEffectPane()
{
    mTrackEffectsPanel = safenew wxPanel(this);

    auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

    auto header = safenew ThemedWindowWrapper<ListNavigationPanel>(mTrackEffectsPanel, wxID_ANY);
#if wxUSE_ACCESSIBILITY
    safenew WindowAccessible(header);
#endif
    header->SetBackgroundColorIndex(clrMedium);
    {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        auto toggleEffects = safenew ThemedAButtonWrapper<AButton>(header);
        toggleEffects->SetImageIndices(0, bmpEffectOff, bmpEffectOff, bmpEffectOn, bmpEffectOn, bmpEffectOff);
        toggleEffects->SetButtonToggles(true);
        toggleEffects->SetTranslatableLabel(XO("Power"));
        toggleEffects->SetBackgroundColorIndex(clrMedium);
        mToggleTrackEffects = toggleEffects;

        toggleEffects->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
            if (mTrackEffectList) {
                mTrackEffectList->EnableEffects(mToggleTrackEffects->IsDown());

                ProjectHistory::Get(mProject).ModifyState(false);
                UndoManager::Get(mProject).MarkUnsaved();
            }
        });

        hSizer->Add(toggleEffects, 0, wxSTRETCH_NOT | wxALIGN_CENTER | wxLEFT, 5);
        {
            auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

            auto headerText = safenew ThemedWindowWrapper<wxStaticText>(header, wxID_ANY, wxEmptyString);
            headerText->SetFont(wxFont(wxFontInfo().Bold()));
            headerText->SetTranslatableLabel(XO("Realtime Effects"));
            headerText->SetForegroundColorIndex(clrTrackPanelText);

            auto trackTitle = safenew ThemedWindowWrapper<wxStaticText>(header, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                                                                        wxST_ELLIPSIZE_END);
            trackTitle->SetForegroundColorIndex(clrTrackPanelText);
            mTrackTitle = trackTitle;

            vSizer->Add(headerText);
            vSizer->Add(trackTitle);

            hSizer->Add(vSizer.release(), 1, wxEXPAND | wxALL, 10);
        }
        auto close = safenew ThemedAButtonWrapper<AButton>(header);
        close->SetTranslatableLabel(XO("Close"));
        close->SetImageIndices(0, bmpCloseNormal, bmpCloseHover, bmpCloseDown, bmpCloseHover, bmpCloseDisabled);
        close->SetBackgroundColorIndex(clrMedium);

        close->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) { Close(); });

        hSizer->Add(close, 0, wxSTRETCH_NOT | wxALIGN_CENTER | wxRIGHT, 5);

        header->SetSizer(hSizer.release());
    }
    vSizer->Add(header, 0, wxEXPAND);

    auto effectList = safenew ThemedWindowWrapper<RealtimeEffectListWindow>(mTrackEffectsPanel, wxID_ANY);
    effectList->SetBackgroundColorIndex(clrMedium);
    {
        auto footer = safenew ThemedWindowWrapper<wxPanel>(effectList, wxID_ANY);
        footer->SetBackgroundColorIndex(clrMedium);

        auto addEffectHint = safenew ThemedWindowWrapper<wxStaticText>(footer, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                                                                       wxST_NO_AUTORESIZE);
        //Workaround: text is set in the OnSizeChange
        addEffectHint->SetForegroundColorIndex(clrTrackPanelText);

        auto addEffectTutorialLink = safenew ThemedWindowWrapper<wxHyperlinkCtrl>(
            footer, wxID_ANY, _("Watch video"),
            "https://www.audacityteam.org/realtime-video", wxDefaultPosition,
            wxDefaultSize, wxHL_ALIGN_LEFT | wxHL_CONTEXTMENU);

        addEffectTutorialLink->Bind(
            wxEVT_HYPERLINK, [](wxHyperlinkEvent& event)
        { BasicUI::OpenInDefaultBrowser(event.GetURL()); });

        auto footerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        footerSizer->Add(addEffectHint, 0, wxLEFT | wxRIGHT | wxBOTTOM | wxEXPAND, 20);
        footerSizer->Add(addEffectTutorialLink, 0, wxLEFT | wxRIGHT | wxEXPAND, 20);
        footer->SetSizer(footerSizer.release());

        footer->Bind(wxEVT_SIZE, [=](wxSizeEvent& event)
        {
            if (auto sizerItem = footer->GetSizer()->GetItem(addEffectHint)) {
                //We need to wrap the text whenever panel width changes and adjust widget height
                //so that text is fully visible, but there is no height-for-width layout algorithm
                //in wxWidgets yet, so for now we just do it manually

                //Restore original text, because 'Wrap' will replace it with wrapped one
                addEffectHint->SetLabel(_("Realtime effects are non-destructive and can be changed at any time."));
                addEffectHint->Wrap(mTrackEffectsPanel->GetClientSize().x - sizerItem->GetBorder() * 2);
                addEffectHint->InvalidateBestSize();
            }
            event.Skip();
        });

        effectList->SetFooter(footer);
    }
    vSizer->Add(effectList, 1, wxEXPAND);

    mTrackEffectsHeader = header;
    mTrackEffectList = effectList;

    mTrackEffectsPanel->SetSizer(vSizer.release());
}

class SashLine : public wxWindow
{
    wxWeakRef<wxSplitterWindow> mSplitter;
    bool mDrag { false };
public:
    SashLine(wxWindow* parent,
             wxWindowID id,
             const wxPoint& pos = wxDefaultPosition,
             const wxSize& size = wxDefaultSize)
        : wxWindow(parent, id, pos, size, wxNO_BORDER, wxEmptyString)
    {
        wxWindow::SetBackgroundStyle(wxBG_STYLE_PAINT);
        SetCursor(wxCursor(wxCURSOR_SIZENS));

        Bind(wxEVT_LEFT_DOWN, &SashLine::OnMouseDown, this);
        Bind(wxEVT_LEFT_UP, &SashLine::OnMouseUp, this);
        Bind(wxEVT_MOTION, &SashLine::OnMove, this);
        Bind(wxEVT_MOUSE_CAPTURE_LOST, &SashLine::OnMouseCaptureLost, this);
        Bind(wxEVT_PAINT, &SashLine::OnPaint, this);
    }

    void SetSplitterWindow(wxSplitterWindow* window)
    {
        mSplitter = window;
    }

    bool AcceptsFocus() const override { return false; }

private:

    void OnMouseCaptureLost(wxMouseCaptureLostEvent& event)
    {
        mDrag = false;
    }

    void OnMouseDown(wxMouseEvent& evt)
    {
        if (!mSplitter) {
            return;
        }
        CaptureMouse();
        mDrag = true;
    }

    void OnMouseUp(wxMouseEvent& evt)
    {
        mDrag = false;
        ReleaseMouse();
    }

    void OnMove(wxMouseEvent& evt)
    {
        if (!mDrag || !mSplitter) {
            evt.Skip();
            return;
        }
        const auto pos = mSplitter->ScreenToClient(ClientToScreen(evt.GetPosition()));
        mSplitter->SetSashPosition(
            std::clamp(
                pos.y,
                mSplitter->GetMinimumPaneSize(),
                mSplitter->GetSize().y - mSplitter->GetMinimumPaneSize()
                ));
    }

    void OnPaint(wxPaintEvent&)
    {
        wxBufferedPaintDC dc(this);
        const auto rect = wxRect(GetSize());

        dc.SetPen(*wxTRANSPARENT_PEN);
        dc.SetBrush(GetBackgroundColour());
        dc.DrawRectangle(rect);

        dc.SetPen(GetForegroundColour());
        dc.SetBrush(*wxTRANSPARENT_BRUSH);
        const auto yy = rect.GetTop() + rect.GetHeight() / 2;
        dc.DrawLine(rect.GetLeft(), yy, rect.GetRight(), yy);
    }
};

void RealtimeEffectPanel::MakeMasterEffectPane()
{
    mProjectEffectsPanel = safenew wxPanel(this);

    auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

    const auto sash = safenew ThemedWindowWrapper<SashLine>(mProjectEffectsPanel, wxID_ANY);
    sash->SetMinSize(wxSize { -1, 3 });
    sash->SetSplitterWindow(this);
    sash->SetBackgroundColorIndex(clrMedium);
    sash->SetForegroundColorIndex(clrDark);
    vSizer->Add(sash, 0, wxEXPAND);

    auto header = safenew ThemedWindowWrapper<ListNavigationPanel>(mProjectEffectsPanel, wxID_ANY);
#if wxUSE_ACCESSIBILITY
    safenew WindowAccessible(header);
#endif
    header->SetBackgroundColorIndex(clrMedium);
    {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        auto toggleEffects = safenew ThemedAButtonWrapper<AButton>(header);
        toggleEffects->SetImageIndices(0, bmpEffectOff, bmpEffectOff, bmpEffectOn, bmpEffectOn, bmpEffectOff);
        toggleEffects->SetButtonToggles(true);
        toggleEffects->SetTranslatableLabel(XO("Power"));
        toggleEffects->SetBackgroundColorIndex(clrMedium);
        mToggleMasterEffects = toggleEffects;

        toggleEffects->Bind(wxEVT_BUTTON, [this](wxCommandEvent&) {
            if (mMasterEffectList) {
                mMasterEffectList->EnableEffects(mToggleMasterEffects->IsDown());

                ProjectHistory::Get(mProject).ModifyState(false);
                UndoManager::Get(mProject).MarkUnsaved();
            }
        });

        hSizer->Add(toggleEffects, 0, wxSTRETCH_NOT | wxALIGN_CENTER | wxLEFT, 5);
        {
            auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

            auto headerText = safenew ThemedWindowWrapper<wxStaticText>(header, wxID_ANY, wxEmptyString);
            headerText->SetFont(wxFont(wxFontInfo().Bold()));
            headerText->SetTranslatableLabel(XO("Master Effects"));
            headerText->SetForegroundColorIndex(clrTrackPanelText);
            header->SetName(headerText->GetLabel());

            auto desc = safenew ThemedWindowWrapper<wxStaticText>(header, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                                                                  wxST_ELLIPSIZE_END);
            desc->SetForegroundColorIndex(clrTrackPanelText);
            desc->SetTranslatableLabel(XO("Applies to all tracks"));

            vSizer->Add(headerText);
            vSizer->Add(desc);

            hSizer->Add(vSizer.release(), 1, wxEXPAND | wxALL, 10);
        }

        header->SetSizer(hSizer.release());
    }
    vSizer->Add(header, 0, wxEXPAND | wxTOP, 5);

    auto effectList = safenew ThemedWindowWrapper<RealtimeEffectListWindow>(mProjectEffectsPanel, wxID_ANY);
    effectList->SetBackgroundColorIndex(clrMedium);
    vSizer->Add(effectList, 1, wxEXPAND);

    mMasterEffectList = effectList;

    mProjectEffectsPanel->SetSizer(vSizer.release());
}

void RealtimeEffectPanel::OnCharHook(wxKeyEvent& evt)
{
    if (evt.GetKeyCode() == WXK_ESCAPE && IsShown() && IsDescendant(FindFocus())) {
        Close();
    } else {
        evt.Skip();
    }
}
