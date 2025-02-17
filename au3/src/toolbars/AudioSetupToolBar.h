/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioSetupToolBar.h

  K. Soze

  **********************************************************************/
#ifndef __AUDACITY_AUDIO_SETUP_TOOLBAR__
#define __AUDACITY_AUDIO_SETUP_TOOLBAR__

#include <optional>
#include <vector>
#include <wx/menu.h>
#include "IteratorX.h"
#include "ToolBar.h"
#include "Observer.h"

enum class DeviceChangeMessage : char;

class wxMenu;
class wxString;
struct DeviceSourceMap;

class AudioSetupToolBar final : public ToolBar
{
    static constexpr int kAudioSettings = 15800;
    static constexpr int kAudioDeviceRescan = 15801;

public:
    static Identifier ID();

    explicit AudioSetupToolBar(AudacityProject& project);
    virtual ~AudioSetupToolBar();

    static AudioSetupToolBar& Get(AudacityProject& project);
    static const AudioSetupToolBar& Get(const AudacityProject& project);

    void Create(wxWindow* parent) override;

    void UpdatePrefs() override;
    void UpdateSelectedPrefs(int) override;

    void DeinitChildren();
    void Populate() override;
    void Repaint(wxDC* dc) override;
    void EnableDisableButtons() override;
    void ReCreateButtons() override;
    void OnFocus(wxFocusEvent& event);
    void OnAudioSetup(wxCommandEvent& event);

private:
    void OnRescannedDevices(DeviceChangeMessage);
    void OnHost(int id);
    void OnInput(int id);
    void OnChannels(int id);
    void OnOutput(int id);
    void OnAudioDeviceRescan(wxCommandEvent&);
    void OnSettings(wxCommandEvent& event);
    void CommonMenuItemSteps(bool audioSettingsChosen);

    bool ChangeHost(int hostId);
    class Choices;
    void ChangeDeviceLabel(int deviceId, Choices& choices, bool isInput);
    void RepopulateMenus();
    void FillHosts();
    void FillHostDevices();
    void FillInputChannels();
    void SetDevices(const DeviceSourceMap* in, const DeviceSourceMap* out);
    void RegenerateTooltips() override;

    void MakeAudioSetupButton();
    void ArrangeButtons();

    using Callback = void (AudioSetupToolBar::*)(int id);
    // Append submenu with one radio item group
    // Bind menu items to lambdas that invoke callback,
    // with successive ids from 0
    // Check the item with given index, or disable the submenu when that is < 0
    static void AppendSubMenu(AudioSetupToolBar& toolbar, wxMenu& menu, const wxArrayString& labels, int checkedItem, Callback callback,
                              const wxString& title);

    enum {
        ID_AUDIO_SETUP_BUTTON = 12000,
        BUTTON_COUNT,
    };

    AButton* mAudioSetup{};

    class Choices
    {
    public:
        void Clear() { mStrings.Clear(); mIndex = -1; }
        [[nodiscard]] bool Empty() const { return mStrings.empty(); }
        std::optional<wxString> Get() const
        {
            if (mIndex < 0 || mIndex >= mStrings.size()) {
                return {}
            }
            return { mStrings[mIndex] };
        }

        wxString GetFirst() const
        {
            if (!Empty()) {
                return mStrings[0];
            }
            return {};
        }

        int GetSmallIntegerId() const
        {
            return mIndex;
        }

        int Find(const wxString& name) const
        {
            return make_iterator_range(mStrings).index(name);
        }

        bool Set(const wxString& name)
        {
            auto index = make_iterator_range(mStrings).index(name);
            if (index != -1) {
                mIndex = index;
                return true;
            }
            // else no state change
            return false;
        }

        void Set(wxArrayString&& names)
        {
            mStrings.swap(names);
            mIndex = mStrings.empty() ? -1 : 0;
        }

        // id is just a small-integer index into the string array
        bool Set(int id)
        {
            if (id < 0 || id >= mStrings.size()) {
                return false; // no change of state then
            }
            mIndex = id;
            return true;
        }

        void AppendSubMenu(AudioSetupToolBar& toolBar, wxMenu& menu, Callback callback, const wxString& title);

    private:
        wxArrayStringEx mStrings;
        int mIndex{ -1 };
    };

    Choices mInput;
    Choices mOutput;
    Choices mInputChannels;
    Choices mHost;

    Observer::Subscription mSubscription;

public:

    DECLARE_CLASS(AudioSetupToolBar)
    DECLARE_EVENT_TABLE()
};

#endif
