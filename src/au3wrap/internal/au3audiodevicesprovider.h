/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
#define AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H

#include <wx/arrstr.h>
#include "global/types/string.h"

#include "libraries/lib-utility/IteratorX.h"
#include "libraries/lib-strings/wxArrayStringEx.h"
#include "playback/iaudiodevicesprovider.h"

namespace au::au3 {
class Au3AudioDevicesProvider : public playback::IAudioDevicesProvider
{
public:
    void init();

    std::vector<std::string> audioOutputDevices() const override;
    std::string currentAudioOutputDevice() const override;
    void setAudioOutputDevice(const std::string& deviceName) override;
    muse::async::Notification audioOutputDeviceChanged() const override;

    std::vector<std::string> audioInputDevices() const override;
    std::string currentAudioInputDevice() const override;
    void setAudioInputDevice(const std::string& deviceName) override;
    muse::async::Notification audioInputDeviceChanged() const override;

    void handleDeviceChange() override;

    std::vector<std::string> audioApiList() const override;
    std::string currentAudioApi() const override;
    void setAudioApi(const std::string& audioApi) override;
    muse::async::Notification audioApiChanged() const override;

private:
    void initHosts();
    void initHostDevices();
    void initInputChannels();

    class Choices
    {
    public:
        void Clear() { mStrings.Clear(); mIndex = -1; }
        [[nodiscard]] bool Empty() const { return mStrings.empty(); }
        std::optional<wxString> Get() const
        {
            if (mIndex < 0 || mIndex >= mStrings.size()) {
                return {};
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

    private:
        wxArrayStringEx mStrings;
        int mIndex{ -1 };
    };

    Choices mInput;
    Choices mOutput;
    Choices mInputChannels;
    Choices mHost;

    muse::async::Notification m_audioOutputDeviceChanged;
    muse::async::Notification m_audioInputDeviceChanged;
    muse::async::Notification m_audioApiChanged;
};
}

#endif // AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
