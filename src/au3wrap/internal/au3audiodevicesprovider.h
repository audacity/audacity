/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
#define AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H

#include "modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "global/types/string.h"

#include "libraries/lib-strings/wxArrayStringEx.h"
#include "libraries/lib-utility/IteratorX.h"

#include <wx/arrstr.h>

#include "playback/iaudiodevicesprovider.h"

namespace au::au3 {
class Au3AudioDevicesProvider : public playback::IAudioDevicesProvider
{
    muse::Inject<context::IGlobalContext> globalContext;
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

    std::vector<std::string> inputChannelsList() const override;
    std::string currentInputChannels() const override;
    void setInputChannels(const std::string& channels) override;
    muse::async::Notification inputChannelsChanged() const override;
    muse::async::Notification inputChannelsListChanged() const override;

    double bufferLength() const override;
    void setBufferLength(double newBufferLength) override;
    muse::async::Notification bufferLengthChanged() const override;

    double latencyCompensation() const override;
    void setLatencyCompensation(double newLatencyCompensation) override;
    muse::async::Notification latencyCompensationChanged() const override;

    std::vector<uint64_t> availableSampleRateList() const override;
    uint64_t defaultSampleRate() const override;
    void setDefaultSampleRate(uint64_t newRate) override;
    muse::async::Notification defaultSampleRateChanged() const override;

    std::vector<std::string> defaultSampleFormatList() const override;
    std::string defaultSampleFormat() const override;
    void setDefaultSampleFormat(const std::string& format) override;
    muse::async::Notification defaultSampleFormatChanged() const override;

private:
    void initHosts();
    void initHostDevices();
    void initInputChannels();

    void updateInputOutputDevices();
    void setupRecordingDevice(const std::string& newDevice);

    std::string defaultOutputDevice();
    std::string defaultInputDevice();

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
    muse::async::Notification m_inputChannelsChanged;
    muse::async::Notification m_inputChannelsListChanged;
    muse::async::Notification m_bufferLengthChanged;
    muse::async::Notification m_latencyCompensationChanged;
    muse::async::Notification m_defaultSampleRateChanged;
    muse::async::Notification m_defaultSampleFormatChanged;
};
}

#endif // AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
