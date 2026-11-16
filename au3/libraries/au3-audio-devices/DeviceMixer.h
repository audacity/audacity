/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <wx/arrstr.h>

//! The hardware mixer of the selected devices, through portmixer where Audacity is built with it: the input's volume and
//! source, and macOS's hardware playthrough. Without a mixer, the input volume is to be applied to the samples.
class DeviceMixer final
{
public:
    DeviceMixer() = default;
    ~DeviceMixer();

    DeviceMixer(const DeviceMixer&) = delete;
    DeviceMixer& operator=(const DeviceMixer&) = delete;

    //! Closes the mixer of the previous devices and opens that of these, selecting the input source of the settings
    void Open(int playDevice, int recDevice);
    void Close();
    bool IsOpen() const { return mMixer != nullptr; }

    //! Whether the input volume can be set accurately; if not, it is to be applied to the samples
    bool InputVolumeWorks() const { return mInputVolumeWorks; }
    float GetInputVolume() const;
    void SetInputVolume(float volume);

    //! -1 if there's no mixer
    int GetInputSource() const;
    void SetInputSource(int source);
    wxArrayString GetInputSourceNames() const;

    //! On macOS, turns the hardware playthrough off while a stream runs, Audacity offering no setting for it
    void DisableHardwarePlaythrough();
    //! Restores the hardware playthrough that DisableHardwarePlaythrough() turned off
    void RestoreHardwarePlaythrough();

private:
    int GetSettingsInputSource() const;

    //! A PxMixer, if open
    void* mMixer = nullptr;
    float mPreviousHardwarePlaythrough = -1.0f;
    bool mInputVolumeWorks = false;
};
