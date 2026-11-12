/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <wx/arrstr.h>

typedef void PxMixer;

//! The hardware mixer of the selected devices, through portmixer where Audacity is built with it: the input's volume and
//! source, and macOS's hardware playthrough. Without a mixer, the input volume is to be applied to the samples.
class DeviceMixer final
{
public:
    DeviceMixer();
    ~DeviceMixer();

    DeviceMixer(const DeviceMixer&) = delete;
    DeviceMixer& operator=(const DeviceMixer&) = delete;

    //! Closes the mixer of the previous devices and opens that of these, selecting the input source of the settings
    void Open(int playDeviceNum, int recDeviceNum);
    void Close();
    bool IsOpen() const;

    bool InputMixerWorks() const { return mInputMixerWorks; }
    float GetInputVolume() const;
    void SetInputVolume(float volume);

    //! -1 if there's no mixer
    int GetInputSource() const;
    /* Mixer services are always available.  If no stream is running, these
     * methods use whatever device is specified by the preferences.  If a
     * stream *is* running, naturally they manipulate the mixer associated
     * with that stream.  If no mixer is available, output is emulated and
     * input is stuck at 1.0f (a volume gain is applied to output samples).
     */
    void SetInputSource(int inputSource);
    wxArrayString GetInputSourceNames() const;

    //! On macOS, turns the hardware playthrough off while a stream runs, Audacity offering no setting for it
    void DisableHardwarePlaythrough();
    //! Restores the hardware playthrough that DisableHardwarePlaythrough() turned off
    void RestoreHardwarePlaythrough();

private:
    PxMixer* mPortMixer;
    float mPreviousHWPlaythrough;

    /** @brief Can we control the hardware input level?
     *
     * This flag is set to true if using portmixer to control the
     * input volume seems to be working (and so we offer the user the control),
     * and to false (locking the control out) otherwise. This avoids stupid
     * scaled clipping problems when trying to do software emulated input volume
     * control */
    bool mInputMixerWorks;

    /** \brief get the index of the device selected in the preferences.
     *
     * If the device isn't found, returns -1
     */
#if USE_PORTMIXER
    static int getRecordSourceIndex(PxMixer* portMixer);
#endif
};
