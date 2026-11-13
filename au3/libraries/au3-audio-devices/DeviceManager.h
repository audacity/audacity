/**********************************************************************

  Audacity: A Digital Audio Editor

  DeviceManager.h

  Created by Michael Chinen (mchinen) on 2/12/11
  Audacity(R) is copyright (c) 1999-2011 Audacity Team.
  License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class DeviceManager
\brief A singleton that manages the audio devices known to Audacity

*//*******************************************************************/

#ifndef __AUDACITY_DEVICEMANAGER__
#define __AUDACITY_DEVICEMANAGER__

#include <chrono>
#include <map>
#include <string>
#include <vector>

#include <wx/string.h> // member variables

#include "DeviceChange.h"

typedef struct DeviceSourceMap {
    int deviceIndex;
    int sourceIndex;
    int hostIndex;
    int totalSources;
    int numChannels;
    wxString sourceString;
    wxString deviceString;
    wxString hostString;
} DeviceSourceMap;

AUDIO_DEVICES_API
std::string MakeDeviceSourceString(const DeviceSourceMap* map, const std::vector<DeviceSourceMap>& deviceMaps);

class AUDIO_DEVICES_API DeviceManager final
#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER) && defined(HAVE_DEVICE_CHANGE)
    : public DeviceChangeHandler
#else
    : public DeviceChangeMessagePublisher
#endif
{
public:
    /// Gets the singleton instance
    static DeviceManager* Instance();

    /// Gets a NEW list of devices by terminating and restarting portaudio
    /// Assumes that DeviceManager is only used on the main thread.
    void Rescan();

    // Time since devices scanned in seconds.
    std::chrono::duration<float> GetTimeSinceRescan();

    DeviceSourceMap* GetDefaultOutputDevice(int hostIndex);
    DeviceSourceMap* GetDefaultInputDevice(int hostIndex);

    int GetHostIndex(const std::string& hostName);

    const std::vector<DeviceSourceMap>& GetInputDeviceMaps();
    const std::vector<DeviceSourceMap>& GetOutputDeviceMaps();

    int GetInputDevicePaIndex(const std::string& hostName, const std::string& deviceId);
    int GetOutputDevicePaIndex(const std::string& hostName, const std::string& deviceId);

    bool UpdateAsioDeviceCaps(int paDeviceIndex);
    static bool IsAsioDevice(int paDeviceIndex);
    static double GetAsioDeviceCurrentSampleRate(int paDeviceIndex);
    static void ShowAsioControlPanel(int paDeviceIndex);

    // Device selection and sample rates

    /** \brief get the index of the supplied (named) recording device, or the
     * device selected in the preferences if none given.
     *
     * Pure utility function, but it comes round a number of times in the code
     * and would be neater done once. If the device isn't found, return the
     * default device index.
     */
    static int getRecordDevIndex(const wxString& devName = {});

    /** \brief get the index of the supplied (named) playback device, or the
     * device selected in the preferences if none given.
     *
     * Pure utility function, but it comes round a number of times in the code
     * and would be neater done once. If the device isn't found, return the
     * default device index.
     */
    static int getPlayDevIndex(const wxString& devName = {});
    /** \brief Get a list of sample rates the output (playback) device
     * supports.
     *
     * If no information about available sample rates can be fetched,
     * an empty list is returned.
     *
     * You can explicitly give the index of the device.  If you don't
     * give it, the currently selected device from the preferences will be used.
     *
     */
    static std::vector<long> GetSupportedPlaybackRates(int DevIndex = -1);

    /** \brief Find the closest supported sample rate for given
     *         playback device.
     *
     * Attempts to find the sample rate that is closest to the requested rate,
     * and is supported by the playback device.
     *
     * If device is not specified the currently selected device will be used.
     *
     * Sample rate check order:
     * - the exact requested rate
     * - higher available rates
     * - lower available rates
     *
     * returns 0 if none is found or the input rate is invalid.
     */
    static long GetClosestSupportedPlaybackRate(int devIndex, long rate);

    /** \brief Get a list of sample rates the input (recording) device
     * supports.
     *
     * If no information about available sample rates can be fetched,
     * an empty list is returned.
     *
     * You can explicitly give the index of the device.  If you don't
     * give it, the currently selected device from the preferences will be used.
     *
     */
    static std::vector<long> GetSupportedCaptureRates(int devIndex = -1);

    /** \brief Find the closest supported sample rate for given
     *         recording device.
     *
     * Attempts to find the sample rate that is closest to the requested rate,
     * and is supported by the recording device.
     *
     * If device is not specified the currently selected device will be used.
     *
     * Sample rate check order:
     * - the exact requested rate
     * - higher available rates
     * - lower available rates
     *
     * returns 0 if none is found or the input rate is invalid.
     */
    static long GetClosestSupportedCaptureRate(int devIndex, long rate);

    /** \brief Get a list of sample rates the current input/output device
     * combination supports.
     *
     * Since there is no concept (yet) for different input/output
     * sample rates, this currently returns only sample rates that are
     * supported on both the output and input device. If no information
     * about available sample rates can be fetched, it returns a default
     * list.
     * You can explicitly give the indexes of the playDevice/recDevice.
     * If you don't give them, the selected devices from the preferences
     * will be used.
     */
    static std::vector<long> GetSupportedSampleRates(int playDevice = -1, int recDevice = -1);

    /** \brief Find the closest supported sample rate for given
     *         playback and recording devices.
     *
     * Attempts to find the sample rate that is closest to the requested rate,
     * and is supported by both playback and recording devices.
     *
     * If devices are not specified the currently selected devices will be used.
     *
     * Sample rate check order:
     * - the exact requested rate
     * - higher available rates
     * - lower available rates
     *
     * returns 0 if none is found or the input rate is invalid.
     */
    static long GetClosestSupportedSampleRate(int playDevice, int recDevice, long rate);

    /** \brief Get a supported sample rate which can be used a an optimal
     * default.
     *
     * Currently, this uses the first supported rate in the list
     * [44100, 48000, highest sample rate]. Used in Project as a default value
     * for project rates if one cannot be retrieved from the preferences.
     * So all in all not that useful or important really
     */
    static int GetOptimalSupportedSampleRate();

    /** \brief Check if the specified playback rate is supported by a device.
     *
     * Verifies if a playback device supports a given rate.
     * If no device index is specified (devIndex == -1), the preferred device is used.
     */
    static bool IsPlaybackRateSupported(int devIndex, long rate);

    /** \brief Check if the specified sample rate is supported by a device.
     *
     * Verifies if a recording device supports a given rate.
     * If no device index is specified (devIndex == -1), the preferred device is used.
     */
    static bool IsCaptureRateSupported(int devIndex, long rate);

    /** \brief Array of common audio sample rates
     *
     * These are the rates we will always support, regardless of hardware support
     * for them (by resampling in audacity if needed) */
    static const int StandardRates[];
    /** \brief How many standard sample rates there are */
    static const int NumStandardRates;

#if defined(EXPERIMENTAL_DEVICE_CHANGE_HANDLER)
#if defined(HAVE_DEVICE_CHANGE)
    // DeviceChangeHandler implementation
    void DeviceChangeNotification();
#endif
#endif

private:
    std::chrono::time_point<std::chrono::steady_clock> mRescanTime;

protected:
    //private constructor - Singleton.
    DeviceManager();
    ~DeviceManager();
    /// Does an initial scan.
    /// Called by GetInputDeviceMaps and GetOutputDeviceMaps when needed.
    void Init();

    DeviceSourceMap* GetDefaultDevice(int hostIndex, int isInput);

    bool m_inited;

    std::vector<DeviceSourceMap> mInputDeviceSourceMaps;
    std::vector<DeviceSourceMap> mOutputDeviceSourceMaps;

    static DeviceManager dm;

    /** \brief Array of audio sample rates to try to use
     *
     * These are the rates we will check if a device supports, and is as long
     * as I can think of (to try and work out what the card can do) */
    static const int RatesToTry[];
    /** \brief How many sample rates to try */
    static const int NumRatesToTry;

    // For cacheing supported sample rates
    static std::map<int, std::vector<long> > mCachedPlaybackRates;
    static std::map<int, std::vector<long> > mCachedCaptureRates;
    static std::map<std::pair<int, int>, std::vector<long> > mCachedSampleRates;
};

#endif
