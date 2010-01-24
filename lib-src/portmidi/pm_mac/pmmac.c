/* pmmac.c -- PortMidi os-dependent code */

/* This file only needs to implement:
pm_init(), which calls various routines to register the 
available midi devices,
Pm_GetDefaultInputDeviceID(), and
Pm_GetDefaultOutputDeviceID().
It is seperate from pmmacosxcm because we might want to register
non-CoreMIDI devices.
*/

#include "stdlib.h"
#include "portmidi.h"
#include "pmutil.h"
#include "pminternal.h"
#include "pmmacosxcm.h"

PmDeviceID pm_default_input_device_id = -1;
PmDeviceID pm_default_output_device_id = -1;

void pm_init()
{
    PmError err = pm_macosxcm_init();
    // this is set when we return to Pm_Initialize, but we need it
    // now in order to (successfully) call Pm_CountDevices()
    pm_initialized = TRUE;
    if (!err) {
        pm_default_input_device_id = find_default_device(
                "/PortMidi/PM_RECOMMENDED_INPUT_DEVICE", TRUE, 
                pm_default_input_device_id);
        pm_default_output_device_id = find_default_device(
                "/PortMidi/PM_RECOMMENDED_OUTPUT_DEVICE", FALSE, 
                pm_default_output_device_id);
    }
}


void pm_term(void)
{
    pm_macosxcm_term();
}


PmDeviceID Pm_GetDefaultInputDeviceID()
{
    Pm_Initialize();
    return pm_default_input_device_id;
}

PmDeviceID Pm_GetDefaultOutputDeviceID() {
    Pm_Initialize();
    return pm_default_output_device_id;
}

void *pm_alloc(size_t s) { return malloc(s); }

void pm_free(void *ptr) { free(ptr); }


