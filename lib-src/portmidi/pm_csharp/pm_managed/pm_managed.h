// pm_managed.h

#pragma once

#include "portmidi.h"

using namespace System;

namespace pm_managed {


	public ref class MpmDeviceInfo
	{
	public:
		int structVersion; 
		System::String^ interf; /* underlying MIDI API, e.g. MMSystem or DirectX */
		System::String^ name;   /* device name, e.g. USB MidiSport 1x1 */
		bool input; /* true iff input is available */
		bool output; /* true iff output is available */
		int opened; /* used by generic PortMidi code to do error checking on arguments */

		MpmDeviceInfo(const PmDeviceInfo* info)
		{
			structVersion = info->structVersion;
			input = (info->input != 0);
			output = (info->output != 0);
			opened = info->opened;

			interf = gcnew System::String(info->interf);
			name = gcnew System::String(info->name);
		}
	};

	public ref class ManagedPortMIDI
	{
	public:
		int Pm_Initialize()
		{
			::Pm_Initialize();
			return 0;
		}

		int Pm_CountDevices()
		{
			return ::Pm_CountDevices();
		}

		MpmDeviceInfo^ Pm_GetDeviceInfo(int id)
		{
			return gcnew MpmDeviceInfo(::Pm_GetDeviceInfo(id));
		}
	};
}
