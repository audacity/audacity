"""
Module for interacting with midi input and output.

The midi module can send output to midi devices, and get input
from midi devices.  It can also list midi devices on the system.

Including real midi devices, and virtual ones.

It uses the portmidi library.  Is portable to which ever platforms
portmidi supports (currently windows, OSX, and linux).
"""

import atexit



_init = False
_pypm = None


__all__ = [ "Input",
            "MidiException",
            "Output",
            "get_count",
            "get_default_input_id",
            "get_default_output_id",
            "get_device_info",
            "init",
            "quit",
            "time",
           ]

__theclasses__ = ["Input", "Output"]


def init():
    """initialize the midi module
    pyportmidi.init(): return None
    
    Call the initialisation function before using the midi module.
    
    It is safe to call this more than once.
    """
    global _init, _pypm
    if not _init:
        import pyportmidi._pyportmidi
        _pypm = pyportmidi._pyportmidi

        _pypm.Initialize()
        _init = True
        atexit.register(quit)


def quit():
    """uninitialize the midi module
    pyportmidi.quit(): return None


    Called automatically atexit if you don't call it.

    It is safe to call this function more than once.
    """
    global _init, _pypm
    if _init:
        # TODO: find all Input and Output classes and close them first?
        _pypm.Terminate()
        _init = False
        del _pypm

def _check_init():
    if not _init:
        raise RuntimeError("pyportmidi not initialised.")

def get_count():
    """gets the number of devices.
    pyportmidi.get_count(): return num_devices


    Device ids range from 0 to get_count() -1
    """
    _check_init()
    return _pypm.CountDevices()




def get_default_input_id():
    """gets default input device number
    pyportmidi.get_default_input_id(): return default_id
    
    
    Return the default device ID or -1 if there are no devices.
    The result can be passed to the Input()/Ouput() class.
    
    On the PC, the user can specify a default device by
    setting an environment variable. For example, to use device #1.
    
        set PM_RECOMMENDED_INPUT_DEVICE=1
    
    The user should first determine the available device ID by using
    the supplied application "testin" or "testout".
    
    In general, the registry is a better place for this kind of info,
    and with USB devices that can come and go, using integers is not
    very reliable for device identification. Under Windows, if
    PM_RECOMMENDED_OUTPUT_DEVICE (or PM_RECOMMENDED_INPUT_DEVICE) is
    *NOT* found in the environment, then the default device is obtained
    by looking for a string in the registry under:
        HKEY_LOCAL_MACHINE/SOFTWARE/PortMidi/Recommended_Input_Device
    and HKEY_LOCAL_MACHINE/SOFTWARE/PortMidi/Recommended_Output_Device
    for a string. The number of the first device with a substring that
    matches the string exactly is returned. For example, if the string
    in the registry is "USB", and device 1 is named
    "In USB MidiSport 1x1", then that will be the default
    input because it contains the string "USB".
    
    In addition to the name, get_device_info() returns "interf", which
    is the interface name. (The "interface" is the underlying software
    system or API used by PortMidi to access devices. Examples are
    MMSystem, DirectX (not implemented), ALSA, OSS (not implemented), etc.)
    At present, the only Win32 interface is "MMSystem", the only Linux
    interface is "ALSA", and the only Max OS X interface is "CoreMIDI".
    To specify both the interface and the device name in the registry,
    separate the two with a comma and a space, e.g.:
        MMSystem, In USB MidiSport 1x1
    In this case, the string before the comma must be a substring of
    the "interf" string, and the string after the space must be a
    substring of the "name" name string in order to match the device.
    
    Note: in the current release, the default is simply the first device
    (the input or output device with the lowest PmDeviceID).
    """
    return _pypm.GetDefaultInputDeviceID()




def get_default_output_id():
    """gets default output device number
    pyportmidi.get_default_output_id(): return default_id
    
    
    Return the default device ID or -1 if there are no devices.
    The result can be passed to the Input()/Ouput() class.
    
    On the PC, the user can specify a default device by
    setting an environment variable. For example, to use device #1.
    
        set PM_RECOMMENDED_OUTPUT_DEVICE=1
    
    The user should first determine the available device ID by using
    the supplied application "testin" or "testout".
    
    In general, the registry is a better place for this kind of info,
    and with USB devices that can come and go, using integers is not
    very reliable for device identification. Under Windows, if
    PM_RECOMMENDED_OUTPUT_DEVICE (or PM_RECOMMENDED_INPUT_DEVICE) is
    *NOT* found in the environment, then the default device is obtained
    by looking for a string in the registry under:
        HKEY_LOCAL_MACHINE/SOFTWARE/PortMidi/Recommended_Input_Device
    and HKEY_LOCAL_MACHINE/SOFTWARE/PortMidi/Recommended_Output_Device
    for a string. The number of the first device with a substring that
    matches the string exactly is returned. For example, if the string
    in the registry is "USB", and device 1 is named
    "In USB MidiSport 1x1", then that will be the default
    input because it contains the string "USB".
    
    In addition to the name, get_device_info() returns "interf", which
    is the interface name. (The "interface" is the underlying software
    system or API used by PortMidi to access devices. Examples are
    MMSystem, DirectX (not implemented), ALSA, OSS (not implemented), etc.)
    At present, the only Win32 interface is "MMSystem", the only Linux
    interface is "ALSA", and the only Max OS X interface is "CoreMIDI".
    To specify both the interface and the device name in the registry,
    separate the two with a comma and a space, e.g.:
        MMSystem, In USB MidiSport 1x1
    In this case, the string before the comma must be a substring of
    the "interf" string, and the string after the space must be a
    substring of the "name" name string in order to match the device.
    
    Note: in the current release, the default is simply the first device
    (the input or output device with the lowest PmDeviceID).
    """
    _check_init()
    return _pypm.GetDefaultOutputDeviceID()


def get_device_info(an_id):
    """ returns information about a midi device
    pyportmidi.get_device_info(an_id): return (interf, name, input, output, opened) 

    interf - a text string describing the device interface, eg 'ALSA'.
    name - a text string for the name of the device, eg 'Midi Through Port-0'
    input - 0, or 1 if the device is an input device.
    output - 0, or 1 if the device is an output device.
    opened - 0, or 1 if the device is opened.

    If the id is out of range, the function returns None.
    """
    _check_init()
    return _pypm.GetDeviceInfo(an_id) 


class Input(object):
    """Input is used to get midi input from midi devices.
    Input(device_id)
    Input(device_id, buffer_size)

    buffer_size -the number of input events to be buffered waiting to 
      be read using Input.read() 
    """

    def __init__(self, device_id, buffer_size=4096):
        """
        The buffer_size specifies the number of input events to be buffered 
        waiting to be read using Input.read().
        """
        _check_init()
 
        if device_id == -1:
            raise MidiException("Device id is -1, not a valid output id.  -1 usually means there were no default Output devices.")
            
        try:
            r = get_device_info(device_id)
        except TypeError:
            raise TypeError("an integer is required")
        except OverflowError:
            raise OverflowError("long int too large to convert to int")

        # and now some nasty looking error checking, to provide nice error 
        #   messages to the kind, lovely, midi using people of whereever.
        if r:
            interf, name, input, output, opened = r
            if input:
                try:
                    self._input = _pypm.Input(device_id, buffer_size)
                except TypeError:
                    raise TypeError("an integer is required")
                self.device_id = device_id

            elif output:
                raise MidiException("Device id given is not a valid input id, it is an output id.")
            else:
                raise MidiException("Device id given is not a valid input id.")
        else:
            raise MidiException("Device id invalid, out of range.")




    def _check_open(self):
        if self._input is None:
            raise MidiException("midi not open.")



    def close(self):
        """ closes a midi stream, flushing any pending buffers.
        Input.close(): return None

        PortMidi attempts to close open streams when the application
        exits -- this is particularly difficult under Windows.
        """
        _check_init()
        if not (self._input is None):
            self._input.Close()
        self._input = None



    def read(self, num_events):
        """reads num_events midi events from the buffer.
        Input.read(num_events): return midi_event_list

        Reads from the Input buffer and gives back midi events.
        [[[status,data1,data2,data3],timestamp],
         [[status,data1,data2,data3],timestamp],...]
        """
        _check_init()
        self._check_open()
        return self._input.Read(num_events)


    def poll(self):
        """returns true if there's data, or false if not.
        Input.poll(): return Bool

        raises a MidiException on error.
        """
        _check_init()
        self._check_open()

        r = self._input.Poll()
        if r == _pypm.TRUE:
            return True
        elif r == _pypm.FALSE:
            return False
        else:
            err_text = GetErrorText(r)
            raise MidiException( (r, err_text) )




class Output(object):
    """Output is used to send midi to an output device
    Output(device_id)
    Output(device_id, latency = 0)
    Output(device_id, buffer_size = 4096)
    Output(device_id, latency, buffer_size)

    The buffer_size specifies the number of output events to be 
    buffered waiting for output.  (In some cases -- see below -- 
    PortMidi does not buffer output at all and merely passes data 
    to a lower-level API, in which case buffersize is ignored.)

    latency is the delay in milliseconds applied to timestamps to determine
    when the output should actually occur. (If latency is < 0, 0 is 
    assumed.)

    If latency is zero, timestamps are ignored and all output is delivered
    immediately. If latency is greater than zero, output is delayed until
    the message timestamp plus the latency. (NOTE: time is measured 
    relative to the time source indicated by time_proc. Timestamps are 
    absolute, not relative delays or offsets.) In some cases, PortMidi 
    can obtain better timing than your application by passing timestamps 
    along to the device driver or hardware. Latency may also help you 
    to synchronize midi data to audio data by matching midi latency to 
    the audio buffer latency.

    """

    def __init__(self, device_id, latency = 0, buffer_size = 4096):
        """Output(device_id)
        Output(device_id, latency = 0)
        Output(device_id, buffer_size = 4096)
        Output(device_id, latency, buffer_size)

        The buffer_size specifies the number of output events to be 
        buffered waiting for output.  (In some cases -- see below -- 
        PortMidi does not buffer output at all and merely passes data 
        to a lower-level API, in which case buffersize is ignored.)

        latency is the delay in milliseconds applied to timestamps to determine
        when the output should actually occur. (If latency is < 0, 0 is 
        assumed.)

        If latency is zero, timestamps are ignored and all output is delivered
        immediately. If latency is greater than zero, output is delayed until
        the message timestamp plus the latency. (NOTE: time is measured 
        relative to the time source indicated by time_proc. Timestamps are 
        absolute, not relative delays or offsets.) In some cases, PortMidi 
        can obtain better timing than your application by passing timestamps 
        along to the device driver or hardware. Latency may also help you 
        to synchronize midi data to audio data by matching midi latency to 
        the audio buffer latency.
        """
     
        _check_init()
        self._aborted = 0

        if device_id == -1:
            raise MidiException("Device id is -1, not a valid output id.  -1 usually means there were no default Output devices.")
            
        try:
            r = get_device_info(device_id)
        except TypeError:
            raise TypeError("an integer is required")
        except OverflowError:
            raise OverflowError("long int too large to convert to int")

        # and now some nasty looking error checking, to provide nice error 
        #   messages to the kind, lovely, midi using people of whereever.
        if r:
            interf, name, input, output, opened = r
            if output:
                try:
                    self._output = _pypm.Output(device_id, latency)
                except TypeError:
                    raise TypeError("an integer is required")
                self.device_id = device_id

            elif input:
                raise MidiException("Device id given is not a valid output id, it is an input id.")
            else:
                raise MidiException("Device id given is not a valid output id.")
        else:
            raise MidiException("Device id invalid, out of range.")

    def _check_open(self):
        if self._output is None:
            raise MidiException("midi not open.")

        if self._aborted:
            raise MidiException("midi aborted.")


    def close(self):
        """ closes a midi stream, flushing any pending buffers.
        Output.close(): return None

        PortMidi attempts to close open streams when the application
        exits -- this is particularly difficult under Windows.
        """
        _check_init()
        if not (self._output is None):
            self._output.Close()
        self._output = None

    def abort(self):
        """terminates outgoing messages immediately
        Output.abort(): return None

        The caller should immediately close the output port;
        this call may result in transmission of a partial midi message.
        There is no abort for Midi input because the user can simply
        ignore messages in the buffer and close an input device at
        any time.
        """

        _check_init()
        if self._output:
            self._output.Abort()
        self._aborted = 1





    def write(self, data):
        """writes a list of midi data to the Output
        Output.write(data)

        writes series of MIDI information in the form of a list:
             write([[[status <,data1><,data2><,data3>],timestamp],
                    [[status <,data1><,data2><,data3>],timestamp],...])
        <data> fields are optional
        example: choose program change 1 at time 20000 and
        send note 65 with velocity 100 500 ms later.
             write([[[0xc0,0,0],20000],[[0x90,60,100],20500]])
        notes:
          1. timestamps will be ignored if latency = 0.
          2. To get a note to play immediately, send MIDI info with
             timestamp read from function Time.
          3. understanding optional data fields:
               write([[[0xc0,0,0],20000]]) is equivalent to
               write([[[0xc0],20000]])

        Can send up to 1024 elements in your data list, otherwise an 
         IndexError exception is raised.
        """
        _check_init()
        self._check_open()

        self._output.Write(data)


    def write_short(self, status, data1 = 0, data2 = 0):
        """write_short(status <, data1><, data2>)
        Output.write_short(status)
        Output.write_short(status, data1 = 0, data2 = 0)

        output MIDI information of 3 bytes or less.
        data fields are optional
        status byte could be:
             0xc0 = program change
             0x90 = note on
             etc.
             data bytes are optional and assumed 0 if omitted
        example: note 65 on with velocity 100
             write_short(0x90,65,100)
        """
        _check_init()
        self._check_open()
        self._output.WriteShort(status, data1, data2)


    def write_sys_ex(self, when, msg):
        """writes a timestamped system-exclusive midi message.
        Output.write_sys_ex(when, msg)

        msg - can be a *list* or a *string*
        when - a timestamp in miliseconds
        example:
          (assuming o is an onput MIDI stream)
            o.write_sys_ex(0,'\\xF0\\x7D\\x10\\x11\\x12\\x13\\xF7')
          is equivalent to
            o.write_sys_ex(pyportmidi.time(),
                           [0xF0,0x7D,0x10,0x11,0x12,0x13,0xF7])
        """
        _check_init()
        self._check_open()
        self._output.WriteSysEx(when, msg)


    def note_on(self, note, velocity=None, channel = 0):
        """turns a midi note on.  Note must be off.
        Output.note_on(note, velocity=None, channel = 0)

        Turn a note on in the output stream.  The note must already
        be off for this to work correctly.
        """
        if velocity is None:
            velocity = 0

        if not (0 <= channel <= 15):
            raise ValueError("Channel not between 0 and 15.")

        self.write_short(0x90+channel, note, velocity)

    def note_off(self, note, velocity=None, channel = 0):
        """turns a midi note off.  Note must be on.
        Output.note_off(note, velocity=None, channel = 0)

        Turn a note off in the output stream.  The note must already
        be on for this to work correctly.
        """
        if velocity is None:
            velocity = 0

        if not (0 <= channel <= 15):
            raise ValueError("Channel not between 0 and 15.")

        self.write_short(0x80 + channel, note, velocity)


    def set_instrument(self, instrument_id, channel = 0):
        """select an instrument, with a value between 0 and 127
        Output.set_instrument(instrument_id, channel = 0)

        """
        if not (0 <= instrument_id <= 127):
            raise ValueError("Undefined instrument id: %d" % instrument_id)

        if not (0 <= channel <= 15):
            raise ValueError("Channel not between 0 and 15.")

        self.write_short(0xc0+channel, instrument_id)



def time():
    """returns the current time in ms of the PortMidi timer
    pyportmidi.time(): return time

    The time is reset to 0, when the module is inited.
    """
    return _pypm.Time()








class MidiException(Exception):
    """MidiException(errno) that can be raised.
    """
    def __init__(self, value):
        self.parameter = value
    def __str__(self):
        return repr(self.parameter)



