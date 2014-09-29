# pyPortMidi
# Python bindings for PortMidi
# John Harrison
# http://sound.media.mit.edu/~harrison
# harrison@media.mit.edu
# written in Pyrex
__version__="0.07"

import array

# CHANGES:

# 0.0.5: (June 1st, 2009)
#   Output no longer calls abort when it deallocates.
#   Added abort and close methods.
#   Need to call Abort() explicityly if you want that to happen.


#
# 0.0.3: (March 15, 2005)
#   changed everything from tuples to lists
#   return 4 values for PmRead instead of 3 (for SysEx)
#   minor fixes for flexibility and error checking
#   flushed out DistUtils package and added Mac and Linux compile support
#   Markus Pfaff: added ability for WriteSysEx to accept lists as well
#                 as strings

# 0.0.2:
#   fixed pointer to function calls to avoid necessity of pyport library

# 0.0.1:
#   initial release

cdef extern from "portmidi.h":
    ctypedef enum PmError:
        pmNoError = 0,
        pmHostError = -10000,
        pmInvalidDeviceId, #/* out of range or output device when input is requested or vice versa */
        pmInsufficientMemory,
        pmBufferTooSmall,
        pmBufferOverflow,
        pmBadPtr,
        pmBadData, #/* illegal midi data, e.g. missing EOX */
        pmInternalError,
        pmBufferMaxSize, #/* buffer is already as large as it can be */
    PmError Pm_Initialize()
    PmError Pm_Terminate()
    ctypedef void PortMidiStream
    ctypedef PortMidiStream PmStream # CHECK THIS!
    ctypedef int PmDeviceID
    int Pm_HasHostError( PortMidiStream * stream )  
    char *Pm_GetErrorText( PmError errnum )
    Pm_GetHostErrorText(char * msg, unsigned int len)
    ctypedef struct PmDeviceInfo:
        int structVersion
        char *interf #/* underlying MIDI API, e.g. MMSystem or DirectX */
        char *name   #/* device name, e.g. USB MidiSport 1x1 */
        int input    #/* true iff input is available */
        int output   #/* true iff output is available */
        int opened   #/* used by generic PortMidi code to do error checking on arguments */
    int Pm_CountDevices()
    PmDeviceID Pm_GetDefaultInputDeviceID()
    PmDeviceID Pm_GetDefaultOutputDeviceID()
    ctypedef long PmTimestamp
    ctypedef PmTimestamp (*PmTimeProcPtr)(void *time_info)
    #PmBefore is not defined...
    PmDeviceInfo* Pm_GetDeviceInfo( PmDeviceID id )
    PmError Pm_OpenInput( PortMidiStream** stream,
                          PmDeviceID inputDevice,
                          void *inputDriverInfo,
                          long bufferSize,
                          long (*PmPtr) (), # long = PtTimestamp
                          void *time_info )
    PmError Pm_OpenOutput( PortMidiStream** stream,
                           PmDeviceID outputDevice,
                           void *outputDriverInfo,
                           long bufferSize,
                           #long (*PmPtr) (), # long = PtTimestamp
                           PmTimeProcPtr time_proc, # long = PtTimestamp
                           void *time_info,
                           long latency )
    PmError Pm_SetFilter( PortMidiStream* stream, long filters )
    PmError Pm_Abort( PortMidiStream* stream )
    PmError Pm_Close( PortMidiStream* stream )
    ctypedef long PmMessage
    ctypedef struct PmEvent:
        PmMessage message
        PmTimestamp timestamp
    PmError Pm_Read( PortMidiStream *stream, PmEvent *buffer, long length )
    PmError Pm_Poll( PortMidiStream *stream)
    int Pm_Channel(int channel)
    PmError Pm_SetChannelMask(PortMidiStream *stream, int mask)
    PmError Pm_Write( PortMidiStream *stream, PmEvent *buffer, long length )
    PmError Pm_WriteSysEx( PortMidiStream *stream, PmTimestamp when, unsigned char *msg)

cdef extern from "porttime.h":
    ctypedef enum PtError:
        ptNoError = 0,
        ptHostError = -10000,
        ptAlreadyStarted,
        ptAlreadyStopped,
        ptInsufficientMemory
    ctypedef long PtTimestamp
    ctypedef void (* PtCallback)( PtTimestamp timestamp, void *userData )
    PtError Pt_Start(int resolution, PtCallback *callback, void *userData)
    PtTimestamp Pt_Time()

FILT_ACTIVE=0x1
FILT_SYSEX=0x2
FILT_CLOCK=0x4
FILT_PLAY=0x8
FILT_F9=0x10
FILT_TICK=0x10
FILT_FD=0x20
FILT_UNDEFINED=0x30
FILT_RESET=0x40
FILT_REALTIME=0x7F
FILT_NOTE=0x80
FILT_CHANNEL_AFTERTOUCH=0x100
FILT_POLY_AFTERTOUCH=0x200
FILT_AFTERTOUCH=0x300
FILT_PROGRAM=0x400
FILT_CONTROL=0x800
FILT_PITCHBEND=0x1000
FILT_MTC=0x2000
FILT_SONG_POSITION=0x4000
FILT_SONG_SELECT=0x8000
FILT_TUNE=0x10000
FALSE=0
TRUE=1

def Initialize():
    """
Initialize: call this first
    """
    Pm_Initialize()
    Pt_Start(1, NULL, NULL) # /* equiv to TIME_START: start timer w/ ms accuracy */

def Terminate():
    """
Terminate: call this to clean up Midi streams when done.
If you do not call this on Windows machines when you are
done with MIDI, your system may crash.
    """
    Pm_Terminate()

def GetDefaultInputDeviceID():
    return Pm_GetDefaultInputDeviceID()

def GetDefaultOutputDeviceID():
    return Pm_GetDefaultOutputDeviceID()

def CountDevices():
    return Pm_CountDevices()

def GetDeviceInfo(i):
    """
GetDeviceInfo(<device number>): returns 5 parameters
  - underlying MIDI API
  - device name
  - TRUE iff input is available
  - TRUE iff output is available
  - TRUE iff device stream is already open
    """
    cdef PmDeviceInfo *info

    # disregarding the constness from Pm_GetDeviceInfo, since pyrex doesn't do const.
    info = <PmDeviceInfo *>Pm_GetDeviceInfo(i)

    if info <> NULL: return info.interf, info.name, info.input, info.output, info.opened
    else: return 

def Time():
    """
Time() returns the current time in ms
of the PortMidi timer
    """
    return Pt_Time()

def GetErrorText(err):
    """
GetErrorText(<err num>) returns human-readable error
messages translated from error numbers
    """
    return Pm_GetErrorText(err)

def Channel(chan):
    """
Channel(<chan>) is used with ChannelMask on input MIDI streams.
Example: to receive input on channels 1 and 10 on a MIDI
         stream called MidiIn:
MidiIn.SetChannelMask(pypm.Channel(1) | pypm.Channel(10))

note: PyPortMidi Channel function has been altered from
      the original PortMidi c call to correct for what
      seems to be a bug --- i.e. channel filters were
      all numbered from 0 to 15 instead of 1 to 16.
    """
    return Pm_Channel(chan-1)

cdef class Output:
    """
class Output:
    define an output MIDI stream. Takes the form:
        x = pypm.Output(MidiOutputDevice, latency)
    latency is in ms.
    If latency = 0 then timestamps for output are ignored.
    """
    cdef int i
    cdef PmStream *midi
    cdef int debug
    cdef int _aborted

    def __init__(self, OutputDevice, latency=0):
        
        cdef PmError err
        #cdef PtTimestamp (*PmPtr) ()
        cdef PmTimeProcPtr PmPtr

        self.i = OutputDevice
        self.debug = 0
        self._aborted = 0
        
        if latency == 0:
            PmPtr = NULL
        else:
            PmPtr = <PmTimeProcPtr>&Pt_Time
        if self.debug: print "Opening Midi Output"
	# Why is bufferSize 0 here?
        err = Pm_OpenOutput(&(self.midi), self.i, NULL, 0, PmPtr, NULL, latency)
        if err < 0:
                s = Pm_GetErrorText(err)
                # Something's amiss here - if we try to throw an Exception
               	# here, we crash.
                if not err == -10000:
                        raise Exception,s
                else:
                        print "Unable to open Midi OutputDevice=",OutputDevice," err=",s

    def __dealloc__(self):
        if self.debug: print "Closing MIDI output stream and destroying instance"
        #err = Pm_Abort(self.midi)
        #if err < 0: raise Exception, Pm_GetErrorText(err)
        err = Pm_Close(self.midi)
        if err < 0: raise Exception, Pm_GetErrorText(err) 


    def _check_open(self):
        """ checks to see if the midi is open, and if not, raises an error.
        """

        if self.midi == NULL:
            raise Exception, "midi Output not open."

        if self._aborted:
            raise Exception, "midi Output aborted.  Need to call Close after Abort."

    def Close(self):
        """
Close()
    closes a midi stream, flushing any pending buffers.
    (PortMidi attempts to close open streams when the application
    exits -- this is particularly difficult under Windows.)
        """
        #if not self.midi:
        #    return

        err = Pm_Close(self.midi)
        if err < 0:
            raise Exception, Pm_GetErrorText(err)
        #self.midi = NULL


    def Abort(self):
        """
Abort() terminates outgoing messages immediately
    The caller should immediately close the output port;
    this call may result in transmission of a partial midi message.
    There is no abort for Midi input because the user can simply
    ignore messages in the buffer and close an input device at
    any time.
        """
        #if not self.midi:
        #    return

        err = Pm_Abort(self.midi)
        if err < 0:
            raise Exception, Pm_GetErrorText(err)

        self._aborted = 1


    def Write(self, data):
        """
Write(data)
    output a series of MIDI information in the form of a list:
         Write([[[status <,data1><,data2><,data3>],timestamp],
                [[status <,data1><,data2><,data3>],timestamp],...])
    <data> fields are optional
    example: choose program change 1 at time 20000 and
    send note 65 with velocity 100 500 ms later.
         Write([[[0xc0,0,0],20000],[[0x90,60,100],20500]])
    notes:
      1. timestamps will be ignored if latency = 0.
      2. To get a note to play immediately, send MIDI info with
         timestamp read from function Time.
      3. understanding optional data fields:
           Write([[[0xc0,0,0],20000]]) is equivalent to
           Write([[[0xc0],20000]])
        """
        cdef PmEvent buffer[1024]
        cdef PmError err
        cdef int i

        self._check_open()


        if len(data) > 1024: raise IndexError, 'maximum list length is 1024'
        else:
            for loop1 in range(len(data)):
                if ((len(data[loop1][0]) > 4) |
                    (len(data[loop1][0]) < 1)):
                    raise IndexError, str(len(data[loop1][0]))+' arguments in event list'
                buffer[loop1].message = 0
                for i in range(len(data[loop1][0])):
                    buffer[loop1].message = buffer[loop1].message + ((data[loop1][0][i]&0xFF) << (8*i))
                buffer[loop1].timestamp = data[loop1][1]
                if self.debug: print loop1," : ",buffer[loop1].message," : ",buffer[loop1].timestamp
        if self.debug: print "writing to midi buffer"
        err= Pm_Write(self.midi, buffer, len(data))
        if err < 0: raise Exception, Pm_GetErrorText(err)
        
    def WriteShort(self, status, data1 = 0, data2 = 0):
        """
WriteShort(status <, data1><, data2>)
     output MIDI information of 3 bytes or less.
     data fields are optional
     status byte could be:
          0xc0 = program change
          0x90 = note on
          etc.
          data bytes are optional and assumed 0 if omitted
     example: note 65 on with velocity 100
          WriteShort(0x90,65,100)
        """
        cdef PmEvent buffer[1]
        cdef PmError err
        self._check_open()

        buffer[0].timestamp = Pt_Time()
        buffer[0].message = ((((data2) << 16) & 0xFF0000) | (((data1) << 8) & 0xFF00) | ((status) & 0xFF))
        if self.debug: print "Writing to MIDI buffer"
        err = Pm_Write(self.midi, buffer, 1) # stream, buffer, length
        if err < 0 : raise Exception, Pm_GetErrorText(err)

    def WriteSysEx(self, when, msg):
        """
        WriteSysEx(<timestamp>,<msg>)
        writes a timestamped system-exclusive midi message.
        <msg> can be a *list* or a *string*
        example:
            (assuming y is an input MIDI stream)
            y.WriteSysEx(0,'\\xF0\\x7D\\x10\\x11\\x12\\x13\\xF7')
                              is equivalent to
            y.WriteSysEx(pypm.Time,
            [0xF0, 0x7D, 0x10, 0x11, 0x12, 0x13, 0xF7])
        """
        cdef PmError err
        cdef char *cmsg
        cdef PtTimestamp CurTime

        self._check_open()

        if type(msg) is list:
            msg = array.array('B',msg).tostring() # Markus Pfaff contribution
        cmsg = msg

        CurTime = Pt_Time()
        err = Pm_WriteSysEx(self.midi, when, <unsigned char *> cmsg)
        if err < 0 : raise Exception, Pm_GetErrorText(err)
        while Pt_Time() == CurTime: # wait for SysEx to go thru or...my
            pass                    # win32 machine crashes w/ multiple SysEx











cdef class Input:
    """
class Input:
    define an input MIDI stream. Takes the form:
        x = pypm.Input(MidiInputDevice)
    """
    cdef PmStream *midi
    cdef int debug
    cdef int i

    def __init__(self, InputDevice, buffersize=4096):
        cdef PmError err
        self.i = InputDevice
        self.debug = 0
        err= Pm_OpenInput(&(self.midi),self.i,NULL,buffersize,&Pt_Time,NULL)
        if err < 0: raise Exception, Pm_GetErrorText(err)
        if self.debug: print "MIDI input opened."

    def __dealloc__(self):
        cdef PmError err
        if self.debug: print "Closing MIDI input stream and destroying instance"

        err = Pm_Close(self.midi)
        if err < 0:
            raise Exception, Pm_GetErrorText(err)



    def _check_open(self):
        """ checks to see if the midi is open, and if not, raises an error.
        """

        if self.midi == NULL:
            raise Exception, "midi Input not open."


    def Close(self):
        """
Close()
    closes a midi stream, flushing any pending buffers.
    (PortMidi attempts to close open streams when the application
    exits -- this is particularly difficult under Windows.)
        """
        #if not self.midi:
        #    return

        err = Pm_Close(self.midi)
        if err < 0:
            raise Exception, Pm_GetErrorText(err)
        #self.midi = NULL



    def SetFilter(self, filters):
        """
    SetFilter(<filters>) sets filters on an open input stream
    to drop selected input types. By default, only active sensing
    messages are filtered. To prohibit, say, active sensing and
    sysex messages, call
    SetFilter(stream, FILT_ACTIVE | FILT_SYSEX);

    Filtering is useful when midi routing or midi thru functionality
    is being provided by the user application.
    For example, you may want to exclude timing messages
    (clock, MTC, start/stop/continue), while allowing note-related
    messages to pass. Or you may be using a sequencer or drum-machine
    for MIDI clock information but want to exclude any notes
    it may play.

    Note: SetFilter empties the buffer after setting the filter,
    just in case anything got through.
        """
        cdef PmEvent buffer[1]
        cdef PmError err

        self._check_open()


        err = Pm_SetFilter(self.midi, filters)

        if err < 0: raise Exception, Pm_GetErrorText(err)

        while(Pm_Poll(self.midi) != pmNoError):

            err = Pm_Read(self.midi,buffer,1)
            if err < 0: raise Exception, Pm_GetErrorText(err)

    def SetChannelMask(self, mask):
        """
    SetChannelMask(<mask>) filters incoming messages based on channel.
    The mask is a 16-bit bitfield corresponding to appropriate channels
    Channel(<channel>) can assist in calling this function.
    i.e. to set receive only input on channel 1, call with
    SetChannelMask(Channel(1))
    Multiple channels should be OR'd together, like
    SetChannelMask(Channel(10) | Channel(11))
    note: PyPortMidi Channel function has been altered from
          the original PortMidi c call to correct for what
          seems to be a bug --- i.e. channel filters were
          all numbered from 0 to 15 instead of 1 to 16.
        """
        cdef PmError err

        self._check_open()

        err = Pm_SetChannelMask(self.midi,mask)
        if err < 0: raise Exception, Pm_GetErrorText(err)
        
    def Poll(self):
        """
    Poll tests whether input is available,
    returning TRUE, FALSE, or an error value.
        """
        cdef PmError err
        self._check_open()

        err = Pm_Poll(self.midi)
        if err < 0: raise Exception, Pm_GetErrorText(err)
        return err
    
    def Read(self,length):
        """
Read(length): returns up to <length> midi events stored in
the buffer and returns them as a list:
[[[status,data1,data2,data3],timestamp],
 [[status,data1,data2,data3],timestamp],...]
example: Read(50) returns all the events in the buffer,
         up to 50 events.
        """
        cdef PmEvent buffer[1024]
        
        self._check_open()

        x = []
        
        if length > 1024: raise IndexError, 'maximum buffer length is 1024'
        if length < 1: raise IndexError, 'minimum buffer length is 1'
        NumEvents = Pm_Read(self.midi,buffer,length)
        if NumEvents < 0: raise Exception, Pm_GetErrorText(NumEvents)
        x=[]
        if NumEvents >= 1:
            for loop in range(NumEvents):
                 x.append([[buffer[loop].message & 0xff, (buffer[loop].message >> 8) & 0xFF, (buffer[loop].message >> 16) & 0xFF, (buffer[loop].message >> 24) & 0xFF], buffer[loop].timestamp])
        return x
