package jportmidi;

public class JPortMidiApi {
    public static class PortMidiStream {
        private long address;
    }
    public static class PmEvent {
        public int message;
        public int timestamp;
    }

    // PmError bindings
    public final int pmNoError = 0;
    public final int pmNoData = 0;
    public final int pmGotData = -1;
    public final int pmHostError = -10000;
    public final int pmInvalidDeviceId = -9999;
    public final int pmInsufficientMemory = -9998;
    public final int pmBufferTooSmall = -9997;
    public final int pmBufferOverflow = -9996;
    public final int pmBadPtr = -9995;
    public final int pmBadData = -9994;
    public final int pmInternalError = -9993;
    public final int pmBufferMaxSize = -9992;
    
    static public native int Pm_Initialize();
    static public native int Pm_Terminate();
    static public native int Pm_HasHostError(PortMidiStream stream);
    static public native String Pm_GetErrorText(int errnum);
    static public native String Pm_GetHostErrorText();
    final int pmNoDevice = -1;
    static public native int Pm_CountDevices();
    static public native int Pm_GetDefaultInputDeviceID();
    static public native int Pm_GetDefaultOutputDeviceID();
    static public native String Pm_GetDeviceInterf(int i);
    static public native String Pm_GetDeviceName(int i);
    static public native boolean Pm_GetDeviceInput(int i);
    static public native boolean Pm_GetDeviceOutput(int i);
    static public native int Pm_OpenInput(PortMidiStream stream,
                                          int inputDevice, 
                                          String inputDriverInfo, 
                                          int bufferSize);
    static public native int Pm_OpenOutput(PortMidiStream stream,
                                           int outputDevice, 
                                           String outnputDriverInfo, 
                                           int bufferSize,
                                           int latency);
    final static public int PM_FILT_ACTIVE = (1 << 0x0E);
    final static public int PM_FILT_SYSEX = (1 << 0x00);
    final static public int PM_FILT_CLOCK = (1 << 0x08);
    final static public int PM_FILT_PLAY = 
            (1 << 0x0A) | (1 << 0x0C) | (1 << 0x0B);
    final static public int PM_FILT_TICK = (1 << 0x09);
    final static public int PM_FILT_FD = (1 << 0x0D);
    final static public int PM_FILT_UNDEFINED = PM_FILT_FD;
    final static public int PM_FILT_RESET = (1 << 0x0F);
    final static public int PM_FILT_REALTIME =
            PM_FILT_ACTIVE | PM_FILT_SYSEX | PM_FILT_CLOCK;
    final static public int PM_FILT_NOTE = (1 << 0x19) | (1 << 0x18);
    final static public int PM_FILT_CHANNEL_AFTERTOUCH = (1 << 0x1D);
    final static public int PM_FILT_POLY_AFTERTOUCH = (1 << 0x1A);
    final static public int PM_FILT_AFTERTOUCH = 
            (PM_FILT_CHANNEL_AFTERTOUCH | PM_FILT_POLY_AFTERTOUCH);
    final static public int PM_FILT_PROGRAM = (1 << 0x1C);
    final static public int PM_FILT_CONTROL = (1 << 0x1B);
    final static public int PM_FILT_PITCHBEND = (1 << 0x1E);
    final static public int PM_FILT_MTC = (1 << 0x01);
    final static public int PM_FILT_SONG_POSITION = (1 << 0x02);
    final static public int PM_FILT_SONG_SELECT = (1 << 0x03);
    final static public int PM_FILT_TUNE = (1 << 0x06);
    final static public int PM_FILT_SYSTEMCOMMON =
        (PM_FILT_MTC | PM_FILT_SONG_POSITION | 
         PM_FILT_SONG_SELECT | PM_FILT_TUNE);
    static public native int Pm_SetFilter(PortMidiStream stream, int filters);
    static public int Pm_Channel(int channel) { return 1 << channel; }
    final static public native int Pm_SetChannelMask(PortMidiStream stream, 
                                                     int mask);
    final static public native int Pm_Abort(PortMidiStream stream);
    final static public native int Pm_Close(PortMidiStream stream);
    static public int Pm_Message(int status, int data1, int data2) {
        return (((data2 << 16) & 0xFF0000) |
                ((data1 << 8) & 0xFF00) |
                (status & 0xFF));
    }
    static public int Pm_MessageStatus(int msg) {
        return msg & 0xFF;
    }
    static public int Pm_MessageData1(int msg) {
        return (msg >> 8) & 0xFF;
    }
    static public int Pm_MessageData2(int msg) {
        return (msg >> 16) & 0xFF;
    }
    // only supports reading one buffer at a time
    static public native int Pm_Read(PortMidiStream stream, PmEvent buffer);
    static public native int Pm_Poll(PortMidiStream stream);
    // only supports writing one buffer at a time
    static public native int Pm_Write(PortMidiStream stream, PmEvent buffer);
    static public native int Pm_WriteShort(PortMidiStream stream, 
                                           int when, int msg);
    static public native int Pm_WriteSysEx(PortMidiStream stream, 
                                           int when, byte msg[]);
    
    public final int ptNoError = 0;
    public final int ptAlreadyStarted = -10000;
    public final int ptAlreadyStopped = -9999;
    public final int PtInsufficientMemory = -9998;
    static public native int Pt_TimeStart(int resolution);
    static public native int Pt_TimeStop();
    static public native int Pt_Time();
    static public native boolean Pt_TimeStarted();
    static {
        System.out.println("Loading pmjni");
        System.loadLibrary("pmjni");
        System.out.println("done loading pmjni");
    }
}
