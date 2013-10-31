package jportmidi;

/* PortMidi is a general class intended for any Java program using
   the PortMidi library. It encapsulates JPortMidiApi with a more
   object-oriented interface. A single PortMidi object can manage
   up to one input stream and one output stream.

   This class is not safely callable from multiple threads. It
   is the client's responsibility to periodically call the Poll
   method which checks for midi input and handles it.
*/

import jportmidi.*;
import jportmidi.JPortMidiApi.*;

public class JPortMidi {

    // timecode to send message immediately
    public final int NOW = 0;

    // midi codes
    public final int MIDI_NOTE_OFF = 0x80;
    public final int MIDI_NOTE_ON = 0x90;
    public final int CTRL_ALL_OFF = 123;
    public final int MIDI_PITCH_BEND = 0xE0;
    public final int MIDI_CLOCK = 0xF8;
    public final int MIDI_CONTROL = 0xB0;
    public final int MIDI_PROGRAM = 0xC0;
    public final int MIDI_START = 0xFA;
    public final int MIDI_STOP = 0xFC;
    public final int MIDI_POLY_TOUCH = 0xA0;
    public final int MIDI_TOUCH = 0xD0;

    // error code -- cannot refresh device list while stream is open:
    public final int pmStreamOpen = -5000;
    public final int pmOutputNotOpen = -4999;

    // access to JPortMidiApi is through a single, global instance
    private static JPortMidiApi pm;
    // a reference count tracks how many objects have it open
    private static int pmRefCount = 0;
    private static int openCount = 0;

    public int error; // user can check here for error codes
    private PortMidiStream input;
    private PortMidiStream output;
    private PmEvent buffer;
    protected int timestamp; // remember timestamp from incoming messages
    protected boolean trace = false; // used to print midi msgs for debugging
    

    public JPortMidi() throws JPortMidiException {
        if (pmRefCount == 0) {
            pm = new JPortMidiApi();
            pmRefCount++;
            System.out.println("Calling Pm_Initialize");
            checkError(pm.Pm_Initialize());
            System.out.println("Called Pm_Initialize");
        }
        buffer = new PmEvent();
    }
    
    public boolean getTrace() { return trace; }

    // set the trace flag and return previous value
    public boolean setTrace(boolean flag) {
        boolean previous = trace;
        trace = flag;
        return previous;
    }

    // WARNING: you must not call this if any devices are open
    public void refreshDeviceLists()
            throws JPortMidiException 
    {
        if (openCount > 0) {
            throw new JPortMidiException(pmStreamOpen,
                    "RefreshDeviceLists called while stream is open");
        }
        checkError(pm.Pm_Terminate());
        checkError(pm.Pm_Initialize());
    }

    // there is no control over when/whether this is called, but it seems
    // to be a good idea to close things when this object is collected
    public void finalize() {
        if (input != null) {
            error = pm.Pm_Close(input);
        }
        if (input != null) {
            int rslt = pm.Pm_Close(output);
            // we may lose an error code from closing output, but don't
            // lose any real error from closing input...
            if (error == pm.pmNoError) error = rslt;
        }
        pmRefCount--;
        if (pmRefCount == 0) {
            error = pm.Pm_Terminate();
        }
    }

    int checkError(int err) throws JPortMidiException
    {
        // note that Pm_Read and Pm_Write return positive result values 
        // which are not errors, so compare with >=
        if (err >= pm.pmNoError) return err;
        if (err == pm.pmHostError) {
            throw new JPortMidiException(err, pm.Pm_GetHostErrorText());
        } else {
            throw new JPortMidiException(err, pm.Pm_GetErrorText(err));
        }
    }

    // ******** ACCESS TO TIME ***********
    
    public void timeStart(int resolution) throws JPortMidiException {
        checkError(pm.Pt_TimeStart(resolution));
    }
        
    public void timeStop() throws JPortMidiException {
        checkError(pm.Pt_TimeStop());
    }
        
    public int timeGet() {
        return pm.Pt_Time();
    }
        
    public boolean timeStarted() {
        return pm.Pt_TimeStarted();
    }

    // ******* QUERY DEVICE INFORMATION *********
        
    public int countDevices() throws JPortMidiException {
        return checkError(pm.Pm_CountDevices());
    }

    public int getDefaultInputDeviceID()  throws JPortMidiException {
        return checkError(pm.Pm_GetDefaultInputDeviceID());
    }

    public int getDefaultOutputDeviceID() throws JPortMidiException {
        return checkError(pm.Pm_GetDefaultOutputDeviceID());
    }

    public String getDeviceInterf(int i) {
        return pm.Pm_GetDeviceInterf(i);
    }

    public String getDeviceName(int i) {
        return pm.Pm_GetDeviceName(i);
    }

    public boolean getDeviceInput(int i) {
        return pm.Pm_GetDeviceInput(i);
    }

    public boolean getDeviceOutput(int i) {
        return pm.Pm_GetDeviceOutput(i);
    }

    // ********** MIDI INTERFACE ************

    public boolean isOpenInput() {
        return input != null;
    }

    public void openInput(int inputDevice, int bufferSize) 
            throws JPortMidiException 
    {
        openInput(inputDevice, "", bufferSize);
    }

    public void openInput(int inputDevice, String inputDriverInfo, int bufferSize) 
            throws JPortMidiException
    {
        if (isOpenInput()) pm.Pm_Close(input);
        else input = new PortMidiStream();
        if (trace) {
            System.out.println("openInput " + getDeviceName(inputDevice));
        }
        checkError(pm.Pm_OpenInput(input, inputDevice, 
                                   inputDriverInfo, bufferSize));
        // if no exception, then increase count of open streams
        openCount++;
    }

    public boolean isOpenOutput() {
        return output != null;
    }

    public void openOutput(int outputDevice, int bufferSize, int latency)
            throws JPortMidiException 
    {
        openOutput(outputDevice, "", bufferSize, latency);
    }

    public void openOutput(int outputDevice, String outputDriverInfo,
            int bufferSize, int latency) throws JPortMidiException {
        if (isOpenOutput()) pm.Pm_Close(output);
        else output = new PortMidiStream();
        if (trace) {
            System.out.println("openOutput " + getDeviceName(outputDevice));
        }
        checkError(pm.Pm_OpenOutput(output, outputDevice, outputDriverInfo, 
                                    bufferSize, latency));
        // if no exception, then increase count of open streams
        openCount++;
    }

    public void setFilter(int filters) throws JPortMidiException {
        if (input == null) return; // no effect if input not open
        checkError(pm.Pm_SetFilter(input, filters));
    }

    public void setChannelMask(int mask) throws JPortMidiException {
        if (input == null) return; // no effect if input not open
        checkError(pm.Pm_SetChannelMask(input, mask));
    }

    public void abort() throws JPortMidiException {
        if (output == null) return; // no effect if output not open
        checkError(pm.Pm_Abort(output));
    }

    // In keeping with the idea that this class represents an input and output,
    // there are separate Close methods for input and output streams, avoiding
    // the need for clients to ever deal directly with a stream object
    public void closeInput() throws JPortMidiException {
        if (input == null) return; // no effect if input not open
        checkError(pm.Pm_Close(input));
        input = null;
        openCount--;
    }

    public void closeOutput() throws JPortMidiException {
        if (output == null) return; // no effect if output not open
        checkError(pm.Pm_Close(output));
        output = null;
        openCount--;
    }

    // Poll should be called by client to process input messages (if any)
    public void poll() throws JPortMidiException {
        if (input == null) return; // does nothing until input is opened
        while (true) {
            int rslt = pm.Pm_Read(input, buffer);
            checkError(rslt);
            if (rslt == 0) return; // no more messages
            handleMidiIn(buffer);
        }
    }    

    public void writeShort(int when, int msg) throws JPortMidiException {
        if (output == null) 
            throw new JPortMidiException(pmOutputNotOpen, 
                                         "Output stream not open");
        if (trace) {
            System.out.println("writeShort: " + Integer.toHexString(msg));
        }
        checkError(pm.Pm_WriteShort(output, when, msg));
    }

    public void writeSysEx(int when, byte msg[]) throws JPortMidiException {
        if (output == null) 
            throw new JPortMidiException(pmOutputNotOpen, 
                                         "Output stream not open");
        if (trace) {
            System.out.print("writeSysEx: ");
            for (int i = 0; i < msg.length; i++) {
                System.out.print(Integer.toHexString(msg[i]));
            }
            System.out.print("\n");
        }
        checkError(pm.Pm_WriteSysEx(output, when, msg));
    }
    
    public int midiChanMessage(int chan, int status, int data1, int data2) {
        return (((data2 << 16) & 0xFF0000) |
                ((data1 << 8) & 0xFF00) |
                (status & 0xF0) |
                (chan & 0xF));
    }

    public int midiMessage(int status, int data1, int data2) {
        return ((((data2) << 16) & 0xFF0000) |
                (((data1) << 8) & 0xFF00) |
                ((status) & 0xFF));
    }

    public void midiAllOff(int channel) throws JPortMidiException {
        midiAllOff(channel, NOW); 
    }

    public void midiAllOff(int chan, int when) throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_CONTROL, CTRL_ALL_OFF, 0));
    }
        
    public void midiPitchBend(int chan, int value) throws JPortMidiException {
        midiPitchBend(chan, value, NOW);
    }

    public void midiPitchBend(int chan, int value, int when)
            throws JPortMidiException {
        writeShort(when, 
                   midiChanMessage(chan, MIDI_PITCH_BEND, value, value >> 7));
    }

    public void midiClock() throws JPortMidiException {
        midiClock(NOW);
    }

    public void midiClock(int when) throws JPortMidiException {
        writeShort(when, midiMessage(MIDI_CLOCK, 0, 0));
    }

    public void midiControl(int chan, int control, int value)
            throws JPortMidiException {
        midiControl(chan, control, value, NOW);
    }

    public void midiControl(int chan, int control, int value, int when)
            throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_CONTROL, control, value));
    }

    public void midiNote(int chan, int pitch, int vel)
            throws JPortMidiException {
        midiNote(chan, pitch, vel, NOW);
    }

    public void midiNote(int chan, int pitch, int vel, int when)
            throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_NOTE_ON, pitch, vel));
    }

    public void midiProgram(int chan, int program)
            throws JPortMidiException {
        midiProgram(chan, program, NOW);
    }

    public void midiProgram(int chan, int program, int when)
            throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_PROGRAM, program, 0));
    }

    public void midiStart()
            throws JPortMidiException {
        midiStart(NOW);
    }

    public void midiStart(int when)
            throws JPortMidiException {
        writeShort(when, midiMessage(MIDI_START, 0, 0));
    }

    public void midiStop()
            throws JPortMidiException {
        midiStop(NOW);
    }

    public void midiStop(int when)
            throws JPortMidiException {
        writeShort(when, midiMessage(MIDI_STOP, 0, 0));
    }

    public void midiPolyTouch(int chan, int key, int value)
            throws JPortMidiException {
        midiPolyTouch(chan, key, value, NOW);
    }

    public void midiPolyTouch(int chan, int key, int value, int when)
            throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_POLY_TOUCH, key, value));
    }

    public void midiTouch(int chan, int value)
            throws JPortMidiException {
        midiTouch(chan, value, NOW);
    }

    public void midiTouch(int chan, int value, int when)
            throws JPortMidiException {
        writeShort(when, midiChanMessage(chan, MIDI_TOUCH, value, 0));
    }

    // ****** now we implement the message handlers ******

    // an array for incoming sysex messages that can grow. 
    // The downside is that after getting a message, we 

    private byte sysexBuffer[] = null;
    private int sysexBufferIndex = 0;

    void sysexBufferReset() {
        sysexBufferIndex = 0;
        if (sysexBuffer == null) sysexBuffer = new byte[256];
    }

    void sysexBufferCheck() {
        if (sysexBuffer.length < sysexBufferIndex + 4) {
            byte bigger[] = new byte[sysexBuffer.length * 2];
            for (int i = 0; i < sysexBufferIndex; i++) {
                bigger[i] = sysexBuffer[i];
            }
            sysexBuffer = bigger;
        }
        // now we have space to write some bytes
    }

    // call this to insert Sysex and EOX status bytes
    // call sysexBufferAppendBytes to insert anything else
    void sysexBufferAppendStatus(byte status) {
        sysexBuffer[sysexBufferIndex++] = status;
    }

    void sysexBufferAppendBytes(int msg, int len) {
        for (int i = 0; i < len; i++) {
            byte b = (byte) msg;
            if ((msg & 0x80) != 0) {
                if (b == 0xF7) { // end of sysex
                    sysexBufferAppendStatus(b);
                    sysex(sysexBuffer, sysexBufferIndex);
                    return;
                }
                // recursively handle embedded real-time messages
                PmEvent buffer = new PmEvent();
                buffer.timestamp = timestamp;
                buffer.message = b;
                handleMidiIn(buffer);
            } else {
                sysexBuffer[sysexBufferIndex++] = b;
            }
            msg = msg >> 8;
        }
    }

    void sysexBegin(int msg) {
        sysexBufferReset(); // start from 0, we have at least 256 bytes now
        sysexBufferAppendStatus((byte) (msg & 0xFF)); // first byte is special
        sysexBufferAppendBytes(msg >> 8, 3); // process remaining bytes
    }

    public void handleMidiIn(PmEvent buffer)
    {
        if (trace) {
            System.out.println("handleMidiIn: " + 
                               Integer.toHexString(buffer.message));
        }
        // rather than pass timestamps to every handler, where typically 
        // timestamps are ignored, just save the timestamp as a member
        // variable where methods can access it if they want it
        timestamp = buffer.timestamp;
        int status = buffer.message & 0xFF;
        if (status < 0x80) {
            sysexBufferCheck(); // make enough space
            sysexBufferAppendBytes(buffer.message, 4); // process 4 bytes
            return;
        }
        int command = status & 0xF0;
        int channel = status & 0x0F;
        int data1 = (buffer.message >> 8) & 0xFF;
        int data2 = (buffer.message >> 16) & 0xFF;
        switch (command) {
        case MIDI_NOTE_OFF:
            noteOff(channel, data1, data2); break;
        case MIDI_NOTE_ON:
            if (data2 > 0) {
                noteOn(channel, data1, data2); break;
            } else {
                noteOff(channel, data1);
            }
            break;
        case MIDI_CONTROL:
            control(channel, data1, data2); break;
        case MIDI_POLY_TOUCH:
            polyTouch(channel, data1, data2); break;
        case MIDI_TOUCH:
            touch(channel, data1); break;
        case MIDI_PITCH_BEND:
            pitchBend(channel, (data1 + (data2 << 7)) - 8192); break;
        case MIDI_PROGRAM:
            program(channel, data1); break;
        case 0xF0:
            switch (channel) {
            case 0: sysexBegin(buffer.message); break;
            case 1: mtcQuarterFrame(data1);
            case 2: songPosition(data1 + (data2 << 7)); break;
            case 3: songSelect(data1); break;
            case 4: /* unused */ break;
            case 5: /* unused */ break;
            case 6: tuneRequest(); break;
            case 7: sysexBufferAppendBytes(buffer.message, buffer.message); break;
            case 8: clock(); break;
            case 9: tick(); break;
            case 0xA: clockStart(); break;
            case 0xB: clockContinue(); break;
            case 0xC: clockStop(); break;
            case 0xD: /* unused */ break;
            case 0xE: activeSense(); break;
            case 0xF: reset(); break;
            }
        }
    }
          
    // the value ranges from +8181 to -8192. The interpretation is 
    // synthesizer dependent. Often the range is +/- one whole step
    // (two semitones), but the range is usually adjustable within
    // the synthesizer.
    void pitchBend(int channel, int value) { return; }
    void control(int channel, int control, int value) { return; }
    void noteOn(int channel, int pitch, int velocity) { return; }
    // you can handle velocity in note-off if you want, but the default
    // is to drop the velocity and call the simpler NoteOff handler
    void noteOff(int channel, int pitch, int velocity) { 
        noteOff(channel, pitch);
    }
    // if the subclass wants to implement NoteOff with velocity, it
    // should override the following to make sure all NoteOffs are handled
    void noteOff(int channel, int pitch) { return; }
    void program(int channel, int program) { return; }
    // the byte array may be bigger than the message, length tells how
    // many bytes in the array are part of the message
    void sysex(byte[] msg, int length) { return; }
    void polyTouch(int channel, int key, int value) { return; }
    void touch(int channel, int value) { return; }
    void mtcQuarterFrame(int value) { return; }
    // the value is a 14-bit integer representing 16th notes
    void songPosition(int value) { return; }
    void songSelect(int value) { return; }
    void tuneRequest() { return; }
    void clock() { return; } // represents 1/24th of a quarter note
    void tick() { return; } // represents 10ms
    void clockStart() { return; }
    void clockStop() { return; }
    void clockContinue() { return; }
    void activeSense() { return; }
    void reset() { return; }
}
