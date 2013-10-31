// JPortMidiException -- thrown by JPortMidi methods

package jportmidi;

public class JPortMidiException extends Exception {
    public int error = 0;
    public JPortMidiException(int err, String msg) {
        super(msg);
        error = err;
    }
}

