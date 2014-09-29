// PmDefaults -- a small application to set PortMIDI default input/output

/* Implementation notes:

This program uses PortMidi to enumerate input and output devices and also
to send output messages to test output devices and 
to receive input messages to test input devices.

*/

package pmdefaults;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.Math.*;
import jportmidi.*;
import jportmidi.JPortMidiApi.*;
import java.util.ArrayList;
import java.util.prefs.*;
import java.net.*;

public class PmDefaultsFrame extends JFrame 
        implements ActionListener, ComponentListener {

    // This class extends JPortMidi in order to override midi input handling
    // In this case, midi input simply blinks the activity light
    public class JPM extends JPortMidi {
        ActivityLight light;
        PmDefaultsFrame frame;
        int lightTime;
        boolean lightState;
        int now; // current time in ms
        final int HALF_BLINK_PERIOD = 250; // ms

        public JPM(ActivityLight al, PmDefaultsFrame df) 
                throws JPortMidiException {
            light = al;
            frame = df;
            lightTime = 0;
            lightState = false; // meaning "off"
            now = 0;
        }
        
        public void poll(int ms) throws JPortMidiException {
            // blink the light. lightState is initially false (off).
            // to blink the light, set lightState to true and lightTime
            // to now + 0.25s; turn on light
            // now > ligntTime && lightState => set lightTime = now + 0.25s;
            //                                  set lightState = false
            //                                  turn off light
            // light can be blinked again when now > lightTime && !lightState
            now = ms;
            if (now > lightTime && lightState) {
                lightTime = now + HALF_BLINK_PERIOD;
                lightState = false;
                light.setState(false);
            }
            super.poll();
        }

        public void handleMidiIn(PmEvent buffer) {
            System.out.println("midi in: now " + now + 
                               " lightTime " + lightTime +
                               " lightState " + lightState);
            if (now > lightTime && !lightState) {
                lightState = true;
                lightTime = now + HALF_BLINK_PERIOD;
                light.setState(true);
            }
        }
    }

    public class ActivityLight extends JPanel {
        Color color;
        final Color OFF_COLOR = Color.BLACK;
        final Color ON_COLOR = Color.GREEN;

        ActivityLight() {
            super();
            Dimension size = new Dimension(50, 25);
            setMaximumSize(size);
            setPreferredSize(size);
            setMinimumSize(size);
            color = OFF_COLOR;
            System.out.println("ActivityLight " + getSize());
        }

        public void setState(boolean on) {
            color = (on ? ON_COLOR : OFF_COLOR);
            repaint();
        }

        public void paintComponent(Graphics g) {
            super.paintComponent(g); // paint background
            g.setColor(color);
            int x = (getWidth() / 2) - 5;
            int y = (getHeight() / 2) - 5;
            g.fillOval(x, y, 10, 10);
            g.setColor(OFF_COLOR);
            g.drawOval(x, y, 10, 10);
        }
    }

    private JLabel inputLabel;
    private JComboBox inputSelection;
    // inputIds maps from the index of an item in inputSelection to the
    // device ID. Used to open the selected device.
    private ArrayList<Integer> inputIds;
    private ActivityLight inputActivity;
    private JLabel outputLabel;
    private JComboBox outputSelection;
    // analogous to inputIds, outputIds maps selection index to device ID
    private ArrayList<Integer> outputIds;
    private JButton testButton;
    private JButton refreshButton;
    private JButton updateButton;
    private JButton closeButton;
    private JLabel logo;

    private JPM jpm;
    private Timer timer;

    public void componentResized(ComponentEvent e) {
        System.out.println(e);
        if (e.getComponent() == this) {
            Insets insets = getInsets();
            Dimension dim = getSize();
            layoutComponents(dim.width - insets.left - insets.right,
                             dim.height - insets.top - insets.bottom);
        }
    }
    public void componentMoved(ComponentEvent e) {
        System.out.println(e);
    }
    public void componentHidden(ComponentEvent e) {
        System.out.println(e);
    }
    public void componentShown(ComponentEvent e) {
        System.out.println(e);
    }

    
    PmDefaultsFrame(String title) {
        super(title);
        initComponents();
        System.out.println("initComponents returned\n");
        pack(); // necessary before calling getInsets();
        // initially, only width matters to layout:
        layoutComponents(550, 300);
        System.out.println("after layout, pref " + getPreferredSize());
        // now, based on layout-computed preferredSize, set the Frame size
        Insets insets = getInsets();
        Dimension dim = getPreferredSize();
        dim.width += (insets.left + insets.right);
        dim.height += (insets.top + insets.bottom);
        setSize(dim);
        System.out.println("size" + getPreferredSize());
        addComponentListener(this);

        timer = new Timer(50 /* ms */, this);
        timer.addActionListener(this);
        try {
            jpm = new JPM(inputActivity, this);
            jpm.setTrace(true);
            loadDeviceChoices();
            timer.start(); // don't start timer if there's an error
        } catch(JPortMidiException e) {
            System.out.println(e);
        }
    }

    void openInputSelection() {
        int id = inputSelection.getSelectedIndex();
        if (id < 0) return; // nothing selected
        id = (Integer) (inputIds.get(id)); // map to device ID
        // openInput will close any previously open input stream
        try {
            jpm.openInput(id, 100); // buffer size hopes to avoid overflow
        } catch(JPortMidiException e) {
            System.out.println(e);
        }
    }

    // make a string to put into preferences describing this device
    String makePrefName(int id) {
        String name = jpm.getDeviceName(id);
        String interf = jpm.getDeviceInterf(id);
        // the syntax requires comma-space separator (see portmidi.h)
        return interf + ", " + name;
    }


    public void savePreferences() {
        Preferences prefs = Preferences.userRoot().node("/PortMidi");
        int id = outputSelection.getSelectedIndex();
        if (id >= 0) {
            String prefName = makePrefName(outputIds.get(id));
            System.out.println("output pref: " + prefName);
            prefs.put("PM_RECOMMENDED_OUTPUT_DEVICE", prefName);
        }
        id = inputSelection.getSelectedIndex();
        if (id >= 0) {
            String prefName = makePrefName(inputIds.get(id));
            System.out.println("input pref: " + prefName);
            prefs.put("PM_RECOMMENDED_INPUT_DEVICE", prefName);
        }
        try {
            prefs.flush();
        } catch(BackingStoreException e) {
            System.out.println(e);
        }
    }

    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();
        try {
            if (source == timer) {
                jpm.poll(jpm.timeGet());
            } else if (source == refreshButton) {
                if (jpm.isOpenInput()) jpm.closeInput();
                if (jpm.isOpenOutput()) jpm.closeOutput();
                jpm.refreshDeviceLists();
                loadDeviceChoices();
            } else if (source == updateButton) {
                savePreferences();
            } else if (source == closeButton) {
                if (jpm.isOpenInput()) jpm.closeInput();
                if (jpm.isOpenOutput()) jpm.closeOutput();
            } else if (source == testButton) {
                sendTestMessages();
            } else if (source == inputSelection) {
                // close previous selection and open new one
                openInputSelection();
            } else if (source == outputSelection) {
                jpm.closeOutput(); // remains closed until Test button reopens
            }
        } catch(JPortMidiException ex) {
            System.out.println(ex);
        }
    };

    private void layoutComponents(int width, int height) {
        // I tried to do this with various layout managers, but failed
        // It seems pretty straightforward to just compute locations and
        // sizes.

        int gap = 2; // pixel separation between components
        int indent = 20;
        int y = gap;

        // inputLabel goes in upper left
        inputLabel.setLocation(0, y);
        inputLabel.setSize(inputLabel.getPreferredSize());

        // inputSelection goes below and indented, width based on panel
        y += inputLabel.getHeight() + gap;
        inputSelection.setLocation(indent, y);
        // size of inputSelection must leave room at right for inputButton
        // (in fact, inputActivity goes there, but we'll make inputSelection
        // and outputSelection the same size, based on leaving room for
        // testButton, which is larger than inputActivity.)
        Dimension dim = inputSelection.getPreferredSize();
        Dimension dimButton = testButton.getPreferredSize();
        // make button and selection the same height so they align
        dim.height = dimButton.height = Math.max(dim.height, dimButton.height);
        // make selection width as wide as possible
        dim.width = width - indent - dimButton.width - gap;
        inputSelection.setSize(dim);

        // inputActivity goes to the right of inputSelection
        inputActivity.setLocation(indent + dim.width + gap, y);
        // square size to match the height of inputSelection
        inputActivity.setSize(dim.height, dim.height);

        // outputLabel goes below
        y += dim.height + gap;
        outputLabel.setLocation(0, y);
        outputLabel.setSize(outputLabel.getPreferredSize());
       
        // outputSelection is like inputSelection
        y += outputLabel.getHeight() + gap;
        outputSelection.setLocation(indent, y);
        outputSelection.setSize(dim);

        // testButton is like inputActivity
        testButton.setLocation(indent + dim.width + gap, y);
        testButton.setSize(dimButton);
        System.out.println("button " + dimButton + " selection " + dim);

        // refreshButton is below
        y += dim.height + gap;
        dim = refreshButton.getPreferredSize();
        refreshButton.setLocation(indent, y);
        refreshButton.setSize(dim);

        // updateButton to right of refreshButton
        int x = indent + dim.width + gap;
        updateButton.setLocation(x, y);
        dim = updateButton.getPreferredSize();
        updateButton.setSize(dim);

        // closeButton to right of updateButton
        x += dim.width + gap;
        closeButton.setLocation(x, y);
        dim = closeButton.getPreferredSize();
        closeButton.setSize(dim);

        // place logo centered at bottom
        y += dim.height + gap;
        logo.setLocation((width - logo.getWidth()) / 2, 
                         height - gap - logo.getHeight());

        // set overall size
        y += logo.getHeight() + gap;
        System.out.println("computed best size " + width + ", " + y);
        setPreferredSize(new Dimension(width, y));
    }

    private void initComponents() {
        Container wholePanel = getContentPane();
        wholePanel.setLayout(null);
        setLayout(null);

        inputLabel = new JLabel();
        inputLabel.setText("Default Input");
        wholePanel.add(inputLabel);

        inputSelection = new JComboBox();
        inputSelection.addActionListener(this);
        inputSelection.setLocation(20, 30);
        inputSelection.setSize(inputSelection.getPreferredSize());
        System.out.println("Adding inputSelection to panel");
        wholePanel.add(inputSelection);
        inputIds = new ArrayList<Integer>();

        inputActivity = new ActivityLight();
        wholePanel.add(inputActivity);

        outputLabel = new JLabel();
        outputLabel.setText("Default Output");
        wholePanel.add(outputLabel);

        outputSelection = new JComboBox();
        outputSelection.addActionListener(this);
        wholePanel.add(outputSelection);
        testButton = new JButton();
        testButton.setText("Test");
        testButton.addActionListener(this);
        wholePanel.add(testButton);
        outputIds = new ArrayList<Integer>();

        refreshButton = new JButton();
        refreshButton.setText("Refresh Device Lists");
        System.out.println("refresh " + refreshButton.getPreferredSize());
        System.out.println(getLayout());
        refreshButton.addActionListener(this);
        wholePanel.add(refreshButton);

        updateButton = new JButton();
        updateButton.setText("Update Preferences");
        updateButton.setSize(refreshButton.getPreferredSize());
        updateButton.addActionListener(this);
        wholePanel.add(updateButton);

        closeButton = new JButton();
        closeButton.setText("Close/Release Ports");
        closeButton.setSize(refreshButton.getPreferredSize());
        closeButton.addActionListener(this);
        wholePanel.add(closeButton);

	// load the logo from the jar file (on Linux and Windows)
	ClassLoader cldr = this.getClass().getClassLoader();
        ImageIcon icon;
        URL logoURL = cldr.getResource("portmusic_logo.png");
        if (logoURL == null) {
            // on Mac, load from bundle
            icon = new ImageIcon("portmusic_logo.png");
        } else {
            icon = new ImageIcon(logoURL);
        }
        logo = new JLabel(icon);
        logo.setSize(logo.getPreferredSize());
        wholePanel.add(logo);

        setVisible(true);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    void loadDeviceChoices() throws JPortMidiException {
        // initialize and load combo boxes with device descriptions
        int n = jpm.countDevices();
        inputSelection.removeAllItems();
        inputIds.clear();
        outputSelection.removeAllItems();
        outputIds.clear();
        for (int i = 0; i < n; i++) {
            String interf = jpm.getDeviceInterf(i);
            String name = jpm.getDeviceName(i);
	    System.out.println("name " + name);
            String selection = name + " [" + interf + "]";
            if (jpm.getDeviceInput(i)) {
                inputIds.add(i);
                inputSelection.addItem(selection);
            } else {
                outputIds.add(i);
                outputSelection.addItem(selection);
            }
        }
    }

    void sendTestMessages() {
        try {
            if (!jpm.isOpenOutput()) {
                int id = outputSelection.getSelectedIndex();
                if (id < 0) return; // nothing selected
                id = (Integer) (outputIds.get(id));
                System.out.println("calling openOutput");
                jpm.openOutput(id, 10, 10);
            }
            jpm.midiNote(0, 67, 100); // send an A (440)
            jpm.midiNote(0, 67, 0, jpm.timeGet() + 500);
        } catch(JPortMidiException e) {
            System.out.println(e);
        }
    }
}

