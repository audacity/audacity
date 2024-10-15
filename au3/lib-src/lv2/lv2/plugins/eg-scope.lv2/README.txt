== Simple Oscilloscope ==

This plugin displays the waveform of an incoming audio signal using a simple
GTK+Cairo GUI.

This plugin illustrates:

- UI <==> Plugin communication via http://lv2plug.in/ns/ext/atom/[LV2 Atom] events
- Atom vector usage and resize-port extension
- Save/Restore UI state by communicating state to backend
- Saving simple key/value state via the http://lv2plug.in/ns/ext/state/[LV2 State] extension
- Cairo drawing and partial exposure

This plugin intends to outline the basics for building visualization plugins
that rely on atom communication.  The UI looks like an oscilloscope, but is not
a real oscilloscope implementation:

- There is no display synchronisation, results will depend on LV2 host.
- It displays raw audio samples, which a proper scope must not do.
- The display itself just connects min/max line segments.
- No triggering or synchronization.
- No labels, no scale, no calibration, no markers, no numeric readout, etc.

Addressing these issues is beyond the scope of this example.

Please see http://lac.linuxaudio.org/2013/papers/36.pdf for scope design,
https://wiki.xiph.org/Videos/Digital_Show_and_Tell for background information,
and http://lists.lv2plug.in/pipermail/devel-lv2plug.in/2013-November/000545.html
for general LV2 related conceptual criticism regarding real-time visualizations.

A proper oscilloscope based on this example can be found at
https://github.com/x42/sisco.lv2
