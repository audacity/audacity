== Sampler ==

This plugin loads a single sample from a .wav file and plays it back when a MIDI
note on is received.  Any sample on the system can be loaded via another event.
A Gtk UI is included which does this, but the host can as well.

This plugin illustrates:

- UI <==> Plugin communication via events
- Use of the worker extension for non-realtime tasks (sample loading)
- Use of the log extension to print log messages via the host
- Saving plugin state via the state extension
- Dynamic plugin control via the same properties saved to state
- Network-transparent waveform display with incremental peak transmission
