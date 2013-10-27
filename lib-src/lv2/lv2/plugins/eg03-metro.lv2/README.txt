== Metronome ==

This plugin demonstrates tempo synchronisation by clicking on every beat.  The
host sends this information to the plugin as events, so an event with new time
and tempo information will be received whenever there is a change.

Time is assumed to continue rolling at the tempo and speed defined by the last
received tempo event, even across cycles, until a new tempo event is received
or the plugin is deactivated.
