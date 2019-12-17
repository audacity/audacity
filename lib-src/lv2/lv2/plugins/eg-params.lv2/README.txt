== Params ==

The basic LV2 mechanism for controls is
http://lv2plug.in/ns/lv2core#ControlPort[lv2:ControlPort], inherited from
LADSPA.  Control ports are problematic because they are not sample accurate,
support only one type (`float`), and require that plugins poll to know when a
control has changed.

Parameters can be used instead to address these issues.  Parameters can be
thought of as properties of a plugin instance; they are identified by URI and
have a value of any type.  This deliberately meshes with the concept of plugin
state defined by the http://lv2plug.in/ns/ext/state[LV2 state extension].
The state extension allows plugins to save and restore their parameters (along
with other internal state information, if necessary).

Parameters are accessed and manipulated using messages sent via a sequence
port.  The http://lv2plug.in/ns/ext/patch[LV2 patch extension] defines the
standard messages for working with parameters.  Typically, only two are used
for simple plugins: http://lv2plug.in/ns/ext/patch#Set[patch:Set] sets a
parameter to some value, and http://lv2plug.in/ns/ext/patch#Get[patch:Get]
requests that the plugin send a description of its parameters.
