
Backward Compatibility Statement for Vamp Plugin SDK version 2.0
================================================================

Plugin binary compatibility
---------------------------
Version 2.0 of the Vamp plugin binary interface is backward compatible
with version 1.0.

A plugin that was compiled and (statically) linked using version 1.x
of the SDK should load and run without modification in a host that was
compiled and linked using version 2.0 of the SDK.

A plugin that was compiled and (statically) linked using version 2.0
of the SDK should load and run in a host that was compiled and linked
using version 1.x of the SDK.  However, the 1.x host will be unable to
see any durations that the plugin specifies for its returned features,
as there was no support for duration in version 1 of the Vamp plugin
interface.

Plugin/host version discrimination
----------------------------------
A Vamp plugin library receives the Vamp SDK version number for the
host as the first argument to its vampGetPluginDescriptor function.
It may use this information to provide different behaviour depending
on the version of the host.

For example, the plugin may structure its outputs differently in older
hosts that do not support feature duration.  Or, if the plugins rely
on version 2.0 features, the library could make itself invisible to
older hosts (returning no plugin descriptors).

The version argument passed to vampGetPluginDescriptor will be 1 for
Vamp 1.x hosts or 2 for Vamp 2.0 hosts.  (Plugin libraries should
behave as for version 2 if passed a version number greater than 2.)

Plugin SDK library compatibility
--------------------------------
For plugin code, version 2.0 of the Vamp plugin SDK is source
compatible but not library ABI compatible with version 1.x.

Plugins written for version 1.x should compile and link without
modification using version 2.0.  Plugins dynamically linked against
version 1.x SDK libraries will need to be rebuilt if they are to work
with version 2.0 libraries.  To avoid dynamic library resolution
issues, it is generally preferable to link the SDK statically when
distributing binary plugins.

Host SDK library compatibility
------------------------------
For host code, version 2.0 of the Vamp plugin SDK is neither source
nor binary compatible with version 1.x.

The host SDK header include location has moved for version 2.0; hosts
should now only include headers from the vamp-hostsdk/ include
directory -- the vamp-sdk/ directory is reserved for inclusion in
plugin code only.  There is also no longer a separate subdirectory for
hostext headers.

Hosts written for version 1.x will therefore need to have their
#include directives updated as follows:

  Old                                            New

  <vamp-sdk/PluginBase.h>                        <vamp-hostsdk/PluginBase.h>
  <vamp-sdk/Plugin.h>                            <vamp-hostsdk/Plugin.h>
  <vamp-sdk/RealTime.h>                          <vamp-hostsdk/RealTime.h>
  <vamp-sdk/hostext/PluginLoader.h>              <vamp-hostsdk/PluginLoader.h>
  <vamp-sdk/hostext/PluginBufferingAdapter.h>    <vamp-hostsdk/PluginBufferingAdapter.h>
  <vamp-sdk/hostext/PluginChannelAdapter.h>      <vamp-hostsdk/PluginChannelAdapter.h>
  <vamp-sdk/hostext/PluginInputDomainAdapter.h>  <vamp-hostsdk/PluginInputDomainAdapter.h>
  <vamp-sdk/PluginHostAdapter.h>                 <vamp-hostsdk/PluginHostAdapter.h>

For most hosts, these should be the only changes necessary; the actual
code remains the same.

Hosts that incorporate plugin code
----------------------------------
One of the changes in this version of the SDK is that separate
top-level C++ namespaces are used for classes compiled into plugins
(the _VampPlugin namespace) and hosts (the _VampHost namespace), to
avoid any confusion between host and plugin namespaces in unusual
linkage situations (as the host and plugin SDKs contain many of the
same classes, there is a risk that the wrong class may be picked up by
a stupid dynamic linker in cases where the host and plugin SDK
versions do not match).  This additional namespace is added and opened
silently in a manner that is transparent in most circumstances, and
neither plugin nor host authors will normally need to know about it.

However, hosts that directly incorporate code from plugins, for
example to provide functionality that is the same as those plugins
without having to explicitly load them, will find that they cannot
resolve plugin symbols at link time because of this namespace
mismatch.  To avoid this, you may define the preprocessor symbol
_VAMP_PLUGIN_IN_HOST_NAMESPACE when compiling the plugin code in the
context of the host, to ensure that both host and plugin code exist
within the same namespace.

(If your host does this, why not make it load the plugins dynamically
instead using the normal Vamp plugin loader method?  There are many
advantages to that.)

