"""Lilv Python interface"""

__author__ = "David Robillard"
__copyright__ = "Copyright 2016-2019 David Robillard"
__license__ = "ISC"
__version__ = "0.24.5"
__maintainer__ = "David Robillard"
__email__ = "d@drobilla.net"
__status__ = "Production"

import sys

from ctypes import Structure, CDLL, POINTER, CFUNCTYPE
from ctypes import c_bool, c_double, c_float, c_int, c_size_t, c_uint, c_uint32
from ctypes import c_char, c_char_p, c_void_p
from ctypes import byref, cast

# Option constants
OPTION_FILTER_LANG = "http://drobilla.net/ns/lilv#filter-lang"
OPTION_DYN_MANIFEST = "http://drobilla.net/ns/lilv#dyn-manifest"


class _LilvLib:
    """Object that represents the liblilv C library"""

    def __init__(self):
        if sys.platform == "darwin":
            self.lib = CDLL("liblilv-0.dylib")
        elif sys.platform == "win32":
            self.lib = CDLL("lilv-0.dll")
        else:
            self.lib = CDLL("liblilv-0.so")


# Load lilv C library and define library global (which is populated below)
c = _LilvLib()


def _as_uri(obj):
    """Utility function for converting some object into a URI node"""
    if type(obj) in [Plugin, PluginClass, UI]:
        return obj.get_uri()
    else:
        assert type(obj) == Node
        assert obj.node
        return Node(obj.world, c.node_duplicate(obj.node))


# LV2 types

LV2_Handle = POINTER(None)
LV2_URID_Map_Handle = POINTER(None)
LV2_URID_Unmap_Handle = POINTER(None)
LV2_URID = c_uint32


class LV2_Feature(Structure):
    __slots__ = ["URI", "data"]
    _fields_ = [("URI", c_char_p), ("data", POINTER(None))]


class LV2_Descriptor(Structure):
    __slots__ = [
        "URI",
        "instantiate",
        "connect_port",
        "activate",
        "run",
        "deactivate",
        "cleanup",
        "extension_data",
    ]


LV2_Descriptor._fields_ = [
    ("URI", c_char_p),
    (
        "instantiate",
        CFUNCTYPE(
            LV2_Handle,
            POINTER(LV2_Descriptor),
            c_double,
            c_char_p,
            POINTER(POINTER(LV2_Feature)),
        ),
    ),
    ("connect_port", CFUNCTYPE(None, LV2_Handle, c_uint32, POINTER(None))),
    ("activate", CFUNCTYPE(None, LV2_Handle)),
    ("run", CFUNCTYPE(None, LV2_Handle, c_uint32)),
    ("deactivate", CFUNCTYPE(None, LV2_Handle)),
    ("cleanup", CFUNCTYPE(None, LV2_Handle)),
    ("extension_data", CFUNCTYPE(c_void_p, c_char_p)),
]


class LV2_URID_Map(Structure):
    __slots__ = ["handle", "map"]
    _fields_ = [
        ("handle", LV2_URID_Map_Handle),
        ("map", CFUNCTYPE(LV2_URID, LV2_URID_Map_Handle, c_char_p)),
    ]


class LV2_URID_Unmap(Structure):
    __slots__ = ["handle", "unmap"]
    _fields_ = [
        ("handle", LV2_URID_Unmap_Handle),
        ("unmap", CFUNCTYPE(c_char_p, LV2_URID_Unmap_Handle, LV2_URID)),
    ]


# Lilv types


class Plugin(Structure):
    """LV2 Plugin."""

    @classmethod
    def wrap(cls, world, plugin):
        return Plugin(world, plugin) if world is not None and plugin else None

    def __init__(self, world, plugin):
        assert isinstance(world, World)
        assert type(plugin) == POINTER(Plugin)
        assert plugin

        self.world = world
        self.plugin = plugin

    def __eq__(self, other):
        return self.get_uri() == other.get_uri()

    def verify(self):
        """Check if `plugin` is valid.

        This is not a rigorous validator, but can be used to reject some
        malformed plugins that could cause bugs (e.g. plugins with missing
        required fields).

        Note that normal hosts do NOT need to use this - lilv does not load
        invalid plugins into plugin lists.  This is included for plugin testing
        utilities, etc.
        """
        return c.plugin_verify(self.plugin)

    def get_uri(self):
        """Get the URI of `plugin`.

        Any serialization that refers to plugins should refer to them by this.
        Hosts SHOULD NOT save any filesystem paths, plugin indexes, etc. in
        saved files pass save only the URI.

        The URI is a globally unique identifier for one specific plugin.  Two
        plugins with the same URI are compatible in port signature, and should
        be guaranteed to work in a compatible and consistent way.  If a plugin
        is upgraded in an incompatible way (eg if it has different ports), it
        MUST have a different URI than it's predecessor.
        """
        return Node.wrap(
            self.world, c.node_duplicate(c.plugin_get_uri(self.plugin))
        )

    def get_bundle_uri(self):
        """Get the (resolvable) URI of the plugin's "main" bundle.

        This returns the URI of the bundle where the plugin itself was found.
        Note that the data for a plugin may be spread over many bundles, that
        is, get_data_uris() may return URIs which are not within this bundle.

        Typical hosts should not need to use this function.  Note this always
        returns a fully qualified URI.  If you want a local filesystem path,
        use lilv.file_uri_parse().
        """
        return Node.wrap(
            self.world, c.node_duplicate(c.plugin_get_bundle_uri(self.plugin))
        )

    def get_data_uris(self):
        """Get the (resolvable) URIs of the RDF data files that define a plugin.

        Typical hosts should not need to use this function.  Note this always
        returns fully qualified URIs.  If you want local filesystem paths, use
        lilv.file_uri_parse().
        """
        return Nodes(self.world, c.plugin_get_data_uris(self.plugin), False)

    def get_library_uri(self):
        """Get the (resolvable) URI of the shared library for `plugin`.

        Note this always returns a fully qualified URI.  If you want a local
        filesystem path, use lilv.file_uri_parse().
        """
        return Node.wrap(
            self.world, c.node_duplicate(c.plugin_get_library_uri(self.plugin))
        )

    def get_name(self):
        """Get the name of `plugin`.

        This returns the name (doap:name) of the plugin.  The name may be
        translated according to the current locale, this value MUST NOT be used
        as a plugin identifier (use the URI for that).
        """
        return Node.wrap(self.world, c.plugin_get_name(self.plugin))

    def get_class(self):
        """Get the class this plugin belongs to (e.g. Filters)."""
        return PluginClass(self.world, c.plugin_get_class(self.plugin))

    def get_value(self, predicate):
        """Get a value associated with the plugin in a plugin's data files.

        `predicate` must be either a URI or a QName.

        Returns the ?object of all triples found of the form:

        plugin-uri predicate ?object

        May return None if the property was not found, or if object(s) is not
        sensibly represented as a LilvNodes (e.g. blank nodes).
        """
        return Nodes(
            self.world, c.plugin_get_value(self.plugin, predicate.node), True
        )

    def has_feature(self, feature_uri):
        """Return whether a feature is supported by a plugin.

        This will return true if the feature is an optional or required feature
        of the plugin.
        """
        return c.plugin_has_feature(self.plugin, feature_uri.node)

    def get_supported_features(self):
        """Get the LV2 Features supported (required or optionally) by a plugin.

        A feature is "supported" by a plugin if it is required OR optional.

        Since required features have special rules the host must obey, this
        function probably shouldn't be used by normal hosts.  Using
        get_optional_features() and get_required_features() separately is best
        in most cases.
        """
        return Nodes(
            self.world, c.plugin_get_supported_features(self.plugin), True
        )

    def get_required_features(self):
        """Get the LV2 Features required by a plugin.

        If a feature is required by a plugin, hosts MUST NOT use the plugin if
        they do not understand (or are unable to support) that feature.

        All values returned here MUST be return plugin_(self.plugin)ed to the
        plugin's instantiate method (along with data, if necessary, as defined
        by the feature specification) or plugin instantiation will fail.
        """
        return Nodes(
            self.world, c.plugin_get_required_features(self.plugin), True
        )

    def get_optional_features(self):
        """Get the LV2 Features optionally supported by a plugin.

        Hosts MAY ignore optional plugin features for whatever reasons.
        Plugins MUST operate (at least somewhat) if they are instantiated
        without being passed optional features.
        """
        return Nodes(
            self.world, c.plugin_get_optional_features(self.plugin), True
        )

    def has_extension_data(self, uri):
        """Return whether or not a plugin provides specific extension data."""
        return c.plugin_has_extension_data(self.plugin, uri.node)

    def get_extension_data(self):
        """Get a sequence of all extension data provided by a plugin.

        This can be used to find which URIs get_extension_data()
        will return a value for without instantiating the plugin.
        """
        return Nodes(
            self.world, c.plugin_get_extension_data(self.plugin), True
        )

    def get_num_ports(self):
        """Get the number of ports on this plugin."""
        return c.plugin_get_num_ports(self.plugin)

    def get_num_ports_of_class(self, *args):
        """Get the number of ports of some class(es) on this plugin."""
        return c.plugin_get_num_ports_of_class(
            self.plugin, *(list(map(lambda n: n.node, args)) + [None])
        )

    def has_latency(self):
        """Return whether or not the plugin introduces (and reports) latency.

        The index of the latency port can be found with
        get_latency_port() ONLY if this function returns true.
        """
        return c.plugin_has_latency(self.plugin)

    def get_latency_port_index(self):
        """Return the index of the plugin's latency port.

        Returns None if the plugin has no latency port.

        Any plugin that introduces unwanted latency that should be compensated
        for (by hosts with the ability/need) MUST provide this port, which is a
        control rate output port that reports the latency for each cycle in
        frames.
        """
        return (
            c.plugin_get_latency_port_index(self.plugin)
            if self.has_latency()
            else None
        )

    def get_port(self, key):
        """Get a port on `plugin` by index or symbol."""
        if type(key) == int:
            return self.get_port_by_index(key)
        else:
            return self.get_port_by_symbol(key)

    def get_port_by_index(self, index):
        """Get a port on `plugin` by `index`."""
        assert type(index) == int
        return Port.wrap(self, c.plugin_get_port_by_index(self.plugin, index))

    def get_port_by_symbol(self, symbol):
        """Get a port on `plugin` by `symbol`.

        Note this function is slower than get_port_by_index(),
        especially on plugins with a very large number of ports.
        """
        assert type(symbol) == str or isinstance(symbol, Node)
        if type(symbol) == str:
            symbol = self.world.new_string(symbol)

        assert isinstance(symbol, Node)
        assert symbol.node is not None
        return Port.wrap(
            self, c.plugin_get_port_by_symbol(self.plugin, symbol.node)
        )

    def get_port_by_designation(self, port_class, designation):
        """Get a port on `plugin` by its lv2:designation.

        The designation of a port describes the meaning, assignment, allocation
        or role of the port, e.g. "left channel" or "gain".  If found, the port
        with matching `port_class` and `designation` is be returned, otherwise
        None is returned.  The `port_class` can be used to distinguish the
        input and output ports for a particular designation.  If `port_class`
        is None, any port with the given designation will be returned.
        """
        return Port.wrap(
            self,
            c.plugin_get_port_by_designation(
                self.plugin, port_class.node, designation.node
            ),
        )

    def get_project(self):
        """Get the project the plugin is a part of.

        More information about the project can be read via find_nodes(),
        typically using properties from DOAP (e.g. doap:name).
        """
        return Node.wrap(self.world, c.plugin_get_project(self.plugin))

    def get_author_name(self):
        """Get the full name of the plugin's author.

        Returns None if author name is not present.
        """
        return Node.wrap(self.world, c.plugin_get_author_name(self.plugin))

    def get_author_email(self):
        """Get the email address of the plugin's author.

        Returns None if author email address is not present.
        """
        return Node.wrap(self.world, c.plugin_get_author_email(self.plugin))

    def get_author_homepage(self):
        """Get the address of the plugin author's home page.

        Returns None if author homepage is not present.
        """
        return Node.wrap(self.world, c.plugin_get_author_homepage(self.plugin))

    def is_replaced(self):
        """Return true iff `plugin` has been replaced by another plugin.

        The plugin will still be usable, but hosts should hide them from their
        user interfaces to prevent users from using deprecated plugins.
        """
        return c.plugin_is_replaced(self.plugin)

    def get_related(self, resource_type):
        """Get the resources related to `plugin` with lv2:appliesTo.

        Some plugin-related resources are not linked directly to the plugin
        with rdfs:seeAlso and thus will not be automatically loaded along with
        the plugin data (usually for performance reasons).  All such resources
        of the given @c type related to `plugin` can be accessed with this
        function.

        If `resource_type` is None, all such resources will be returned,
        regardless of type.

        To actually load the data for each returned resource, use
        world.load_resource().
        """
        return Nodes(
            self.world, c.plugin_get_related(self.plugin, resource_type), True
        )

    def get_uis(self):
        """Get all UIs for `plugin`."""
        return UIs(self.world, c.plugin_get_uis(self.plugin))


class PluginClass(Structure):
    """Plugin Class (type/category)."""

    def __init__(self, world, plugin_class):
        assert isinstance(world, World)
        assert type(plugin_class) == POINTER(PluginClass)
        assert plugin_class

        self.world = world
        self.plugin_class = plugin_class

    def __str__(self):
        return self.get_uri().__str__()

    def get_parent_uri(self):
        """Get the URI of this class' superclass.

        May return None if class has no parent.
        """
        return Node.wrap(
            self.world,
            c.node_duplicate(c.plugin_class_get_parent_uri(self.plugin_class)),
        )

    def get_uri(self):
        """Get the URI of this plugin class."""
        return Node.wrap(
            self.world,
            c.node_duplicate(c.plugin_class_get_uri(self.plugin_class)),
        )

    def get_label(self):
        """Get the label of this plugin class, ie "Oscillators"."""
        return Node.wrap(
            self.world,
            c.node_duplicate(c.plugin_class_get_label(self.plugin_class)),
        )

    def get_children(self):
        """Get the subclasses of this plugin class."""
        return PluginClasses(
            self.world, c.plugin_class_get_children(self.plugin_class), True
        )


class Port(Structure):
    """Port on a Plugin."""

    @classmethod
    def wrap(cls, plugin, port):
        if plugin is not None and port:
            return Port(plugin, port)

        return None

    def __init__(self, plugin, port):
        assert isinstance(plugin, Plugin)
        assert type(port) == POINTER(Port)
        assert port

        self.plugin = plugin
        self.port = port

    def get_node(self):
        """Get the RDF node of `port`.

        Ports nodes may be may be URIs or blank nodes.
        """
        return Node.wrap(
            self.plugin.world,
            c.node_duplicate(c.port_get_node(self.plugin, self.port)),
        )

    def get_value(self, predicate):
        """Port analog of Plugin.get_value()."""
        return Nodes(
            self.plugin.world,
            c.port_get_value(self.plugin.plugin, self.port, predicate.node),
            True,
        )

    def get(self, predicate):
        """Get a single property value of a port.

        This is equivalent to lilv_nodes_get_first(lilv_port_get_value(...))
        but is simpler to use in the common case of only caring about one
        value.  The caller is responsible for freeing the returned node.
        """
        return Node.wrap(
            self.plugin.world,
            c.port_get(self.plugin.plugin, self.port, predicate.node),
        )

    def get_properties(self):
        """Return the LV2 port properties of a port."""
        return Nodes(
            self.plugin.world,
            c.port_get_properties(self.plugin.plugin, self.port),
            True,
        )

    def has_property(self, property_uri):
        """Return whether a port has a certain property."""
        return c.port_has_property(
            self.plugin.plugin, self.port, property_uri.node
        )

    def supports_event(self, event_type):
        """Return whether a port supports a certain event type.

        More precisely, this returns true iff the port has an atom:supports or
        an ev:supportsEvent property with `event_type` as the value.
        """
        return c.port_supports_event(
            self.plugin.plugin, self.port, event_type.node
        )

    def get_index(self):
        """Get the index of a port.

        The index is only valid for the life of the plugin and may change
        between versions.  For a stable identifier, use the symbol.
        """
        return c.port_get_index(self.plugin.plugin, self.port)

    def get_symbol(self):
        """Get the symbol of a port.

        The 'symbol' is a short string, a valid C identifier.
        """
        return Node.wrap(
            self.plugin.world,
            c.node_duplicate(c.port_get_symbol(self.plugin.plugin, self.port)),
        )

    def get_name(self):
        """Get the name of a port.

        This is guaranteed to return the untranslated name (the doap:name in
        the data file without a language tag).
        """
        return Node.wrap(
            self.plugin.world, c.port_get_name(self.plugin.plugin, self.port)
        )

    def get_classes(self):
        """Get all the classes of a port.

        This can be used to determine if a port is an input, output, audio,
        control, midi, etc, etc, though it's simpler to use is_a().
        The returned list does not include lv2:Port, which is implied.
        Returned value is shared and must not be destroyed by caller.
        """
        return Nodes(
            self.plugin.world,
            c.port_get_classes(self.plugin.plugin, self.port),
            False,
        )

    def is_a(self, port_class):
        """Determine if a port is of a given class (input, output, audio, etc).

        For convenience/performance/extensibility reasons, hosts are expected
        to create a LilvNode for each port class they "care about".  Well-known
        type URI strings are defined (e.g. LILV_URI_INPUT_PORT) for
        convenience, but this function is designed so that Lilv is usable with
        any port types without requiring explicit support in Lilv.
        """
        return c.port_is_a(self.plugin.plugin, self.port, port_class.node)

    def get_range(self):
        """Return the default, minimum, and maximum values of a port as a tuple.
        """
        pdef = POINTER(Node)()
        pmin = POINTER(Node)()
        pmax = POINTER(Node)()
        c.port_get_range(
            self.plugin.plugin,
            self.port,
            byref(pdef),
            byref(pmin),
            byref(pmax),
        )
        return (
            Node.wrap(self.plugin.world, pdef),
            Node.wrap(self.plugin.world, pmin),
            Node.wrap(self.plugin.world, pmax),
        )

    def get_scale_points(self):
        """Get a list of the scale points (enumeration values) of a port.

        This returns a collection of 'interesting' named values of a port
        (e.g. appropriate entries for a UI selector associated with this port).
        """

        cpoints = c.port_get_scale_points(self.plugin.plugin, self.port)
        points = []
        it = c.scale_points_begin(cpoints)
        while not c.scale_points_is_end(cpoints, it):
            points += [
                ScalePoint(self.plugin.world, c.scale_points_get(cpoints, it))
            ]
            it = c.scale_points_next(cpoints, it)

        c.scale_points_free(cpoints)
        return points


class ScalePoint(Structure):
    """Scale point (detent)."""

    def __init__(self, world, point):
        assert isinstance(world, World)
        assert type(point) == POINTER(ScalePoint)
        assert point

        self.label = Node.wrap(
            world, c.node_duplicate(c.scale_point_get_label(point))
        )
        self.value = Node.wrap(
            world, c.node_duplicate(c.scale_point_get_value(point))
        )

    def get_label(self):
        """Get the label of this scale point (enumeration value)."""
        return self.label

    def get_value(self):
        """Get the value of this scale point (enumeration value)."""
        return self.value


class UI(Structure):
    """Plugin UI."""

    def __init__(self, world, ui):
        assert isinstance(world, World)
        assert type(ui) == POINTER(UI)
        assert ui
        self.world = world
        self.ui = ui

    def __str__(self):
        return str(self.get_uri())

    def __eq__(self, other):
        if type(other) == str or type(other) == Node:
            return self.get_uri() == other

        return self.get_uri() == other.get_uri()

    def get_uri(self):
        """Get the URI of a Plugin UI."""
        return Node.wrap(self.world, c.node_duplicate(c.ui_get_uri(self.ui)))

    def get_classes(self):
        """Get the types (URIs of RDF classes) of a Plugin UI.

        Note that in most cases is_supported() should be used, which avoids
           the need to use this function (and type specific logic).
        """
        return Nodes(self.world, c.ui_get_classes(self.ui), False)

    def is_a(self, class_uri):
        """Check whether a plugin UI has a given type."""
        return c.ui_is_a(self.ui, class_uri.node)

    def get_bundle_uri(self):
        """Get the URI of the UI's bundle."""
        return Node.wrap(
            self.world, c.node_duplicate(c.ui_get_bundle_uri(self.ui))
        )

    def get_binary_uri(self):
        """Get the URI for the UI's shared library."""
        return Node.wrap(
            self.world, c.node_duplicate(c.ui_get_binary_uri(self.ui))
        )


class Node(Structure):
    """Data node (URI, string, integer, etc.).

    A Node can be converted to the corresponding Python datatype, and all nodes
    can be converted to strings, for example::

       >>> world = lilv.World()
       >>> i = world.new_int(42)
       >>> print(i)
       42
       >>> int(i) * 2
       84
    """

    @classmethod
    def wrap(cls, world, node):
        assert isinstance(world, World)
        assert (node is None) or (type(node) == POINTER(Node))
        if node:
            return Node(world, node)

        return None

    def __init__(self, world, node):
        assert type(node) == POINTER(Node)
        assert node
        self.world = world
        self.node = node

    def __del__(self):
        # Note that since Python 3.4, cycles are deleted and the world can be
        # destroyed before nodes (which contain a pointer to it).  This causes
        # a crash, so we only free here if the world is still alive.  It does
        # not seem possible to enforce the right order (it happens even if
        # everything has a reference to the world), but this normally only
        # happens on exit anyway so it shouldn't matter much.
        if self.world.world:
            c.node_free(self.node)

    def __eq__(self, other):
        if other is None:
            return False

        otype = type(other)
        if otype == Node:
            return c.node_equals(self.node, other.node)

        return otype(self) == other

    def __ne__(self, other):
        return not c.node_equals(self.node, other.node)

    def __str__(self):
        return c.node_as_string(self.node).decode("utf-8")

    def __int__(self):
        if not self.is_int():
            raise ValueError("node %s is not an integer" % str(self))
        return c.node_as_int(self.node)

    def __float__(self):
        if not self.is_float():
            raise ValueError("node %s is not a float" % str(self))
        return c.node_as_float(self.node)

    def __bool__(self):
        if not self.is_bool():
            raise ValueError("node %s is not a bool" % str(self))
        return c.node_as_bool(self.node)

    __nonzero__ = __bool__

    def get_turtle_token(self):
        """Return this value as a Turtle/SPARQL token."""
        c_str = c.node_get_turtle_token(self.node)
        string = cast(c_str, c_char_p).value.decode("utf-8")
        c.free(c_str)
        return string

    def is_uri(self):
        """Return whether the value is a URI (resource)."""
        return c.node_is_uri(self.node)

    def is_blank(self):
        """Return whether the value is a blank node (resource with no URI)."""
        return c.node_is_blank(self.node)

    def is_literal(self):
        """Return whether this value is a literal (i.e. not a URI)."""
        return c.node_is_literal(self.node)

    def is_string(self):
        """Return whether this value is a string literal.

        Returns true if value is a string value (and not numeric).
        """
        return c.node_is_string(self.node)

    def get_path(self, hostname=None):
        """Return the path of a file URI node.

        Returns None if value is not a file URI."""
        c_str = c.node_get_path(self.node, hostname)
        string = cast(c_str, c_char_p).value.decode("utf-8")
        if sys.platform != 'win32': # TODO: Memory comes from libserd
            c.free(c_str)
        return string

    def is_float(self):
        """Return whether this value is a decimal literal."""
        return c.node_is_float(self.node)

    def is_int(self):
        """Return whether this value is an integer literal."""
        return c.node_is_int(self.node)

    def is_bool(self):
        """Return whether this value is a boolean."""
        return c.node_is_bool(self.node)


class Iter(Structure):
    """Collection iterator."""

    def __init__(
        self,
        collection,
        iterator,
        constructor,
        iter_get,
        iter_next,
        iter_is_end,
    ):
        assert isinstance(collection, Collection)

        self.collection = collection
        self.iterator = iterator
        self.constructor = constructor
        self.iter_get = iter_get
        self.iter_next = iter_next
        self.iter_is_end = iter_is_end

    def get(self):
        """Get the current item."""
        return self.constructor(
            self.collection.world,
            self.iter_get(self.collection.collection, self.iterator),
        )

    def next(self):
        """Move to and return the next item."""
        if self.is_end():
            raise StopIteration
        elem = self.get()
        self.iterator = self.iter_next(
            self.collection.collection, self.iterator
        )
        return elem

    def is_end(self):
        """Return true if the end of the collection has been reached."""
        return self.iter_is_end(self.collection.collection, self.iterator)

    __next__ = next


class Collection(Structure):
    # Base class for all lilv collection wrappers.
    def __init__(
        self,
        world,
        collection,
        iter_begin,
        constructor,
        iter_get,
        iter_next,
        is_end,
    ):
        assert isinstance(world, World)
        self.world = world
        self.collection = collection
        self.constructor = constructor
        self.iter_begin = iter_begin
        self.iter_get = iter_get
        self.iter_next = iter_next
        self.is_end = is_end

    def __iter__(self):
        return Iter(
            self,
            self.iter_begin(self.collection),
            self.constructor,
            self.iter_get,
            self.iter_next,
            self.is_end,
        )

    def __getitem__(self, index):
        pos = 0
        it = self.iter_begin(self.collection)

        while not self.is_end(self.collection, it):
            if pos == index:
                return self.constructor(
                    self.world, self.iter_get(self.collection, it)
                )

            it = self.iter_next(self.collection, it)
            pos = pos + 1

        raise IndexError(index)

    def begin(self):
        return self.__iter__()

    def get(self, iterator):
        return iterator.get()


class Plugins(Collection):
    """Collection of plugins."""

    def __init__(self, world, collection):
        assert type(collection) == POINTER(Plugins)
        assert collection

        def constructor(world, plugin):
            return Plugin.wrap(world, plugin)

        super(Plugins, self).__init__(
            world,
            collection,
            c.plugins_begin,
            constructor,
            c.plugins_get,
            c.plugins_next,
            c.plugins_is_end,
        )
        self.world = world

    def __contains__(self, key):
        return bool(self.get_by_uri(_as_uri(key)))

    def __len__(self):
        return c.plugins_size(self.collection)

    def __getitem__(self, key):
        if type(key) == int:
            return super(Plugins, self).__getitem__(key)

        plugin = self.get_by_uri(key)
        if plugin is None:
            raise KeyError("Plugin not found: " + str(key))

        return plugin

    def get_by_uri(self, uri):
        if type(uri) == str:
            uri = self.world.new_uri(uri)

        return Plugin.wrap(
            self.world, c.plugins_get_by_uri(self.collection, uri.node)
        )


class PluginClasses(Collection):
    """Collection of plugin classes."""

    def __init__(self, world, collection, owning=False):
        assert type(collection) == POINTER(PluginClasses)
        assert collection

        self.owning = owning
        super(PluginClasses, self).__init__(
            world,
            collection,
            c.plugin_classes_begin,
            PluginClass,
            c.plugin_classes_get,
            c.plugin_classes_next,
            c.plugin_classes_is_end,
        )

    def __del__(self):
        if self.owning:
            c.plugin_classes_free(self.collection)

    def __contains__(self, key):
        return bool(self.get_by_uri(_as_uri(key)))

    def __len__(self):
        return c.plugin_classes_size(self.collection)

    def __getitem__(self, key):
        if type(key) == int:
            return super(PluginClasses, self).__getitem__(key)

        klass = self.get_by_uri(key)
        if klass is None:
            raise KeyError("Plugin class not found: " + str(key))

        return klass

    def get_by_uri(self, uri):
        if type(uri) == str:
            uri = self.world.new_uri(uri)

        plugin_class = c.plugin_classes_get_by_uri(self.collection, uri.node)
        return PluginClass(self.world, plugin_class) if plugin_class else None


class ScalePoints(Structure):
    """Collection of scale points."""

    pass


class UIs(Collection):
    """Collection of plugin UIs."""

    def __init__(self, world, collection):
        assert type(collection) == POINTER(UIs)
        assert collection
        super(UIs, self).__init__(
            world,
            collection,
            c.uis_begin,
            UI,
            c.uis_get,
            c.uis_next,
            c.uis_is_end,
        )

    def __del__(self):
        if self.world.world:
            c.uis_free(self.collection)

    def __contains__(self, uri):
        return bool(self.get_by_uri(_as_uri(uri)))

    def __len__(self):
        return c.uis_size(self.collection)

    def __getitem__(self, key):
        if type(key) == int:
            return super(UIs, self).__getitem__(key)

        ui = self.get_by_uri(key)
        if ui is None:
            raise KeyError("Plugin UI not found: " + str(key))

        return ui

    def get_by_uri(self, uri):
        if type(uri) == str:
            uri = self.world.new_uri(uri)

        ui = c.uis_get_by_uri(self.collection, uri.node)
        return UI(self.world, ui) if ui else None


class Nodes(Collection):
    """Collection of data nodes."""

    @classmethod
    def constructor(cls, world, node):
        assert isinstance(world, World)
        assert type(node) == POINTER(Node)
        return Node.wrap(world, c.node_duplicate(node))

    def __init__(self, world, collection, owning=False):
        assert type(collection) == POINTER(Nodes)

        self.owning = owning
        super(Nodes, self).__init__(
            world,
            collection,
            c.nodes_begin,
            Nodes.constructor,
            c.nodes_get,
            c.nodes_next,
            c.nodes_is_end,
        )

    def __del__(self):
        if self.owning and self.world.world:
            c.nodes_free(self.collection)

    def __contains__(self, value):
        return c.nodes_contains(self.collection, value.node)

    def __len__(self):
        return c.nodes_size(self.collection)

    def merge(self, b):
        return Nodes(
            self.world, c.nodes_merge(self.collection, b.collection), True
        )


class Namespace:
    """Namespace prefix.

    Use attribute syntax to easily create URIs within this namespace, for
    example::

       >>> world = lilv.World()
       >>> ns = Namespace(world, "http://example.org/")
       >>> print(ns.foo)
       http://example.org/foo
    """

    def __init__(self, world, prefix):
        assert isinstance(world, World)
        assert type(prefix) == str

        self.world = world
        self.prefix = prefix

    def __eq__(self, other):
        return str(self) == str(other)

    def __str__(self):
        return self.prefix

    def __getattr__(self, suffix):
        return self.world.new_uri(self.prefix + suffix)


class Namespaces:
    """Set of namespaces.

    Use to easily construct uris, like: ns.lv2.InputPort"""

    def __init__(self, world):
        assert isinstance(world, World)
        self.world = world
        self.atom = Namespace(world, "http://lv2plug.in/ns/ext/atom#")
        self.doap = Namespace(world, "http://usefulinc.com/ns/doap#")
        self.foaf = Namespace(world, "http://xmlns.com/foaf/0.1/")
        self.lilv = Namespace(world, "http://drobilla.net/ns/lilv#")
        self.lv2 = Namespace(world, "http://lv2plug.in/ns/lv2core#")
        self.midi = Namespace(world, "http://lv2plug.in/ns/ext/midi#")
        self.owl = Namespace(world, "http://www.w3.org/2002/07/owl#")
        self.rdf = Namespace(
            world, "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        )
        self.rdfs = Namespace(world, "http://www.w3.org/2000/01/rdf-schema#")
        self.ui = Namespace(world, "http://lv2plug.in/ns/extensions/ui#")
        self.xsd = Namespace(world, "http://www.w3.org/2001/XMLSchema#")


class World(Structure):
    """Library context.

    Includes a set of namespaces as the instance variable `ns`, so URIs can be
    constructed like::

        uri = world.ns.lv2.Plugin

    Common LV2 namespace prefixes: atom, doap, foaf, lilv, lv2, midi, owl, rdf,
    rdfs, ui, xsd.
    """

    def __init__(self):
        self.world = c.world_new()
        self.ns = Namespaces(self)

    def __del__(self):
        c.world_free(self.world)
        self.world = None

    def set_option(self, uri, value):
        """Set a world option.

        Currently recognized options:
        lilv.OPTION_FILTER_LANG
        lilv.OPTION_DYN_MANIFEST
        """
        return c.world_set_option(self.world, uri, value.node)

    def load_all(self):
        """Load all installed LV2 bundles on the system.

        This is the recommended way for hosts to load LV2 data.  It implements
        the established/standard best practice for discovering all LV2 data on
        the system.  The environment variable LV2_PATH may be used to control
        where this function will look for bundles.

        Hosts should use this function rather than explicitly load bundles,
        except in special circumstances (e.g. development utilities, or hosts
        that ship with special plugin bundles which are installed to a known
        location).
        """
        c.world_load_all(self.world)

    def load_bundle(self, bundle_uri):
        """Load a specific bundle.

        `bundle_uri` must be a fully qualified URI to the bundle directory,
        with the trailing slash, eg. file:///usr/lib/lv2/foo.lv2/

        Normal hosts should not need this function (use load_all()).

        Hosts MUST NOT attach any long-term significance to bundle paths
        (e.g. in save files), since there are no guarantees they will remain
        unchanged between (or even during) program invocations. Plugins (among
        other things) MUST be identified by URIs (not paths) in save files.
        """
        c.world_load_bundle(self.world, bundle_uri.node)

    def load_specifications(self):
        """Load all specifications from currently loaded bundles.

        This is for hosts that explicitly load specific bundles, its use is not
        necessary when using load_all().  This function parses the
        specifications and adds them to the model.
        """
        c.world_load_specifications(self.world)

    def load_plugin_classes(self):
        """Load all plugin classes from currently loaded specifications.

        Must be called after load_specifications().  This is for hosts that
        explicitly load specific bundles, its use is not necessary when using
        load_all().
        """
        c.world_load_plugin_classes(self.world)

    def unload_bundle(self, bundle_uri):
        """Unload a specific bundle.

        This unloads statements loaded by load_bundle().  Note that this is not
        necessarily all information loaded from the bundle.  If any resources
        have been separately loaded with load_resource(), they must be
        separately unloaded with unload_resource().
        """
        return c.world_unload_bundle(self.world, bundle_uri.node)

    def load_resource(self, resource):
        """Load all the data associated with the given `resource`.

        The resource must be a subject (i.e. a URI or a blank node).
        Returns the number of files parsed, or -1 on error.

        All accessible data files linked to `resource` with rdfs:seeAlso will
        be loaded into the world model.
        """
        uri = _as_uri(resource)
        ret = c.world_load_resource(self.world, uri.node)
        return ret

    def unload_resource(self, resource):
        """Unload all the data associated with the given `resource`.

        The resource must be a subject (i.e. a URI or a blank node).

        This unloads all data loaded by a previous call to
        load_resource() with the given `resource`.
        """
        uri = _as_uri(resource)
        ret = c.world_unload_resource(self.world, uri.node)
        return ret

    def get_plugin_class(self):
        """Get the parent of all other plugin classes, lv2:Plugin."""
        return PluginClass(self, c.world_get_plugin_class(self.world))

    def get_plugin_classes(self):
        """Return a list of all found plugin classes."""
        return PluginClasses(self, c.world_get_plugin_classes(self.world))

    def get_all_plugins(self):
        """Return a list of all found plugins.

        The returned list contains just enough references to query
        or instantiate plugins.  The data for a particular plugin will not be
        loaded into memory until a call to an lilv_plugin_* function results in
        a query (at which time the data is cached with the LilvPlugin so future
        queries are very fast).

        The returned list and the plugins it contains are owned by `world`
        and must not be freed by caller.
        """
        return Plugins(self, c.world_get_all_plugins(self.world))

    def find_nodes(self, subject, predicate, obj):
        """Find nodes matching a triple pattern.

        Either `subject` or `object` may be None (i.e. a wildcard), but not
        both.  Returns all matches for the wildcard field, or None.
        """
        return Nodes(
            self,
            c.world_find_nodes(
                self.world,
                subject.node if subject is not None else None,
                predicate.node if predicate is not None else None,
                obj.node if obj is not None else None,
            ),
            True,
        )

    def get(self, subject, predicate, obj):
        """Find a single node that matches a pattern.

        Exactly one of `subject`, `predicate`, `object` must be None.

        Returns the first matching node, or None if no matches are found.
        """
        return Node.wrap(
            self,
            c.world_get(
                self.world,
                subject.node if subject is not None else None,
                predicate.node if predicate is not None else None,
                obj.node if obj is not None else None,
            ),
        )

    def ask(self, subject, predicate, obj):
        """Return true iff a statement matching a certain pattern exists.

        This is useful for checking if particular statement exists without
        having to bother with collections and memory management.
        """
        return c.world_ask(
            self.world,
            subject.node if subject is not None else None,
            predicate.node if predicate is not None else None,
            obj.node if obj is not None else None,
        )

    def get_symbol(self, subject):
        """Get an LV2 symbol for some subject.

        This will return the lv2:symbol property of the subject if it is given
        explicitly, and otherwise will attempt to derive a symbol from the URI.

        Returns a string, which is possibly empty on error.
        """
        if isinstance(subject, Port):
            return subject.get_symbol()

        uri = _as_uri(subject)
        ret = ""
        if uri is not None:
            node = c.world_get_symbol(self.world, uri.node)
            ret = c.node_as_string(node).decode("ascii") if node else ""
            c.node_free(node)

        return ret

    def new_uri(self, uri):
        """Create a new URI node."""
        c_node = c.new_uri(self.world, uri)
        if not c_node:
            raise ValueError("Invalid URI '%s'" % uri)

        return Node.wrap(self, c_node)

    def new_file_uri(self, host, path):
        """Create a new file URI node.  The host may be None."""
        return Node.wrap(self, c.new_file_uri(self.world, host, path))

    def new_string(self, string):
        """Create a new string node."""
        return Node.wrap(self, c.new_string(self.world, string))

    def new_int(self, val):
        """Create a new int node."""
        return Node.wrap(self, c.new_int(self.world, val))

    def new_float(self, val):
        """Create a new float node."""
        return Node.wrap(self, c.new_float(self.world, val))

    def new_bool(self, val):
        """Create a new bool node."""
        return Node.wrap(self, c.new_bool(self.world, val))


class Instance(Structure):
    """Plugin instance."""

    __slots__ = [
        "lv2_descriptor",
        "lv2_handle",
        "pimpl",
        "plugin",
        "rate",
        "instance",
    ]
    _fields_ = [
        ("lv2_descriptor", POINTER(LV2_Descriptor)),
        ("lv2_handle", LV2_Handle),
        ("pimpl", POINTER(None)),
    ]

    def __init__(self, plugin, rate, features=None):
        assert isinstance(plugin, Plugin)
        self.plugin = plugin
        self.rate = rate
        self.instance = c.plugin_instantiate(plugin.plugin, rate, features)

    def __del__(self):
        if hasattr(self, "instance"):
            c.instance_free(self.instance[0])

    def get_uri(self):
        """Get the URI of the plugin which `instance` is an instance of.

           Returned string is shared and must not be modified or deleted.
        """
        return self.get_descriptor().URI.decode("utf-8")

    def connect_port(self, port_index, data):
        """Connect a port to a data location.

           This may be called regardless of whether the plugin is activated,
           activation and deactivation does not destroy port connections.
        """
        import numpy

        if data is None:
            self.get_descriptor().connect_port(
                self.get_handle(), port_index, data
            )
        elif type(data) == numpy.ndarray:
            self.get_descriptor().connect_port(
                self.get_handle(),
                port_index,
                data.ctypes.data_as(POINTER(c_float)),
            )
        else:
            raise Exception("Unsupported data type")

    def activate(self):
        """Activate a plugin instance.

        This resets all state information in the plugin, except for port data
        locations (as set by connect_port()).  This MUST be called before
        calling run().
        """
        if self.get_descriptor().activate:
            self.get_descriptor().activate(self.get_handle())

    def run(self, sample_count):
        """Run `instance` for `sample_count` frames.

        If the hint lv2:hardRTCapable is set for this plugin, this function is
        guaranteed not to block.
        """
        self.get_descriptor().run(self.get_handle(), sample_count)

    def deactivate(self):
        """Deactivate a plugin instance.

        Note that to run the plugin after this you must activate it, which will
        reset all state information (except port connections).
        """
        if self.get_descriptor().deactivate:
            self.get_descriptor().deactivate(self.get_handle())

    def get_extension_data(self, uri):
        """Get extension data from the plugin instance.

        The type and semantics of the data returned is specific to the
        particular extension, though in all cases it is shared and must not be
        deleted.
        """
        if self.get_descriptor().extension_data:
            return self.get_descriptor().extension_data(
                str(uri).encode("utf-8")
            )

    def get_descriptor(self):
        """Get the LV2_Descriptor of the plugin instance.

        Normally hosts should not need to access the LV2_Descriptor directly,
        use the lilv_instance_* functions.
        """
        return self.instance[0].lv2_descriptor[0]

    def get_handle(self):
        """Get the LV2_Handle of the plugin instance.

        Normally hosts should not need to access the LV2_Handle directly, use
        the lilv_instance_* functions.
        """
        return self.instance[0].lv2_handle


class State(Structure):
    """Plugin state (TODO)."""

    pass


class VariadicFunction(object):
    # Wrapper for calling C variadic functions
    def __init__(self, function, restype, argtypes):
        self.function = function
        self.function.restype = restype
        self.argtypes = argtypes

    def __call__(self, *args):
        fixed_args = []
        i = 0
        for argtype in self.argtypes:
            fixed_args.append(argtype.from_param(args[i]))
            i += 1
        return self.function(*(fixed_args + list(args[i:])))


# Set up C bindings


class String(str):
    # Wrapper for string parameters to pass as raw C UTF-8 strings
    def from_param(cls, obj):
        assert isinstance(obj, str)
        return obj.encode("utf-8")

    from_param = classmethod(from_param)


def _cfunc(name, restype, *argtypes):
    """Set the `name` attribute of the `c` global to a C function"""
    assert isinstance(c, _LilvLib)
    f = getattr(c.lib, "lilv_" + name)
    f.restype = restype
    f.argtypes = argtypes
    setattr(c, name, f)


def P(x):
    """Shorthand for ctypes.POINTER"""
    return POINTER(x)


_cfunc("free", None, c_void_p)

# Node

_cfunc("file_uri_parse", c_char_p, String, P(c_char_p))
_cfunc("new_uri", P(Node), P(World), String)
_cfunc("new_file_uri", P(Node), P(World), c_char_p, String)
_cfunc("new_string", P(Node), P(World), String)
_cfunc("new_int", P(Node), P(World), c_int)
_cfunc("new_float", P(Node), P(World), c_float)
_cfunc("new_bool", P(Node), P(World), c_bool)
_cfunc("node_free", None, P(Node))
_cfunc("node_duplicate", P(Node), P(Node))
_cfunc("node_equals", c_bool, P(Node), P(Node))
_cfunc("node_get_turtle_token", P(c_char), P(Node))
_cfunc("node_is_uri", c_bool, P(Node))
_cfunc("node_as_uri", c_char_p, P(Node))
_cfunc("node_is_blank", c_bool, P(Node))
_cfunc("node_as_blank", c_char_p, P(Node))
_cfunc("node_is_literal", c_bool, P(Node))
_cfunc("node_is_string", c_bool, P(Node))
_cfunc("node_as_string", c_char_p, P(Node))
_cfunc("node_get_path", P(c_char), P(Node), P(P(c_char)))
_cfunc("node_is_float", c_bool, P(Node))
_cfunc("node_as_float", c_float, P(Node))
_cfunc("node_is_int", c_bool, P(Node))
_cfunc("node_as_int", c_int, P(Node))
_cfunc("node_is_bool", c_bool, P(Node))
_cfunc("node_as_bool", c_bool, P(Node))

# Collections

_cfunc("plugin_classes_free", None, P(PluginClasses))
_cfunc("plugin_classes_size", c_uint, P(PluginClasses))
_cfunc("plugin_classes_begin", P(Iter), P(PluginClasses))
_cfunc("plugin_classes_get", P(PluginClass), P(PluginClasses), P(Iter))
_cfunc("plugin_classes_next", P(Iter), P(PluginClasses), P(Iter))
_cfunc("plugin_classes_is_end", c_bool, P(PluginClasses), P(Iter))
_cfunc("plugin_classes_get_by_uri", P(PluginClass), P(PluginClasses), P(Node))
_cfunc("scale_points_free", None, P(ScalePoints))
_cfunc("scale_points_size", c_uint, P(ScalePoints))
_cfunc("scale_points_begin", P(Iter), P(ScalePoints))
_cfunc("scale_points_get", P(ScalePoint), P(ScalePoints), P(Iter))
_cfunc("scale_points_next", P(Iter), P(ScalePoints), P(Iter))
_cfunc("scale_points_is_end", c_bool, P(ScalePoints), P(Iter))
_cfunc("uis_free", None, P(UIs))
_cfunc("uis_size", c_uint, P(UIs))
_cfunc("uis_begin", P(Iter), P(UIs))
_cfunc("uis_get", P(UI), P(UIs), P(Iter))
_cfunc("uis_next", P(Iter), P(UIs), P(Iter))
_cfunc("uis_is_end", c_bool, P(UIs), P(Iter))
_cfunc("uis_get_by_uri", P(UI), P(UIs), P(Node))
_cfunc("nodes_free", None, P(Nodes))
_cfunc("nodes_size", c_uint, P(Nodes))
_cfunc("nodes_begin", P(Iter), P(Nodes))
_cfunc("nodes_get", P(Node), P(Nodes), P(Iter))
_cfunc("nodes_next", P(Iter), P(Nodes), P(Iter))
_cfunc("nodes_is_end", c_bool, P(Nodes), P(Iter))
_cfunc("nodes_get_first", P(Node), P(Nodes))
_cfunc("nodes_contains", c_bool, P(Nodes), P(Node))
_cfunc("nodes_merge", P(Nodes), P(Nodes), P(Nodes))
_cfunc("plugins_size", c_uint, P(Plugins))
_cfunc("plugins_begin", P(Iter), P(Plugins))
_cfunc("plugins_get", P(Plugin), P(Plugins), P(Iter))
_cfunc("plugins_next", P(Iter), P(Plugins), P(Iter))
_cfunc("plugins_is_end", c_bool, P(Plugins), P(Iter))
_cfunc("plugins_get_by_uri", P(Plugin), P(Plugins), P(Node))

# World

_cfunc("world_new", P(World))
_cfunc("world_set_option", None, P(World), String, P(Node))
_cfunc("world_free", None, P(World))
_cfunc("world_load_all", None, P(World))
_cfunc("world_load_bundle", None, P(World), P(Node))
_cfunc("world_load_specifications", None, P(World))
_cfunc("world_load_plugin_classes", None, P(World))
_cfunc("world_unload_bundle", c_int, P(World), P(Node))
_cfunc("world_load_resource", c_int, P(World), P(Node))
_cfunc("world_unload_resource", c_int, P(World), P(Node))
_cfunc("world_get_plugin_class", P(PluginClass), P(World))
_cfunc("world_get_plugin_classes", P(PluginClasses), P(World))
_cfunc("world_get_all_plugins", P(Plugins), P(World))
_cfunc("world_find_nodes", P(Nodes), P(World), P(Node), P(Node), P(Node))
_cfunc("world_get", P(Node), P(World), P(Node), P(Node), P(Node))
_cfunc("world_ask", c_bool, P(World), P(Node), P(Node), P(Node))
_cfunc("world_get_symbol", P(Node), P(World), P(Node))

# Plugin

_cfunc("plugin_verify", c_bool, P(Plugin))
_cfunc("plugin_get_uri", P(Node), P(Plugin))
_cfunc("plugin_get_bundle_uri", P(Node), P(Plugin))
_cfunc("plugin_get_data_uris", P(Nodes), P(Plugin))
_cfunc("plugin_get_library_uri", P(Node), P(Plugin))
_cfunc("plugin_get_name", P(Node), P(Plugin))
_cfunc("plugin_get_class", P(PluginClass), P(Plugin))
_cfunc("plugin_get_value", P(Nodes), P(Plugin), P(Node))
_cfunc("plugin_has_feature", c_bool, P(Plugin), P(Node))
_cfunc("plugin_get_supported_features", P(Nodes), P(Plugin))
_cfunc("plugin_get_required_features", P(Nodes), P(Plugin))
_cfunc("plugin_get_optional_features", P(Nodes), P(Plugin))
_cfunc("plugin_has_extension_data", c_bool, P(Plugin), P(Node))
_cfunc("plugin_get_extension_data", P(Nodes), P(Plugin))
_cfunc("plugin_get_num_ports", c_uint32, P(Plugin))

c.plugin_get_num_ports_of_class = VariadicFunction(
    c.lib.lilv_plugin_get_num_ports_of_class, c_uint32, [P(Plugin), P(Node)]
)

_cfunc("plugin_has_latency", c_bool, P(Plugin))
_cfunc("plugin_get_latency_port_index", c_uint32, P(Plugin))
_cfunc("plugin_get_port_by_index", P(Port), P(Plugin), c_uint32)
_cfunc("plugin_get_port_by_symbol", P(Port), P(Plugin), P(Node))
_cfunc("plugin_get_port_by_designation", P(Port), P(Plugin), P(Node), P(Node))
_cfunc("plugin_get_project", P(Node), P(Plugin))
_cfunc("plugin_get_author_name", P(Node), P(Plugin))
_cfunc("plugin_get_author_email", P(Node), P(Plugin))
_cfunc("plugin_get_author_homepage", P(Node), P(Plugin))
_cfunc("plugin_is_replaced", c_bool, P(Plugin))
_cfunc("plugin_get_related", P(Nodes), P(Plugin), P(Node))

# Port

_cfunc("port_get_node", P(Node), P(Plugin), P(Port))
_cfunc("port_get_value", P(Nodes), P(Plugin), P(Port), P(Node))
_cfunc("port_get", P(Node), P(Plugin), P(Port), P(Node))
_cfunc("port_get_properties", P(Nodes), P(Plugin), P(Port))
_cfunc("port_has_property", c_bool, P(Plugin), P(Port), P(Node))
_cfunc("port_supports_event", c_bool, P(Plugin), P(Port), P(Node))
_cfunc("port_get_index", c_uint32, P(Plugin), P(Port))
_cfunc("port_get_symbol", P(Node), P(Plugin), P(Port))
_cfunc("port_get_name", P(Node), P(Plugin), P(Port))
_cfunc("port_get_classes", P(Nodes), P(Plugin), P(Port))
_cfunc("port_is_a", c_bool, P(Plugin), P(Port), P(Node))

_cfunc(
    "port_get_range",
    None,
    P(Plugin),
    P(Port),
    P(P(Node)),
    P(P(Node)),
    P(P(Node)),
)

_cfunc("port_get_scale_points", P(ScalePoints), P(Plugin), P(Port))

# Plugin State

_cfunc("state_new_from_world", P(State), P(World), P(LV2_URID_Map), P(Node))

_cfunc(
    "state_new_from_file", P(State), P(World), P(LV2_URID_Map), P(Node), String
)

_cfunc("state_new_from_string", P(State), P(World), P(LV2_URID_Map), String)

LilvGetPortValueFunc = CFUNCTYPE(
    c_void_p, c_char_p, P(None), P(c_uint32), P(c_uint32)
)

_cfunc(
    "state_new_from_instance",
    P(State),
    P(Plugin),
    P(Instance),
    P(LV2_URID_Map),
    c_char_p,
    c_char_p,
    c_char_p,
    String,
    LilvGetPortValueFunc,
    P(None),
    c_uint32,
    P(P(LV2_Feature)),
)

_cfunc("state_free", None, P(State))
_cfunc("state_equals", c_bool, P(State), P(State))
_cfunc("state_get_num_properties", c_uint, P(State))
_cfunc("state_get_plugin_uri", P(Node), P(State))
_cfunc("state_get_uri", P(Node), P(State))
_cfunc("state_get_label", c_char_p, P(State))
_cfunc("state_set_label", None, P(State), String)

_cfunc(
    "state_set_metadata",
    c_int,
    P(State),
    c_uint32,
    P(None),
    c_size_t,
    c_uint32,
    c_uint32,
)

LilvSetPortValueFunc = CFUNCTYPE(
    None, c_char_p, P(None), P(None), c_uint32, c_uint32
)
_cfunc("state_emit_port_values", None, P(State), LilvSetPortValueFunc, P(None))

_cfunc(
    "state_restore",
    None,
    P(State),
    P(Instance),
    LilvSetPortValueFunc,
    P(None),
    c_uint32,
    P(P(LV2_Feature)),
)

_cfunc(
    "state_save",
    c_int,
    P(World),
    P(LV2_URID_Map),
    P(LV2_URID_Unmap),
    P(State),
    c_char_p,
    c_char_p,
    String,
)

_cfunc(
    "state_to_string",
    c_char_p,
    P(World),
    P(LV2_URID_Map),
    P(LV2_URID_Unmap),
    P(State),
    c_char_p,
    String,
)

_cfunc("state_delete", c_int, P(World), P(State))

# Scale Point

_cfunc("scale_point_get_label", P(Node), P(ScalePoint))
_cfunc("scale_point_get_value", P(Node), P(ScalePoint))

# Plugin Class

_cfunc("plugin_class_get_parent_uri", P(Node), P(PluginClass))
_cfunc("plugin_class_get_uri", P(Node), P(PluginClass))
_cfunc("plugin_class_get_label", P(Node), P(PluginClass))
_cfunc("plugin_class_get_children", P(PluginClasses), P(PluginClass))

# Plugin Instance

_cfunc(
    "plugin_instantiate", P(Instance), P(Plugin), c_double, P(P(LV2_Feature))
)

_cfunc("instance_free", None, P(Instance))
_cfunc("plugin_get_uis", P(UIs), P(Plugin))

# Plugin UI

_cfunc("ui_get_uri", P(Node), P(UI))
_cfunc("ui_get_classes", P(Nodes), P(UI))
_cfunc("ui_is_a", c_bool, P(UI), P(Node))

LilvUISupportedFunc = CFUNCTYPE(c_uint, c_char_p, c_char_p)

_cfunc(
    "ui_is_supported", c_uint, P(UI), LilvUISupportedFunc, P(Node), P(P(Node))
)

_cfunc("ui_get_bundle_uri", P(Node), P(UI))
_cfunc("ui_get_binary_uri", P(Node), P(UI))

# Define URI constants for compatibility with old Python bindings

LILV_NS_DOAP = "http://usefulinc.com/ns/doap#"
LILV_NS_FOAF = "http://xmlns.com/foaf/0.1/"
LILV_NS_LILV = "http://drobilla.net/ns/lilv#"
LILV_NS_LV2 = "http://lv2plug.in/ns/lv2core#"
LILV_NS_OWL = "http://www.w3.org/2002/07/owl#"
LILV_NS_RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
LILV_NS_RDFS = "http://www.w3.org/2000/01/rdf-schema#"
LILV_NS_XSD = "http://www.w3.org/2001/XMLSchema#"
LILV_URI_ATOM_PORT = "http://lv2plug.in/ns/ext/atom#AtomPort"
LILV_URI_AUDIO_PORT = "http://lv2plug.in/ns/lv2core#AudioPort"
LILV_URI_CONTROL_PORT = "http://lv2plug.in/ns/lv2core#ControlPort"
LILV_URI_CV_PORT = "http://lv2plug.in/ns/lv2core#CVPort"
LILV_URI_EVENT_PORT = "http://lv2plug.in/ns/ext/event#EventPort"
LILV_URI_INPUT_PORT = "http://lv2plug.in/ns/lv2core#InputPort"
LILV_URI_MIDI_EVENT = "http://lv2plug.in/ns/ext/midi#MidiEvent"
LILV_URI_OUTPUT_PORT = "http://lv2plug.in/ns/lv2core#OutputPort"
LILV_URI_PORT = "http://lv2plug.in/ns/lv2core#Port"
LILV_OPTION_FILTER_LANG = "http://drobilla.net/ns/lilv#filter-lang"
LILV_OPTION_DYN_MANIFEST = "http://drobilla.net/ns/lilv#dyn-manifest"
