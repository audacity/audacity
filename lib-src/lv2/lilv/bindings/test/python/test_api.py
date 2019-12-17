# Copyright 2016-2019 David Robillard <d@drobilla.net>
# Copyright 2013 Kaspar Emanuel <kaspar.emanuel@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

import lilv
import os
import sys
import unittest

path = os.path.abspath("bindings/bindings_test_plugin.lv2/")

if sys.version_info[0] == 2:
    import urllib
    import urlparse

    location = urlparse.urljoin("file:", urllib.pathname2url(path) + "/")
else:
    from urllib.parse import urljoin
    from urllib.request import pathname2url

    location = urljoin("file:", pathname2url(path) + "/")


class NodeTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()

    def testNodes(self):
        aint = self.world.new_int(1)
        aint2 = self.world.new_int(1)
        aint3 = self.world.new_int(3)
        afloat = self.world.new_float(2.0)
        atrue = self.world.new_bool(True)
        afalse = self.world.new_bool(False)
        auri = self.world.new_uri("http://example.org")
        afile = self.world.new_file_uri(None, "/foo/bar")
        astring = self.world.new_string("hello")
        self.assertEqual(auri.get_turtle_token(), "<http://example.org>")
        self.assertTrue(aint.is_int())
        self.assertTrue(afloat.is_float())
        self.assertTrue(auri.is_uri())
        self.assertTrue(astring.is_string())
        self.assertTrue(astring.is_literal())
        self.assertFalse(auri.is_blank())
        self.assertTrue(int(aint) == 1)
        self.assertTrue(float(afloat) == 2.0)
        self.assertTrue(bool(atrue))
        self.assertFalse(bool(afalse))
        self.assertEqual(afile.get_path(), "/foo/bar")
        self.assertTrue(aint == aint2)
        self.assertTrue(aint != aint3)
        self.assertTrue(aint != afloat)
        with self.assertRaises(ValueError):
            int(atrue)
        with self.assertRaises(ValueError):
            float(aint)
        with self.assertRaises(ValueError):
            bool(astring)


class UriTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.world.load_all()

    def testInvalidURI(self):
        with self.assertRaises(ValueError):
            self.plugin_uri = self.world.new_uri("invalid_uri")

    def testNonExistentURI(self):
        self.plugin_uri = self.world.new_uri("exist:does_not")
        self.plugin = self.world.get_all_plugins().get_by_uri(self.plugin_uri)
        self.assertEqual(self.plugin, None)

    def testPortTypes(self):
        self.assertIsNotNone(self.world.new_uri(lilv.LILV_URI_INPUT_PORT))

    def testPortTypes2(self):
        self.assertIsNotNone(self.world.new_uri(lilv.LILV_URI_OUTPUT_PORT))

    def testPortTypes3(self):
        self.assertIsNotNone(self.world.new_uri(lilv.LILV_URI_AUDIO_PORT))

    def testPortTypes4(self):
        self.assertIsNotNone(self.world.new_uri(lilv.LILV_URI_CONTROL_PORT))


class PluginClassTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()

    def testPluginClasses(self):
        pclass = self.world.get_plugin_class()
        self.assertIsNotNone(pclass)
        self.assertIsNone(pclass.get_parent_uri())
        self.assertIsNotNone(pclass.get_uri())
        self.assertIsNotNone(pclass.get_label())
        self.assertEqual(str(pclass.get_uri()), str(pclass))
        for i in pclass.get_children():
            self.assertIsNotNone(i)
            self.assertIsNotNone(i.get_uri())
            self.assertIsNotNone(i.get_label())


class PluginClassesTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.world.load_all()

    def testPluginClasses(self):
        classes = self.world.get_plugin_classes()
        pclass = self.world.get_plugin_class()
        self.assertIsNotNone(classes)
        self.assertIsNotNone(pclass)
        self.assertTrue(pclass in classes)
        self.assertTrue(pclass.get_uri() in classes)
        self.assertGreater(len(classes), 1)
        self.assertIsNotNone(classes[0])
        self.assertIsNotNone(classes[pclass.get_uri()])
        with self.assertRaises(KeyError):
            classes["http://example.org/notaclass"].get_uri()


class LoadTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.bundle_uri = self.world.new_uri(location)
        self.world.load_specifications()
        self.world.load_plugin_classes()

    def testLoadUnload(self):
        self.world.load_bundle(self.bundle_uri)
        plugins = self.world.get_all_plugins()
        plugin = plugins.get(plugins.begin())
        self.world.load_resource(plugin)
        self.world.unload_resource(plugin)
        self.world.unload_bundle(self.bundle_uri)


class PluginTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.world.set_option(
            lilv.OPTION_FILTER_LANG, self.world.new_bool(True)
        )
        self.bundle_uri = self.world.new_uri(location)
        self.assertIsNotNone(
            self.bundle_uri, "Invalid URI: '" + location + "'"
        )
        self.world.load_bundle(self.bundle_uri)
        self.plugins = self.world.get_all_plugins()
        self.plugin = self.plugins.get(self.plugins.begin())
        self.assertTrue(self.plugin.verify())
        self.assertTrue(self.plugin in self.plugins)
        self.assertTrue(self.plugin.get_uri() in self.plugins)
        self.assertEqual(self.plugins[self.plugin.get_uri()], self.plugin)
        with self.assertRaises(KeyError):
            self.plugins["http://example.org/notaplugin"].get_uri()

        self.assertIsNotNone(
            self.plugin,
            msg="Test plugin not found at location: '" + location + "'",
        )
        self.assertEqual(location, str(self.plugin.get_bundle_uri()))
        self.plugin_uri = self.plugin.get_uri()
        self.assertEqual(
            self.plugin.get_uri(), self.plugin_uri, "URI equality broken"
        )
        self.lv2_InputPort = self.world.new_uri(lilv.LILV_URI_INPUT_PORT)
        self.lv2_OutputPort = self.world.new_uri(lilv.LILV_URI_OUTPUT_PORT)
        self.lv2_AudioPort = self.world.new_uri(lilv.LILV_URI_AUDIO_PORT)
        self.lv2_ControlPort = self.world.new_uri(lilv.LILV_URI_CONTROL_PORT)

    def testGetters(self):
        self.assertEqual(
            self.world.get_symbol(self.plugin), "lilv_bindings_test_plugin"
        )
        self.assertIsNotNone(self.plugin.get_bundle_uri())
        self.assertGreater(len(self.plugin.get_data_uris()), 0)
        self.assertIsNotNone(self.plugin.get_library_uri())
        self.assertTrue(self.plugin.get_name().is_string())
        self.assertTrue(self.plugin.get_class().get_uri().is_uri())
        self.assertEqual(
            len(self.plugin.get_value(self.world.ns.doap.license)), 1
        )
        licenses = self.plugin.get_value(self.world.ns.doap.license)
        features = self.plugin.get_value(self.world.ns.lv2.optionalFeature)
        self.assertEqual(len(licenses), 1)
        self.assertTrue(licenses[0] in licenses)
        with self.assertRaises(IndexError):
            self.assertIsNone(licenses[len(licenses)])
        self.assertEqual(
            len(licenses) + len(features), len(licenses.merge(features))
        )
        self.assertEqual(
            licenses.get(licenses.begin()),
            self.world.new_uri("http://opensource.org/licenses/isc"),
        )
        self.assertEqual(licenses[0], licenses.get(licenses.begin()))
        self.assertTrue(
            self.plugin.has_feature(self.world.ns.lv2.hardRTCapable)
        )
        self.assertEqual(len(self.plugin.get_supported_features()), 1)
        self.assertEqual(len(self.plugin.get_optional_features()), 1)
        self.assertEqual(len(self.plugin.get_required_features()), 0)
        self.assertFalse(
            self.plugin.has_extension_data(
                self.world.new_uri("http://example.org/nope")
            )
        )
        self.assertEqual(len(self.plugin.get_extension_data()), 0)
        self.assertEqual(len(self.plugin.get_extension_data()), 0)
        self.assertFalse(self.plugin.has_latency())
        self.assertIsNone(self.plugin.get_latency_port_index())

    def testPorts(self):
        self.assertEqual(self.plugin.get_num_ports(), 4)
        self.assertIsNotNone(self.plugin.get_port(0))
        self.assertIsNotNone(self.plugin.get_port(1))
        self.assertIsNotNone(self.plugin.get_port(2))
        self.assertIsNotNone(self.plugin.get_port(3))
        self.assertIsNone(self.plugin.get_port_by_index(4))
        self.assertIsNotNone(self.plugin.get_port("input"))
        self.assertIsNotNone(self.plugin.get_port("output"))
        self.assertIsNotNone(self.plugin.get_port("audio_input"))
        self.assertIsNotNone(self.plugin.get_port("audio_output"))
        self.assertIsNone(self.plugin.get_port_by_symbol("nonexistent"))
        self.assertIsNone(
            self.plugin.get_port_by_designation(
                self.world.ns.lv2.InputPort, self.world.ns.lv2.control
            )
        )
        self.assertIsNone(self.plugin.get_project())
        self.assertIsNone(self.plugin.get_author_name())
        self.assertIsNone(self.plugin.get_author_email())
        self.assertIsNone(self.plugin.get_author_homepage())
        self.assertFalse(self.plugin.is_replaced())
        self.assertEqual(
            0,
            len(
                self.plugin.get_related(
                    self.world.new_uri("http://example.org/Type")
                )
            ),
        )
        self.assertEqual(
            1,
            self.plugin.get_num_ports_of_class(
                self.lv2_InputPort, self.lv2_AudioPort
            ),
        )
        port = self.plugin.get_port("input")
        self.assertEqual(self.world.get_symbol(port), "input")
        self.assertTrue(port.get_node().is_blank())
        self.assertEqual(0, port.get(self.world.ns.lv2.index))
        self.assertEqual(1, len(port.get_value(self.world.ns.lv2.symbol)))
        self.assertEqual(port.get_value(self.world.ns.lv2.symbol)[0], "input")
        self.assertFalse(port.has_property(self.world.ns.lv2.latency))
        self.assertFalse(port.supports_event(self.world.ns.midi.MidiEvent))
        self.assertEqual(0, port.get_index())
        self.assertEqual("input", port.get_symbol())
        self.assertEqual("Input", port.get_name())
        self.assertEqual(
            [
                str(self.world.ns.lv2.ControlPort),
                str(self.world.ns.lv2.InputPort),
            ],
            sorted(list(map(str, port.get_classes()))),
        )
        self.assertTrue(port.is_a(self.world.ns.lv2.ControlPort))
        self.assertFalse(port.is_a(self.world.ns.lv2.AudioPort))
        self.assertEqual((0.5, 0.0, 1.0), port.get_range())
        self.assertEqual(0, len(port.get_properties()))

    def testScalePoints(self):
        port = self.plugin.get_port("input")
        points = port.get_scale_points()
        point_dict = {
            float(points[0].get_value()): points[0].get_label(),
            float(points[1].get_value()): points[1].get_label(),
        }

        self.assertEqual(point_dict, {0.0: "off", 1.0: "on"})

    def testPortCount(self):
        self.assertEqual(
            1,
            self.plugin.get_num_ports_of_class(
                self.lv2_OutputPort, self.lv2_AudioPort
            ),
        )
        self.assertEqual(
            1,
            self.plugin.get_num_ports_of_class(
                self.lv2_OutputPort, self.lv2_ControlPort
            ),
        )
        self.assertEqual(
            1,
            self.plugin.get_num_ports_of_class(
                self.lv2_InputPort, self.lv2_AudioPort
            ),
        )
        self.assertEqual(
            1,
            self.plugin.get_num_ports_of_class(
                self.lv2_InputPort, self.lv2_ControlPort
            ),
        )


class QueryTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.world.load_all()
        self.bundle_uri = self.world.new_uri(location)
        self.world.load_bundle(self.bundle_uri)
        self.plugins = self.world.get_all_plugins()
        self.plugin = self.plugins.get(self.plugins.begin())

    def testNamespaces(self):
        self.assertEqual(self.world.ns.lv2, "http://lv2plug.in/ns/lv2core#")
        self.assertEqual(
            self.world.ns.lv2.Plugin, "http://lv2plug.in/ns/lv2core#Plugin"
        )

    def testQuery(self):
        self.assertTrue(
            self.world.ask(
                None, self.world.ns.rdf.type, self.world.ns.lv2.Plugin
            )
        )
        self.assertLess(
            0,
            len(
                self.world.find_nodes(
                    None, self.world.ns.rdf.type, self.world.ns.lv2.Plugin
                )
            ),
        )
        self.assertEqual(
            self.plugin.get_uri(),
            self.world.get(
                None, self.world.ns.rdf.type, self.world.ns.lv2.Plugin
            ),
        )


class InstanceTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.bundle_uri = self.world.new_uri(location)
        self.world.load_bundle(self.bundle_uri)
        self.plugins = self.world.get_all_plugins()
        self.plugin = self.plugins[0]
        self.instance = lilv.Instance(self.plugin, 48000)
        self.assertEqual(self.plugin.get_uri(), self.instance.get_uri())
        self.assertIsNone(
            self.instance.get_extension_data(
                self.world.new_uri("http://example.org/ext")
            )
        )
        self.assertIsNone(
            self.instance.get_extension_data("http://example.org/ext")
        )

    def testRun(self):
        try:
            import numpy
        except ImportError:
            sys.stderr.write("warning: Missing numpy, not testing instance\n")
            return

        n_samples = 100
        buf = numpy.zeros(n_samples)
        with self.assertRaises(Exception):
            self.instance.connect_port(0, "hello")
        self.instance.connect_port(0, None)
        self.instance.connect_port(0, None)
        self.instance.connect_port(2, buf)
        self.instance.connect_port(3, buf)
        self.instance.activate()
        self.instance.run(n_samples)
        self.instance.deactivate()


class UITests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.bundle_uri = self.world.new_uri(location)
        self.world.load_bundle(self.bundle_uri)
        self.plugins = self.world.get_all_plugins()
        self.plugin = self.plugins[0]

    def testUI(self):
        uis = self.plugin.get_uis()
        ui_uri = self.world.new_uri(
            "http://example.org/lilv-bindings-test-plugin-ui"
        )
        self.assertEqual(1, len(uis))
        self.assertEqual(str(uis[0]), str(ui_uri))
        with self.assertRaises(KeyError):
            uis["http://example.org/notaui"].get_uri()

        self.assertEqual(uis[0], str(ui_uri))
        self.assertEqual(uis[0].get_uri(), ui_uri)
        self.assertEqual(uis[0].get_bundle_uri(), self.bundle_uri)
        self.assertEqual(
            uis[0].get_binary_uri(), str(self.bundle_uri) + "TODO"
        )
        self.assertEqual(uis[uis[0].get_uri()], uis[0])
        self.assertTrue(uis[0].is_a(self.world.ns.ui.GtkUI))
        self.assertTrue(uis[0] in uis)
        self.assertTrue(uis[0].get_uri() in uis)
        self.assertEqual([self.world.ns.ui.GtkUI], list(uis[0].get_classes()))
