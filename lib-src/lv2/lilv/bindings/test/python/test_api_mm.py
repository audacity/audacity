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
import unittest
import os

class UriTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        self.world.load_all();
    def testInvalidURI(self):
        self.plugin_uri = self.world.new_uri("invalid_uri")
        self.assertEqual(self.plugin_uri, None)
    def testInvalidURI2(self):
        self.plugin_uri = self.world.new_uri("invalid_uri")
        self.assertFalse( lilv.lilv_node_is_uri(self.plugin_uri) )
    def testNonExistentURI(self):
        self.plugin_uri = self.world.new_uri("exist:does_not")
        self.plugin = self.world.get_all_plugins().get_by_uri(self.plugin_uri)
        self.assertEqual(self.plugin, None)
    def testPortTypes(self):
        self.assertIsNotNone( self.world.new_uri(lilv.LILV_URI_INPUT_PORT) )
    def testPortTypes2(self):
        self.assertIsNotNone( self.world.new_uri(lilv.LILV_URI_OUTPUT_PORT) )
    def testPortTypes3(self):
        self.assertIsNotNone( self.world.new_uri(lilv.LILV_URI_AUDIO_PORT) )
    def testPortTypes4(self):
        self.assertIsNotNone( self.world.new_uri(lilv.LILV_URI_CONTROL_PORT) )

class PluginTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv.World()
        location = "file://" + os.getcwd() + "/bindings/bindings_test_plugin.lv2/"
        self.plugin_uri = self.world.new_uri(location)
        self.assertIsNotNone(self.plugin_uri, "Invalid URI: '" + location + "'")
        self.world.load_bundle(self.plugin_uri)
        self.plugins = self.world.get_all_plugins()
        self.plugin  = self.plugins.get(self.plugins.begin())
        self.assertIsNotNone(self.plugin, msg="Test plugin not found at location: '" + location + "'")
        self.assertEqual(location, self.plugin.get_bundle_uri().as_string())
        self.instance = lilv.Instance(self.plugin, 48000, None)
        self.assertIsNotNone(self.instance)
        self.lv2_InputPort    = self.world.new_uri(lilv.LILV_URI_INPUT_PORT)
        self.lv2_OutputPort   = self.world.new_uri(lilv.LILV_URI_OUTPUT_PORT)
        self.lv2_AudioPort    = self.world.new_uri(lilv.LILV_URI_AUDIO_PORT)
        self.lv2_ControlPort  = self.world.new_uri(lilv.LILV_URI_CONTROL_PORT)
    def testPorts(self):
        n = self.plugin.get_num_ports_of_class(self.lv2_InputPort, self.lv2_AudioPort)
        self.assertEqual(n, 1)
    def testPorts2(self):
        n = self.plugin.get_num_ports_of_class(self.lv2_OutputPort, self.lv2_AudioPort)
        self.assertEqual(n, 1)
    def testPorts3(self):
        n = self.plugin.get_num_ports_of_class(self.lv2_OutputPort, self.lv2_ControlPort)
        self.assertEqual(n, 1)
    def testPorts4(self):
        n = self.plugin.get_num_ports_of_class(self.lv2_InputPort, self.lv2_ControlPort)
        self.assertEqual(n, 1)
