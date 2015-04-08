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

from lilv import *
import unittest
import os

class UriTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv_world_new()
        lilv_world_load_all(self.world)
    def testInvalidURI(self):
        self.uri = lilv_new_uri(self.world, "invalid_uri")
        self.assertIsNone(self.uri)
    def testInvalidURI2(self):
        self.uri = lilv_new_uri(self.world, "invalid_uri")
        self.assertFalse( lilv_node_is_uri(self.uri) )
    def testNonExistentURI(self):
        self.uri = lilv_new_uri(self.world, "exist:does_not")
        plugins = lilv_world_get_all_plugins(self.world)
        self.plugin = lilv_plugins_get_by_uri(plugins, self.uri)
        self.assertIsNone(self.plugin)
    def testPortTypes(self):
        self.uri = lilv_new_uri(self.world, LILV_URI_INPUT_PORT) 
        self.assertIsNotNone(self.uri)
    def testPortTypes2(self):
        self.uri = lilv_new_uri(self.world, LILV_URI_OUTPUT_PORT)
        self.assertIsNotNone(self.uri)
    def testPortTypes3(self):
        self.uri = lilv_new_uri(self.world, LILV_URI_AUDIO_PORT)
        self.assertIsNotNone(self.uri)
    def testPortTypes4(self):
        self.uri = lilv_new_uri(self.world, LILV_URI_CONTROL_PORT)
        self.assertIsNotNone(self.uri)
    def tearDown(self):
        lilv_node_free(self.uri)
        lilv_world_free(self.world)

class PluginTests(unittest.TestCase):
    def setUp(self):
        self.world = lilv_world_new()
        location = "file://" + os.getcwd() + "/bindings/bindings_test_plugin.lv2/"
        self.plugin_uri = lilv_new_uri(self.world, location)
        self.assertIsNotNone(self.plugin_uri, "Invalid URI: '" + location + "'")
        lilv_world_load_bundle(self.world, self.plugin_uri)
        self.plugins = lilv_world_get_all_plugins(self.world)
        self.plugin  = lilv_plugins_get(self.plugins, lilv_plugins_begin(self.plugins))
        self.assertIsNotNone(self.plugin, msg="Test plugin not found at location: '" + location + "'")
        self.assertEqual(location, lilv_node_as_string(lilv_plugin_get_bundle_uri(self.plugin)))
        self.instance = lilv_plugin_instantiate(self.plugin, 48000, None)
        self.assertIsNotNone(self.instance)
        self.lv2_InputPort    = lilv_new_uri(self.world, LILV_URI_INPUT_PORT)
        self.lv2_OutputPort   = lilv_new_uri(self.world, LILV_URI_OUTPUT_PORT)
        self.lv2_AudioPort    = lilv_new_uri(self.world, LILV_URI_AUDIO_PORT)
        self.lv2_ControlPort  = lilv_new_uri(self.world, LILV_URI_CONTROL_PORT)
    def testPorts(self):
        n = lilv_plugin_get_num_ports_of_class(self.plugin, self.lv2_InputPort, self.lv2_AudioPort)
        self.assertEqual(n, 1)
    def testPorts2(self):
        n = lilv_plugin_get_num_ports_of_class(self.plugin, self.lv2_OutputPort, self.lv2_AudioPort)
        self.assertEqual(n, 1)
    def testPorts3(self):
        n = lilv_plugin_get_num_ports_of_class(self.plugin, self.lv2_OutputPort, self.lv2_ControlPort)
        self.assertEqual(n, 1)
    def testPorts4(self):
        n = lilv_plugin_get_num_ports_of_class(self.plugin, self.lv2_InputPort, self.lv2_ControlPort)
        self.assertEqual(n, 1)
    def tearDown(self):
        lilv_node_free(self.lv2_InputPort)
        lilv_node_free(self.lv2_OutputPort)
        lilv_node_free(self.lv2_AudioPort)
        lilv_node_free(self.plugin_uri)
        lilv_world_free(self.world)
