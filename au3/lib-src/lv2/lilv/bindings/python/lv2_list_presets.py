#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import lilv


NS_PRESETS = "http://lv2plug.in/ns/ext/presets#"


def print_presets(uri):
    """Print all presets of an LV2 plugin to stdout."""

    world = lilv.World()
    world.load_all()
    world.ns.presets = lilv.Namespace(world, NS_PRESETS)
    plugins = world.get_all_plugins()
    plugin = plugins[uri]
    presets = plugin.get_related(world.ns.presets.Preset)

    preset_list = []
    for preset in presets:
        world.load_resource(preset)
        labels = world.find_nodes(preset, world.ns.rdfs.label, None)
        label = str(labels[0]) if len(labels) > 0 else ""

        if not label:
            sys.stderr.write("warning: Preset <%s> has no label\n" % preset)

        preset_list.append((str(preset), str(label)))

    for preset in sorted(preset_list):
        print('<%s> "%s"' % preset)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.stderr.write("Usage: %s PLUGIN_URI\n" % (sys.argv[0]))
        sys.exit(1)

    try:
        print_presets(sys.argv[1])
    except ValueError as e:
        sys.stderr.write("error: %s\n" % e)
    except KeyError as e:
        sys.stderr.write("error: %s\n" % str(e).strip("'"))
