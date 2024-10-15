#!/usr/bin/env python

import lilv

world = lilv.World()
world.load_all()

for i in world.get_all_plugins():
    print(i.get_uri())
