#!/usr/bin/env python
import slv2;

w = slv2.World()
w.load_all()

for p in w.get_all_plugins():
    print p.uri()
