Autowaf
=======

This is autowaf, a bundle of waf and a few extensions intended to be easy to
use directly as source code in a project.  Using this as a submodule or subtree
named `waflib` in a project allows waf to be used without including binary
encoded data in the waf script.  This gets along with revision control and
distributions better, among other advantages, without losing
self-containedness.

To use this in a project, add this repository as a directory named `waflib` in
the top level of the project, and link or copy `waf` to the top level.

Two waf extras are also included: `autowaf.py` and `lv2.py`.

The `autowaf.py` module is a kitchen sink of Python utilities for building
consistent packages, and can be imported in a wcript as
`waflib.extras.autowaf`.

The `lv2.py` extra defines options for LV2 plugin installation paths.  It can
be used by calling `opt.load('lv2')` and `conf.load('lv2')` in the appropriate
locations in a wscript.

 -- David Robillard <d@drobilla.net>
