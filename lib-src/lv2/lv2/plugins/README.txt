= Programming LV2 Plugins =
David Robillard <d@drobilla.net>
:Author Initials: DER
:toc:
:website: http://lv2plug.in/
:doctype: book

== Introduction ==

This is a series of well-documented example plugins that demonstrate the various features of LV2.
Starting with the most basic plugin possible,
each adds new functionality and explains the features used from a high level perspective.

API and vocabulary reference documentation explains details,
but not the ``big picture''.
This book is intended to complement the reference documentation by providing good reference implementations of plugins,
while also conveying a higher-level understanding of LV2.

The chapters/plugins are arranged so that each builds incrementally on its predecessor.
Reading this book front to back is a good way to become familiar with modern LV2 programming.
The reader is expected to be familiar with C, but otherwise no special knowledge is required;
the first plugin describes the basics in detail.

This book is compiled from plugin source code into a single document for pleasant reading and ease of reference.
Each chapter corresponds to executable plugin code which can be found in the +plugins+ directory of the LV2 distribution.
If you prefer to read actual source code, all the content here is also available in the source code as comments.
