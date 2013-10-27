== Simple Amplifier ==

This plugin is a simple example of a basic LV2 plugin with no additional features.
It has audio ports which contain an array of `float`,
and a control port which contain a single `float`.

LV2 plugins are defined in two parts: code and data.
The code is written in C, or any C compatible language such as C++.
Static data is described separately in the human and machine friendly http://www.w3.org/TeamSubmission/turtle/[Turtle] syntax.
Turtle is a syntax for the RDF data model,
but familiarity with RDF is not required to understand this documentation.

Generally, code is kept minimal,
and all static information is described in the data.
There are several advantages to this approach:

 * Hosts can discover and inspect plugins without loading or executing any plugin code
 * It is simple to work with plugin data using scripting languages, command line tools, etc.
 * The standard format allow the use of existing vocabularies to describe plugins and related information
 * The data inherently integrates with the web, databases, etc.
 * Labels and documentation are translatable, and available to hosts for display in user interfaces
