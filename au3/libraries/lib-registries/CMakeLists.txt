#[[
Some utilities for allowing open-endedness and decoupling of designs, by
maintaining global tables that can be populated at static initialization by
code in scattered places, on which the registry has no build dependency.

AttachedVirtualFunction implements "open methods" -- functions that type-switch
on the first argument, and dispatch to the correct implementation, but without
intrusiveness into the base of the class hierarchy.  This allows the set of
methods and the set of subclasses both to be open-ended.

(That is unlike in the "Gang of Four" Visitor pattern, in which the set of
methods becomes open-ended while the set of subclasses is closed-ended: the
Visitor can't handle a new subclass without intrusion into the abstract
Visitor definition.)

ClientData allows a "Site" class to act as a container of attached objects
produced by registered factory functions, and retrieved by the various attaching
modules.  The Site's constructor is effectively hookable.  This in particular
allows the Project object to be low-level in the graph of file dependencies,
while it is also the top-level object of an ownership tree of various important
sub-structures.  This is another "Dependency Inversion" of sorts (the D
principle of SOLID).

Registry implements trees of objects identified by textual paths, and allows
scattered code to insert items or subtrees at specified paths.  Registry
computes the merging of trees, and supports visitation.  It is used notably
by the tree of menus, allowing insertion of menu items in decoupled code.
]]

set( SOURCES
   AttachedVirtualFunction.h
   ClientData.cpp
   ClientData.h
   ClientDataHelpers.h
   Registrar.h
   Registry.cpp
   Registry.h
)
set( LIBRARIES
   lib-preferences-interface
   lib-exceptions
)
audacity_library( lib-registries "${SOURCES}" "${LIBRARIES}"
   "" ""
)
