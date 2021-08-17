#[[
Utilities to serialize and deserialize trees of objects in XML form, and a
class template XMLMethodRegistry to generate registries for serializable
objects attached to a host object.  The template is parametrized by the host
type.
]]#

set( SOURCES
   XMLFileReader.cpp
   XMLFileReader.h
   XMLMethodRegistry.cpp
   XMLMethodRegistry.h
   XMLTagHandler.cpp
   XMLTagHandler.h
   XMLWriter.cpp
   XMLWriter.h
   audacityproject.dtd
)
set( LIBRARIES
   lib-files-interface
   expat::expat
   PRIVATE
   wxBase
)
audacity_library( lib-xml "${SOURCES}" "${LIBRARIES}"
   "" ""
)
