#[[
Utilities to serialize and deserialize trees of objects in XML form, and a
class template XMLMethodRegistry to generate registries for serializable
objects attached to a host object.  The template is parametrized by the host
type.
]]

set( TARGET au3-xml )

set( SOURCES
   XMLAttributeValueView.cpp
   XMLAttributeValueView.h
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
PUBLIC
   au3-files
   au3-strings
   au3-utility
PRIVATE
   au3-exceptions
   au3-string-utils
   expat::expat
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
