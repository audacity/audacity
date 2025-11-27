#[[
Utilities to serialize and deserialize trees of objects in XML form, and a
class template XMLMethodRegistry to generate registries for serializable
objects attached to a host object.  The template is parametrized by the host
type.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
   au3-files-interface
   au3-string-utils-interface
   PRIVATE
   expat::expat
)
audacity_library( au3-xml "${SOURCES}" "${LIBRARIES}"
   "" ""
)
