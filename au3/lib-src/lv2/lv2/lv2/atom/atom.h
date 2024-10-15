/*
  Copyright 2008-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
   @defgroup atom Atom

   A generic value container and several data types, see
   <http://lv2plug.in/ns/ext/atom> for details.

   @{
*/

#ifndef LV2_ATOM_H
#define LV2_ATOM_H

#include <stddef.h>
#include <stdint.h>

#define LV2_ATOM_URI    "http://lv2plug.in/ns/ext/atom"  ///< http://lv2plug.in/ns/ext/atom
#define LV2_ATOM_PREFIX LV2_ATOM_URI "#"                 ///< http://lv2plug.in/ns/ext/atom#

#define LV2_ATOM__Atom          LV2_ATOM_PREFIX "Atom"           ///< http://lv2plug.in/ns/ext/atom#Atom
#define LV2_ATOM__AtomPort      LV2_ATOM_PREFIX "AtomPort"       ///< http://lv2plug.in/ns/ext/atom#AtomPort
#define LV2_ATOM__Blank         LV2_ATOM_PREFIX "Blank"          ///< http://lv2plug.in/ns/ext/atom#Blank
#define LV2_ATOM__Bool          LV2_ATOM_PREFIX "Bool"           ///< http://lv2plug.in/ns/ext/atom#Bool
#define LV2_ATOM__Chunk         LV2_ATOM_PREFIX "Chunk"          ///< http://lv2plug.in/ns/ext/atom#Chunk
#define LV2_ATOM__Double        LV2_ATOM_PREFIX "Double"         ///< http://lv2plug.in/ns/ext/atom#Double
#define LV2_ATOM__Event         LV2_ATOM_PREFIX "Event"          ///< http://lv2plug.in/ns/ext/atom#Event
#define LV2_ATOM__Float         LV2_ATOM_PREFIX "Float"          ///< http://lv2plug.in/ns/ext/atom#Float
#define LV2_ATOM__Int           LV2_ATOM_PREFIX "Int"            ///< http://lv2plug.in/ns/ext/atom#Int
#define LV2_ATOM__Literal       LV2_ATOM_PREFIX "Literal"        ///< http://lv2plug.in/ns/ext/atom#Literal
#define LV2_ATOM__Long          LV2_ATOM_PREFIX "Long"           ///< http://lv2plug.in/ns/ext/atom#Long
#define LV2_ATOM__Number        LV2_ATOM_PREFIX "Number"         ///< http://lv2plug.in/ns/ext/atom#Number
#define LV2_ATOM__Object        LV2_ATOM_PREFIX "Object"         ///< http://lv2plug.in/ns/ext/atom#Object
#define LV2_ATOM__Path          LV2_ATOM_PREFIX "Path"           ///< http://lv2plug.in/ns/ext/atom#Path
#define LV2_ATOM__Property      LV2_ATOM_PREFIX "Property"       ///< http://lv2plug.in/ns/ext/atom#Property
#define LV2_ATOM__Resource      LV2_ATOM_PREFIX "Resource"       ///< http://lv2plug.in/ns/ext/atom#Resource
#define LV2_ATOM__Sequence      LV2_ATOM_PREFIX "Sequence"       ///< http://lv2plug.in/ns/ext/atom#Sequence
#define LV2_ATOM__Sound         LV2_ATOM_PREFIX "Sound"          ///< http://lv2plug.in/ns/ext/atom#Sound
#define LV2_ATOM__String        LV2_ATOM_PREFIX "String"         ///< http://lv2plug.in/ns/ext/atom#String
#define LV2_ATOM__Tuple         LV2_ATOM_PREFIX "Tuple"          ///< http://lv2plug.in/ns/ext/atom#Tuple
#define LV2_ATOM__URI           LV2_ATOM_PREFIX "URI"            ///< http://lv2plug.in/ns/ext/atom#URI
#define LV2_ATOM__URID          LV2_ATOM_PREFIX "URID"           ///< http://lv2plug.in/ns/ext/atom#URID
#define LV2_ATOM__Vector        LV2_ATOM_PREFIX "Vector"         ///< http://lv2plug.in/ns/ext/atom#Vector
#define LV2_ATOM__atomTransfer  LV2_ATOM_PREFIX "atomTransfer"   ///< http://lv2plug.in/ns/ext/atom#atomTransfer
#define LV2_ATOM__beatTime      LV2_ATOM_PREFIX "beatTime"       ///< http://lv2plug.in/ns/ext/atom#beatTime
#define LV2_ATOM__bufferType    LV2_ATOM_PREFIX "bufferType"     ///< http://lv2plug.in/ns/ext/atom#bufferType
#define LV2_ATOM__childType     LV2_ATOM_PREFIX "childType"      ///< http://lv2plug.in/ns/ext/atom#childType
#define LV2_ATOM__eventTransfer LV2_ATOM_PREFIX "eventTransfer"  ///< http://lv2plug.in/ns/ext/atom#eventTransfer
#define LV2_ATOM__frameTime     LV2_ATOM_PREFIX "frameTime"      ///< http://lv2plug.in/ns/ext/atom#frameTime
#define LV2_ATOM__supports      LV2_ATOM_PREFIX "supports"       ///< http://lv2plug.in/ns/ext/atom#supports
#define LV2_ATOM__timeUnit      LV2_ATOM_PREFIX "timeUnit"       ///< http://lv2plug.in/ns/ext/atom#timeUnit

#define LV2_ATOM_REFERENCE_TYPE 0  ///< The special type for a reference atom

#ifdef __cplusplus
extern "C" {
#endif

/** @cond */
/** This expression will fail to compile if double does not fit in 64 bits. */
typedef char lv2_atom_assert_double_fits_in_64_bits[
	((sizeof(double) <= sizeof(uint64_t)) * 2) - 1];
/** @endcond */

/**
   Return a pointer to the contents of an Atom.  The "contents" of an atom
   is the data past the complete type-specific header.
   @param type The type of the atom, e.g. LV2_Atom_String.
   @param atom A variable-sized atom.
*/
#define LV2_ATOM_CONTENTS(type, atom) \
	((void*)((uint8_t*)(atom) + sizeof(type)))

/**
   Const version of LV2_ATOM_CONTENTS.
*/
#define LV2_ATOM_CONTENTS_CONST(type, atom) \
	((const void*)((const uint8_t*)(atom) + sizeof(type)))

/**
   Return a pointer to the body of an Atom.  The "body" of an atom is the
   data just past the LV2_Atom head (i.e. the same offset for all types).
*/
#define LV2_ATOM_BODY(atom) LV2_ATOM_CONTENTS(LV2_Atom, atom)

/**
   Const version of LV2_ATOM_BODY.
*/
#define LV2_ATOM_BODY_CONST(atom) LV2_ATOM_CONTENTS_CONST(LV2_Atom, atom)

/** The header of an atom:Atom. */
typedef struct {
	uint32_t size;  /**< Size in bytes, not including type and size. */
	uint32_t type;  /**< Type of this atom (mapped URI). */
} LV2_Atom;

/** An atom:Int or atom:Bool.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	int32_t  body;  /**< Integer value. */
} LV2_Atom_Int;

/** An atom:Long.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	int64_t  body;  /**< Integer value. */
} LV2_Atom_Long;

/** An atom:Float.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	float    body;  /**< Floating point value. */
} LV2_Atom_Float;

/** An atom:Double.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	double   body;  /**< Floating point value. */
} LV2_Atom_Double;

/** An atom:Bool.  May be cast to LV2_Atom. */
typedef LV2_Atom_Int LV2_Atom_Bool;

/** An atom:URID.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	uint32_t body;  /**< URID. */
} LV2_Atom_URID;

/** An atom:String.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	/* Contents (a null-terminated UTF-8 string) follow here. */
} LV2_Atom_String;

/** The body of an atom:Literal. */
typedef struct {
	uint32_t datatype;  /**< Datatype URID. */
	uint32_t lang;      /**< Language URID. */
	/* Contents (a null-terminated UTF-8 string) follow here. */
} LV2_Atom_Literal_Body;

/** An atom:Literal.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom              atom;  /**< Atom header. */
	LV2_Atom_Literal_Body body;  /**< Body. */
} LV2_Atom_Literal;

/** An atom:Tuple.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom atom;  /**< Atom header. */
	/* Contents (a series of complete atoms) follow here. */
} LV2_Atom_Tuple;

/** The body of an atom:Vector. */
typedef struct {
	uint32_t child_size;  /**< The size of each element in the vector. */
	uint32_t child_type;  /**< The type of each element in the vector. */
	/* Contents (a series of packed atom bodies) follow here. */
} LV2_Atom_Vector_Body;

/** An atom:Vector.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom             atom;  /**< Atom header. */
	LV2_Atom_Vector_Body body;  /**< Body. */
} LV2_Atom_Vector;

/** The body of an atom:Property (e.g. in an atom:Object). */
typedef struct {
	uint32_t key;      /**< Key (predicate) (mapped URI). */
	uint32_t context;  /**< Context URID (may be, and generally is, 0). */
	LV2_Atom value;    /**< Value atom header. */
	/* Value atom body follows here. */
} LV2_Atom_Property_Body;

/** An atom:Property.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom               atom;  /**< Atom header. */
	LV2_Atom_Property_Body body;  /**< Body. */
} LV2_Atom_Property;

/** The body of an atom:Object. May be cast to LV2_Atom. */
typedef struct {
	uint32_t id;     /**< URID, or 0 for blank. */
	uint32_t otype;  /**< Type URID (same as rdf:type, for fast dispatch). */
	/* Contents (a series of property bodies) follow here. */
} LV2_Atom_Object_Body;

/** An atom:Object.  May be cast to LV2_Atom. */
typedef struct {
	LV2_Atom             atom;  /**< Atom header. */
	LV2_Atom_Object_Body body;  /**< Body. */
} LV2_Atom_Object;

/** The header of an atom:Event.  Note this type is NOT an LV2_Atom. */
typedef struct {
	/** Time stamp.  Which type is valid is determined by context. */
	union {
		int64_t frames;  /**< Time in audio frames. */
		double  beats;   /**< Time in beats. */
	} time;
	LV2_Atom body;  /**< Event body atom header. */
	/* Body atom contents follow here. */
} LV2_Atom_Event;

/**
   The body of an atom:Sequence (a sequence of events).

   The unit field is either a URID that described an appropriate time stamp
   type, or may be 0 where a default stamp type is known.  For
   LV2_Descriptor::run(), the default stamp type is audio frames.

   The contents of a sequence is a series of LV2_Atom_Event, each aligned
   to 64-bits, e.g.:
   <pre>
   | Event 1 (size 6)                              | Event 2
   |       |       |       |       |       |       |       |       |
   | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
   |FRAMES |SUBFRMS|TYPE   |SIZE   |DATADATADATAPAD|FRAMES |SUBFRMS|...
   </pre>
*/
typedef struct {
	uint32_t unit;  /**< URID of unit of event time stamps. */
	uint32_t pad;   /**< Currently unused. */
	/* Contents (a series of events) follow here. */
} LV2_Atom_Sequence_Body;

/** An atom:Sequence. */
typedef struct {
	LV2_Atom               atom;  /**< Atom header. */
	LV2_Atom_Sequence_Body body;  /**< Body. */
} LV2_Atom_Sequence;

/**
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_ATOM_H */
