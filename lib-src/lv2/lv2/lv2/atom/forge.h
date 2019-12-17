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
   @file forge.h An API for constructing LV2 atoms.

   This file provides an API for constructing Atoms which makes it relatively
   simple to build nested atoms of arbitrary complexity without requiring
   dynamic memory allocation.

   The API is based on successively appending the appropriate pieces to build a
   complete Atom.  The size of containers is automatically updated.  Functions
   that begin a container return (via their frame argument) a stack frame which
   must be popped when the container is finished.

   All output is written to a user-provided buffer or sink function.  This
   makes it popssible to create create atoms on the stack, on the heap, in LV2
   port buffers, in a ringbuffer, or elsewhere, all using the same API.

   This entire API is realtime safe if used with a buffer or a realtime safe
   sink, except lv2_atom_forge_init() which is only realtime safe if the URI
   map function is.

   Note these functions are all static inline, do not take their address.

   This header is non-normative, it is provided for convenience.
*/

/**
   @defgroup forge Forge
   @ingroup atom
   @{
*/

#ifndef LV2_ATOM_FORGE_H
#define LV2_ATOM_FORGE_H

#include "lv2/atom/atom.h"
#include "lv2/atom/util.h"
#include "lv2/core/attributes.h"
#include "lv2/urid/urid.h"

#include <assert.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Disable deprecation warnings for Blank and Resource
LV2_DISABLE_DEPRECATION_WARNINGS

/** Handle for LV2_Atom_Forge_Sink. */
typedef void* LV2_Atom_Forge_Sink_Handle;

/** A reference to a chunk of written output. */
typedef intptr_t LV2_Atom_Forge_Ref;

/** Sink function for writing output.  See lv2_atom_forge_set_sink(). */
typedef LV2_Atom_Forge_Ref
(*LV2_Atom_Forge_Sink)(LV2_Atom_Forge_Sink_Handle handle,
                       const void*                buf,
                       uint32_t                   size);

/** Function for resolving a reference.  See lv2_atom_forge_set_sink(). */
typedef LV2_Atom*
(*LV2_Atom_Forge_Deref_Func)(LV2_Atom_Forge_Sink_Handle handle,
                             LV2_Atom_Forge_Ref         ref);

/** A stack frame used for keeping track of nested Atom containers. */
typedef struct _LV2_Atom_Forge_Frame {
	struct _LV2_Atom_Forge_Frame* parent;
	LV2_Atom_Forge_Ref            ref;
} LV2_Atom_Forge_Frame;

/** A "forge" for creating atoms by appending to a buffer. */
typedef struct {
	uint8_t* buf;
	uint32_t offset;
	uint32_t size;

	LV2_Atom_Forge_Sink        sink;
	LV2_Atom_Forge_Deref_Func  deref;
	LV2_Atom_Forge_Sink_Handle handle;

	LV2_Atom_Forge_Frame* stack;

	LV2_URID Blank LV2_DEPRECATED;
	LV2_URID Bool;
	LV2_URID Chunk;
	LV2_URID Double;
	LV2_URID Float;
	LV2_URID Int;
	LV2_URID Long;
	LV2_URID Literal;
	LV2_URID Object;
	LV2_URID Path;
	LV2_URID Property;
	LV2_URID Resource LV2_DEPRECATED;
	LV2_URID Sequence;
	LV2_URID String;
	LV2_URID Tuple;
	LV2_URID URI;
	LV2_URID URID;
	LV2_URID Vector;
} LV2_Atom_Forge;

static inline void
lv2_atom_forge_set_buffer(LV2_Atom_Forge* forge, uint8_t* buf, size_t size);

/**
   Initialise `forge`.

   URIs will be mapped using `map` and stored, a reference to `map` itself is
   not held.
*/
static inline void
lv2_atom_forge_init(LV2_Atom_Forge* forge, LV2_URID_Map* map)
{
	lv2_atom_forge_set_buffer(forge, NULL, 0);
	forge->Blank    = map->map(map->handle, LV2_ATOM__Blank);
	forge->Bool     = map->map(map->handle, LV2_ATOM__Bool);
	forge->Chunk    = map->map(map->handle, LV2_ATOM__Chunk);
	forge->Double   = map->map(map->handle, LV2_ATOM__Double);
	forge->Float    = map->map(map->handle, LV2_ATOM__Float);
	forge->Int      = map->map(map->handle, LV2_ATOM__Int);
	forge->Long     = map->map(map->handle, LV2_ATOM__Long);
	forge->Literal  = map->map(map->handle, LV2_ATOM__Literal);
	forge->Object   = map->map(map->handle, LV2_ATOM__Object);
	forge->Path     = map->map(map->handle, LV2_ATOM__Path);
	forge->Property = map->map(map->handle, LV2_ATOM__Property);
	forge->Resource = map->map(map->handle, LV2_ATOM__Resource);
	forge->Sequence = map->map(map->handle, LV2_ATOM__Sequence);
	forge->String   = map->map(map->handle, LV2_ATOM__String);
	forge->Tuple    = map->map(map->handle, LV2_ATOM__Tuple);
	forge->URI      = map->map(map->handle, LV2_ATOM__URI);
	forge->URID     = map->map(map->handle, LV2_ATOM__URID);
	forge->Vector   = map->map(map->handle, LV2_ATOM__Vector);
}

/** Access the Atom pointed to by a reference. */
static inline LV2_Atom*
lv2_atom_forge_deref(LV2_Atom_Forge* forge, LV2_Atom_Forge_Ref ref)
{
	return forge->buf ? (LV2_Atom*)ref : forge->deref(forge->handle, ref);
}

/**
   @name Object Stack
   @{
*/

/**
   Push a stack frame.
   This is done automatically by container functions (which take a stack frame
   pointer), but may be called by the user to push the top level container when
   writing to an existing Atom.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_push(LV2_Atom_Forge*       forge,
                    LV2_Atom_Forge_Frame* frame,
                    LV2_Atom_Forge_Ref    ref)
{
	frame->parent = forge->stack;
	frame->ref    = ref;
	forge->stack  = frame;
	return ref;
}

/** Pop a stack frame.  This must be called when a container is finished. */
static inline void
lv2_atom_forge_pop(LV2_Atom_Forge* forge, LV2_Atom_Forge_Frame* frame)
{
	assert(frame == forge->stack);
	forge->stack = frame->parent;
}

/** Return true iff the top of the stack has the given type. */
static inline bool
lv2_atom_forge_top_is(LV2_Atom_Forge* forge, uint32_t type)
{
	return forge->stack && forge->stack->ref &&
		(lv2_atom_forge_deref(forge, forge->stack->ref)->type == type);
}

/** Return true iff `type` is an atom:Object. */
static inline bool
lv2_atom_forge_is_object_type(const LV2_Atom_Forge* forge, uint32_t type)
{
	return (type == forge->Object ||
	        type == forge->Blank ||
	        type == forge->Resource);
}

/** Return true iff `type` is an atom:Object with a blank ID. */
static inline bool
lv2_atom_forge_is_blank(const LV2_Atom_Forge*       forge,
                        uint32_t                    type,
                        const LV2_Atom_Object_Body* body)
{
	return (type == forge->Blank ||
	        (type == forge->Object && body->id == 0));
}

/**
   @}
   @name Output Configuration
   @{
*/

/** Set the output buffer where `forge` will write atoms. */
static inline void
lv2_atom_forge_set_buffer(LV2_Atom_Forge* forge, uint8_t* buf, size_t size)
{
	forge->buf    = buf;
	forge->size   = (uint32_t)size;
	forge->offset = 0;
	forge->deref  = NULL;
	forge->sink   = NULL;
	forge->handle = NULL;
	forge->stack  = NULL;
}

/**
   Set the sink function where `forge` will write output.

   The return value of forge functions is an LV2_Atom_Forge_Ref which is an
   integer type safe to use as a pointer but is otherwise opaque.  The sink
   function must return a ref that can be dereferenced to access as least
   sizeof(LV2_Atom) bytes of the written data, so sizes can be updated.  For
   ringbuffers, this should be possible as long as the size of the buffer is a
   multiple of sizeof(LV2_Atom), since atoms are always aligned.

   Note that 0 is an invalid reference, so if you are using a buffer offset be
   sure to offset it such that 0 is never a valid reference.  You will get
   confusing errors otherwise.
*/
static inline void
lv2_atom_forge_set_sink(LV2_Atom_Forge*            forge,
                        LV2_Atom_Forge_Sink        sink,
                        LV2_Atom_Forge_Deref_Func  deref,
                        LV2_Atom_Forge_Sink_Handle handle)
{
	forge->buf    = NULL;
	forge->size   = forge->offset = 0;
	forge->deref  = deref;
	forge->sink   = sink;
	forge->handle = handle;
	forge->stack  = NULL;
}

/**
   @}
   @name Low Level Output
   @{
*/

/**
   Write raw output.  This is used internally, but is also useful for writing
   atom types not explicitly supported by the forge API.  Note the caller is
   responsible for ensuring the output is approriately padded.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_raw(LV2_Atom_Forge* forge, const void* data, uint32_t size)
{
	LV2_Atom_Forge_Ref out = 0;
	if (forge->sink) {
		out = forge->sink(forge->handle, data, size);
	} else {
		out = (LV2_Atom_Forge_Ref)forge->buf + forge->offset;
		uint8_t* mem = forge->buf + forge->offset;
		if (forge->offset + size > forge->size) {
			return 0;
		}
		forge->offset += size;
		memcpy(mem, data, size);
	}
	for (LV2_Atom_Forge_Frame* f = forge->stack; f; f = f->parent) {
		lv2_atom_forge_deref(forge, f->ref)->size += size;
	}
	return out;
}

/** Pad output accordingly so next write is 64-bit aligned. */
static inline void
lv2_atom_forge_pad(LV2_Atom_Forge* forge, uint32_t written)
{
	const uint64_t pad      = 0;
	const uint32_t pad_size = lv2_atom_pad_size(written) - written;
	lv2_atom_forge_raw(forge, &pad, pad_size);
}

/** Write raw output, padding to 64-bits as necessary. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_write(LV2_Atom_Forge* forge, const void* data, uint32_t size)
{
	LV2_Atom_Forge_Ref out = lv2_atom_forge_raw(forge, data, size);
	if (out) {
		lv2_atom_forge_pad(forge, size);
	}
	return out;
}

/** Write a null-terminated string body. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_string_body(LV2_Atom_Forge* forge,
                           const char*     str,
                           uint32_t        len)
{
	LV2_Atom_Forge_Ref out = lv2_atom_forge_raw(forge, str, len);
	if (out && (out = lv2_atom_forge_raw(forge, "", 1))) {
		lv2_atom_forge_pad(forge, len + 1);
	}
	return out;
}

/**
   @}
   @name Atom Output
   @{
*/

/** Write an atom:Atom header. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_atom(LV2_Atom_Forge* forge, uint32_t size, uint32_t type)
{
	const LV2_Atom a = { size, type };
	return lv2_atom_forge_raw(forge, &a, sizeof(a));
}

/** Write a primitive (fixed-size) atom. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_primitive(LV2_Atom_Forge* forge, const LV2_Atom* a)
{
	return (lv2_atom_forge_top_is(forge, forge->Vector)
	        ? lv2_atom_forge_raw(forge, LV2_ATOM_BODY_CONST(a), a->size)
	        : lv2_atom_forge_write(
		        forge, a, (uint32_t)sizeof(LV2_Atom) + a->size));
}

/** Write an atom:Int. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_int(LV2_Atom_Forge* forge, int32_t val)
{
	const LV2_Atom_Int a = { { sizeof(val), forge->Int }, val };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom:Long. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_long(LV2_Atom_Forge* forge, int64_t val)
{
	const LV2_Atom_Long a = { { sizeof(val), forge->Long }, val };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom:Float. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_float(LV2_Atom_Forge* forge, float val)
{
	const LV2_Atom_Float a = { { sizeof(val), forge->Float }, val };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom:Double. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_double(LV2_Atom_Forge* forge, double val)
{
	const LV2_Atom_Double a = { { sizeof(val), forge->Double }, val };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom:Bool. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_bool(LV2_Atom_Forge* forge, bool val)
{
	const LV2_Atom_Bool a = { { sizeof(int32_t), forge->Bool }, val ? 1 : 0 };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom:URID. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_urid(LV2_Atom_Forge* forge, LV2_URID id)
{
	const LV2_Atom_URID a = { { sizeof(id), forge->URID }, id };
	return lv2_atom_forge_primitive(forge, &a.atom);
}

/** Write an atom compatible with atom:String.  Used internally. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_typed_string(LV2_Atom_Forge* forge,
                            uint32_t        type,
                            const char*     str,
                            uint32_t        len)
{
	const LV2_Atom_String a   = { { len + 1, type } };
	LV2_Atom_Forge_Ref    out = lv2_atom_forge_raw(forge, &a, sizeof(a));
	if (out) {
		if (!lv2_atom_forge_string_body(forge, str, len)) {
			LV2_Atom* atom = lv2_atom_forge_deref(forge, out);
			atom->size = atom->type = 0;
			out = 0;
		}
	}
	return out;
}

/** Write an atom:String.  Note that `str` need not be NULL terminated. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_string(LV2_Atom_Forge* forge, const char* str, uint32_t len)
{
	return lv2_atom_forge_typed_string(forge, forge->String, str, len);
}

/**
   Write an atom:URI.  Note that `uri` need not be NULL terminated.
   This does not map the URI, but writes the complete URI string.  To write
   a mapped URI, use lv2_atom_forge_urid().
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_uri(LV2_Atom_Forge* forge, const char* uri, uint32_t len)
{
	return lv2_atom_forge_typed_string(forge, forge->URI, uri, len);
}

/** Write an atom:Path.  Note that `path` need not be NULL terminated. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_path(LV2_Atom_Forge* forge, const char* path, uint32_t len)
{
	return lv2_atom_forge_typed_string(forge, forge->Path, path, len);
}

/** Write an atom:Literal. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_literal(LV2_Atom_Forge* forge,
                       const char*     str,
                       uint32_t        len,
                       uint32_t        datatype,
                       uint32_t        lang)
{
	const LV2_Atom_Literal a = {
		{ (uint32_t)(sizeof(LV2_Atom_Literal) - sizeof(LV2_Atom) + len + 1),
		  forge->Literal },
		{ datatype,
		  lang }
	};
	LV2_Atom_Forge_Ref out = lv2_atom_forge_raw(forge, &a, sizeof(a));
	if (out) {
		if (!lv2_atom_forge_string_body(forge, str, len)) {
			LV2_Atom* atom = lv2_atom_forge_deref(forge, out);
			atom->size = atom->type = 0;
			out = 0;
		}
	}
	return out;
}

/** Start an atom:Vector. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_vector_head(LV2_Atom_Forge*       forge,
                           LV2_Atom_Forge_Frame* frame,
                           uint32_t              child_size,
                           uint32_t              child_type)
{
	const LV2_Atom_Vector a = {
		{ sizeof(LV2_Atom_Vector_Body), forge->Vector },
		{ child_size, child_type }
	};
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/** Write a complete atom:Vector. */
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_vector(LV2_Atom_Forge* forge,
                      uint32_t        child_size,
                      uint32_t        child_type,
                      uint32_t        n_elems,
                      const void*     elems)
{
	const LV2_Atom_Vector a = {
		{ (uint32_t)(sizeof(LV2_Atom_Vector_Body) + n_elems * child_size),
		  forge->Vector },
		{ child_size, child_type }
	};
	LV2_Atom_Forge_Ref out = lv2_atom_forge_write(forge, &a, sizeof(a));
	if (out) {
		lv2_atom_forge_write(forge, elems, child_size * n_elems);
	}
	return out;
}

/**
   Write the header of an atom:Tuple.

   The passed frame will be initialised to represent this tuple.  To complete
   the tuple, write a sequence of atoms, then pop the frame with
   lv2_atom_forge_pop().

   For example:
   @code
   // Write tuple (1, 2.0)
   LV2_Atom_Forge_Frame frame;
   LV2_Atom* tup = (LV2_Atom*)lv2_atom_forge_tuple(forge, &frame);
   lv2_atom_forge_int32(forge, 1);
   lv2_atom_forge_float(forge, 2.0);
   lv2_atom_forge_pop(forge, &frame);
   @endcode
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_tuple(LV2_Atom_Forge* forge, LV2_Atom_Forge_Frame* frame)
{
	const LV2_Atom_Tuple a = { { 0, forge->Tuple } };
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/**
   Write the header of an atom:Object.

   The passed frame will be initialised to represent this object.  To complete
   the object, write a sequence of properties, then pop the frame with
   lv2_atom_forge_pop().

   For example:
   @code
   LV2_URID eg_Cat  = map("http://example.org/Cat");
   LV2_URID eg_name = map("http://example.org/name");

   // Start object with type eg_Cat and blank ID
   LV2_Atom_Forge_Frame frame;
   lv2_atom_forge_object(forge, &frame, 0, eg_Cat);

   // Append property eg:name = "Hobbes"
   lv2_atom_forge_key(forge, eg_name);
   lv2_atom_forge_string(forge, "Hobbes", strlen("Hobbes"));

   // Finish object
   lv2_atom_forge_pop(forge, &frame);
   @endcode
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_object(LV2_Atom_Forge*       forge,
                      LV2_Atom_Forge_Frame* frame,
                      LV2_URID              id,
                      LV2_URID              otype)
{
	const LV2_Atom_Object a = {
		{ (uint32_t)sizeof(LV2_Atom_Object_Body), forge->Object },
		{ id, otype }
	};
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/**
   The same as lv2_atom_forge_object(), but for object:Resource.

   This function is deprecated and should not be used in new code.
   Use lv2_atom_forge_object() directly instead.
*/
LV2_DEPRECATED
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_resource(LV2_Atom_Forge*       forge,
                        LV2_Atom_Forge_Frame* frame,
                        LV2_URID              id,
                        LV2_URID              otype)
{
	const LV2_Atom_Object a = {
		{ (uint32_t)sizeof(LV2_Atom_Object_Body), forge->Resource },
		{ id, otype }
	};
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/**
   The same as lv2_atom_forge_object(), but for object:Blank.

   This function is deprecated and should not be used in new code.
   Use lv2_atom_forge_object() directly instead.
*/
LV2_DEPRECATED
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_blank(LV2_Atom_Forge*       forge,
                     LV2_Atom_Forge_Frame* frame,
                     uint32_t              id,
                     LV2_URID              otype)
{
	const LV2_Atom_Object a = {
		{ (uint32_t)sizeof(LV2_Atom_Object_Body), forge->Blank },
		{ id, otype }
	};
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/**
   Write a property key in an Object, to be followed by the value.

   See lv2_atom_forge_object() documentation for an example.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_key(LV2_Atom_Forge* forge,
                   LV2_URID        key)
{
	const LV2_Atom_Property_Body a = { key, 0, { 0, 0 } };
	return lv2_atom_forge_write(forge, &a, 2 * (uint32_t)sizeof(uint32_t));
}

/**
   Write the header for a property body in an object, with context.

   If you do not need the context, which is almost certainly the case,
   use the simpler lv2_atom_forge_key() instead.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_property_head(LV2_Atom_Forge* forge,
                             LV2_URID        key,
                             LV2_URID        context)
{
	const LV2_Atom_Property_Body a = { key, context, { 0, 0 } };
	return lv2_atom_forge_write(forge, &a, 2 * (uint32_t)sizeof(uint32_t));
}

/**
   Write the header for a Sequence.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_sequence_head(LV2_Atom_Forge*       forge,
                             LV2_Atom_Forge_Frame* frame,
                             uint32_t              unit)
{
	const LV2_Atom_Sequence a = {
		{ (uint32_t)sizeof(LV2_Atom_Sequence_Body), forge->Sequence },
		{ unit, 0 }
	};
	return lv2_atom_forge_push(
		forge, frame, lv2_atom_forge_write(forge, &a, sizeof(a)));
}

/**
   Write the time stamp header of an Event (in a Sequence) in audio frames.
   After this, call the appropriate forge method(s) to write the body.  Note
   the returned reference is to an LV2_Event which is NOT an Atom.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_frame_time(LV2_Atom_Forge* forge, int64_t frames)
{
	return lv2_atom_forge_write(forge, &frames, sizeof(frames));
}

/**
   Write the time stamp header of an Event (in a Sequence) in beats.  After
   this, call the appropriate forge method(s) to write the body.  Note the
   returned reference is to an LV2_Event which is NOT an Atom.
*/
static inline LV2_Atom_Forge_Ref
lv2_atom_forge_beat_time(LV2_Atom_Forge* forge, double beats)
{
	return lv2_atom_forge_write(forge, &beats, sizeof(beats));
}

/**
   @}
   @}
*/

LV2_RESTORE_WARNINGS

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_ATOM_FORGE_H */
