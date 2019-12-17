/*
  Copyright 2011-2013 David Robillard <http://drobilla.net>

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
   @file sordmm.hpp
   Public Sord C++ API.
*/

#ifndef SORD_SORDMM_HPP
#define SORD_SORDMM_HPP

#include <cassert>
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <set>
#include <string>
#include <sstream>

#include "serd/serd.h"
#include "sord/sord.h"

#define SORD_NS_XSD "http://www.w3.org/2001/XMLSchema#"

namespace Sord {

/** Utility base class to prevent copying. */
class Noncopyable {
protected:
	Noncopyable() {}
	~Noncopyable() {}
private:
	Noncopyable(const Noncopyable&);
	const Noncopyable& operator=(const Noncopyable&);
};

/** C++ wrapper for a Sord object. */
template <typename T>
class Wrapper {
public:
	inline Wrapper(T c_obj = NULL) : _c_obj(c_obj) {}

	inline T       c_obj()       { return _c_obj; }
	inline const T c_obj() const { return _c_obj; }

protected:
	T _c_obj;
};

/** Collection of RDF namespaces with prefixes. */
class Namespaces : public Wrapper<SerdEnv*> {
public:
	Namespaces() : Wrapper<SerdEnv*>(serd_env_new(NULL)) {}
	~Namespaces() { serd_env_free(_c_obj); }

	static inline SerdNode string_to_node(SerdType type, const std::string& s) {
		SerdNode ret = {
			(const uint8_t*)s.c_str(), s.length(), s.length(), 0, type };
		return ret;
	}

	inline void add(const std::string& name,
	                const std::string& uri) {
		const SerdNode name_node = string_to_node(SERD_LITERAL, name);
		const SerdNode uri_node  = string_to_node(SERD_URI,     uri);
		serd_env_set_prefix(_c_obj, &name_node, &uri_node);
	}

	inline std::string qualify(std::string uri) const {
		const SerdNode uri_node = string_to_node(SERD_URI, uri);
		SerdNode       prefix;
		SerdChunk      suffix;
		if (serd_env_qualify(_c_obj, &uri_node, &prefix, &suffix)) {
			std::string ret((const char*)prefix.buf, prefix.n_bytes);
			ret.append(":").append((const char*)suffix.buf, suffix.len);
			return ret;
		}
		return uri;
	}

	inline std::string expand(const std::string& curie) const {
		assert(curie.find(":") != std::string::npos);
		SerdNode  curie_node = string_to_node(SERD_CURIE, curie);
		SerdChunk uri_prefix;
		SerdChunk uri_suffix;
		if (!serd_env_expand(_c_obj, &curie_node, &uri_prefix, &uri_suffix)) {
			std::string ret((const char*)uri_prefix.buf, uri_prefix.len);
			ret.append((const char*)uri_suffix.buf, uri_suffix.len);
			return ret;
		}
		std::cerr << "CURIE `" << curie << "' has unknown prefix." << std::endl;
		return curie;
	}
};

/** Sord library state. */
class World : public Noncopyable, public Wrapper<SordWorld*> {
public:
	inline World()
		: _next_blank_id(0)
	{
		_c_obj = sord_world_new();
	}

	inline ~World() {
		sord_world_free(_c_obj);
	}

	inline uint64_t blank_id() { return _next_blank_id++; }

	inline void add_prefix(const std::string& prefix, const std::string& uri) {
		_prefixes.add(prefix, uri);
	}

	inline const Namespaces& prefixes() const { return _prefixes; }
	inline SordWorld*        world()          { return _c_obj; }

private:
	Namespaces            _prefixes;
	std::set<std::string> _blank_ids;
	uint64_t              _next_blank_id;
};

/** An RDF Node (resource, literal, etc)
 */
class Node : public Wrapper<SordNode*> {
public:
	enum Type {
		UNKNOWN  = 0,
		URI      = SORD_URI,
		BLANK    = SORD_BLANK,
		LITERAL  = SORD_LITERAL
	};

	inline Node() : Wrapper<SordNode*>(NULL), _world(NULL) {}

	inline Node(World& world, Type t, const std::string& s);
	inline Node(World& world);
	inline Node(World& world, const SordNode* node);
	inline Node(World& world, SordNode* node, bool copy=false);
	inline Node(const Node& other);
	inline ~Node();

	inline Type type() const {
		return _c_obj ? (Type)sord_node_get_type(_c_obj) : UNKNOWN;
	}

	inline const SordNode* get_node() const { return _c_obj; }
	inline SordNode*       get_node()       { return _c_obj; }

	const SerdNode* to_serd_node() const {
		return sord_node_to_serd_node(_c_obj);
	}

	inline bool is_valid() const { return type() != UNKNOWN; }

	inline bool operator<(const Node& other) const {
		if (type() != other.type()) {
			return type() < other.type();
		} else {
			return to_string() < other.to_string();
		}
	}

	Node& operator=(const Node& other) {
		if (&other != this) {
			if (_c_obj) {
				sord_node_free(_world->c_obj(), _c_obj);
			}
			_world = other._world;
			_c_obj = other._c_obj ? sord_node_copy(other._c_obj) : NULL;
		}
		return *this;
	}

	inline bool operator==(const Node& other) const {
		return sord_node_equals(_c_obj, other._c_obj);
	}

	inline const uint8_t* to_u_string() const;
	inline const char*    to_c_string() const;
	inline std::string    to_string() const;

	inline bool is_literal_type(const char* type_uri) const;

	inline bool is_uri()   const { return _c_obj && type() == URI; }
	inline bool is_blank() const { return _c_obj && type() == BLANK; }
	inline bool is_int()   const { return is_literal_type(SORD_NS_XSD "integer"); }
	inline bool is_float() const { return is_literal_type(SORD_NS_XSD "decimal"); }
	inline bool is_bool()  const { return is_literal_type(SORD_NS_XSD "boolean"); }

	inline int   to_int()   const;
	inline float to_float() const;
	inline bool  to_bool()  const;

	inline static Node blank_id(World& world, const std::string base="b") {
		const uint64_t num = world.blank_id();
		std::ostringstream ss;
		ss << base << num;
		return Node(world, Node::BLANK, ss.str());
	}

private:
	World* _world;
};

inline std::ostream&
operator<<(std::ostream& os, const Node& node)
{
	return os << node.to_string();
}

class URI : public Node {
public:
	inline URI(World& world, const std::string& s)
		: Node(world, Node::URI, s) {}
	inline URI(World& world, const std::string& s, const std::string& base)
		: Node(world, sord_new_relative_uri(world.world(),
		                                    (const uint8_t*)s.c_str(),
		                                    (const uint8_t*)base.c_str()))
	{}
};

class Curie : public Node {
public:
	inline Curie(World& world, const std::string& s)
		: Node(world, Node::URI, world.prefixes().expand(s)) {}
};

class Literal : public Node {
public:
	inline Literal(World& world, const std::string& s)
		: Node(world, Node::LITERAL, s) {}

	static inline Node decimal(World& world, double d, unsigned frac_digits) {
		const SerdNode val  = serd_node_new_decimal(d, frac_digits);
		const SerdNode type = serd_node_from_string(
			SERD_URI, (const uint8_t*)SORD_NS_XSD "decimal");

		return Node(
			world,
			sord_node_from_serd_node(
				world.c_obj(), world.prefixes().c_obj(), &val, &type, NULL),
			false);
	}

	static inline Node integer(World& world, int64_t i) {
		const SerdNode val  = serd_node_new_integer(i);
		const SerdNode type = serd_node_from_string(
			SERD_URI, (const uint8_t*)SORD_NS_XSD "integer");

		return Node(
			world,
			sord_node_from_serd_node(
				world.c_obj(), world.prefixes().c_obj(), &val, &type, NULL),
			false);
	}
};

inline
Node::Node(World& world, Type type, const std::string& s)
	: _world(&world)
{
	switch (type) {
	case URI:
		_c_obj = sord_new_uri(
			world.world(), (const unsigned char*)s.c_str());
		break;
	case LITERAL:
		_c_obj = sord_new_literal(
			world.world(), NULL, (const unsigned char*)s.c_str(), NULL);
		break;
	case BLANK:
		_c_obj = sord_new_blank(
			world.world(), (const unsigned char*)s.c_str());
		break;
	default:
		_c_obj = NULL;
	}

	assert(this->type() == type);
}

inline
Node::Node(World& world)
	: _world(&world)
{
	Node me = blank_id(world);
	*this = me;
}

inline
Node::Node(World& world, const SordNode* node)
	: _world(&world)
{
	_c_obj = sord_node_copy(node);
}

inline
Node::Node(World& world, SordNode* node, bool copy)
	: _world(&world)
{
	_c_obj = copy ? sord_node_copy(node) : node;
}

inline
Node::Node(const Node& other)
	: Wrapper<SordNode*>()
	, _world(other._world)
{
	if (_world) {
		_c_obj = other._c_obj ? sord_node_copy(other._c_obj) : NULL;
	}

	assert((!_c_obj && !other._c_obj) || to_string() == other.to_string());
}

inline
Node::~Node()
{
	if (_world) {
		sord_node_free(_world->c_obj(), _c_obj);
	}
}

inline std::string
Node::to_string() const
{
	return _c_obj ? (const char*)sord_node_get_string(_c_obj) : "";
}

inline const char*
Node::to_c_string() const
{
	return (const char*)sord_node_get_string(_c_obj);
}

inline const uint8_t*
Node::to_u_string() const
{
	return sord_node_get_string(_c_obj);
}

inline bool
Node::is_literal_type(const char* type_uri) const
{
	if (_c_obj && sord_node_get_type(_c_obj) == SORD_LITERAL) {
		const SordNode* datatype = sord_node_get_datatype(_c_obj);
		if (datatype && !strcmp((const char*)sord_node_get_string(datatype),
		                        type_uri))
			return true;
	}
	return false;
}

inline int
Node::to_int() const
{
	assert(is_int());
	char* endptr;
	return strtol((const char*)sord_node_get_string(_c_obj), &endptr, 10);
}

inline float
Node::to_float() const
{
	assert(is_float());
	return serd_strtod((const char*)sord_node_get_string(_c_obj), NULL);
}

inline bool
Node::to_bool() const
{
	assert(is_bool());
	return !strcmp((const char*)sord_node_get_string(_c_obj), "true");
}

struct Iter : public Wrapper<SordIter*> {
	inline Iter(World& world, SordIter* c_obj)
		: Wrapper<SordIter*>(c_obj), _world(world) {}
	inline ~Iter() { sord_iter_free(_c_obj); }
	inline bool end()  const { return sord_iter_end(_c_obj); }
	inline bool next() const { return sord_iter_next(_c_obj); }
	inline Iter& operator++() {
		assert(!end());
		next();
		return *this;
	}
	inline const Node get_subject() const {
		SordQuad quad;
		sord_iter_get(_c_obj, quad);
		return Node(_world, quad[SORD_SUBJECT]);
	}
	inline const Node get_predicate() const {
		SordQuad quad;
		sord_iter_get(_c_obj, quad);
		return Node(_world, quad[SORD_PREDICATE]);
	}
	inline const Node get_object() const {
		SordQuad quad;
		sord_iter_get(_c_obj, quad);
		return Node(_world, quad[SORD_OBJECT]);
	}
	World& _world;
};

/** An RDF Model (collection of triples).
 */
class Model : public Noncopyable, public Wrapper<SordModel*> {
public:
	inline Model(World&             world,
	             const std::string& base_uri,
	             unsigned           indices = (SORD_SPO | SORD_OPS),
	             bool               graphs  = true);

	inline ~Model();

	inline const Node& base_uri() const { return _base; }

	size_t num_quads() const { return sord_num_quads(_c_obj); }

	inline void load_file(SerdEnv*           env,
	                      SerdSyntax         syntax,
	                      const std::string& uri,
	                      const std::string& base_uri="");

	inline void load_string(SerdEnv*           env,
	                        SerdSyntax         syntax,
	                        const char*        str,
	                        size_t             len,
	                        const std::string& base_uri);

	inline SerdStatus write_to_file(
		const std::string& uri,
		SerdSyntax         syntax = SERD_TURTLE,
		SerdStyle          style  = (SerdStyle)(SERD_STYLE_ABBREVIATED
		                                        |SERD_STYLE_CURIED
		                                        |SERD_STYLE_RESOLVED));

	inline std::string write_to_string(
		const std::string& base_uri,
		SerdSyntax         syntax = SERD_TURTLE,
		SerdStyle          style  = (SerdStyle)(SERD_STYLE_ABBREVIATED
		                                        |SERD_STYLE_CURIED
		                                        |SERD_STYLE_RESOLVED));

	inline void add_statement(const Node& subject,
	                          const Node& predicate,
	                          const Node& object);

	inline Iter find(const Node& subject,
	                 const Node& predicate,
	                 const Node& object);

	inline Node get(const Node& subject,
	                const Node& predicate,
	                const Node& object);

	inline World& world() const { return _world; }

private:
	World& _world;
	Node   _base;
};

/** Create an empty in-memory RDF model.
 */
inline
Model::Model(World&             world,
             const std::string& base_uri,
             unsigned           indices,
             bool               graphs)
	: _world(world)
	, _base(world, Node::URI, base_uri)
{
	_c_obj = sord_new(_world.world(), indices, graphs);
}

inline void
Model::load_string(SerdEnv*           env,
                   SerdSyntax         syntax,
                   const char*        str,
                   size_t             /*len*/,
                   const std::string& /*base_uri*/)
{
	SerdReader* reader = sord_new_reader(_c_obj, env, syntax, NULL);
	serd_reader_read_string(reader, (const uint8_t*)str);
	serd_reader_free(reader);
}

inline Model::~Model()
{
	sord_free(_c_obj);
}

inline void
Model::load_file(SerdEnv*           env,
                 SerdSyntax         syntax,
                 const std::string& data_uri,
                 const std::string& /*base_uri*/)
{
	uint8_t* path = serd_file_uri_parse((const uint8_t*)data_uri.c_str(), NULL);
	if (!path) {
		fprintf(stderr, "Failed to parse file URI <%s>\n", data_uri.c_str());
		return;
	}

	// FIXME: blank prefix parameter?
	SerdReader* reader = sord_new_reader(_c_obj, env, syntax, NULL);
	serd_reader_read_file(reader, path);
	serd_reader_free(reader);
	serd_free(path);
}

inline SerdStatus
Model::write_to_file(const std::string& uri, SerdSyntax syntax, SerdStyle style)
{
	uint8_t* path = serd_file_uri_parse((const uint8_t*)uri.c_str(), NULL);
	if (!path) {
		fprintf(stderr, "Failed to parse file URI <%s>\n", uri.c_str());
		return SERD_ERR_BAD_ARG;
	}

	FILE* const fd = fopen((const char*)path, "w");
	if (!fd) {
		fprintf(stderr, "Failed to open file %s\n", path);
		serd_free(path);
		return SERD_ERR_UNKNOWN;
	}
	serd_free(path);

	SerdURI base_uri = SERD_URI_NULL;
	if (serd_uri_parse((const uint8_t*)uri.c_str(), &base_uri)) {
		fprintf(stderr, "Invalid base URI <%s>\n", uri.c_str());
		fclose(fd);
		return SERD_ERR_BAD_ARG;
	}

	SerdWriter* writer = serd_writer_new(syntax,
	                                     style,
	                                     _world.prefixes().c_obj(),
	                                     &base_uri,
	                                     serd_file_sink,
	                                     fd);

	serd_env_foreach(_world.prefixes().c_obj(),
	                 (SerdPrefixSink)serd_writer_set_prefix,
	                 writer);

	sord_write(_c_obj, writer, 0);
	serd_writer_free(writer);
	fclose(fd);

	return SERD_SUCCESS;
}

static size_t
string_sink(const void* buf, size_t len, void* stream)
{
	std::string* str = (std::string*)stream;
	str->append((const char*)buf, len);
	return len;
}

inline std::string
Model::write_to_string(const std::string& base_uri_str,
                       SerdSyntax         syntax,
                       SerdStyle          style)
{
	SerdURI base_uri = SERD_URI_NULL;
	if (serd_uri_parse((const uint8_t*)base_uri_str.c_str(), &base_uri)) {
		fprintf(stderr, "Invalid base URI <%s>\n", base_uri_str.c_str());
		return "";
	}

	std::string ret;

	SerdWriter* writer = serd_writer_new(syntax,
	                                     style,
	                                     _world.prefixes().c_obj(),
	                                     &base_uri,
	                                     string_sink,
	                                     &ret);

	const SerdNode base_uri_node = serd_node_from_string(
		SERD_URI, (const uint8_t*)base_uri_str.c_str());
	serd_writer_set_base_uri(writer, &base_uri_node);

	serd_env_foreach(_world.prefixes().c_obj(),
	                 (SerdPrefixSink)serd_writer_set_prefix,
	                 writer);

	sord_write(_c_obj, writer, 0);

	serd_writer_free(writer);
	return ret;
}

inline void
Model::add_statement(const Node& subject,
                     const Node& predicate,
                     const Node& object)
{
	SordQuad quad = { subject.c_obj(),
	                  predicate.c_obj(),
	                  object.c_obj(),
	                  NULL };

	sord_add(_c_obj, quad);
}

inline Iter
Model::find(const Node& subject,
            const Node& predicate,
            const Node& object)
{
	SordQuad quad = { subject.c_obj(),
	                  predicate.c_obj(),
	                  object.c_obj(),
	                  NULL };

	return Iter(_world, sord_find(_c_obj, quad));
}

inline Node
Model::get(const Node& subject,
           const Node& predicate,
           const Node& object)
{
	SordNode* c_node = sord_get(
		_c_obj, subject.c_obj(), predicate.c_obj(), object.c_obj(), NULL);
	return Node(_world, c_node, false);
}

}  // namespace Sord

#endif  // SORD_SORDMM_HPP
