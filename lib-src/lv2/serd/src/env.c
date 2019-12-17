/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

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

#include "serd_internal.h"

#include <stdlib.h>
#include <string.h>

typedef struct {
	SerdNode name;
	SerdNode uri;
} SerdPrefix;

struct SerdEnvImpl {
	SerdPrefix* prefixes;
	size_t      n_prefixes;
	SerdNode    base_uri_node;
	SerdURI     base_uri;
};

SerdEnv*
serd_env_new(const SerdNode* base_uri)
{
	SerdEnv* env = (SerdEnv*)calloc(1, sizeof(struct SerdEnvImpl));
	if (env && base_uri) {
		serd_env_set_base_uri(env, base_uri);
	}
	return env;
}

void
serd_env_free(SerdEnv* env)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		serd_node_free(&env->prefixes[i].name);
		serd_node_free(&env->prefixes[i].uri);
	}
	free(env->prefixes);
	serd_node_free(&env->base_uri_node);
	free(env);
}

const SerdNode*
serd_env_get_base_uri(const SerdEnv* env,
                      SerdURI*       out)
{
	if (out) {
		*out = env->base_uri;
	}
	return &env->base_uri_node;
}

SerdStatus
serd_env_set_base_uri(SerdEnv*        env,
                      const SerdNode* uri)
{
	if (!env || !uri) {
		return SERD_ERR_BAD_ARG;
	}

	// Resolve base URI and create a new node and URI for it
	SerdURI  base_uri;
	SerdNode base_uri_node = serd_node_new_uri_from_node(
		uri, &env->base_uri, &base_uri);

	if (base_uri_node.buf) {
		// Replace the current base URI
		serd_node_free(&env->base_uri_node);
		env->base_uri_node = base_uri_node;
		env->base_uri      = base_uri;
		return SERD_SUCCESS;
	}
	return SERD_ERR_BAD_ARG;
}

static inline SerdPrefix*
serd_env_find(const SerdEnv* env,
              const uint8_t* name,
              size_t         name_len)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		const SerdNode* const prefix_name = &env->prefixes[i].name;
		if (prefix_name->n_bytes == name_len) {
			if (!memcmp(prefix_name->buf, name, name_len)) {
				return &env->prefixes[i];
			}
		}
	}
	return NULL;
}

static void
serd_env_add(SerdEnv*        env,
             const SerdNode* name,
             const SerdNode* uri)
{
	SerdPrefix* const prefix = serd_env_find(env, name->buf, name->n_bytes);
	if (prefix) {
		SerdNode old_prefix_uri = prefix->uri;
		prefix->uri = serd_node_copy(uri);
		serd_node_free(&old_prefix_uri);
	} else {
		env->prefixes = (SerdPrefix*)realloc(
			env->prefixes, (++env->n_prefixes) * sizeof(SerdPrefix));
		env->prefixes[env->n_prefixes - 1].name = serd_node_copy(name);
		env->prefixes[env->n_prefixes - 1].uri  = serd_node_copy(uri);
	}
}

SerdStatus
serd_env_set_prefix(SerdEnv*        env,
                    const SerdNode* name,
                    const SerdNode* uri)
{
	if (!name->buf || uri->type != SERD_URI) {
		return SERD_ERR_BAD_ARG;
	} else if (serd_uri_string_has_scheme(uri->buf)) {
		// Set prefix to absolute URI
		serd_env_add(env, name, uri);
	} else {
		// Resolve relative URI and create a new node and URI for it
		SerdURI  abs_uri;
		SerdNode abs_uri_node = serd_node_new_uri_from_node(
			uri, &env->base_uri, &abs_uri);

		// Set prefix to resolved (absolute) URI
		serd_env_add(env, name, &abs_uri_node);
		serd_node_free(&abs_uri_node);
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_env_set_prefix_from_strings(SerdEnv*       env,
                                 const uint8_t* name,
                                 const uint8_t* uri)
{
	const SerdNode name_node = serd_node_from_string(SERD_LITERAL, name);
	const SerdNode uri_node  = serd_node_from_string(SERD_URI, uri);

	return serd_env_set_prefix(env, &name_node, &uri_node);
}

bool
serd_env_qualify(const SerdEnv*  env,
                 const SerdNode* uri,
                 SerdNode*       prefix,
                 SerdChunk*      suffix)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		const SerdNode* const prefix_uri = &env->prefixes[i].uri;
		if (uri->n_bytes >= prefix_uri->n_bytes) {
			if (!strncmp((const char*)uri->buf,
			             (const char*)prefix_uri->buf,
			             prefix_uri->n_bytes)) {
				*prefix = env->prefixes[i].name;
				suffix->buf = uri->buf + prefix_uri->n_bytes;
				suffix->len = uri->n_bytes - prefix_uri->n_bytes;
				return true;
			}
		}
	}
	return false;
}

SerdStatus
serd_env_expand(const SerdEnv*  env,
                const SerdNode* curie,
                SerdChunk*      uri_prefix,
                SerdChunk*      uri_suffix)
{
	const uint8_t* const colon = (const uint8_t*)memchr(
		curie->buf, ':', curie->n_bytes + 1);
	if (curie->type != SERD_CURIE || !colon) {
		return SERD_ERR_BAD_ARG;
	}

	const size_t            name_len = colon - curie->buf;
	const SerdPrefix* const prefix   = serd_env_find(env, curie->buf, name_len);
	if (prefix) {
		uri_prefix->buf = prefix->uri.buf;
		uri_prefix->len = prefix->uri.n_bytes;
		uri_suffix->buf = colon + 1;
		uri_suffix->len = curie->n_bytes - (colon - curie->buf) - 1;
		return SERD_SUCCESS;
	}
	return SERD_ERR_BAD_CURIE;
}

SerdNode
serd_env_expand_node(const SerdEnv*  env,
                     const SerdNode* node)
{
	switch (node->type) {
	case SERD_CURIE: {
		SerdChunk prefix;
		SerdChunk suffix;
		if (serd_env_expand(env, node, &prefix, &suffix)) {
			return SERD_NODE_NULL;
		}
		const size_t len = prefix.len + suffix.len;
		uint8_t*     buf = (uint8_t*)malloc(len + 1);
		SerdNode     ret = { buf, len, 0, 0, SERD_URI };
		snprintf((char*)buf, len + 1, "%s%s", prefix.buf, suffix.buf);
		ret.n_chars = serd_strlen(buf, NULL, NULL);
		return ret;
	}
	case SERD_URI: {
		SerdURI ignored;
		return serd_node_new_uri_from_node(node, &env->base_uri, &ignored);
	}
	default:
		return SERD_NODE_NULL;
	}
}

void
serd_env_foreach(const SerdEnv* env,
                 SerdPrefixSink func,
                 void*          handle)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		func(handle, &env->prefixes[i].name, &env->prefixes[i].uri);
	}
}
