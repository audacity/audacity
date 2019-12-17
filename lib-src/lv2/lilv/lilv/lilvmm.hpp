/*
  Copyright 2007-2017 David Robillard <http://drobilla.net>

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

#ifndef LILV_LILVMM_HPP
#define LILV_LILVMM_HPP

#include "lilv/lilv.h"

namespace Lilv {

#if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wdeprecated-declarations"
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

LILV_DEPRECATED
static inline const char*
uri_to_path(const char* uri) {
	return lilv_uri_to_path(uri);
}

#if defined(__clang__)
#    pragma clang diagnostic pop
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic pop
#endif

#define LILV_WRAP0(RT, prefix, name) \
	inline RT name() { return lilv_ ## prefix ## _ ## name (me); }

#define LILV_WRAP0_VOID(prefix, name) \
	inline void name() { lilv_ ## prefix ## _ ## name(me); }

#define LILV_WRAP1(RT, prefix, name, T1, a1) \
	inline RT name(T1 a1) { return lilv_ ## prefix ## _ ## name (me, a1); }

#define LILV_WRAP1_VOID(prefix, name, T1, a1) \
	inline void name(T1 a1) { lilv_ ## prefix ## _ ## name(me, a1); }

#define LILV_WRAP2(RT, prefix, name, T1, a1, T2, a2) \
	inline RT name(T1 a1, T2 a2) { \
		return lilv_ ## prefix ## _ ## name(me, a1, a2); \
	}

#define LILV_WRAP3(RT, prefix, name, T1, a1, T2, a2, T3, a3) \
	inline RT name(T1 a1, T2 a2, T3 a3) { \
		return lilv_ ## prefix ## _ ## name(me, a1, a2, a3); \
	}

#define LILV_WRAP2_VOID(prefix, name, T1, a1, T2, a2) \
	inline void name(T1 a1, T2 a2) { lilv_ ## prefix ## _ ## name(me, a1, a2); }

#ifndef SWIG
#define LILV_WRAP_CONVERSION(CT) \
	inline operator CT*() const { return me; }
#else
#define LILV_WRAP_CONVERSION(CT)
#endif

struct Node {
	inline Node(const LilvNode* node) : me(lilv_node_duplicate(node)) {}
	inline Node(const Node& copy)     : me(lilv_node_duplicate(copy.me)) {}

	inline ~Node() { lilv_node_free(me); }

	inline bool equals(const Node& other) const {
		return lilv_node_equals(me, other.me);
	}

	inline bool operator==(const Node& other) const { return equals(other); }

	LILV_WRAP_CONVERSION(LilvNode);

	LILV_WRAP0(char*,       node, get_turtle_token);
	LILV_WRAP0(bool,        node, is_uri);
	LILV_WRAP0(const char*, node, as_uri);
	LILV_WRAP0(bool,        node, is_blank);
	LILV_WRAP0(const char*, node, as_blank);
	LILV_WRAP0(bool,        node, is_literal);
	LILV_WRAP0(bool,        node, is_string);
	LILV_WRAP0(const char*, node, as_string);
	LILV_WRAP0(bool,        node, is_float);
	LILV_WRAP0(float,       node, as_float);
	LILV_WRAP0(bool,        node, is_int);
	LILV_WRAP0(int,         node, as_int);
	LILV_WRAP0(bool,        node, is_bool);
	LILV_WRAP0(bool,        node, as_bool);

	LilvNode* me;
};

struct ScalePoint {
	inline ScalePoint(const LilvScalePoint* c_obj) : me(c_obj) {}
	LILV_WRAP_CONVERSION(const LilvScalePoint);

	LILV_WRAP0(const LilvNode*, scale_point, get_label);
	LILV_WRAP0(const LilvNode*, scale_point, get_value);

	const LilvScalePoint* me;
};

struct PluginClass {
	inline PluginClass(const LilvPluginClass* c_obj) : me(c_obj) {}
	LILV_WRAP_CONVERSION(const LilvPluginClass);

	LILV_WRAP0(Node, plugin_class, get_parent_uri);
	LILV_WRAP0(Node, plugin_class, get_uri);
	LILV_WRAP0(Node, plugin_class, get_label);
	LILV_WRAP0(LilvPluginClasses*, plugin_class, get_children);

	const LilvPluginClass* me;
};

#define LILV_WRAP_COLL(CT, ET, prefix) \
	inline CT(const Lilv ## CT* c_obj) : me(c_obj) {} \
	LILV_WRAP_CONVERSION(const Lilv ## CT); \
	LILV_WRAP0(unsigned, prefix, size); \
	LILV_WRAP1(const ET, prefix, get, LilvIter*, i); \
	LILV_WRAP0(LilvIter*, prefix, begin); \
	LILV_WRAP1(LilvIter*, prefix, next, LilvIter*, i); \
	LILV_WRAP1(bool, prefix, is_end, LilvIter*, i); \
	const Lilv ## CT* me; \

struct PluginClasses {
	LILV_WRAP_COLL(PluginClasses, PluginClass, plugin_classes);
	LILV_WRAP1(const PluginClass, plugin_classes,
	           get_by_uri, const LilvNode*, uri);
};

struct ScalePoints {
	LILV_WRAP_COLL(ScalePoints, ScalePoint, scale_points);
};

struct Nodes {
	LILV_WRAP_COLL(Nodes, Node, nodes);
	LILV_WRAP1(bool, nodes, contains, const Node, node);
	LILV_WRAP0(Node, nodes, get_first);
};

struct UI {
	inline UI(const LilvUI* c_obj) : me(c_obj) {}
	LILV_WRAP_CONVERSION(const LilvUI);

	LILV_WRAP0(const LilvNode*, ui, get_uri);
	LILV_WRAP0(const LilvNode*, ui, get_bundle_uri);
	LILV_WRAP0(const LilvNode*, ui, get_binary_uri);
	LILV_WRAP0(const LilvNodes*, ui, get_classes);
	/*LILV_WRAP3(bool, ui, is_supported,
	           LilvUISupportedFunc, supported_func,
	           const LilvNode*,     container_type,
	           const LilvNode**,    ui_type);*/
	LILV_WRAP1(bool, ui, is_a, const LilvNode*, class_uri);

	const LilvUI* me;
};

struct UIs {
	LILV_WRAP_COLL(UIs, UI, uis);
};

struct Port {
	inline Port(const LilvPlugin* p, const LilvPort* c_obj)
		: parent(p), me(c_obj)
	{}

	LILV_WRAP_CONVERSION(const LilvPort);

#define LILV_PORT_WRAP0(RT, name) \
	inline RT name () { return lilv_port_ ## name (parent, me); }

#define LILV_PORT_WRAP1(RT, name, T1, a1) \
	inline RT name (T1 a1) { return lilv_port_ ## name (parent, me, a1); }

	LILV_PORT_WRAP1(LilvNodes*, get_value, LilvNode*, predicate);
	LILV_PORT_WRAP0(LilvNodes*, get_properties)
	LILV_PORT_WRAP1(bool, has_property, LilvNode*, property_uri);
	LILV_PORT_WRAP1(bool, supports_event, LilvNode*, event_uri);
	LILV_PORT_WRAP0(const LilvNode*,  get_symbol);
	LILV_PORT_WRAP0(LilvNode*,  get_name);
	LILV_PORT_WRAP0(const LilvNodes*, get_classes);
	LILV_PORT_WRAP1(bool, is_a, LilvNode*, port_class);
	LILV_PORT_WRAP0(LilvScalePoints*, get_scale_points);

	// TODO: get_range (output parameters)

	const LilvPlugin* parent;
	const LilvPort*   me;
};

struct Plugin {
	inline Plugin(const LilvPlugin* c_obj) : me(c_obj) {}
	LILV_WRAP_CONVERSION(const LilvPlugin);

	LILV_WRAP0(bool,        plugin, verify);
	LILV_WRAP0(Node,        plugin, get_uri);
	LILV_WRAP0(Node,        plugin, get_bundle_uri);
	LILV_WRAP0(Nodes,       plugin, get_data_uris);
	LILV_WRAP0(Node,        plugin, get_library_uri);
	LILV_WRAP0(Node,        plugin, get_name);
	LILV_WRAP0(PluginClass, plugin, get_class);
	LILV_WRAP1(Nodes,       plugin, get_value, Node, pred);
	LILV_WRAP1(bool,        plugin, has_feature, Node, feature_uri);
	LILV_WRAP0(Nodes,       plugin, get_supported_features);
	LILV_WRAP0(Nodes,       plugin, get_required_features);
	LILV_WRAP0(Nodes,       plugin, get_optional_features);
	LILV_WRAP0(unsigned,    plugin, get_num_ports);
	LILV_WRAP0(bool,        plugin, has_latency);
	LILV_WRAP0(unsigned,    plugin, get_latency_port_index);
	LILV_WRAP0(Node,        plugin, get_author_name);
	LILV_WRAP0(Node,        plugin, get_author_email);
	LILV_WRAP0(Node,        plugin, get_author_homepage);
	LILV_WRAP0(bool,        plugin, is_replaced);
	LILV_WRAP0(Nodes,       plugin, get_extension_data);
	LILV_WRAP0(UIs,         plugin, get_uis);
	LILV_WRAP1(Nodes,       plugin, get_related, Node, type);

	inline Port get_port_by_index(unsigned index) {
		return Port(me, lilv_plugin_get_port_by_index(me, index));
	}

	inline Port get_port_by_symbol(LilvNode* symbol) {
		return Port(me, lilv_plugin_get_port_by_symbol(me, symbol));
	}

	inline void get_port_ranges_float(float* min_values,
	                                  float* max_values,
	                                  float* def_values) {
		return lilv_plugin_get_port_ranges_float(
			me, min_values, max_values, def_values);
	}

	inline unsigned get_num_ports_of_class(LilvNode* class_1, ...) {
		va_list args;
		va_start(args, class_1);

		const uint32_t count = lilv_plugin_get_num_ports_of_class_va(
			me, class_1, args);

		va_end(args);
		return count;
	}

	const LilvPlugin* me;
};

struct Plugins {
	LILV_WRAP_COLL(Plugins, Plugin, plugins);
	LILV_WRAP1(const Plugin, plugins, get_by_uri, const LilvNode*, uri);
};

struct Instance {
	inline Instance(LilvInstance* instance) : me(instance) {}

	LILV_DEPRECATED
	inline Instance(Plugin plugin, double sample_rate) {
		me = lilv_plugin_instantiate(plugin, sample_rate, NULL);
	}

	LILV_DEPRECATED inline Instance(Plugin              plugin,
	                                double              sample_rate,
	                                LV2_Feature* const* features) {
		me = lilv_plugin_instantiate(plugin, sample_rate, features);
	}

	static inline Instance* create(Plugin              plugin,
	                               double              sample_rate,
	                               LV2_Feature* const* features) {
		LilvInstance* me = lilv_plugin_instantiate(
			plugin, sample_rate, features);

		return me ? new Instance(me) : NULL;
	}

	LILV_WRAP_CONVERSION(LilvInstance);

	LILV_WRAP2_VOID(instance, connect_port,
	                unsigned, port_index,
	                void*,    data_location);

	LILV_WRAP0_VOID(instance, activate);
	LILV_WRAP1_VOID(instance, run, unsigned, sample_count);
	LILV_WRAP0_VOID(instance, deactivate);

	inline const void* get_extension_data(const char* uri) {
		return lilv_instance_get_extension_data(me, uri);
	}

	inline const LV2_Descriptor* get_descriptor() {
		return lilv_instance_get_descriptor(me);
	}

	inline LV2_Handle get_handle() {
		return lilv_instance_get_handle(me);
	}

	LilvInstance* me;
};

struct World {
	inline World() : me(lilv_world_new()) {}
	inline ~World() { lilv_world_free(me); }

	inline LilvNode* new_uri(const char* uri) {
		return lilv_new_uri(me, uri);
	}
	inline LilvNode* new_string(const char* str) {
		return lilv_new_string(me, str);
	}
	inline LilvNode* new_int(int val) {
		return lilv_new_int(me, val);
	}
	inline LilvNode* new_float(float val) {
		return lilv_new_float(me, val);
	}
	inline LilvNode* new_bool(bool val) {
		return lilv_new_bool(me, val);
	}
	inline Nodes find_nodes(const LilvNode* subject,
	                        const LilvNode* predicate,
	                        const LilvNode* object) {
		return lilv_world_find_nodes(me, subject, predicate, object);
	}

	LILV_WRAP2_VOID(world, set_option, const char*, uri, LilvNode*, value);
	LILV_WRAP0_VOID(world, load_all);
	LILV_WRAP1_VOID(world, load_bundle, LilvNode*, bundle_uri);
	LILV_WRAP0(const LilvPluginClass*, world, get_plugin_class);
	LILV_WRAP0(const LilvPluginClasses*, world, get_plugin_classes);
	LILV_WRAP0(const Plugins, world, get_all_plugins);
	LILV_WRAP1(int, world, load_resource, const LilvNode*, resource);

	LilvWorld* me;
};

} /* namespace Lilv */

#endif /* LILV_LILVMM_HPP */
