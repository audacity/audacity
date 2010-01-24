/* SLV2
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 *  
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef __SLV2_PLUGIN_H__
#define __SLV2_PLUGIN_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
#include <slv2/types.h>
#include <slv2/port.h>
#include <slv2/values.h>

/** \defgroup slv2_data Plugin data access
 *
 * These functions work exclusively with the plugin's RDF data,
 * they do not access the plugin's shared library in any way.
 *
 * An SLV2Plugin contains an in-memory cache of the plugin data, loaded
 * on demand.  Duplicating plugins should be avoided when possible for
 * performance reasons.
 *
 * @{
 */


/** Check if this plugin is valid.
 *
 * This is used by plugin lists to avoid loading plugins that are not valid
 * and will not work with libslv2 (eg plugins missing required fields, or
 * having multiple values for mandatory single-valued fields, etc.
 * 
 * Note that normal hosts do NOT need to use this - slv2 does not
 * load invalid plugins into plugin lists.  This is included for plugin
 * testing utilities, etc.
 *
 * \return true if \a plugin is valid.
 *
 * Time = Query
 */
bool
slv2_plugin_verify(SLV2Plugin plugin);


/** Get the URI of \a plugin.
 *
 * Any serialization that refers to plugins should refer to them by this.
 * Hosts SHOULD NOT save any filesystem paths, plugin indexes, etc. in saved
 * files; save only the URI.
 *
 * The URI is a globally unique identifier for one specific plugin.  Two
 * plugins with the same URI are compatible in port signature, and should
 * be guaranteed to work in a compatible and consistent way.  If a plugin
 * is upgraded in an incompatible way (eg if it has different ports), it
 * MUST have a different URI than it's predecessor.
 *
 * \return a shared string which must not be modified or free()'d.
 *
 * Time = O(1)
 */
SLV2Value
slv2_plugin_get_uri(SLV2Plugin plugin);


/** Get the (resolvable) URI of the plugins "main" bundle.
 *
 * This returns the URI of the bundle where the plugin itself was found.
 * Note that the data for a plugin may be spread over many bundles, that is,
 * slv2_plugin_get_data_uris may returns URIs which are not below this one.
 *
 * Typical hosts should not need to use this function.
 *
 * Note this always returns a fully qualified URI.  If you want a local
 * filesystem path, use slv2_uri_to_path.
 * 
 * \return a shared string which must not be modified or freed.
 *
 * Time = O(1)
 */
SLV2Value
slv2_plugin_get_bundle_uri(SLV2Plugin plugin);


/** Get the (resolvable) URIs of the RDF data files that define a plugin.
 *
 * Typical hosts should not need to use this function.
 *
 * Note this always returns fully qualified URIs.  If you want local
 * filesystem paths, use slv2_uri_to_path.
 *
 * \return a list of complete URLs eg. "file:///foo/ABundle.lv2/aplug.ttl",
 * which is shared and must not be modified or freed.
 *
 * Time = O(1)
 */
SLV2Values
slv2_plugin_get_data_uris(SLV2Plugin plugin);


/** Get the (resolvable) URI of the shared library for \a plugin.
 *
 * Note this always returns a fully qualified URI.  If you want a local
 * filesystem path, use slv2_uri_to_path.
 * 
 * \return a shared string which must not be modified or freed.
 *
 * Time = O(1)
 */
SLV2Value
slv2_plugin_get_library_uri(SLV2Plugin plugin);


/** Get the name of \a plugin.
 *
 * This is guaranteed to return the untranslated name (the doap:name in the
 * data file without a language tag).  Returned value must be freed by
 * the caller.
 *
 * Time = Query
 */
SLV2Value
slv2_plugin_get_name(SLV2Plugin plugin);


/** Get the class this plugin belongs to (ie Filters).
 */
SLV2PluginClass
slv2_plugin_get_class(SLV2Plugin plugin);


/** Get a value associated with the plugin in a plugin's data files.
 *
 * Returns the ?object of all triples found of the form:
 *
 * <code>&lt;plugin-uri&gt; predicate ?object</code>
 * 
 * May return NULL if the property was not found, or if object(s) is not
 * sensibly represented as an SLV2Values (e.g. blank nodes).
 *
 * Return value must be freed by caller with slv2_values_free.
 *
 * \a predicate must be either a URI or a QName.
 * See SLV2URIType documentation for examples.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_get_value(SLV2Plugin p,
                      SLV2Value  predicate);


/** Get a value associated with the plugin in a plugin's data files.
 *
 * This function is identical to slv2_plugin_get_value, but takes a QName
 * string parameter for a predicate instead of an SLV2Value, which may be
 * more convenient.
 */
SLV2Values
slv2_plugin_get_value_by_qname(SLV2Plugin  p,
                               const char* predicate);


/** Get a translated value associated with the plugin in a plugin's data files.
 *
 * This function is identical to slv2_plugin_get_value, but takes a QName
 * string parameter for a predicate instead of an SLV2Value, which may be
 * more convenient. It returns the value translated to the current language
 * if possible.
 */
SLV2Values
slv2_plugin_get_value_by_qname_i18n(SLV2Plugin  p,
				    const char* predicate);


/** Get a value associated with some subject in a plugin's data files.
 *
 * Returns the ?object of all triples found of the form:
 *
 * <code>subject predicate ?object</code>
 *
 * This can be used to investigate URIs returned by slv2_plugin_get_value
 * (if information about it is contained in the plugin's data files).
 *
 * May return NULL if the property was not found, or if object is not
 * sensibly represented as an SLV2Values (e.g. blank nodes).
 *
 * \a predicate must be either a URI or a QName.
 * See SLV2URIType documentation for examples.
 *
 * Return value must be freed by caller with slv2_values_free.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_get_value_for_subject(SLV2Plugin  p,
                                  SLV2Value   subject_uri,
								  SLV2Value   predicate_uri);


/** Return whether a feature is supported by a plugin.
 *
 * This will return true if the feature is an optional or required feature
 * of the plugin.
 *
 * Time = Query
 */
bool
slv2_plugin_has_feature(SLV2Plugin p,
                        SLV2Value  feature_uri);


/** Get the LV2 Features supported (required or optionally) by a plugin.
 *
 * A feature is "supported" by a plugin if it is required OR optional.
 *
 * Since required features have special rules the host must obey, this function
 * probably shouldn't be used by normal hosts.  Using slv2_plugin_get_optional_features
 * and slv2_plugin_get_required_features separately is best in most cases.
 *
 * Returned value must be freed by caller with slv2_values_free.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_get_supported_features(SLV2Plugin p);


/** Get the LV2 Features required by a plugin.
 *
 * If a feature is required by a plugin, hosts MUST NOT use the plugin if they do not
 * understand (or are unable to support) that feature.
 *
 * All values returned here MUST be passed to the plugin's instantiate method
 * (along with data, if necessary, as defined by the feature specification)
 * or plugin instantiation will fail.
 *
 * Return value must be freed by caller with slv2_values_free.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_get_required_features(SLV2Plugin p);


/** Get the LV2 Features optionally supported by a plugin.
 *
 * Hosts MAY ignore optional plugin features for whatever reasons.  Plugins
 * MUST operate (at least somewhat) if they are instantiated without being
 * passed optional features.
 *
 * Return value must be freed by caller with slv2_values_free.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_get_optional_features(SLV2Plugin p);


/** Get the number of ports on this plugin.
 *
 * Time = O(1)
 */
uint32_t
slv2_plugin_get_num_ports(SLV2Plugin p);


/** Get the port ranges (minimum, maximum and default values) for all ports.
 *
 * \a min_values, \a max_values and \a def_values must either point to an array
 * of N floats, where N is the value returned by slv2_plugin_get_num_ports()
 * for this plugin, or NULL.  The elements of the array will be set to the
 * the minimum, maximum and default values of the ports on this plugin,
 * with array index corresponding to port index.  If a port doesn't have a
 * minimum, maximum or default value, or the port's type is not float, the
 * corresponding array element will be set to NAN.
 * 
 * This is a convenience method for the common case of getting the range of
 * all float ports on a plugin, and may be significantly faster than
 * repeated calls to slv2_port_get_range.
 */
void
slv2_plugin_get_port_ranges_float(SLV2Plugin p, 
                                  float*     min_values, 
                                  float*     max_values,
                                  float*     def_values);

/** Get the number of ports on this plugin that are members of some class(es).
 *
 * Note that this is a varargs function so ports fitting any type 'profile'
 * desired can be found quickly.  REMEMBER TO TERMINATE THE PARAMETER LIST
 * OF THIS FUNCTION WITH NULL OR VERY NASTY THINGS WILL HAPPEN.
 *
 * Time = O(1)
 */
uint32_t
slv2_plugin_get_num_ports_of_class(SLV2Plugin p,
                                   SLV2Value  class_1, ...);

/** Return whether or not the plugin introduces (and reports) latency.
 *
 * The index of the latency port can be found with slv2_plugin_get_latency_port
 * ONLY if this function returns true.
 *
 * Time = Query
 */
bool
slv2_plugin_has_latency(SLV2Plugin p);


/** Return the index of the plugin's latency port.
 *
 * It is a fatal error to call this on a plugin without checking if the port
 * exists by first calling slv2_plugin_has_latency.
 *
 * Any plugin that introduces unwanted latency that should be compensated for
 * (by hosts with the ability/need) MUST provide this port, which is a control
 * rate output port that reports the latency for each cycle in frames.
 *
 * Time = Query
 */
uint32_t
slv2_plugin_get_latency_port_index(SLV2Plugin p);


/** Query a plugin for a single variable (i.e. SELECT a single ?value).
 *
 * \param plugin The plugin to query.
 * \param sparql_str A SPARQL SELECT query.
 * \param variable The index of the variable to return results for
 *        (e.g. with "<code>SELECT ?foo ?bar</code>" foo=0, bar=1).
 * \return All matches for \a variable.
 *
 * Time = Query
 */
SLV2Values
slv2_plugin_query_variable(SLV2Plugin  plugin,
                           const char* sparql_str,
                           unsigned    variable);


/** Query a plugin and return the number of results found.
 *
 * Note that this function will work, but is mostly meaningless for queries
 * that are not SELECT DISTINCT.
 *
 * \param plugin The plugin to query.
 * \param sparql_str A SPARQL SELECT DISTINCT query.
 *
 * Time = Query
 */
unsigned
slv2_plugin_query_count(SLV2Plugin  plugin,
                        const char* sparql_str);


/** Get a port on this plugin by \a index.
 *
 * To perform multiple calls on a port, the returned value should
 * be cached and used repeatedly.
 *
 * Time = O(1)
 */
SLV2Port
slv2_plugin_get_port_by_index(SLV2Plugin plugin,
                              uint32_t   index);


/** Get a port on this plugin by \a symbol.
 *
 * To perform multiple calls on a port, the returned value should
 * be cached and used repeatedly.  Note this function is slower
 * than slv2_plugin_get_port_by_index, especially on plugins
 * with a very large number of ports.
 *
 * Time = O(n)
 */
SLV2Port
slv2_plugin_get_port_by_symbol(SLV2Plugin plugin,
                               SLV2Value  symbol);


/** Get a list of all UIs available for this plugin.
 *
 * Note this returns the URI of the UI, and not the path/URI to its shared
 * library, use slv2_ui_get_library_uri with the values returned
 * here for that.
 *
 * Returned value must be freed by caller using slv2_uis_free.
 *
 * \param plugin The plugin to get the UIs for. 
 *
 * Time = Query
 */
SLV2UIs
slv2_plugin_get_uis(SLV2Plugin plugin);


/** Get the full name of the plugin's author.
 *
 * Returns NULL if author name is not present.
 * Returned value must be freed by caller.
 *
 * Time = Query
 */
SLV2Value
slv2_plugin_get_author_name(SLV2Plugin plugin);


/** Get the email address of the plugin's author.
 *
 * Returns NULL if author email address is not present.
 * Returned value must be freed by caller.
 *
 * Time = Query
 */
SLV2Value
slv2_plugin_get_author_email(SLV2Plugin plugin);


/** Get the email address of the plugin's author.
 *
 * Returns NULL if author homepage is not present.
 * Returned value must be freed by caller.
 *
 * Time = Query
 */
SLV2Value
slv2_plugin_get_author_homepage(SLV2Plugin plugin);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_PLUGIN_H__ */

