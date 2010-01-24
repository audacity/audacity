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

#ifndef __SLV2_PLUGINUIINSTANCE_H__
#define __SLV2_PLUGINUIINSTANCE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <assert.h>
#include <slv2/lv2_ui.h>
#include <slv2/plugin.h>

typedef struct _SLV2UIInstanceImpl* SLV2UIInstanceImpl;

/* Instance of a plugin UI.
 *
 * All details are in hidden in the pimpl member to avoid making the
 * implementation a part of the ABI.
 */
typedef struct _SLV2UIInstance {
	SLV2UIInstanceImpl pimpl; ///< Private implementation
}* SLV2UIInstance;


/** \addtogroup slv2_library
 * @{
 */


/** Instantiate a plugin UI.
 *
 * The returned object represents shared library objects loaded into memory,
 * it must be cleaned up with slv2_ui_instance_free when no longer
 * needed.
 * 
 * \a plugin is not modified or directly referenced by the returned object
 * (instances store only a copy of the plugin's URI).
 * 
 * \a host_features NULL-terminated array of features the host supports.
 * NULL may be passed if the host supports no additional features (unlike
 * the LV2 specification - SLV2 takes care of it).
 *
 * \return NULL if instantiation failed.
 */
SLV2UIInstance
slv2_ui_instantiate(SLV2Plugin                plugin,
                    SLV2UI                    ui,
                    LV2UI_Write_Function      write_function,
                    LV2UI_Controller          controller,
                    const LV2_Feature* const* features);


/** Free a plugin UI instance.
 *
 * It is the caller's responsibility to ensure all references to the UI
 * instance (including any returned widgets) are cut before calling
 * this function.
 *
 * \a instance is invalid after this call.
 */
void
slv2_ui_instance_free(SLV2UIInstance instance);


/** Get the widget for the UI instance.
 */
LV2UI_Widget
slv2_ui_instance_get_widget(SLV2UIInstance instance);


/** Get the LV2UI_Descriptor of the plugin UI instance.
 *
 * Normally hosts should not need to access the LV2UI_Descriptor directly,
 * use the slv2_ui_instance_* functions.
 *
 * The returned descriptor is shared and must not be deleted.
 */
const LV2UI_Descriptor*
slv2_ui_instance_get_descriptor(SLV2UIInstance instance);


/** Get the LV2UI_Handle of the plugin UI instance.
 *
 * Normally hosts should not need to access the LV2UI_Handle directly,
 * use the slv2_ui_instance_* functions.
 * 
 * The returned handle is shared and must not be deleted.
 */
LV2UI_Handle
slv2_ui_instance_get_handle(SLV2UIInstance instance);


/** @} */

#ifdef __cplusplus
}
#endif


#endif /* __SLV2_PLUGINUIINSTANCE_H__ */

