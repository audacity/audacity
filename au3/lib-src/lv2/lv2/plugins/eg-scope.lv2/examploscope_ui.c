/*
  Copyright 2013 Robin Gareus <robin@gareus.org>

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

#include "./uris.h"

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/core/lv2.h"
#include "lv2/ui/ui.h"
#include "lv2/urid/urid.h"

#include <cairo.h>
#include <gdk/gdk.h>
#include <glib-object.h>
#include <glib.h>
#include <gobject/gclosure.h>
#include <gtk/gtk.h>

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Drawing area size
#define DAWIDTH  (640)
#define DAHEIGHT (200)

/**
   Max continuous points on path.  Many short-path segments are
   expensive|inefficient long paths are not supported by all surfaces (usually
   its a miter - not point - limit, depending on used cairo backend)
*/
#define MAX_CAIRO_PATH (128)

/**
   Representation of the raw audio-data for display (min | max) values for a
   given 'index' position.
*/
typedef struct {
	float data_min[DAWIDTH];
	float data_max[DAWIDTH];

	uint32_t idx;
	uint32_t sub;
} ScoChan;

typedef struct {
	LV2_Atom_Forge forge;
	LV2_URID_Map*  map;
	ScoLV2URIs     uris;

	LV2UI_Write_Function write;
	LV2UI_Controller     controller;

	GtkWidget*     hbox;
	GtkWidget*     vbox;
	GtkWidget*     sep[2];
	GtkWidget*     darea;
	GtkWidget*     btn_pause;
	GtkWidget*     lbl_speed;
	GtkWidget*     lbl_amp;
	GtkWidget*     spb_speed;
	GtkWidget*     spb_amp;
	GtkAdjustment* spb_speed_adj;
	GtkAdjustment* spb_amp_adj;

	ScoChan  chn[2];
	uint32_t stride;
	uint32_t n_channels;
	bool     paused;
	float    rate;
	bool     updating;
} EgScopeUI;


/** Send current UI settings to backend. */
static void
send_ui_state(LV2UI_Handle handle)
{
	EgScopeUI*  ui   = (EgScopeUI*)handle;
	const float gain = gtk_spin_button_get_value(GTK_SPIN_BUTTON(ui->spb_amp));

	// Use local buffer on the stack to build atom
	uint8_t obj_buf[1024];
	lv2_atom_forge_set_buffer(&ui->forge, obj_buf, sizeof(obj_buf));

	// Start a ui:State object
	LV2_Atom_Forge_Frame frame;
	LV2_Atom*            msg = (LV2_Atom*)lv2_atom_forge_object(
		&ui->forge, &frame, 0, ui->uris.ui_State);

	// msg[samples-per-pixel] = integer
	lv2_atom_forge_key(&ui->forge, ui->uris.ui_spp);
	lv2_atom_forge_int(&ui->forge, ui->stride);

	// msg[amplitude] = float
	lv2_atom_forge_key(&ui->forge, ui->uris.ui_amp);
	lv2_atom_forge_float(&ui->forge, gain);

	// Finish ui:State object
	lv2_atom_forge_pop(&ui->forge, &frame);

	// Send message to plugin port '0'
	ui->write(ui->controller,
	          0,
	          lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);
}

/** Notify backend that UI is closed. */
static void
send_ui_disable(LV2UI_Handle handle)
{
	EgScopeUI* ui = (EgScopeUI*)handle;
	send_ui_state(handle);

	uint8_t obj_buf[64];
	lv2_atom_forge_set_buffer(&ui->forge, obj_buf, sizeof(obj_buf));

	LV2_Atom_Forge_Frame frame;
	LV2_Atom*            msg = (LV2_Atom*)lv2_atom_forge_object(
		&ui->forge, &frame, 0, ui->uris.ui_Off);
	lv2_atom_forge_pop(&ui->forge, &frame);
	ui->write(ui->controller,
	          0,
	          lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);
}

/**
   Notify backend that UI is active.

   The plugin should send state and enable data transmission.
*/
static void
send_ui_enable(LV2UI_Handle handle)
{
	EgScopeUI* ui = (EgScopeUI*)handle;

	uint8_t obj_buf[64];
	lv2_atom_forge_set_buffer(&ui->forge, obj_buf, sizeof(obj_buf));

	LV2_Atom_Forge_Frame frame;
	LV2_Atom*            msg = (LV2_Atom*)lv2_atom_forge_object(
		&ui->forge, &frame, 0, ui->uris.ui_On);
	lv2_atom_forge_pop(&ui->forge, &frame);
	ui->write(ui->controller,
	          0,
	          lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);
}

/** Gtk widget callback. */
static gboolean
on_cfg_changed(GtkWidget* widget, gpointer data)
{
	EgScopeUI* ui = (EgScopeUI*)data;
	if (!ui->updating) {
		// Only send UI state if the change is from user interaction
		send_ui_state(data);
	}
	return TRUE;
}

/**
   Gdk drawing area draw callback.

   Called in Gtk's main thread and uses Cairo to draw the data.
*/
static gboolean
on_expose_event(GtkWidget* widget, GdkEventExpose* ev, gpointer data)
{
	EgScopeUI*  ui   = (EgScopeUI*)data;
	const float gain = gtk_spin_button_get_value(GTK_SPIN_BUTTON(ui->spb_amp));

	// Get cairo type for the gtk window
	cairo_t* cr;
	cr = gdk_cairo_create(ui->darea->window);

	// Limit cairo-drawing to exposed area
	cairo_rectangle(cr, ev->area.x, ev->area.y, ev->area.width, ev->area.height);
	cairo_clip(cr);

	// Clear background
	cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 1.0);
	cairo_rectangle(cr, 0, 0, DAWIDTH, DAHEIGHT * ui->n_channels);
	cairo_fill(cr);

	cairo_set_line_width(cr, 1.0);

	const uint32_t start = ev->area.x;
	const uint32_t end   = ev->area.x + ev->area.width;

	assert(start < DAWIDTH);
	assert(end <= DAWIDTH);
	assert(start < end);

	for (uint32_t c = 0; c < ui->n_channels; ++c) {
		ScoChan* chn = &ui->chn[c];

		/* Drawing area Y-position of given sample-value.
		 * Note: cairo-pixel at 0 spans -0.5 .. +0.5, hence (DAHEIGHT / 2.0 -0.5)
		 * also the cairo Y-axis points upwards (hence 'minus value')
		 *
		 * == (   DAHEIGHT * (CHN)        // channel offset
		 *      + (DAHEIGHT / 2) - 0.5    // vertical center -- '0'
		 *      - (DAHEIGHT / 2) * (VAL) * (GAIN)
		 *    )
		 */
		const float chn_y_offset = DAHEIGHT * c + DAHEIGHT * 0.5f - 0.5f;
		const float chn_y_scale  = DAHEIGHT * 0.5f * gain;

#define CYPOS(VAL) (chn_y_offset - (VAL) * chn_y_scale)

		cairo_save(cr);

		/* Restrict drawing to current channel area, don't bleed drawing into
		   neighboring channels. */
		cairo_rectangle(cr, 0, DAHEIGHT * c, DAWIDTH, DAHEIGHT);
		cairo_clip(cr);

		// Set color of wave-form
		cairo_set_source_rgba(cr, 0.0, 1.0, 0.0, 1.0);

		/* This is a somewhat 'smart' mechanism to plot audio data using
		   alternating up/down line-directions.  It works well for both cases:
		   1 pixel <= 1 sample and 1 pixel represents more than 1 sample, but
		   is not ideal for either. */
		if (start == chn->idx) {
			cairo_move_to(cr, start - 0.5, CYPOS(0));
		} else {
			cairo_move_to(cr, start - 0.5, CYPOS(chn->data_max[start]));
		}

		uint32_t pathlength = 0;
		for (uint32_t i = start; i < end; ++i) {
			if (i == chn->idx) {
				continue;
			} else if (i % 2) {
				cairo_line_to(cr, i - .5, CYPOS(chn->data_min[i]));
				cairo_line_to(cr, i - .5, CYPOS(chn->data_max[i]));
				++pathlength;
			} else {
				cairo_line_to(cr, i - .5, CYPOS(chn->data_max[i]));
				cairo_line_to(cr, i - .5, CYPOS(chn->data_min[i]));
				++pathlength;
			}

			/** Limit the max cairo path length.  This is an optimization trade
			    off: too short path: high load CPU/GPU load.  too-long path:
			    bad anti-aliasing, or possibly lost points */
			if (pathlength > MAX_CAIRO_PATH) {
				pathlength = 0;
				cairo_stroke(cr);
				if (i % 2) {
					cairo_move_to(cr, i - .5, CYPOS(chn->data_max[i]));
				} else {
					cairo_move_to(cr, i - .5, CYPOS(chn->data_min[i]));
				}
			}
		}

		if (pathlength > 0) {
			cairo_stroke(cr);
		}

		// Draw current position vertical line if display is slow
		if (ui->stride >= ui->rate / 4800.0f || ui->paused) {
			cairo_set_source_rgba(cr, .9, .2, .2, .6);
			cairo_move_to(cr, chn->idx - .5, DAHEIGHT * c);
			cairo_line_to(cr, chn->idx - .5, DAHEIGHT * (c + 1));
			cairo_stroke(cr);
		}

		// Undo the 'clipping' restriction
		cairo_restore(cr);

		// Channel separator
		if (c > 0) {
			cairo_set_source_rgba(cr, .5, .5, .5, 1.0);
			cairo_move_to(cr, 0, DAHEIGHT * c - .5);
			cairo_line_to(cr, DAWIDTH, DAHEIGHT * c - .5);
			cairo_stroke(cr);
		}

		// Zero scale line
		cairo_set_source_rgba(cr, .3, .3, .7, .5);
		cairo_move_to(cr, 0, DAHEIGHT * (c + .5) - .5);
		cairo_line_to(cr, DAWIDTH, DAHEIGHT * (c + .5) - .5);
		cairo_stroke(cr);
	}

	cairo_destroy(cr);
	return TRUE;
}

/**
   Parse raw audio data and prepare for later drawing.

   Note this is a toy example, which is really a waveform display, not an
   oscilloscope.  A serious scope would not display samples as is.

   Signals above ~ 1/10 of the sampling-rate will not yield a useful visual
   display and result in a rather unintuitive representation of the actual
   waveform.

   Ideally the audio-data would be buffered and upsampled here and after that
   written in a display buffer for later use.

   For more information, see
   https://wiki.xiph.org/Videos/Digital_Show_and_Tell
   http://lac.linuxaudio.org/2013/papers/36.pdf
   and https://github.com/x42/sisco.lv2
*/
static int
process_channel(EgScopeUI*   ui,
                ScoChan*     chn,
                const size_t n_elem,
                float const* data,
                uint32_t*    idx_start,
                uint32_t*    idx_end)
{
	int overflow = 0;
	*idx_start = chn->idx;
	for (size_t i = 0; i < n_elem; ++i) {
		if (data[i] < chn->data_min[chn->idx]) {
			chn->data_min[chn->idx] = data[i];
		}
		if (data[i] > chn->data_max[chn->idx]) {
			chn->data_max[chn->idx] = data[i];
		}
		if (++chn->sub >= ui->stride) {
			chn->sub = 0;
			chn->idx = (chn->idx + 1) % DAWIDTH;
			if (chn->idx == 0) {
				++overflow;
			}
			chn->data_min[chn->idx] = 1.0;
			chn->data_max[chn->idx] = -1.0;
		}
	}
	*idx_end = chn->idx;
	return overflow;
}

/**
   Called via port_event() which is called by the host, typically at a rate of
   around 25 FPS.
*/
static void
update_scope(EgScopeUI*    ui,
             const int32_t channel,
             const size_t  n_elem,
             float const*  data)
{
	// Never trust input data which could lead to application failure.
	if (channel < 0 || (uint32_t)channel > ui->n_channels) {
		return;
	}

	// Update state in sync with 1st channel
	if (channel == 0) {
		ui->stride = gtk_spin_button_get_value(GTK_SPIN_BUTTON(ui->spb_speed));
		const bool paused = gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(ui->btn_pause));

		if (paused != ui->paused) {
			ui->paused = paused;
			gtk_widget_queue_draw(ui->darea);
		}
	}
	if (ui->paused) {
		return;
	}

	uint32_t idx_start;  // Display pixel start
	uint32_t idx_end;    // Display pixel end
	int      overflow;   // Received more audio-data than display-pixel

	// Process this channel's audio-data for display
	ScoChan* chn = &ui->chn[channel];
	overflow = process_channel(ui, chn, n_elem, data, &idx_start, &idx_end);

	// Signal gtk's main thread to redraw the widget after the last channel
	if ((uint32_t)channel + 1 == ui->n_channels) {
		if (overflow > 1) {
			// Redraw complete widget
			gtk_widget_queue_draw(ui->darea);
		} else if (idx_end > idx_start) {
			// Redraw area between start -> end pixel
			gtk_widget_queue_draw_area(ui->darea, idx_start - 2, 0, 3
			                           + idx_end - idx_start,
			                           DAHEIGHT * ui->n_channels);
		} else if (idx_end < idx_start) {
			// Wrap-around: redraw area between 0->start AND end->right-end
			gtk_widget_queue_draw_area(
				ui->darea,
				idx_start - 2, 0,
				3 + DAWIDTH - idx_start, DAHEIGHT * ui->n_channels);
			gtk_widget_queue_draw_area(
				ui->darea,
				0, 0,
				idx_end + 1, DAHEIGHT * ui->n_channels);
		}
	}
}

static LV2UI_Handle
instantiate(const LV2UI_Descriptor*   descriptor,
            const char*               plugin_uri,
            const char*               bundle_path,
            LV2UI_Write_Function      write_function,
            LV2UI_Controller          controller,
            LV2UI_Widget*             widget,
            const LV2_Feature* const* features)
{
	EgScopeUI* ui = (EgScopeUI*)calloc(1, sizeof(EgScopeUI));

	if (!ui) {
		fprintf(stderr, "EgScope.lv2 UI: out of memory\n");
		return NULL;
	}

	ui->map = NULL;
	*widget = NULL;

	if (!strcmp(plugin_uri, SCO_URI "#Mono")) {
		ui->n_channels = 1;
	} else if (!strcmp(plugin_uri, SCO_URI "#Stereo")) {
		ui->n_channels = 2;
	} else {
		free(ui);
		return NULL;
	}

	for (int i = 0; features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_URID_URI "#map")) {
			ui->map = (LV2_URID_Map*)features[i]->data;
		}
	}

	if (!ui->map) {
		fprintf(stderr, "EgScope.lv2 UI: Host does not support urid:map\n");
		free(ui);
		return NULL;
	}

	// Initialize private data structure
	ui->write      = write_function;
	ui->controller = controller;

	ui->vbox   = NULL;
	ui->hbox   = NULL;
	ui->darea  = NULL;
	ui->stride = 25;
	ui->paused = false;
	ui->rate   = 48000;

	ui->chn[0].idx = 0;
	ui->chn[0].sub = 0;
	ui->chn[1].idx = 0;
	ui->chn[1].sub = 0;
	memset(ui->chn[0].data_min, 0, sizeof(float) * DAWIDTH);
	memset(ui->chn[0].data_max, 0, sizeof(float) * DAWIDTH);
	memset(ui->chn[1].data_min, 0, sizeof(float) * DAWIDTH);
	memset(ui->chn[1].data_max, 0, sizeof(float) * DAWIDTH);

	map_sco_uris(ui->map, &ui->uris);
	lv2_atom_forge_init(&ui->forge, ui->map);

	// Setup UI
	ui->hbox = gtk_hbox_new(FALSE, 0);
	ui->vbox = gtk_vbox_new(FALSE, 0);

	ui->darea = gtk_drawing_area_new();
	gtk_widget_set_size_request(ui->darea, DAWIDTH, DAHEIGHT * ui->n_channels);

	ui->lbl_speed = gtk_label_new("Samples/Pixel");
	ui->lbl_amp   = gtk_label_new("Amplitude");

	ui->sep[0]    = gtk_hseparator_new();
	ui->sep[1]    = gtk_label_new("");
	ui->btn_pause = gtk_toggle_button_new_with_label("Pause");

	ui->spb_speed_adj = (GtkAdjustment*)gtk_adjustment_new(
			25.0, 1.0, 1000.0, 1.0, 5.0, 0.0);
	ui->spb_speed = gtk_spin_button_new(ui->spb_speed_adj, 1.0, 0);

	ui->spb_amp_adj = (GtkAdjustment*)gtk_adjustment_new(
		1.0, 0.1, 6.0, 0.1, 1.0, 0.0);
	ui->spb_amp = gtk_spin_button_new(ui->spb_amp_adj, 0.1, 1);

	gtk_box_pack_start(GTK_BOX(ui->hbox), ui->darea,     FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(ui->hbox), ui->vbox,      FALSE, FALSE, 4);

	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->lbl_speed, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->spb_speed, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->sep[0],    FALSE, FALSE, 8);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->lbl_amp,   FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->spb_amp,   FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->sep[1],     TRUE, FALSE, 8);
	gtk_box_pack_start(GTK_BOX(ui->vbox), ui->btn_pause, FALSE, FALSE, 2);

	g_signal_connect(G_OBJECT(ui->darea), "expose_event",
	                 G_CALLBACK(on_expose_event), ui);
	g_signal_connect(G_OBJECT(ui->spb_amp), "value-changed",
	                 G_CALLBACK(on_cfg_changed), ui);
	g_signal_connect(G_OBJECT(ui->spb_speed), "value-changed",
	                 G_CALLBACK(on_cfg_changed), ui);

	*widget = ui->hbox;

	/* Send UIOn message to plugin, which will request state and enable message
	   transmission. */
	send_ui_enable(ui);

	return ui;
}

static void
cleanup(LV2UI_Handle handle)
{
	EgScopeUI* ui = (EgScopeUI*)handle;
	/* Send UIOff message to plugin, which will save state and disable message
	 * transmission. */
	send_ui_disable(ui);
	gtk_widget_destroy(ui->darea);
	free(ui);
}

static int
recv_raw_audio(EgScopeUI* ui, const LV2_Atom_Object* obj)
{
	const LV2_Atom* chan_val = NULL;
	const LV2_Atom* data_val = NULL;
	const int n_props  = lv2_atom_object_get(
		obj,
		ui->uris.channelID, &chan_val,
		ui->uris.audioData, &data_val,
		NULL);

	if (n_props != 2 ||
	    chan_val->type != ui->uris.atom_Int ||
	    data_val->type != ui->uris.atom_Vector) {
		// Object does not have the required properties with correct types
		fprintf(stderr, "eg-scope.lv2 UI error: Corrupt audio message\n");
		return 1;
	}

	// Get the values we need from the body of the property value atoms
	const int32_t          chn = ((const LV2_Atom_Int*)chan_val)->body;
	const LV2_Atom_Vector* vec = (const LV2_Atom_Vector*)data_val;
	if (vec->body.child_type != ui->uris.atom_Float) {
		return 1;  // Vector has incorrect element type
	}

	// Number of elements = (total size - header size) / element size
	const size_t n_elem = ((data_val->size - sizeof(LV2_Atom_Vector_Body))
	                       / sizeof(float));

	// Float elements immediately follow the vector body header
	const float* data = (const float*)(&vec->body + 1);

	// Update display
	update_scope(ui, chn, n_elem, data);
	return 0;
}

static int
recv_ui_state(EgScopeUI* ui, const LV2_Atom_Object* obj)
{
	const LV2_Atom* spp_val  = NULL;
	const LV2_Atom* amp_val  = NULL;
	const LV2_Atom* rate_val = NULL;
	const int n_props  = lv2_atom_object_get(
		obj,
		ui->uris.ui_spp, &spp_val,
		ui->uris.ui_amp, &amp_val,
		ui->uris.param_sampleRate, &rate_val,
		NULL);

	if (n_props != 3 ||
		spp_val->type != ui->uris.atom_Int ||
		amp_val->type != ui->uris.atom_Float ||
	    rate_val->type != ui->uris.atom_Float) {
		// Object does not have the required properties with correct types
		fprintf(stderr, "eg-scope.lv2 UI error: Corrupt state message\n");
		return 1;
	}

	// Get the values we need from the body of the property value atoms
	const int32_t spp  = ((const LV2_Atom_Int*)spp_val)->body;
	const float   amp  = ((const LV2_Atom_Float*)amp_val)->body;
	const float   rate = ((const LV2_Atom_Float*)rate_val)->body;

	// Disable transmission and update UI
	ui->updating = true;
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(ui->spb_speed), spp);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(ui->spb_amp),   amp);
	ui->updating = false;
	ui->rate = rate;

	return 0;
}

/**
   Receive data from the DSP-backend.

   This is called by the host, typically at a rate of around 25 FPS.

   Ideally this happens regularly and with relatively low latency, but there
   are no hard guarantees about message delivery.
*/
static void
port_event(LV2UI_Handle handle,
           uint32_t     port_index,
           uint32_t     buffer_size,
           uint32_t     format,
           const void*  buffer)
{
	EgScopeUI*      ui   = (EgScopeUI*)handle;
	const LV2_Atom* atom = (const LV2_Atom*)buffer;

	/* Check type of data received
	 *  - format == 0: Control port event (float)
	 *  - format > 0:  Message (atom)
	 */
	if (format == ui->uris.atom_eventTransfer &&
	    lv2_atom_forge_is_object_type(&ui->forge, atom->type)) {
		const LV2_Atom_Object* obj = (const LV2_Atom_Object*)atom;
		if (obj->body.otype == ui->uris.RawAudio) {
			recv_raw_audio(ui, obj);
		} else if (obj->body.otype == ui->uris.ui_State) {
			recv_ui_state(ui, obj);
		}
	}
}

static const LV2UI_Descriptor descriptor = {
	SCO_URI "#ui",
	instantiate,
	cleanup,
	port_event,
	NULL
};

LV2_SYMBOL_EXPORT
const LV2UI_Descriptor*
lv2ui_descriptor(uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}
