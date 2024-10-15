/*
  LV2 Sampler Example Plugin UI
  Copyright 2011-2016 David Robillard <d@drobilla.net>

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

#include "peaks.h"
#include "uris.h"

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/core/lv2.h"
#include "lv2/core/lv2_util.h"
#include "lv2/log/log.h"
#include "lv2/log/logger.h"
#include "lv2/midi/midi.h"
#include "lv2/ui/ui.h"
#include "lv2/urid/urid.h"

#include <cairo.h>
#include <gdk/gdk.h>
#include <glib-object.h>
#include <glib.h>
#include <gobject/gclosure.h>
#include <gtk/gtk.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define SAMPLER_UI_URI "http://lv2plug.in/plugins/eg-sampler#ui"

#define MIN_CANVAS_W 128
#define MIN_CANVAS_H 80

typedef struct {
	LV2_Atom_Forge forge;
	LV2_URID_Map*  map;
	LV2_Log_Logger logger;
	SamplerURIs    uris;
	PeaksReceiver  precv;

	LV2UI_Write_Function write;
	LV2UI_Controller     controller;

	GtkWidget* box;
	GtkWidget* play_button;
	GtkWidget* file_button;
	GtkWidget* button_box;
	GtkWidget* canvas;
	GtkWidget* window;  /* For optional show interface. */

	uint32_t width;
	uint32_t requested_n_peaks;
	char*    filename;

	uint8_t forge_buf[1024];
} SamplerUI;

static void
on_file_set(GtkFileChooserButton* widget, void* handle)
{
	SamplerUI* ui = (SamplerUI*)handle;

	// Get the filename from the file chooser
	char* filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget));

	// Write a set message to the plugin to load new file
	lv2_atom_forge_set_buffer(&ui->forge, ui->forge_buf, sizeof(ui->forge_buf));
	LV2_Atom* msg = (LV2_Atom*)write_set_file(&ui->forge, &ui->uris,
	                                          filename, strlen(filename));

	ui->write(ui->controller, 0, lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);

	g_free(filename);
}

static void
on_play_clicked(GtkFileChooserButton* widget, void* handle)
{
	SamplerUI* ui = (SamplerUI*)handle;
	struct {
		LV2_Atom atom;
		uint8_t  msg[3];
	} note_on;

	note_on.atom.type = ui->uris.midi_Event;
	note_on.atom.size = 3;
	note_on.msg[0]    = LV2_MIDI_MSG_NOTE_ON;
	note_on.msg[1]    = 60;
	note_on.msg[2]    = 60;
	ui->write(ui->controller, 0, sizeof(note_on),
	          ui->uris.atom_eventTransfer,
	          &note_on);
}

static void
request_peaks(SamplerUI* ui, uint32_t n_peaks)
{
	if (n_peaks == ui->requested_n_peaks) {
		return;
	}

	lv2_atom_forge_set_buffer(&ui->forge, ui->forge_buf, sizeof(ui->forge_buf));

	LV2_Atom_Forge_Frame frame;
	lv2_atom_forge_object(&ui->forge, &frame, 0, ui->uris.patch_Get);
	lv2_atom_forge_key(&ui->forge, ui->uris.patch_accept);
	lv2_atom_forge_urid(&ui->forge, ui->precv.uris.peaks_PeakUpdate);
	lv2_atom_forge_key(&ui->forge, ui->precv.uris.peaks_total);
	lv2_atom_forge_int(&ui->forge, n_peaks);
	lv2_atom_forge_pop(&ui->forge, &frame);

	LV2_Atom* msg = lv2_atom_forge_deref(&ui->forge, frame.ref);
	ui->write(ui->controller, 0, lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);

	ui->requested_n_peaks = n_peaks;
}

/** Set Cairo color to a GDK color (to follow Gtk theme). */
static void
cairo_set_source_gdk(cairo_t* cr, const GdkColor* color)
{
	cairo_set_source_rgb(
		cr, color->red / 65535.0, color->green / 65535.0, color->blue / 65535.0);

}

static gboolean
on_canvas_expose(GtkWidget* widget, GdkEventExpose* event, gpointer data)
{
	SamplerUI* ui = (SamplerUI*)data;

	GtkAllocation size;
	gtk_widget_get_allocation(widget, &size);

	ui->width = size.width;
	if ((uint32_t)ui->width > 2 * ui->requested_n_peaks) {
		request_peaks(ui, 2 * ui->requested_n_peaks);
	}

	cairo_t* cr = gdk_cairo_create(gtk_widget_get_window(widget));

	cairo_set_line_width(cr, 1.0);
	cairo_translate(cr, 0.5, 0.5);

	const int mid_y = size.height / 2;

	const float* const peaks   = ui->precv.peaks;
	const int32_t      n_peaks = ui->precv.n_peaks;
	if (peaks) {
		// Draw waveform
		const double scale = size.width / ((double)n_peaks - 1.0f);

		// Start at left origin
		cairo_move_to(cr, 0, mid_y);

		// Draw line through top peaks
		for (int i = 0; i < n_peaks; ++i) {
			const float peak = peaks[i];
			cairo_line_to(cr, i * scale, mid_y + (peak / 2.0f) * size.height);
		}

		// Continue through bottom peaks
		for (int i = n_peaks - 1; i >= 0; --i) {
			const float peak = peaks[i];
			cairo_line_to(cr, i * scale, mid_y - (peak / 2.0f) * size.height);
		}

		// Close shape
		cairo_line_to(cr, 0, mid_y);

		cairo_set_source_gdk(cr, widget->style->mid);
		cairo_fill_preserve(cr);

		cairo_set_source_gdk(cr, widget->style->fg);
		cairo_stroke(cr);
	}

	cairo_destroy(cr);
	return TRUE;
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
	SamplerUI* ui = (SamplerUI*)calloc(1, sizeof(SamplerUI));
	if (!ui) {
		return NULL;
	}

	ui->write      = write_function;
	ui->controller = controller;
	ui->width      = MIN_CANVAS_W;
	*widget        = NULL;

	// Get host features
	const char* missing = lv2_features_query(
		features,
		LV2_LOG__log,  &ui->logger.log, false,
		LV2_URID__map, &ui->map,        true,
		NULL);
	lv2_log_logger_set_map(&ui->logger, ui->map);
	if (missing) {
		lv2_log_error(&ui->logger, "Missing feature <%s>\n", missing);
		free(ui);
		return NULL;
	}

	// Map URIs and initialise forge
	map_sampler_uris(ui->map, &ui->uris);
	lv2_atom_forge_init(&ui->forge, ui->map);
	peaks_receiver_init(&ui->precv, ui->map);

	// Construct Gtk UI
	ui->box         = gtk_vbox_new(FALSE, 4);
	ui->play_button = gtk_button_new_with_label("▶");
	ui->canvas      = gtk_drawing_area_new();
	ui->button_box  = gtk_hbox_new(FALSE, 4);
	ui->file_button = gtk_file_chooser_button_new(
		"Load Sample", GTK_FILE_CHOOSER_ACTION_OPEN);
	gtk_widget_set_size_request(ui->canvas, MIN_CANVAS_W, MIN_CANVAS_H);
	gtk_container_set_border_width(GTK_CONTAINER(ui->box), 4);
	gtk_box_pack_start(GTK_BOX(ui->box), ui->canvas, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(ui->box), ui->button_box, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(ui->button_box), ui->play_button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(ui->button_box), ui->file_button, TRUE, TRUE, 0);

	g_signal_connect(ui->file_button, "file-set",
	                 G_CALLBACK(on_file_set), ui);

	g_signal_connect(ui->play_button, "clicked",
	                 G_CALLBACK(on_play_clicked), ui);

	g_signal_connect(G_OBJECT(ui->canvas), "expose_event",
	                 G_CALLBACK(on_canvas_expose), ui);

	// Request state (filename) from plugin
	lv2_atom_forge_set_buffer(&ui->forge, ui->forge_buf, sizeof(ui->forge_buf));
	LV2_Atom_Forge_Frame frame;
	LV2_Atom*            msg = (LV2_Atom*)lv2_atom_forge_object(
		&ui->forge, &frame, 0, ui->uris.patch_Get);
	lv2_atom_forge_pop(&ui->forge, &frame);

	ui->write(ui->controller, 0, lv2_atom_total_size(msg),
	          ui->uris.atom_eventTransfer,
	          msg);

	*widget = ui->box;

	return ui;
}

static void
cleanup(LV2UI_Handle handle)
{
	SamplerUI* ui = (SamplerUI*)handle;
	gtk_widget_destroy(ui->box);
	gtk_widget_destroy(ui->play_button);
	gtk_widget_destroy(ui->canvas);
	gtk_widget_destroy(ui->button_box);
	gtk_widget_destroy(ui->file_button);
	free(ui);
}

static void
port_event(LV2UI_Handle handle,
           uint32_t     port_index,
           uint32_t     buffer_size,
           uint32_t     format,
           const void*  buffer)
{
	SamplerUI* ui = (SamplerUI*)handle;
	if (format == ui->uris.atom_eventTransfer) {
		const LV2_Atom* atom = (const LV2_Atom*)buffer;
		if (lv2_atom_forge_is_object_type(&ui->forge, atom->type)) {
			const LV2_Atom_Object* obj = (const LV2_Atom_Object*)atom;
			if (obj->body.otype == ui->uris.patch_Set) {
				const char* path = read_set_file(&ui->uris, obj);
				if (path && (!ui->filename || strcmp(path, ui->filename))) {
					g_free(ui->filename);
					ui->filename = g_strdup(path);
					gtk_file_chooser_set_filename(
						GTK_FILE_CHOOSER(ui->file_button), path);
					peaks_receiver_clear(&ui->precv);
					ui->requested_n_peaks = 0;
					request_peaks(ui, ui->width / 2 * 2);
				} else if (!path) {
					lv2_log_warning(&ui->logger, "Set message has no path\n");
				}
			} else if (obj->body.otype == ui->precv.uris.peaks_PeakUpdate) {
				if (!peaks_receiver_receive(&ui->precv, obj)) {
					gtk_widget_queue_draw(ui->canvas);
				}
			}
		} else {
			lv2_log_error(&ui->logger, "Unknown message type\n");
		}
	} else {
		lv2_log_warning(&ui->logger, "Unknown port event format\n");
	}
}

/* Optional non-embedded UI show interface. */
static int
ui_show(LV2UI_Handle handle)
{
	SamplerUI* ui = (SamplerUI*)handle;

	int argc = 0;
	gtk_init(&argc, NULL);

	ui->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_container_add(GTK_CONTAINER(ui->window), ui->box);
	gtk_widget_show_all(ui->window);
	gtk_window_present(GTK_WINDOW(ui->window));

	return 0;
}

/* Optional non-embedded UI hide interface. */
static int
ui_hide(LV2UI_Handle handle)
{
	return 0;
}

/* Idle interface for optional non-embedded UI. */
static int
ui_idle(LV2UI_Handle handle)
{
	SamplerUI* ui = (SamplerUI*)handle;
	if (ui->window) {
		gtk_main_iteration();
	}
	return 0;
}

static const void*
extension_data(const char* uri)
{
	static const LV2UI_Show_Interface show = { ui_show, ui_hide };
	static const LV2UI_Idle_Interface idle = { ui_idle };
	if (!strcmp(uri, LV2_UI__showInterface)) {
		return &show;
	} else if (!strcmp(uri, LV2_UI__idleInterface)) {
		return &idle;
	}
	return NULL;
}

static const LV2UI_Descriptor descriptor = {
	SAMPLER_UI_URI,
	instantiate,
	cleanup,
	port_event,
	extension_data
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
