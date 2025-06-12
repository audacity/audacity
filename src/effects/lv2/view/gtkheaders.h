#pragma once

// Qt defines the `signals` macro, which is the name of a struct member in some GTK header file ...
#pragma push_macro("signals")
#undef signals
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>
#include <gobject/gsignal.h>
#pragma pop_macro("signals")
