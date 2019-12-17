/*
  Copyright 2016 David Robillard <d@drobilla.net>

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

#include "lv2/atom/forge.h"

/**
   A forge sink that writes to an atom buffer.

   It is assumed that the handle points to an LV2_Atom large enough to store
   the forge output.  The forged result is in the body of the buffer atom.
*/
static LV2_Atom_Forge_Ref
atom_sink(LV2_Atom_Forge_Sink_Handle handle, const void* buf, uint32_t size)
{
	LV2_Atom*      atom   = (LV2_Atom*)handle;
	const uint32_t offset = lv2_atom_total_size(atom);
	memcpy((char*)atom + offset, buf, size);
	atom->size += size;
	return offset;
}

/**
   Dereference counterpart to atom_sink().
*/
static LV2_Atom*
atom_sink_deref(LV2_Atom_Forge_Sink_Handle handle, LV2_Atom_Forge_Ref ref)
{
	return (LV2_Atom*)((char*)handle + ref);
}
