## Process this file with automake to produce Makefile.in

apidocdir = $(htmldir)/libogg

dist_apidoc_DATA = bitpacking.html datastructures.html decoding.html encoding.html\
	general.html index.html ogg_iovec_t.html ogg_packet.html ogg_packet_clear.html\
	ogg_page.html ogg_page_bos.html ogg_page_checksum_set.html\
	ogg_page_continued.html ogg_page_eos.html ogg_page_granulepos.html\
	ogg_page_packets.html ogg_page_pageno.html ogg_page_serialno.html\
	ogg_page_version.html ogg_stream_check.html ogg_stream_clear.html ogg_stream_destroy.html\
	ogg_stream_eos.html ogg_stream_flush.html ogg_stream_flush_fill.html ogg_stream_init.html\
	ogg_stream_iovecin.html ogg_stream_packetin.html ogg_stream_packetout.html\
	ogg_stream_packetpeek.html ogg_stream_pagein.html\
	ogg_stream_pageout.html ogg_stream_pageout_fill.html ogg_stream_reset.html\
	ogg_stream_reset_serialno.html ogg_stream_state.html\
	ogg_sync_buffer.html ogg_sync_check.html ogg_sync_clear.html ogg_sync_destroy.html\
	ogg_sync_init.html ogg_sync_pageout.html ogg_sync_pageseek.html\
	ogg_sync_reset.html ogg_sync_state.html ogg_sync_wrote.html\
	oggpack_adv.html oggpack_adv1.html oggpack_bits.html\
	oggpack_buffer.html oggpack_bytes.html oggpack_get_buffer.html\
	oggpack_look.html oggpack_look1.html oggpack_read.html\
	oggpack_read1.html oggpack_readinit.html oggpack_reset.html\
	oggpack_write.html oggpack_writealign.html oggpack_writecheck.html oggpack_writeclear.html\
	oggpack_writecopy.html oggpack_writeinit.html oggpack_writetrunc.html\
	overview.html reference.html style.css

update-doc-version:
	@YEAR=$$(date +%Y); DAY=$$(date +%Y%m%d);					\
	for f in $(srcdir)/*.html; do							\
		sed -e "s/2000-[0-9]\{4\} Xiph.Org/2000-$$YEAR Xiph.Org/g"		\
		    -e "s/libogg release [0-9. -]\+/libogg release $(VERSION) - $$DAY/g"\
			< $$f > $$f.tmp;						\
		if diff -q $$f $$f.tmp > /dev/null; then				\
			rm $$f.tmp;							\
		else									\
			mv $$f.tmp $$f;							\
		fi;									\
	done;

