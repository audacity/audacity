#!/usr/bin/python

import commands, os, re, string, sys, time

def count_enclosed_functions (source):
	func_count = 0
	open_brace = 0
	close_brace = 0
	for ch in source:
		if ch == '{':
			open_brace += 1
		elif ch == '}':
			close_brace += 1
			if open_brace == close_brace:
				func_count += 1
		if open_brace < close_brace:
			print "count_enclosed_functions : open_brace < close_brace"
			return -1
	return func_count

def find_function_prototype (source, proto_name):
	proto_re = "(^[a-zA-Z_ \t]+\s+%s[^a-zA-Z0-9_]\s*\([^\)]+\)\s+;\n)" % (proto_name)
	proto_result = re.search (proto_re, source, re.MULTILINE | re.DOTALL)
	if not proto_result:
		return None
	proto_text = proto_result.groups ()[0]
	return proto_text

def find_function_definition (source, func_name):
	func_re = "(\n[a-zA-Z_ \t]+\n%s[^a-zA-Z0-9_].* /\* %s \*/\n)" % (func_name, func_name)
	func_result = re.search (func_re, source, re.MULTILINE | re.DOTALL)
	if not func_result:
		sys.exit (1)
		return None
	func_text = func_result.groups ()[0]

	# Now to check that we only have one enclosing function.
	func_count = count_enclosed_functions (func_text)
	if func_count != 1:
		return None
	return func_text

def find_include (source, inc_name):
	inc_re = "(^#include\s+[\<\"]%s[\"\>]\s*)" % inc_name
	inc_result = re.search (inc_re, source, re.MULTILINE | re.DOTALL)
	if not inc_result:
		return None
	inc_text = inc_result.groups ()[0]
	return inc_text

def find_assign_statement (source, var_name):
	var_re = "(^\s+%s\s*=[^;]+;)" % var_name
	var_result = re.search (var_re, source, re.MULTILINE | re.DOTALL)
	if not var_result:
		return None
	assign_text = var_result.groups ()[0]
	return assign_text

#--------------------------------------------------------------------------------

def remove_include (source, inc_name):
	inc_text = find_include (source, inc_name)
	if not inc_text:
		print "remove_include : include '%s' not found. Exiting." % inc_name
		sys.exit (1)

	source = string.replace (source, inc_text, "")
	return source

def remove_assign (source, assign_name):
	assign_text = find_assign (source, inc_name)
	if not inc_text:
		print "remove_include : include '%s' not found. Exiting." % inc_name
		sys.exit (1)

	source = string.replace (source, inc_text, "")
	return source

def remove_prototype (source, proto_name):
	proto_text = find_function_prototype (source, proto_name)
	if not proto_text:
		print "remove_prototype : prototype '%s' not found. Exiting." % proto_name
		sys.exit (1)

	source = string.replace (source, proto_text, "")
	return source

def remove_function (source, func_name):
	func_text = find_function_definition (source, func_name)
	if not func_text:
		print "remove_function : function '%s' not found. Exiting." % func_name
		sys.exit (1)

	source = string.replace (source, func_text, "/* Function %s() removed here. */\n" % func_name)
	return source

def remove_all_assignments (source, var):
	count = 0
	while 1:
		assign_text = find_assign_statement (source, var)
		if not assign_text:
			if count != 0:
				break
			print "remove_all_assignments : variable '%s' not found. Exiting." % var
			sys.exit (1)

		source = string.replace (source, assign_text, "")
		count += 1
	return source



#----------------------------------------------------------------

def remove_funcs_and_protos_from_file (filename, func_list):
	source_code = open (filename, 'r').read ()

	for func in func_list:
		source_code = remove_prototype (source_code, func) ;
		source_code = remove_function (source_code, func) ;
	open (filename, 'w').write (source_code)

def remove_funcs_from_file (filename, func_list):
	source_code = open (filename, 'r').read ()

	for func in func_list:
		source_code = remove_function (source_code, func) ;
	open (filename, 'w').write (source_code)

def remove_protos_from_file (filename, func_list):
	source_code = open (filename, 'r').read ()

	for func in func_list:
		source_code = remove_prototype (source_code, func) ;
	open (filename, 'w').write (source_code)

def remove_includes_from_file (filename, inc_list):
	source_code = open (filename, 'r').read ()

	for inc in inc_list:
		source_code = remove_include (source_code, inc) ;
	open (filename, 'w').write (source_code)

def remove_all_assignments_from_file (filename, var_list):
	source_code = open (filename, 'r').read ()

	for var in var_list:
		source_code = remove_all_assignments (source_code, var) ;
	open (filename, 'w').write (source_code)

def remove_comment_start_end (filename, start_comment, end_comment):
	source_code = open (filename, 'r').read ()

	while 1:
		start_index = string.find (source_code, start_comment)
		end_index = string.find (source_code, end_comment)
		if start_index < 0 or end_index < start_index:
			break
		end_index += len (end_comment)
		source_code = source_code [:start_index-1] + source_code [end_index:] ;

	open (filename, 'w').write (source_code)

def remove_strings_from_file (filename, str_list):
	file_text = open (filename, 'r').read ()
	for current_str in str_list:
		file_text = string.replace (file_text, current_str, '')
	open (filename, 'w').write (file_text)

def string_replace_in_file (filename, from_str, to_str):
	file_text = open (filename, 'r').read ()
	file_text = string.replace (file_text, from_str, to_str)
	open (filename, 'w').write (file_text)

def remove_regex_from_file (filename, regex_list):
	file_text = open (filename, 'r').read ()
	for regex in regex_list:
		file_text = re.sub (regex, '', file_text, re.MULTILINE | re.DOTALL)
	open (filename, 'w').write (file_text)

#==========================================================================

def find_configure_version (filename):
	# AM_INIT_AUTOMAKE(libsndfile,0.0.21pre6)
	file = open (filename)
	while 1:
		line = file.readline ()
		if re.search ("AC_INIT", line):
			x = re.sub ("[^\(]+\(", "", line)
			x = re.sub ("\).*\n", "", x)
			x = string.split (x, ",")
			package = x [0]
			version = x [1]
			break
	file.close ()
	# version = re.escape (version)
	return package, version

def fix_configure_ac_file (filename):
	data = open (filename, 'r').read ()
	data = string.replace (data, "AM_INIT_AUTOMAKE(libsndfile,", "AM_INIT_AUTOMAKE(libsndfile_lite,", 1)

	file = open (filename, 'w')
	file.write (data)
	file.close ()


def make_dist_file (package, version):
	print "Making dist file."
	tar_gz_file = "%s-%s.tar.gz" % (package, version)
	if os.path.exists (tar_gz_file):
		return
	if os.system ("make dist"):
		sys.exit (1)
	return

def delete_files (file_list):
	for file_name in file_list:
		os.remove (file_name)

#=======================================================================

source_dir = os.getcwd ()

conf_package, conf_version =  find_configure_version ('configure.ac')

package_version = "%s-%s" % (conf_package, conf_version)
lite_version = "%s_lite-%s" % (conf_package, conf_version)

os.system ("rm -rf %s%s.tar.gz" % (source_dir, package_version))

os.system ("make dist")

make_dist_file (conf_package, conf_version)

os.chdir ("/tmp")

print "Uncompressing .tar.gz file."
os.system ("rm -rf %s" % package_version)
if os.system ("tar zxf %s/%s.tar.gz" % (source_dir, package_version)):
	sys.exit (1)


print "Renaming to libsndfile_lite."
os.system ("rm -rf %s" % lite_version)
os.rename (package_version, lite_version)

print "Changing into libsndfile_lite directory."
os.chdir (lite_version)

print "Removing un-neeed directories."
delete_dirs = [ 'src/G72x' ]

for dir_name in delete_dirs:
	os.system ("rm -rf %s" % dir_name)

print "Removing un-needed files."
delete_files ([ 'src/ircam.c', 'src/nist.c',
	'src/ima_adpcm.c', 'src/ms_adpcm.c', 'src/au_g72x.c',
	'src/mat4.c', 'src/mat5.c', 'src/dwvw.c', 'src/paf.c',
	'src/ogg.c', 'src/pvf.c', 'src/xi.c', 'src/htk.c',
	'src/sd2.c', 'src/rx2.c', 'src/txw.c', 'src/wve.c',
	'src/dwd.c', 'src/svx.c', 'src/voc.c', 'src/vox_adpcm.c',
	'src/sds.c'
	])


print "Hacking 'configure.ac' and 'src/Makefile.am'."
remove_strings_from_file ('configure.ac', [ 'src/G72x/Makefile' ])
remove_strings_from_file ('src/Makefile.am', [ 'G72x/libg72x.la', 'G72x',
		'ircam.c', 'nist.c', 'ima_adpcm.c', 'ms_adpcm.c', 'au_g72x.c', 'mat4.c', 
		'mat5.c', 'dwvw.c',  'paf.c', 'ogg.c', 'pvf.c', 'xi.c', 'htk.c', 
		'sd2.c', 'rx2.c', 'txw.c', 'wve.c', 'dwd.c', 'svx.c', 'voc.c', 
		'vox_adpcm.c', 'sds.c'
		])

#----------------------------------------------------------------------------

print "Hacking header files."

remove_protos_from_file ('src/common.h', [	'xi_open', 'sd2_open', 'ogg_open',
	'dwvw_init', 'paf_open', 'svx_open', 'nist_open', 'rx2_open', 'mat4_open',
	'voc_open', 'txw_open', 'dwd_open', 'htk_open', 'wve_open', 'mat5_open',
	'pvf_open', 'ircam_open', 'sds_open',
	'float32_init', 'double64_init', 'aiff_ima_init', 'vox_adpcm_init',
	'wav_w64_ima_init', 'wav_w64_msadpcm_init'
	])

remove_protos_from_file ('src/au.h',
		[ 'au_g72x_reader_init', 'au_g72x_writer_init' ])

remove_protos_from_file ('src/wav_w64.h', [ 'msadpcm_write_adapt_coeffs' ])

#----------------------------------------------------------------------------

print "Hacking case statements."

remove_comment_start_end ('src/sndfile.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/aiff.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/au.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/raw.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/w64.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/wav.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/double64.c', '/* Lite remove start */' , '/* Lite remove end */')
remove_comment_start_end ('src/float32.c', '/* Lite remove start */' , '/* Lite remove end */')


#----------------------------------------------------------------------------

print "Hacking src/pcm.c."
remove_funcs_from_file ('src/pcm.c', [
	'f2sc_array', 'f2sc_clip_array', 'f2uc_array', 'f2uc_clip_array',
	'f2bes_array', 'f2bes_clip_array', 'f2les_array', 'f2les_clip_array',
	'f2let_array', 'f2let_clip_array', 'f2bet_array', 'f2bet_clip_array',
	'f2bei_array', 'f2bei_clip_array', 'f2lei_array', 'f2lei_clip_array',
	'd2sc_array', 'd2sc_clip_array', 'd2uc_array', 'd2uc_clip_array',
	'd2bes_array', 'd2bes_clip_array', 'd2les_array', 'd2les_clip_array',
	'd2let_array', 'd2let_clip_array', 'd2bet_array', 'd2bet_clip_array',
	'd2bei_array', 'd2bei_clip_array', 'd2lei_array', 'd2lei_clip_array',
	])

remove_funcs_and_protos_from_file ('src/pcm.c', [
	'pcm_read_sc2f', 'pcm_read_uc2f', 'pcm_read_les2f', 'pcm_read_bes2f',
	'pcm_read_let2f', 'pcm_read_bet2f', 'pcm_read_lei2f', 'pcm_read_bei2f',
	'pcm_read_sc2d', 'pcm_read_uc2d', 'pcm_read_les2d', 'pcm_read_bes2d',
	'pcm_read_let2d', 'pcm_read_bet2d', 'pcm_read_lei2d', 'pcm_read_bei2d',
	'pcm_write_f2sc', 'pcm_write_f2uc', 'pcm_write_f2bes', 'pcm_write_f2les',
	'pcm_write_f2bet', 'pcm_write_f2let', 'pcm_write_f2bei', 'pcm_write_f2lei',
	'pcm_write_d2sc', 'pcm_write_d2uc', 'pcm_write_d2bes', 'pcm_write_d2les',
	'pcm_write_d2bet', 'pcm_write_d2let', 'pcm_write_d2bei', 'pcm_write_d2lei',

	'sc2f_array', 'uc2f_array', 'bes2f_array', 'les2f_array',
	'bet2f_array', 'let2f_array', 'bei2f_array', 'lei2f_array',
	'sc2d_array', 'uc2d_array', 'bes2d_array', 'les2d_array',
	'bet2d_array', 'let2d_array', 'bei2d_array', 'lei2d_array'
	])

remove_includes_from_file ('src/pcm.c', [ 'float_cast.h' ])
remove_all_assignments_from_file ('src/pcm.c', [
	'psf-\>write_float', 'psf\-\>write_double',
	'psf-\>read_float', 'psf\-\>read_double' ])

#----------------------------------------------------------------------------
print "Hacking src/ulaw.c."
remove_funcs_and_protos_from_file ('src/ulaw.c', [
	'ulaw_read_ulaw2f', 'ulaw_read_ulaw2d',
	'ulaw_write_f2ulaw', 'ulaw_write_d2ulaw',
	'ulaw2f_array', 'ulaw2d_array', 'f2ulaw_array', 'd2ulaw_array'
	])

remove_includes_from_file ('src/ulaw.c', [ 'float_cast.h' ])
remove_all_assignments_from_file ('src/ulaw.c', [
	'psf-\>write_float', 'psf\-\>write_double',
	'psf-\>read_float', 'psf\-\>read_double' ])

#----------------------------------------------------------------------------

print "Hacking src/alaw.c."
remove_funcs_and_protos_from_file ('src/alaw.c', [
	'alaw_read_alaw2f', 'alaw_read_alaw2d',
	'alaw_write_f2alaw', 'alaw_write_d2alaw',
	'alaw2f_array', 'alaw2d_array', 'f2alaw_array', 'd2alaw_array'
	])

remove_includes_from_file ('src/alaw.c', [ 'float_cast.h' ])
remove_all_assignments_from_file ('src/alaw.c', [
	'psf-\>write_float', 'psf\-\>write_double',
	'psf-\>read_float', 'psf\-\>read_double' ])

#----------------------------------------------------------------------------

print "Hacking src/gsm610.c."
remove_funcs_and_protos_from_file ('src/gsm610.c', [
	'gsm610_read_f', 'gsm610_read_d', 'gsm610_write_f', 'gsm610_write_d'
	])

remove_includes_from_file ('src/gsm610.c', [ 'float_cast.h' ])
remove_all_assignments_from_file ('src/gsm610.c', [
	'psf-\>write_float', 'psf\-\>write_double',
	'psf-\>read_float', 'psf\-\>read_double' ])

#----------------------------------------------------------------------------

print "Hacking src/float32.c."

# string_replace_in_file ('src/float32.c', '"float_cast.h"', '<math.h>')
remove_funcs_from_file ('src/float32.c', [ 'float32_init'	])

remove_funcs_and_protos_from_file ('src/float32.c', [
	'host_read_f2s', 'host_read_f2i', 'host_read_f', 'host_read_f2d', 
	'host_write_s2f', 'host_write_i2f', 'host_write_f', 'host_write_d2f', 
	'f2s_array', 'f2i_array', 'f2d_array', 's2f_array', 'i2f_array', 'd2f_array', 
	'float32_peak_update', 
	'replace_read_f2s', 'replace_read_f2i', 'replace_read_f', 'replace_read_f2d', 
	'replace_write_s2f', 'replace_write_i2f', 'replace_write_f', 'replace_write_d2f', 
	'bf2f_array', 'f2bf_array', 
	'float32_get_capability', 
	])

#----------------------------------------------------------------------------

print "Hacking src/double64.c."
remove_funcs_from_file ('src/double64.c', [ 'double64_init'	])

remove_funcs_and_protos_from_file ('src/double64.c', [
	'host_read_d2s', 'host_read_d2i', 'host_read_d2f', 'host_read_d',
	'host_write_s2d', 'host_write_i2d', 'host_write_f2d', 'host_write_d',
	'd2s_array', 'd2i_array', 'd2f_array',
	's2d_array', 'i2d_array', 'f2d_array',
	'double64_peak_update', 'double64_get_capability',
	'replace_read_d2s', 'replace_read_d2i', 'replace_read_d2f', 'replace_read_d',
	'replace_write_s2d', 'replace_write_i2d', 'replace_write_f2d', 'replace_write_d',
	'd2bd_read', 'bd2d_write'
	])

#----------------------------------------------------------------------------

print "Hacking test programs."
delete_files ([ 'tests/dwvw_test.c', 'tests/floating_point_test.c', 
	'tests/dft_cmp.c', 'tests/peak_chunk_test.c',
	'tests/scale_clip_test.tpl', 'tests/scale_clip_test.def'
	])

remove_comment_start_end ('tests/write_read_test.def', '/* Lite remove start */', '/* Lite remove end */')
remove_comment_start_end ('tests/write_read_test.tpl', '/* Lite remove start */', '/* Lite remove end */')

remove_comment_start_end ('tests/Makefile.am', '# Lite remove start', '# Lite remove end')

remove_strings_from_file ('tests/Makefile.am', [ 
	'scale_clip_test.tpl', 'scale_clip_test.def',
	'\n\t./dwvw_test',
	'\n\t./floating_point_test', '\n\t./scale_clip_test', 
	'\n\t./peak_chunk_test aiff', '\n\t./peak_chunk_test wav',
	'\n\t./command_test norm', '\n\t./command_test peak',
	'\n\t./lossy_comp_test wav_ima', '\n\t./lossy_comp_test wav_msadpcm',
	'\n\t./lossy_comp_test au_g721', '\n\t./lossy_comp_test au_g723',
	'\n\t./lossy_comp_test vox_adpcm', 
	'\n\t./lossy_comp_test w64_ima', '\n\t./lossy_comp_test w64_msadpcm',
	'peak_chunk_test', 'dwvw_test', 'floating_point_test', 'scale_clip_test',
	
	'paf-tests', 'svx-tests', 'nist-tests', 'ircam-tests', 'voc-tests',
	'mat4-tests', 'mat5-tests', 'pvf-tests', 'xi-tests', 'htk-tests',
	'sds-tests'
	])

remove_comment_start_end ('tests/pcm_test.c', '/* Lite remove start */', '/* Lite remove end */')
remove_funcs_and_protos_from_file ('tests/pcm_test.c', [
	'pcm_test_float', 'pcm_test_double' 
	])

remove_comment_start_end ('tests/lossy_comp_test.c', '/* Lite remove start */', '/* Lite remove end */')
remove_funcs_and_protos_from_file ('tests/lossy_comp_test.c', [
	'lcomp_test_float', 'lcomp_test_double', 'sdlcomp_test_float', 'sdlcomp_test_double',
	'smoothed_diff_float', 'smoothed_diff_double'
	])

remove_comment_start_end ('tests/multi_file_test.c', '/* Lite remove start */', '/* Lite remove end */')

remove_strings_from_file ('tests/stdio_test.c', [ 
	'"paf",', '"svx",', '"nist",', '"ircam",', '"voc",', '"mat4",', '"mat5",', '"pvf",'
	])

remove_comment_start_end ('tests/pipe_test.c', '/* Lite remove start */', '/* Lite remove end */')

#----------------------------------------------------------------------------

print "Fixing configure.ac file."
fix_configure_ac_file ('configure.ac')

print "Building and testing source."
	# Try --disable-shared --disable-gcc-opt
if os.system ("./reconfigure.mk && ./configure --disable-shared --disable-gcc-opt && make check"):
	os.system ('PS1="FIX > " bash --norc')
	sys.exit (1)

print "Making distcheck"
if os.system ("make distcheck"):
	os.system ('PS1="FIX > " bash --norc')
	sys.exit (1)

print "Copying tarball"
if os.system ("cp %s.tar.gz %s" % (lite_version, source_dir)):
	print "??? %s.tar.gz ???" % lite_version
	os.system ('PS1="FIX > " bash --norc')
	sys.exit (1)

os.chdir (source_dir)

os.system ("rm -rf /tmp/%s" % lite_version)

print "Done."
