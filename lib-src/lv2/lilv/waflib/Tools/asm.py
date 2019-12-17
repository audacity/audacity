#!/usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2008-2018 (ita)

"""
Assembly support, used by tools such as gas and nasm

To declare targets using assembly::

	def configure(conf):
		conf.load('gcc gas')

	def build(bld):
		bld(
			features='c cstlib asm',
			source = 'test.S',
			target = 'asmtest')

		bld(
			features='asm asmprogram',
			source = 'test.S',
			target = 'asmtest')

Support for pure asm programs and libraries should also work::

	def configure(conf):
		conf.load('nasm')
		conf.find_program('ld', 'ASLINK')

	def build(bld):
		bld(
			features='asm asmprogram',
			source = 'test.S',
			target = 'asmtest')
"""

import re
from waflib import Errors, Logs, Task
from waflib.Tools.ccroot import link_task, stlink_task
from waflib.TaskGen import extension
from waflib.Tools import c_preproc

re_lines = re.compile(
	'^[ \t]*(?:%)[ \t]*(ifdef|ifndef|if|else|elif|endif|include|import|define|undef)[ \t]*(.*)\r*$',
	re.IGNORECASE | re.MULTILINE)

class asm_parser(c_preproc.c_parser):
	def filter_comments(self, node):
		code = node.read()
		code = c_preproc.re_nl.sub('', code)
		code = c_preproc.re_cpp.sub(c_preproc.repl, code)
		return re_lines.findall(code)

class asm(Task.Task):
	"""
	Compiles asm files by gas/nasm/yasm/...
	"""
	color = 'BLUE'
	run_str = '${AS} ${ASFLAGS} ${ASMPATH_ST:INCPATHS} ${DEFINES_ST:DEFINES} ${AS_SRC_F}${SRC} ${AS_TGT_F}${TGT}'

	def scan(self):
		if self.env.ASM_NAME == 'gas':
			return c_preproc.scan(self)
			Logs.warn('There is no dependency scanner for Nasm!')
			return  [[], []]
		elif self.env.ASM_NAME == 'nasm':
			Logs.warn('The Nasm dependency scanner is incomplete!')

		try:
			incn = self.generator.includes_nodes
		except AttributeError:
			raise Errors.WafError('%r is missing the "asm" feature' % self.generator)

		if c_preproc.go_absolute:
			nodepaths = incn
		else:
			nodepaths = [x for x in incn if x.is_child_of(x.ctx.srcnode) or x.is_child_of(x.ctx.bldnode)]

		tmp = asm_parser(nodepaths)
		tmp.start(self.inputs[0], self.env)
		return (tmp.nodes, tmp.names)

@extension('.s', '.S', '.asm', '.ASM', '.spp', '.SPP')
def asm_hook(self, node):
	"""
	Binds the asm extension to the asm task

	:param node: input file
	:type node: :py:class:`waflib.Node.Node`
	"""
	return self.create_compiled_task('asm', node)

class asmprogram(link_task):
	"Links object files into a c program"
	run_str = '${ASLINK} ${ASLINKFLAGS} ${ASLNK_TGT_F}${TGT} ${ASLNK_SRC_F}${SRC}'
	ext_out = ['.bin']
	inst_to = '${BINDIR}'

class asmshlib(asmprogram):
	"Links object files into a c shared library"
	inst_to = '${LIBDIR}'

class asmstlib(stlink_task):
	"Links object files into a c static library"
	pass # do not remove

def configure(conf):
	conf.env.ASMPATH_ST = '-I%s'
