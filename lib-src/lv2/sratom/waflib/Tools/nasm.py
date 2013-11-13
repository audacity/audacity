#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import waflib.Tools.asm
from waflib.TaskGen import feature
@feature('asm')
def apply_nasm_vars(self):
	self.env.append_value('ASFLAGS',self.to_list(getattr(self,'nasm_flags',[])))
def configure(conf):
	nasm=conf.find_program(['nasm','yasm'],var='AS')
	conf.env.AS_TGT_F=['-o']
	conf.env.ASLNK_TGT_F=['-o']
	conf.load('asm')
