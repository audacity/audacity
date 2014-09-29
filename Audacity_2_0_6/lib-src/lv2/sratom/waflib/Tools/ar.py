#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

from waflib.Configure import conf
@conf
def find_ar(conf):
	conf.load('ar')
def configure(conf):
	conf.find_program('ar',var='AR')
	conf.env.ARFLAGS='rcs'
