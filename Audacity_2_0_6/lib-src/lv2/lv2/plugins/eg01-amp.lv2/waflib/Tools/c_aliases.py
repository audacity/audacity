#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys,re
from waflib import Utils,Build
from waflib.Configure import conf
def get_extensions(lst):
	ret=[]
	for x in Utils.to_list(lst):
		try:
			if not isinstance(x,str):
				x=x.name
			ret.append(x[x.rfind('.')+1:])
		except Exception:
			pass
	return ret
def sniff_features(**kw):
	exts=get_extensions(kw['source'])
	type=kw['_type']
	feats=[]
	if'cxx'in exts or'cpp'in exts or'c++'in exts or'cc'in exts or'C'in exts:
		feats.append('cxx')
	if'c'in exts or'vala'in exts:
		feats.append('c')
	if'd'in exts:
		feats.append('d')
	if'java'in exts:
		feats.append('java')
	if'java'in exts:
		return'java'
	if type in['program','shlib','stlib']:
		for x in feats:
			if x in['cxx','d','c']:
				feats.append(x+type)
	return feats
def set_features(kw,_type):
	kw['_type']=_type
	kw['features']=Utils.to_list(kw.get('features',[]))+Utils.to_list(sniff_features(**kw))
@conf
def program(bld,*k,**kw):
	set_features(kw,'program')
	return bld(*k,**kw)
@conf
def shlib(bld,*k,**kw):
	set_features(kw,'shlib')
	return bld(*k,**kw)
@conf
def stlib(bld,*k,**kw):
	set_features(kw,'stlib')
	return bld(*k,**kw)
@conf
def objects(bld,*k,**kw):
	set_features(kw,'objects')
	return bld(*k,**kw)
