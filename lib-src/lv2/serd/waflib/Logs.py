#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,re,traceback,sys
_nocolor=os.environ.get('NOCOLOR','no')not in('no','0','false')
try:
	if not _nocolor:
		import waflib.ansiterm
except ImportError:
	pass
try:
	import threading
except ImportError:
	if not'JOBS'in os.environ:
		os.environ['JOBS']='1'
else:
	wlock=threading.Lock()
	class sync_stream(object):
		def __init__(self,stream):
			self.stream=stream
			self.encoding=self.stream.encoding
		def write(self,txt):
			try:
				wlock.acquire()
				self.stream.write(txt)
				self.stream.flush()
			finally:
				wlock.release()
		def fileno(self):
			return self.stream.fileno()
		def flush(self):
			self.stream.flush()
		def isatty(self):
			return self.stream.isatty()
	if not os.environ.get('NOSYNC',False):
		if id(sys.stdout)==id(sys.__stdout__):
			sys.stdout=sync_stream(sys.stdout)
			sys.stderr=sync_stream(sys.stderr)
import logging
LOG_FORMAT="%(asctime)s %(c1)s%(zone)s%(c2)s %(message)s"
HOUR_FORMAT="%H:%M:%S"
zones=''
verbose=0
colors_lst={'USE':True,'BOLD':'\x1b[01;1m','RED':'\x1b[01;31m','GREEN':'\x1b[32m','YELLOW':'\x1b[33m','PINK':'\x1b[35m','BLUE':'\x1b[01;34m','CYAN':'\x1b[36m','NORMAL':'\x1b[0m','cursor_on':'\x1b[?25h','cursor_off':'\x1b[?25l',}
got_tty=not os.environ.get('TERM','dumb')in['dumb','emacs']
if got_tty:
	try:
		got_tty=sys.stderr.isatty()and sys.stdout.isatty()
	except AttributeError:
		got_tty=False
if(not got_tty and os.environ.get('TERM','dumb')!='msys')or _nocolor:
	colors_lst['USE']=False
def get_term_cols():
	return 80
try:
	import struct,fcntl,termios
except ImportError:
	pass
else:
	if got_tty:
		def get_term_cols_real():
			dummy_lines,cols=struct.unpack("HHHH",fcntl.ioctl(sys.stderr.fileno(),termios.TIOCGWINSZ,struct.pack("HHHH",0,0,0,0)))[:2]
			return cols
		try:
			get_term_cols_real()
		except Exception:
			pass
		else:
			get_term_cols=get_term_cols_real
get_term_cols.__doc__="""
	Get the console width in characters.

	:return: the number of characters per line
	:rtype: int
	"""
def get_color(cl):
	if not colors_lst['USE']:return''
	return colors_lst.get(cl,'')
class color_dict(object):
	def __getattr__(self,a):
		return get_color(a)
	def __call__(self,a):
		return get_color(a)
colors=color_dict()
re_log=re.compile(r'(\w+): (.*)',re.M)
class log_filter(logging.Filter):
	def __init__(self,name=None):
		pass
	def filter(self,rec):
		rec.c1=colors.PINK
		rec.c2=colors.NORMAL
		rec.zone=rec.module
		if rec.levelno>=logging.INFO:
			if rec.levelno>=logging.ERROR:
				rec.c1=colors.RED
			elif rec.levelno>=logging.WARNING:
				rec.c1=colors.YELLOW
			else:
				rec.c1=colors.GREEN
			return True
		m=re_log.match(rec.msg)
		if m:
			rec.zone=m.group(1)
			rec.msg=m.group(2)
		if zones:
			return getattr(rec,'zone','')in zones or'*'in zones
		elif not verbose>2:
			return False
		return True
class formatter(logging.Formatter):
	def __init__(self):
		logging.Formatter.__init__(self,LOG_FORMAT,HOUR_FORMAT)
	def format(self,rec):
		if rec.levelno>=logging.WARNING or rec.levelno==logging.INFO:
			try:
				msg=rec.msg.decode('utf-8')
			except Exception:
				msg=rec.msg
			return'%s%s%s'%(rec.c1,msg,rec.c2)
		return logging.Formatter.format(self,rec)
log=None
def debug(*k,**kw):
	if verbose:
		k=list(k)
		k[0]=k[0].replace('\n',' ')
		global log
		log.debug(*k,**kw)
def error(*k,**kw):
	global log
	log.error(*k,**kw)
	if verbose>2:
		st=traceback.extract_stack()
		if st:
			st=st[:-1]
			buf=[]
			for filename,lineno,name,line in st:
				buf.append('  File "%s", line %d, in %s'%(filename,lineno,name))
				if line:
					buf.append('	%s'%line.strip())
			if buf:log.error("\n".join(buf))
def warn(*k,**kw):
	global log
	log.warn(*k,**kw)
def info(*k,**kw):
	global log
	log.info(*k,**kw)
def init_log():
	global log
	log=logging.getLogger('waflib')
	log.handlers=[]
	log.filters=[]
	hdlr=logging.StreamHandler()
	hdlr.setFormatter(formatter())
	log.addHandler(hdlr)
	log.addFilter(log_filter())
	log.setLevel(logging.DEBUG)
def make_logger(path,name):
	logger=logging.getLogger(name)
	hdlr=logging.FileHandler(path,'w')
	formatter=logging.Formatter('%(message)s')
	hdlr.setFormatter(formatter)
	logger.addHandler(hdlr)
	logger.setLevel(logging.DEBUG)
	return logger
def make_mem_logger(name,to_log,size=10000):
	from logging.handlers import MemoryHandler
	logger=logging.getLogger(name)
	hdlr=MemoryHandler(size,target=to_log)
	formatter=logging.Formatter('%(message)s')
	hdlr.setFormatter(formatter)
	logger.addHandler(hdlr)
	logger.memhandler=hdlr
	logger.setLevel(logging.DEBUG)
	return logger
def pprint(col,str,label='',sep='\n'):
	sys.stderr.write("%s%s%s %s%s"%(colors(col),str,colors.NORMAL,label,sep))
