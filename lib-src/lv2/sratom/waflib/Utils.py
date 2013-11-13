#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import os,sys,errno,traceback,inspect,re,shutil,datetime,gc
import subprocess
try:
	from collections import deque
except ImportError:
	class deque(list):
		def popleft(self):
			return self.pop(0)
try:
	import _winreg as winreg
except ImportError:
	try:
		import winreg
	except ImportError:
		winreg=None
from waflib import Errors
try:
	from collections import UserDict
except ImportError:
	from UserDict import UserDict
try:
	from hashlib import md5
except ImportError:
	try:
		from md5 import md5
	except ImportError:
		pass
try:
	import threading
except ImportError:
	class threading(object):
		pass
	class Lock(object):
		def acquire(self):
			pass
		def release(self):
			pass
	threading.Lock=threading.Thread=Lock
else:
	run_old=threading.Thread.run
	def run(*args,**kwargs):
		try:
			run_old(*args,**kwargs)
		except(KeyboardInterrupt,SystemExit):
			raise
		except Exception:
			sys.excepthook(*sys.exc_info())
	threading.Thread.run=run
SIG_NIL='iluvcuteoverload'
O644=420
O755=493
rot_chr=['\\','|','/','-']
rot_idx=0
try:
	from collections import defaultdict
except ImportError:
	class defaultdict(dict):
		def __init__(self,default_factory):
			super(defaultdict,self).__init__()
			self.default_factory=default_factory
		def __getitem__(self,key):
			try:
				return super(defaultdict,self).__getitem__(key)
			except KeyError:
				value=self.default_factory()
				self[key]=value
				return value
is_win32=sys.platform in('win32','cli')
indicator='\x1b[K%s%s%s\r'
if is_win32 and'NOCOLOR'in os.environ:
	indicator='%s%s%s\r'
def readf(fname,m='r',encoding='ISO8859-1'):
	if sys.hexversion>0x3000000 and not'b'in m:
		m+='b'
		f=open(fname,m)
		try:
			txt=f.read()
		finally:
			f.close()
		txt=txt.decode(encoding)
	else:
		f=open(fname,m)
		try:
			txt=f.read()
		finally:
			f.close()
	return txt
def writef(fname,data,m='w',encoding='ISO8859-1'):
	if sys.hexversion>0x3000000 and not'b'in m:
		data=data.encode(encoding)
		m+='b'
	f=open(fname,m)
	try:
		f.write(data)
	finally:
		f.close()
def h_file(fname):
	f=open(fname,'rb')
	m=md5()
	try:
		while fname:
			fname=f.read(200000)
			m.update(fname)
	finally:
		f.close()
	return m.digest()
if hasattr(os,'O_NOINHERIT'):
	def readf_win32(f,m='r',encoding='ISO8859-1'):
		flags=os.O_NOINHERIT|os.O_RDONLY
		if'b'in m:
			flags|=os.O_BINARY
		if'+'in m:
			flags|=os.O_RDWR
		try:
			fd=os.open(f,flags)
		except OSError:
			raise IOError('Cannot read from %r'%f)
		if sys.hexversion>0x3000000 and not'b'in m:
			m+='b'
			f=os.fdopen(fd,m)
			try:
				txt=f.read()
			finally:
				f.close()
			txt=txt.decode(encoding)
		else:
			f=os.fdopen(fd,m)
			try:
				txt=f.read()
			finally:
				f.close()
		return txt
	def writef_win32(f,data,m='w',encoding='ISO8859-1'):
		if sys.hexversion>0x3000000 and not'b'in m:
			data=data.encode(encoding)
			m+='b'
		flags=os.O_CREAT|os.O_TRUNC|os.O_WRONLY|os.O_NOINHERIT
		if'b'in m:
			flags|=os.O_BINARY
		if'+'in m:
			flags|=os.O_RDWR
		try:
			fd=os.open(f,flags)
		except OSError:
			raise IOError('Cannot write to %r'%f)
		f=os.fdopen(fd,m)
		try:
			f.write(data)
		finally:
			f.close()
	def h_file_win32(fname):
		try:
			fd=os.open(fname,os.O_BINARY|os.O_RDONLY|os.O_NOINHERIT)
		except OSError:
			raise IOError('Cannot read from %r'%fname)
		f=os.fdopen(fd,'rb')
		m=md5()
		try:
			while fname:
				fname=f.read(200000)
				m.update(fname)
		finally:
			f.close()
		return m.digest()
	readf_old=readf
	writef_old=writef
	h_file_old=h_file
	readf=readf_win32
	writef=writef_win32
	h_file=h_file_win32
try:
	x=''.encode('hex')
except LookupError:
	import binascii
	def to_hex(s):
		ret=binascii.hexlify(s)
		if not isinstance(ret,str):
			ret=ret.decode('utf-8')
		return ret
else:
	def to_hex(s):
		return s.encode('hex')
to_hex.__doc__="""
Return the hexadecimal representation of a string

:param s: string to convert
:type s: string
"""
listdir=os.listdir
if is_win32:
	def listdir_win32(s):
		if not s:
			try:
				import ctypes
			except ImportError:
				return[x+':\\'for x in list('ABCDEFGHIJKLMNOPQRSTUVWXYZ')]
			else:
				dlen=4
				maxdrives=26
				buf=ctypes.create_string_buffer(maxdrives*dlen)
				ndrives=ctypes.windll.kernel32.GetLogicalDriveStringsA(maxdrives*dlen,ctypes.byref(buf))
				return[str(buf.raw[4*i:4*i+2].decode('ascii'))for i in range(int(ndrives/dlen))]
		if len(s)==2 and s[1]==":":
			s+=os.sep
		if not os.path.isdir(s):
			e=OSError('%s is not a directory'%s)
			e.errno=errno.ENOENT
			raise e
		return os.listdir(s)
	listdir=listdir_win32
def num2ver(ver):
	if isinstance(ver,str):
		ver=tuple(ver.split('.'))
	if isinstance(ver,tuple):
		ret=0
		for i in range(4):
			if i<len(ver):
				ret+=256**(3-i)*int(ver[i])
		return ret
	return ver
def ex_stack():
	exc_type,exc_value,tb=sys.exc_info()
	exc_lines=traceback.format_exception(exc_type,exc_value,tb)
	return''.join(exc_lines)
def to_list(sth):
	if isinstance(sth,str):
		return sth.split()
	else:
		return sth
re_nl=re.compile('\r*\n',re.M)
def str_to_dict(txt):
	tbl={}
	lines=re_nl.split(txt)
	for x in lines:
		x=x.strip()
		if not x or x.startswith('#')or x.find('=')<0:
			continue
		tmp=x.split('=')
		tbl[tmp[0].strip()]='='.join(tmp[1:]).strip()
	return tbl
def split_path(path):
	return path.split('/')
def split_path_cygwin(path):
	if path.startswith('//'):
		ret=path.split('/')[2:]
		ret[0]='/'+ret[0]
		return ret
	return path.split('/')
re_sp=re.compile('[/\\\\]')
def split_path_win32(path):
	if path.startswith('\\\\'):
		ret=re.split(re_sp,path)[2:]
		ret[0]='\\'+ret[0]
		return ret
	return re.split(re_sp,path)
if sys.platform=='cygwin':
	split_path=split_path_cygwin
elif is_win32:
	split_path=split_path_win32
split_path.__doc__="""
Split a path by / or \\. This function is not like os.path.split

:type  path: string
:param path: path to split
:return:     list of strings
"""
def check_dir(path):
	if not os.path.isdir(path):
		try:
			os.makedirs(path)
		except OSError ,e:
			if not os.path.isdir(path):
				raise Errors.WafError('Cannot create the folder %r'%path,ex=e)
def def_attrs(cls,**kw):
	for k,v in kw.items():
		if not hasattr(cls,k):
			setattr(cls,k,v)
def quote_define_name(s):
	fu=re.compile("[^a-zA-Z0-9]").sub("_",s)
	fu=fu.upper()
	return fu
def h_list(lst):
	m=md5()
	m.update(str(lst))
	return m.digest()
def h_fun(fun):
	try:
		return fun.code
	except AttributeError:
		try:
			h=inspect.getsource(fun)
		except IOError:
			h="nocode"
		try:
			fun.code=h
		except AttributeError:
			pass
		return h
reg_subst=re.compile(r"(\\\\)|(\$\$)|\$\{([^}]+)\}")
def subst_vars(expr,params):
	def repl_var(m):
		if m.group(1):
			return'\\'
		if m.group(2):
			return'$'
		try:
			return params.get_flat(m.group(3))
		except AttributeError:
			return params[m.group(3)]
	return reg_subst.sub(repl_var,expr)
def destos_to_binfmt(key):
	if key=='darwin':
		return'mac-o'
	elif key in('win32','cygwin','uwin','msys'):
		return'pe'
	return'elf'
def unversioned_sys_platform():
	s=sys.platform
	if s=='java':
		from java.lang import System
		s=System.getProperty('os.name')
		if s=='Mac OS X':
			return'darwin'
		elif s.startswith('Windows '):
			return'win32'
		elif s=='OS/2':
			return'os2'
		elif s=='HP-UX':
			return'hpux'
		elif s in('SunOS','Solaris'):
			return'sunos'
		else:s=s.lower()
	if s=='powerpc':
		return'darwin'
	if s=='win32'or s.endswith('os2')and s!='sunos2':return s
	return re.split('\d+$',s)[0]
def nada(*k,**kw):
	pass
class Timer(object):
	def __init__(self):
		self.start_time=datetime.datetime.utcnow()
	def __str__(self):
		delta=datetime.datetime.utcnow()-self.start_time
		days=int(delta.days)
		hours=delta.seconds//3600
		minutes=(delta.seconds-hours*3600)//60
		seconds=delta.seconds-hours*3600-minutes*60+float(delta.microseconds)/1000/1000
		result=''
		if days:
			result+='%dd'%days
		if days or hours:
			result+='%dh'%hours
		if days or hours or minutes:
			result+='%dm'%minutes
		return'%s%.3fs'%(result,seconds)
if is_win32:
	old=shutil.copy2
	def copy2(src,dst):
		old(src,dst)
		shutil.copystat(src,dst)
	setattr(shutil,'copy2',copy2)
if os.name=='java':
	try:
		gc.disable()
		gc.enable()
	except NotImplementedError:
		gc.disable=gc.enable
def read_la_file(path):
	sp=re.compile(r'^([^=]+)=\'(.*)\'$')
	dc={}
	for line in readf(path).splitlines():
		try:
			_,left,right,_=sp.split(line.strip())
			dc[left]=right
		except ValueError:
			pass
	return dc
def nogc(fun):
	def f(*k,**kw):
		try:
			gc.disable()
			ret=fun(*k,**kw)
		finally:
			gc.enable()
		return ret
	f.__doc__=fun.__doc__
	return f
def run_once(fun):
	cache={}
	def wrap(k):
		try:
			return cache[k]
		except KeyError:
			ret=fun(k)
			cache[k]=ret
			return ret
	wrap.__cache__=cache
	return wrap
def get_registry_app_path(key,filename):
	if not winreg:
		return None
	try:
		result=winreg.QueryValue(key,"Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\%s.exe"%filename[0])
	except WindowsError:
		pass
	else:
		if os.path.isfile(result):
			return result
