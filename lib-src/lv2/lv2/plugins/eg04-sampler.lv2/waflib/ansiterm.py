#! /usr/bin/env python
# encoding: utf-8
# WARNING! Do not edit! http://waf.googlecode.com/git/docs/wafbook/single.html#_obtaining_the_waf_file

import sys,os
try:
	if not(sys.stderr.isatty()and sys.stdout.isatty()):
		raise ValueError('not a tty')
	from ctypes import*
	class COORD(Structure):
		_fields_=[("X",c_short),("Y",c_short)]
	class SMALL_RECT(Structure):
		_fields_=[("Left",c_short),("Top",c_short),("Right",c_short),("Bottom",c_short)]
	class CONSOLE_SCREEN_BUFFER_INFO(Structure):
		_fields_=[("Size",COORD),("CursorPosition",COORD),("Attributes",c_short),("Window",SMALL_RECT),("MaximumWindowSize",COORD)]
	class CONSOLE_CURSOR_INFO(Structure):
		_fields_=[('dwSize',c_ulong),('bVisible',c_int)]
	sbinfo=CONSOLE_SCREEN_BUFFER_INFO()
	csinfo=CONSOLE_CURSOR_INFO()
	hconsole=windll.kernel32.GetStdHandle(-11)
	windll.kernel32.GetConsoleScreenBufferInfo(hconsole,byref(sbinfo))
	if sbinfo.Size.X<9 or sbinfo.Size.Y<9:raise ValueError('small console')
	windll.kernel32.GetConsoleCursorInfo(hconsole,byref(csinfo))
except Exception:
	pass
else:
	import re,threading
	is_vista=getattr(sys,"getwindowsversion",None)and sys.getwindowsversion()[0]>=6
	try:
		_type=unicode
	except NameError:
		_type=str
	to_int=lambda number,default:number and int(number)or default
	wlock=threading.Lock()
	STD_OUTPUT_HANDLE=-11
	STD_ERROR_HANDLE=-12
	class AnsiTerm(object):
		def __init__(self):
			self.encoding=sys.stdout.encoding
			self.hconsole=windll.kernel32.GetStdHandle(STD_OUTPUT_HANDLE)
			self.cursor_history=[]
			self.orig_sbinfo=CONSOLE_SCREEN_BUFFER_INFO()
			self.orig_csinfo=CONSOLE_CURSOR_INFO()
			windll.kernel32.GetConsoleScreenBufferInfo(self.hconsole,byref(self.orig_sbinfo))
			windll.kernel32.GetConsoleCursorInfo(hconsole,byref(self.orig_csinfo))
		def screen_buffer_info(self):
			sbinfo=CONSOLE_SCREEN_BUFFER_INFO()
			windll.kernel32.GetConsoleScreenBufferInfo(self.hconsole,byref(sbinfo))
			return sbinfo
		def clear_line(self,param):
			mode=param and int(param)or 0
			sbinfo=self.screen_buffer_info()
			if mode==1:
				line_start=COORD(0,sbinfo.CursorPosition.Y)
				line_length=sbinfo.Size.X
			elif mode==2:
				line_start=COORD(sbinfo.CursorPosition.X,sbinfo.CursorPosition.Y)
				line_length=sbinfo.Size.X-sbinfo.CursorPosition.X
			else:
				line_start=sbinfo.CursorPosition
				line_length=sbinfo.Size.X-sbinfo.CursorPosition.X
			chars_written=c_int()
			windll.kernel32.FillConsoleOutputCharacterA(self.hconsole,c_wchar(' '),line_length,line_start,byref(chars_written))
			windll.kernel32.FillConsoleOutputAttribute(self.hconsole,sbinfo.Attributes,line_length,line_start,byref(chars_written))
		def clear_screen(self,param):
			mode=to_int(param,0)
			sbinfo=self.screen_buffer_info()
			if mode==1:
				clear_start=COORD(0,0)
				clear_length=sbinfo.CursorPosition.X*sbinfo.CursorPosition.Y
			elif mode==2:
				clear_start=COORD(0,0)
				clear_length=sbinfo.Size.X*sbinfo.Size.Y
				windll.kernel32.SetConsoleCursorPosition(self.hconsole,clear_start)
			else:
				clear_start=sbinfo.CursorPosition
				clear_length=((sbinfo.Size.X-sbinfo.CursorPosition.X)+sbinfo.Size.X*(sbinfo.Size.Y-sbinfo.CursorPosition.Y))
			chars_written=c_int()
			windll.kernel32.FillConsoleOutputCharacterA(self.hconsole,c_wchar(' '),clear_length,clear_start,byref(chars_written))
			windll.kernel32.FillConsoleOutputAttribute(self.hconsole,sbinfo.Attributes,clear_length,clear_start,byref(chars_written))
		def push_cursor(self,param):
			sbinfo=self.screen_buffer_info()
			self.cursor_history.append(sbinfo.CursorPosition)
		def pop_cursor(self,param):
			if self.cursor_history:
				old_pos=self.cursor_history.pop()
				windll.kernel32.SetConsoleCursorPosition(self.hconsole,old_pos)
		def set_cursor(self,param):
			y,sep,x=param.partition(';')
			x=to_int(x,1)-1
			y=to_int(y,1)-1
			sbinfo=self.screen_buffer_info()
			new_pos=COORD(min(max(0,x),sbinfo.Size.X),min(max(0,y),sbinfo.Size.Y))
			windll.kernel32.SetConsoleCursorPosition(self.hconsole,new_pos)
		def set_column(self,param):
			x=to_int(param,1)-1
			sbinfo=self.screen_buffer_info()
			new_pos=COORD(min(max(0,x),sbinfo.Size.X),sbinfo.CursorPosition.Y)
			windll.kernel32.SetConsoleCursorPosition(self.hconsole,new_pos)
		def move_cursor(self,x_offset=0,y_offset=0):
			sbinfo=self.screen_buffer_info()
			new_pos=COORD(min(max(0,sbinfo.CursorPosition.X+x_offset),sbinfo.Size.X),min(max(0,sbinfo.CursorPosition.Y+y_offset),sbinfo.Size.Y))
			windll.kernel32.SetConsoleCursorPosition(self.hconsole,new_pos)
		def move_up(self,param):
			self.move_cursor(y_offset=-to_int(param,1))
		def move_down(self,param):
			self.move_cursor(y_offset=to_int(param,1))
		def move_left(self,param):
			self.move_cursor(x_offset=-to_int(param,1))
		def move_right(self,param):
			self.move_cursor(x_offset=to_int(param,1))
		def next_line(self,param):
			sbinfo=self.screen_buffer_info()
			self.move_cursor(x_offset=-sbinfo.CursorPosition.X,y_offset=to_int(param,1))
		def prev_line(self,param):
			sbinfo=self.screen_buffer_info()
			self.move_cursor(x_offset=-sbinfo.CursorPosition.X,y_offset=-to_int(param,1))
		def rgb2bgr(self,c):
			return((c&1)<<2)|(c&2)|((c&4)>>2)
		def set_color(self,param):
			cols=param.split(';')
			sbinfo=CONSOLE_SCREEN_BUFFER_INFO()
			windll.kernel32.GetConsoleScreenBufferInfo(self.hconsole,byref(sbinfo))
			attr=sbinfo.Attributes
			for c in cols:
				if is_vista:
					c=int(c)
				else:
					c=to_int(c,0)
				if c in range(30,38):
					attr=(attr&0xfff0)|self.rgb2bgr(c-30)
				elif c in range(40,48):
					attr=(attr&0xff0f)|(self.rgb2bgr(c-40)<<4)
				elif c==0:
					attr=self.orig_sbinfo.Attributes
				elif c==1:
					attr|=0x08
				elif c==4:
					attr|=0x80
				elif c==7:
					attr=(attr&0xff88)|((attr&0x70)>>4)|((attr&0x07)<<4)
			windll.kernel32.SetConsoleTextAttribute(self.hconsole,attr)
		def show_cursor(self,param):
			csinfo.bVisible=1
			windll.kernel32.SetConsoleCursorInfo(self.hconsole,byref(csinfo))
		def hide_cursor(self,param):
			csinfo.bVisible=0
			windll.kernel32.SetConsoleCursorInfo(self.hconsole,byref(csinfo))
		ansi_command_table={'A':move_up,'B':move_down,'C':move_right,'D':move_left,'E':next_line,'F':prev_line,'G':set_column,'H':set_cursor,'f':set_cursor,'J':clear_screen,'K':clear_line,'h':show_cursor,'l':hide_cursor,'m':set_color,'s':push_cursor,'u':pop_cursor,}
		ansi_tokens=re.compile('(?:\x1b\[([0-9?;]*)([a-zA-Z])|([^\x1b]+))')
		def write(self,text):
			try:
				wlock.acquire()
				for param,cmd,txt in self.ansi_tokens.findall(text):
					if cmd:
						cmd_func=self.ansi_command_table.get(cmd)
						if cmd_func:
							cmd_func(self,param)
					else:
						self.writeconsole(txt)
			finally:
				wlock.release()
		def writeconsole(self,txt):
			chars_written=c_int()
			writeconsole=windll.kernel32.WriteConsoleA
			if isinstance(txt,_type):
				writeconsole=windll.kernel32.WriteConsoleW
			TINY_STEP=3000
			for x in range(0,len(txt),TINY_STEP):
				tiny=txt[x:x+TINY_STEP]
				writeconsole(self.hconsole,tiny,len(tiny),byref(chars_written),None)
		def flush(self):
			pass
		def isatty(self):
			return True
	sys.stderr=sys.stdout=AnsiTerm()
	os.environ['TERM']='vt100'
