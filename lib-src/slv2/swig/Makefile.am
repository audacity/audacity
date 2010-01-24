EXTRA_DIST = slv2.i lv2_list.py slv2.setup

if WITH_SWIG

all: python mzscheme chicken

if WITH_PYTHON
python:
	swig -DPYTHON -Wall -python -I.. -o slv2_python.c -oh slv2_python.h slv2.i
	gcc -fPIC -shared $(PYTHON_CPPFLAGS) $(PYTHON_EXTRA_LDFLAGS) \
		$(PYTHON_EXTRA_LIBS) $(PYTHON_EXTRA_LDFLAGS) \
		slv2_python.c ../src/.libs/libslv2.so -o _slv2.so
else
python:
	
endif # WITH_PYTHON

if WITH_MZSCHEME
mzscheme:
	swig -DMZSCHEME -Wall -mzscheme -I.. -o slv2_mzscheme.c -oh slv2_mzscheme.h slv2.i
	gcc -fPIC -shared -Iplt \
		-I/usr/include/plt slv2_mzscheme.c -o libslv2_mzscheme.so
endif # WITH_MZSCHEME

if WITH_CHICKEN
chicken:
	rm -f slv2.scm slv2_wrap.c
	rm -rf eggs
	mkdir -p eggs
	swig -DCHICKEN -Wall -chicken -proxy -nounit -I.. -o slv2_wrap.c -oh slv2_wrap.h slv2.i
	tar -czf eggs/slv2.egg slv2.setup slv2.scm slv2_wrap.c
endif # WITH_CHICKEN

endif # WITH_SWIG

clean-local:
	rm -f *.cxx
	rm -f *.so
	rm -f *.o
	rm -f slv2.py
