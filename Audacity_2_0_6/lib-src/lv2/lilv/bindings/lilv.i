%module lilv
%{
#include "lilv/lilv.h"
#include "lilv/lilvmm.hpp"
%}

%include "lilv/lilv.h"
%include "lilv/lilvmm.hpp"

namespace Lilv {

%extend Plugins {
%pythoncode %{
	def __iter__(self):
		class Iterator(object):
			def __init__(self, plugins):
				self.plugins = plugins
				self.iter    = plugins.begin()
	                
			def next(self):
				self.iter = self.plugins.next(self.iter)
				if not self.plugins.is_end(self.iter):
					return self.plugins.get(self.iter)
				else:
					raise StopIteration

		return Iterator(self)
%}
};

%extend Node {
%pythoncode %{
	def __str__(self):
		return self.get_turtle_token()
%}
};

} /* namespace Lilv */
