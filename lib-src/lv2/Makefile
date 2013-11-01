all:
	./build

clean:
	rm -rf {
	rm -rf include lv2/build serd/build sord/build sratom/build lilv/build
	rm -f liblv2.a

distclean: clean
	rm -rf lv2/.waf-* serd/.waf-* sord/.waf-* sratom/.waf-* lilv/.waf-*
	rm -rf lv2/.lock-* serd/.lock-* sord/.lock-* sratom/.lock-* lilv/.lock-*

.PHONY: all clean distclean
