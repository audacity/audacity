:- use_module('../../../motools/mo/ontospec/onto_spec').
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_turtle')).

:- rdf_load('../vamp.rdf').

:- rdf_db:rdf_register_ns(vamp,'http://purl.org/ontology/vamp/').

author_name('').
author_foaf('').
page_title('Vamp Plugins Ontology').

output('vamp.html').

:-  output(Output),
	open(Output,write,Otp),
	header(Header),
	write(Otp,Header),
	open('../doc/glance.htm',read,GlanceIntro),
	copy_stream_data(GlanceIntro, Otp),
	glance_html_desc(Glance),
	write(Otp,Glance),
	write(Otp,'<h2 id="terms_classes">Classes</h2>'),
	classes_html_desc(Classes),
	write(Otp,Classes),
	write(Otp,'<h2 id="terms_props">Properties</h2>'),
	props_html_desc(Props),
	write(Otp,Props),
	write(Otp,'<h2 id="terms_inds">Individuals</h2>'),
	inds_html_desc(Inds),
	write(Otp,Inds),
	deprecs_html_desc(Deprecs),
	write(Otp,Deprecs),
	close(Otp),
	rdf_db:rdf_retractall(_,_,_).

:- halt.
