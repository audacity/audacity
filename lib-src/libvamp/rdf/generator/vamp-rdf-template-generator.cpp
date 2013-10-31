/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#include <vamp-hostsdk/PluginHostAdapter.h>
#include <vamp-hostsdk/PluginChannelAdapter.h>
#include <vamp-hostsdk/PluginInputDomainAdapter.h>
#include <vamp-hostsdk/PluginLoader.h>
#include <vamp/vamp.h>

#include <iostream>
#include <fstream>
#include <sstream>

#include <cmath>
#include <cstdlib>
#include <cstring>

#include <cstdlib>
#include <cstring>

using std::cout;
using std::cin;
using std::cerr;
using std::getline;
using std::endl;
using std::string;
using std::vector;
using std::ofstream;
using std::ios;

using Vamp::HostExt::PluginLoader;
using Vamp::Plugin;

//???
string programURI = "http://www.vamp-plugins.org/doap.rdf#template-generator";

void usage()
{
    cerr << endl;
    cerr << "vamp-rdf-template-generator: Create a skeleton RDF description file describing" << endl;
    cerr << "a Vamp plugin library using the Vamp ontology." << endl;
    cerr << endl;
    cerr << "Usage:" << endl;
    cerr << "   vamp-rdf-template-generator -i vamp:soname[:plugin] [vamp:soname[:plugin] ...]" << endl;
    cerr << "   vamp-rdf-template-generator PLUGIN_BASE_URI [ -m YOUR_URI ] [vamp:]soname[:plugin] [[vamp:]soname[:plugin] ...]" << endl;
    cerr << endl;
    cerr << "Example:" << endl;
    cerr << "   vamp-rdf-template-generator http://vamp-plugins.org/rdf/plugins/ vamp-example-plugins" << endl;
    cerr << endl;
    exit(2);
}

template <class T>
inline string to_string (const T& t)
{
    std::stringstream ss;
    ss << t;
    return ss.str();
}

string describe_namespaces(string pluginBundleBaseURI, string libname)
{
    string res=\
        "@prefix rdfs:     <http://www.w3.org/2000/01/rdf-schema#> .\n\
@prefix xsd:      <http://www.w3.org/2001/XMLSchema#> .\n\
@prefix vamp:     <http://purl.org/ontology/vamp/> .\n\
@prefix plugbase: <"+pluginBundleBaseURI+libname+"#> .\n\
@prefix owl:      <http://www.w3.org/2002/07/owl#> .\n\
@prefix dc:       <http://purl.org/dc/elements/1.1/> .\n\
@prefix af:       <http://purl.org/ontology/af/> .\n\
@prefix foaf:     <http://xmlns.com/foaf/0.1/> .\n\
@prefix cc:       <http://web.resource.org/cc/> .\n\
@prefix :         <#> .\n\n";
	
    return res;
}

string describe_doc(string describerURI, string pluginBundleBaseURI,
                    string libname)
{
    string res=\
        "<>  a   vamp:PluginDescription ;\n";
    if (describerURI != "") {
        res += "    foaf:maker          <"+describerURI+"> ;\n";
    }
    res += "\
    foaf:maker          <"+programURI+"> ;\n\
    foaf:primaryTopic   <"+pluginBundleBaseURI+libname+"> .\n\n";
    return res;
}


string describe_library(string libname, vector<Plugin *> plugins)
{
    string res=\
        ":"+libname+" a  vamp:PluginLibrary ;\n\
    vamp:identifier \""+libname+"\" ";

    for (size_t i = 0; i < plugins.size(); ++i) {
        res += " ; \n\
    vamp:available_plugin plugbase:"+plugins[i]->getIdentifier();
    }

    res += " ; \n\
#   foaf:page <Place more-information HTML page URL here and uncomment> ;\n\
    .\n\n";
    return res;
}

string describe_plugin(Plugin* plugin)
{
    string res=\
        "plugbase:"+plugin->getIdentifier()+" a   vamp:Plugin ;\n\
    dc:title              \""+plugin->getName()+"\" ;\n\
    vamp:name             \""+plugin->getName()+"\" ;\n\
    dc:description        \"\"\""+plugin->getDescription()+"\"\"\" ;\n\
    foaf:maker            [ foaf:name \""+plugin->getMaker()+"\" ] ; # FIXME could give plugin author's URI here\n\
    dc:rights             \"\"\""+plugin->getCopyright()+"\"\"\" ;\n\
#   cc:license            <Place plugin license URI here and uncomment> ; \n\
    vamp:identifier       \""+plugin->getIdentifier()+"\" ;\n\
    vamp:vamp_API_version vamp:api_version_"+to_string(plugin->getVampApiVersion())+" ;\n\
    owl:versionInfo       \""+to_string(plugin->getPluginVersion())+"\" ;\n";
    if (plugin->getInputDomain() == Vamp::Plugin::FrequencyDomain)
        res+="    vamp:input_domain     vamp:FrequencyDomain ;\n\n";
    else
        res+="    vamp:input_domain     vamp:TimeDomain ;\n";
	

    Plugin::ParameterList params = plugin->getParameterDescriptors();
    if (!params.empty()) res+="\n";
    for (Plugin::ParameterList::const_iterator i = params.begin(); i != params.end(); i++)
        res+="    vamp:parameter   plugbase:"+plugin->getIdentifier()+"_param_"+(*i).identifier+" ;\n";
    if (!params.empty()) res+="\n";

    Plugin::OutputList outputs = plugin->getOutputDescriptors();
    for (Plugin::OutputList::const_iterator i = outputs.begin(); i!= outputs.end(); i++)
        res+="    vamp:output      plugbase:"+plugin->getIdentifier()+"_output_"+(*i).identifier+" ;\n";
    res+="    .\n";
	
    return res;
}

string describe_param(Plugin *plugin, Plugin::ParameterDescriptor p)
{

    //FIXME: dc:format and vamp:unit are the same???
    //Should be a QUantizedParameter also a Parameter??
    if(p.isQuantized){
     string res=\
        "plugbase:"+plugin->getIdentifier()+"_param_"+p.identifier+" a  vamp:QuantizedParameter ;\n\
    vamp:identifier     \""+p.identifier+"\" ;\n\
    dc:title            \""+p.name+"\" ;\n\
    dc:format           \""+p.unit+"\" ;\n\
    vamp:min_value       "+to_string(p.minValue)+" ;\n\
    vamp:max_value       "+to_string(p.maxValue)+" ;\n\
    vamp:unit           \""+p.unit+"\" ;\n\
    vamp:quantize_step   "+to_string(p.quantizeStep)+"  ;\n\
    vamp:default_value   "+to_string(p.defaultValue)+" ;\n\
    vamp:value_names     (";

            unsigned int i;
            for (i=0; i+1 < p.valueNames.size(); i++)
                res+=" \""+p.valueNames[i]+"\"";
            if (i < p.valueNames.size())
                res+=" \""+p.valueNames[i]+"\"";
            res+=");\n";
    
    res+="    .\n";

    return res;
            
    }else{
    string res=\
        "plugbase:"+plugin->getIdentifier()+"_param_"+p.identifier+" a  vamp:Parameter ;\n\
    vamp:identifier     \""+p.identifier+"\" ;\n\
    dc:title            \""+p.name+"\" ;\n\
    dc:format           \""+p.unit+"\" ;\n\
    vamp:min_value       "+to_string(p.minValue)+" ;\n\
    vamp:max_value       "+to_string(p.maxValue)+" ;\n\
    vamp:unit           \""+p.unit+"\"  ;\n\
    vamp:default_value   "+to_string(p.defaultValue)+" ;\n\
    vamp:value_names     (";

            unsigned int i;
            for (i=0; i+1 < p.valueNames.size(); i++)
                res+=" \""+p.valueNames[i]+"\"";
            if (i < p.valueNames.size())
                res+=" \""+p.valueNames[i]+"\"";
            res+=");\n";
    
    res+="    .\n";

    return res;

    }  
}

string describe_output(Plugin *plugin, Plugin::OutputDescriptor o)
{

    //we need to distinguish here between different output types:

//Quantize or not
//KnownExtents or not
//Data output classification:
    //DenseOutput
    //SparseOutput
    //TrackLevelOutput


    // SparseOutput: variable sample rate. Events are not evenly
    // spaced so we need to record the time associated with the event
    // as it its not ensured that we have an event after the next one
    // (but there is not time to set the duration, it has to be
    // calculated as the different between 2 different events). The
    // timestamp must be read.

    string res;

    if (o.sampleType == Plugin::OutputDescriptor::VariableSampleRate ||
        !o.hasFixedBinCount)
    {

        res=\
            "plugbase:"+plugin->getIdentifier()+"_output_"+o.identifier+" a  vamp:SparseOutput ;\n\
    vamp:identifier       \""+o.identifier+"\" ;\n\
    dc:title              \""+o.name+"\" ;\n\
    dc:description        \"\"\""+o.description+"\"\"\"  ;\n\
    vamp:fixed_bin_count  \""+(o.hasFixedBinCount == 1 ? "true" : "false")+"\" ;\n\
    vamp:unit             \""+(o.unit)+"\" ;\n";
                          
             
        //another type of output           
        if(o.isQuantized){

            res+="    a                     vamp:QuantizedOutput ;\n";
            res+="    vamp:quantize_step    "+to_string(o.quantizeStep)+"  ;\n";    
        }
        
        //and yet another type
        if(o.hasKnownExtents){

            res+="    a                 vamp:KnownExtentsOutput ;\n";
            res+="    vamp:min_value    "+to_string(o.minValue)+"  ;\n"; 
            res+="    vamp:max_value    "+to_string(o.maxValue)+"  ;\n";    
        }

        // FIXME ? Bin names may vary based on plugin setup, so including them here might be misleading...
        if (o.hasFixedBinCount)
        {
            res+="    vamp:bin_count        "+to_string(o.binCount)+" ;\n";

            bool haveBinNames = false;
            for (unsigned int i=0; i < o.binNames.size(); i++) {
                if (o.binNames[i] != "") {
                    haveBinNames = true;
                    break;
                }
            }
            
            if (haveBinNames) {
                res+="    vamp:bin_names        (";

                unsigned int i;
                for (i=0; i+1 < o.binNames.size(); i++)
                    res+=" \""+o.binNames[i]+"\"";
                if (i < o.binNames.size())
                    res+=" \""+o.binNames[i]+"\"";
                res+=");\n";
            }
        }
       
        res+="    vamp:sample_type      vamp:VariableSampleRate ;\n";
        if (o.sampleRate > 0.0f)
            res+="    vamp:sample_rate      "+to_string(o.sampleRate)+" ;\n";
	        
    }

    //If we do not have SparseOutput, then we have DenseOutput. TrackLevelOutput can not be inferred from the plugin directly without actually
    //running the plugin.
    else{

        res=\
            "plugbase:"+plugin->getIdentifier()+"_output_"+o.identifier+" a  vamp:DenseOutput ;\n\
    vamp:identifier       \""+o.identifier+"\" ;\n\
    dc:title              \""+o.name+"\" ;\n\
    dc:description        \"\"\""+o.description+"\"\"\"  ;\n\
    vamp:fixed_bin_count  \""+(o.hasFixedBinCount == 1 ? "true" : "false")+"\" ;\n\
    vamp:unit             \""+(o.unit)+"\" ;\n";


        //another type of output           
        if(o.isQuantized){

            res+="    a                     vamp:QuantizedOutput ;\n";
            res+="    vamp:quantize_step    "+to_string(o.quantizeStep)+"  ;\n";    
        }
        
        //and yet another type
        if(o.hasKnownExtents){

            res+="    a                 vamp:KnownExtentsOutput ;\n";
            res+="    vamp:min_value    "+to_string(o.minValue)+"  ;\n"; 
            res+="    vamp:max_value    "+to_string(o.maxValue)+"  ;\n";    
        }

        // FIXME ? Bin names may vary based on plugin setup, so including them here might be misleading...
        if (o.hasFixedBinCount)
        {
            res+="    vamp:bin_count        "+to_string(o.binCount)+" ;\n";

            bool haveBinNames = false;
            for (unsigned int i=0; i < o.binNames.size(); i++) {
                if (o.binNames[i] != "") {
                    haveBinNames = true;
                    break;
                }
            }

            if (haveBinNames) {
                res+="    vamp:bin_names        (";

                unsigned int i;
                for (i=0; i+1 < o.binNames.size(); i++)
                    res+=" \""+o.binNames[i]+"\"";
                if (i < o.binNames.size())
                    res+=" \""+o.binNames[i]+"\"";
                res+=");\n";
            }
        }

        else if (o.sampleType == Plugin::OutputDescriptor::FixedSampleRate)
        {
            res+="    vamp:sample_type      vamp:FixedSampleRate ;\n";
            res+="    vamp:sample_rate      "+to_string(o.sampleRate)+" ;\n";
        }
        else if (o.sampleType == Plugin::OutputDescriptor::OneSamplePerStep)
            res+="    vamp:sample_type      vamp:OneSamplePerStep ;\n";
        else
        {
            cerr<<"Incomprehensible sampleType for output descriptor "+o.identifier<<" !"<<endl;
            exit(1);
        }
    }

    //There is no way to know this in advance, but we can use the km a bit for this.
    res+="#   vamp:computes_event_type   <Place event type URI here and uncomment> ;\n";
    res+="#   vamp:computes_feature      <Place feature attribute URI here and uncomment> ;\n";
    res+="#   vamp:computes_signal_type  <Place signal type URI here and uncomment> ;\n";
    res+="    .\n";

    return res;
}

string describe(vector<Plugin *> plugins, string pluginBundleBaseURI,
                string describerURI, string libname)
{
    string res = describe_namespaces(pluginBundleBaseURI, libname);
	
    res += describe_doc(describerURI, pluginBundleBaseURI, libname);
	
    res += describe_library(libname, plugins);

    for (size_t i = 0; i < plugins.size(); ++i) {

        Plugin *plugin = plugins[i];

        res += describe_plugin(plugin);
	
        Plugin::ParameterList params = plugin->getParameterDescriptors();
        for (Plugin::ParameterList::const_iterator i = params.begin(); i != params.end(); i++)
            res += describe_param(plugin, *i);
	
        Plugin::OutputList outputs = plugin->getOutputDescriptors();
        for (Plugin::OutputList::const_iterator i = outputs.begin(); i!= outputs.end(); i++)
            res += describe_output(plugin, *i);
    }
	
    return res;
}

int main(int argc, char **argv)
{
    if (argc < 3) usage();

    bool interactive = false;
    if (!strcmp(argv[1], "-i")) interactive = true;

    if (!interactive && argc < 3) usage();

    string pluginBundleBaseURI, describerURI;

    int argidx = 2;
	
    if (!interactive) {
        pluginBundleBaseURI = argv[1];
        if (!strcmp(argv[2], "-m")) {
            if (argc < 5) usage();
            describerURI = argv[3];
            argidx = 4;
        }
    } else {
        cerr << "Please enter the base URI for the plugin bundle : ";
        getline(cin, pluginBundleBaseURI);
        cerr << "Please enter your URI (empty to omit) : ";
        getline(cin, describerURI);
    }

    vector<Plugin *> plugins;
    string libname;

    PluginLoader *loader = PluginLoader::getInstance();

    while (argidx < argc) {

        string pluginName = argv[argidx];

        if (pluginName.substr(0, 5) == "vamp:") {
            pluginName = pluginName.substr(5);
        }

        string mylibname = pluginName.substr(0, pluginName.find(':'));

        if (libname == "") libname = mylibname;
        else if (libname != mylibname) {
            cerr << "ERROR: All plugins specified on command line must originate in the same library" << endl;
            exit(1);
        }

        if (mylibname == pluginName) { // pluginName is a library, not a plugin

            PluginLoader::PluginKeyList list = loader->listPlugins();
            for (size_t i = 0; i < list.size(); ++i) {
                string thislibname = list[i].substr(0, list[i].find(':'));
                if (thislibname != mylibname) continue;
                Plugin *plugin = loader->loadPlugin(list[i], 44100);
                if (!plugin) {
                    cerr << "ERROR: Plugin \"" << list[i] << "\" could not be loaded" << endl;
                    exit(1);
                }
                plugins.push_back(plugin);
            }

            if (plugins.empty()) {
                cerr << "ERROR: Plugin library \"" << mylibname << "\" does not exist, could not be opened, or contains no plugins" << endl;
                exit(1);
            }

        } else { // pluginName is a plugin

            Plugin *plugin = loader->loadPlugin(pluginName, size_t(44100));
            if (!plugin) {
                cerr << "ERROR: Plugin \"" << pluginName << "\" could not be loaded" << endl;
                exit(1);
            }
            plugins.push_back(plugin);
        }

        ++argidx;
    }
	
    cout << describe(plugins, pluginBundleBaseURI, describerURI, libname) << endl;

    return 0;
}


