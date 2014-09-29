% program to test 2d real fast conv

% let user select file then open it
[fname, pname] = uigetfile('*.c2d', 'select conv file');
cd(pname);
fidout=fopen(fname,'r');

% read header info
aN=fread(fidout,1,'long');
aM=fread(fidout,1,'long');
bN=fread(fidout,1,'long');
bM=fread(fidout,1,'long');
% read in data
%status=fseek(fidout,Nheader,'bof');
a=fread(fidout,aN*aM,'float');
a=reshape(a,aN,aM);
b=fread(fidout,bN*bM,'float');
b=reshape(b,bN,bM);
c=fread(fidout,(aN+bN-1)*(aM+bM-1),'float');
c=reshape(c,(aN+bN-1),(aM+bM-1));
fclose(fidout);

c2=conv2(a,b);

max(max(abs(c2-c)))
