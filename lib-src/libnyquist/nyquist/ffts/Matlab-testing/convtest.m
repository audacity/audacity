% program to test 1d real fast conv
clear c2

% let user select file then open it
[fname, pname] = uigetfile('*.cnv', 'select conv file');
cd(pname);
fidout=fopen(fname,'r');

% read header info
aN=fread(fidout,1,'long');
bN=fread(fidout,1,'long');
M=fread(fidout,1,'long');
% read in data
%status=fseek(fidout,Nheader,'bof');
a=fread(fidout,aN*M,'float');
a=reshape(a,aN,M);
b=fread(fidout,bN*M,'float');
b=reshape(b,bN,M);
c=fread(fidout,(aN+bN-1)*M,'float');
c=reshape(c,(aN+bN-1),M);
fclose(fidout);

for i1=1:M;
	c2(:,i1)=conv(a(:,i1),b(:,i1));
end;
max(max(abs(c2-c)))
