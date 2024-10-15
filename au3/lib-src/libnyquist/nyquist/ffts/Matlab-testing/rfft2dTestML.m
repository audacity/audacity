% program to test 2d real fft

% let user select file then open it
[fname, pname] = uigetfile('*.dr2', 'select conv file');
cd(pname);
fidout=fopen(fname,'r');

% read header info
N=fread(fidout,1,'long');
M=fread(fidout,1,'long');
% read in data
%status=fseek(fidout,Nheader,'bof');
a=fread(fidout,N*M,'float');
a=reshape(a,N,M);
c=fread(fidout,N*M,'float');
c=reshape(c,N,M);
c=c(1:2:N,:)+j*c(2:2:N,:);
fclose(fidout);

c2=fft2(a);

% Remember Matlab is column major order, C is row major order
% Matlab starts with index of 1, f and k start at 0

%%%% check the real transforms of DC and Nyquest frequencies
% check the four real values
maxerr=abs(real(c(1,1))-c2(1,1));					% 0 f, 0 k
maxerr=max(maxerr,abs(imag(c(1,1))-c2(1,M/2+1)));	% 0 f, M/2 k
maxerr=max(maxerr,abs(real(c(1,M/2+1))-c2(N/2+1,1)));% N/2 f, 0 k
maxerr=max(maxerr,abs(imag(c(1,M/2+1))-c2(N/2+1,M/2+1)));% N/2 f, M/2 k
%check the rest of the pos wavenumbers of DC and Nyquest frequencies
maxerr=max(maxerr,max(abs(c(1,2:M/2)-c2(1,2:M/2))));	%f = 0, k=1 to M/2-1
maxerr=max(maxerr,max(abs(c(1,M/2+2:M)-c2(N/2+1,2:M/2))));%f = N/2, k=1 to M/2-1
%%%%

% check all the other positive frequencies at all wavenumbers
	% f from 1 to N/2-1, k from 0 to M-1 (wraps around through negative k)
maxerr=max(maxerr,max(max(abs(c(2:N/2,:)-c2(2:N/2,:)))))

