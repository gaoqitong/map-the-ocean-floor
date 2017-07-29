function driftdelay( filename, drift, NumDepths, Init, MaxDepth)

% Runs bellhopMDrift and delayandsum for a file, env file must have run
% type A. Might save some time. 

% Default Values
if ( nargin<5 ) 
    MaxDepth=900;
end
if (nargin<4)
    Init=100;
end
if (nargin<3)
    NumDepths=10;
end
if (nargin<2)
    drift=100;
end

bellhopMDrift(filename, drift, NumDepths, Init, MaxDepth)

r=0:drift:drift*NumDepths;
if drift==0;
    r=zeros(1,NumDepths);
end

d=linspace(Init,MaxDepth,NumDepths);

for k=1:NumDepths; % Number of Positions
    delayandsum(strcat('Pos',num2str(k),'-d',num2str(d(k)),'-r',num2str(r(k)),...
        '-',filename),'Chirp1s',48000)
end