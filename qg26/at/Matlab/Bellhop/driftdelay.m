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

bellhopMDrift( filename, drift, NumDepths, Init, MaxDepth)
for k=1:10; % Number of Positions
    delayandsum(strcat(filename,'_Pos',num2str(k)),'source',4800)
end