function [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx )
% Read the arrival time/amplitude data computed by BELLHOP %
% usage:
%[ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
%
% Arr is a structure containing all the arrivals information
% Pos is a structure containing the positions of source and receivers %
% ARRFile is the name of the Arrivals File
% Narrmx is the maximum number of arrivals allowed
% mbp 9/96
% Vaishakhi sent this to us 7 Feb

if nargin == 1 
    Narrmx = 50;
end

fid = fopen( [ ARRFile, '.arr.mat'], 'r');
fseek( fid, 4, -1 );
load([ ARRFile, '.arr.mat'])
freq = fread( fid, 1, 'float' );
Nsd = Pos.Nsd;
Nrd = Pos.Nrd;
Nrr = Pos.Nrr;
fseek(fid,8,0);

for isd = 1:Nsd
    Narrmx2 = MxNarr;
    
    for ird = 1 : Nrd
        for ir = 1 : Nrr
            Narr = Arr.Narr( ird, ir, isd);
            if Narr > 0
                da = fread( fid, [ 10, Narr ], 'float');
                Narr1 = min( Narr, Narrmx );
                Arr.Narr( ird, ir, isd ) = Narr;
            end
        end
    end
    
end
fclose( fid );
end
