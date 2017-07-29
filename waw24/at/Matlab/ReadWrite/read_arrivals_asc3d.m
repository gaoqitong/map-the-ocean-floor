function [ Arr, Pos ] = read_arrivals_asc3d( ARRFile, Narrmx )

% Read the arrival time/amplitude data computed by BELLHOP
% Revamped by William Willis, July 2017
%
% usage:
%[ Arr, Pos ] = read_arrivals_asc3d( ARRFile, Narrmx );
%
% Arr is a structure containing all the arrivals information
% Pos is a structure containing the positions of source and receivers
%
% ARRFile is the name of the Arrivals File
% Narrmx is the maximum number of arrivals allowed
% mbp 9/96

if nargin == 1
   Narrmx = 200;
end

fid = fopen( ARRFile, 'r');	% open the file
if ( fid == -1 )
   error( 'read_arrivals_asc: Arrivals file cannot be opened' )
end

% read the header info

freq = fscanf( fid, '%f',  1  );

Nsd    = fscanf( fid, '%i',  1  ); % number of source   depths
Nrd    = fscanf( fid, '%i',  1  ); % number of receiver depths
Nrr    = fscanf( fid, '%i',  1  ); % number of receiver ranges
Ntheta = fscanf( fid, '%i',  1  ); % number of receiver bearings

Pos.s.depth   = fscanf( fid, '%f', Nsd );   % source   depths
Pos.r.depth   = fscanf( fid, '%f', Nrd );   % receiver depths
Pos.r.range   = fscanf( fid, '%f', Nrr );   % receiver ranges
Pos.r.theta   = fscanf( fid, '%f', Ntheta);   % receiver bearings

% loop to read all the arrival info (delay and amplitude)

Arr.Narr      = zeros( Ntheta, Nrr, Nrd, Nsd)
Arr.A         = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );
Arr.delay     = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );
Arr.SrcAngle  = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );
Arr.RcvrAngle = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );
Arr.NumTopBnc = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );
Arr.NumBotBnc = zeros( Ntheta, Nrr, Narrmx, Nrd, Nsd );

for isd = 1:Nsd
   Narrmx2 = fscanf( fid, '%i', 1 );  % max. number of arrivals to follow
   disp( [ 'Max. number of arrivals for source index ', num2str( isd ), ' is ', num2str( Narrmx2 ) ] );
   for itheta = 1:Ntheta
    for ird = 1:Nrd
      for ir = 1:Nrr
         Narr = fscanf( fid, '%i', 1 );	% number of arrivals
         Arr.Narr( itheta, ir, ird, isd ) = Narr;
         
         if Narr > 0   % do we have any arrivals?
            da = zeros(8,Narr);
            for arrloop=1:Narr
                da(1:2, arrloop) = fscanf( fid, '%f', [2 1]);
                da(3:4, arrloop) = fscanf( fid, ' (  %f    ,  %f    )',[2 1]);
                da(5:8, arrloop) = fscanf(fid, '%f', [4 1]);
            end
            % da = fscanf( fid, '%f', [ 8, Narr ] ); % does not work due to
            % parentheses in arr file around delay
            Narr = min( Narr, Narrmx ); % we'll keep no more than Narrmx values
            Arr.Narr( itheta, ir, ird, isd ) = Narr;
            Arr.A(         itheta, ir, 1:Narr, ird, isd ) = da( 1, 1:Narr ) .* exp( 1i * da( 2, 1:Narr ) * pi/180);
            Arr.delay(     itheta, ir, 1:Narr, ird, isd ) = da( 3, 1:Narr ) + 1i * da( 4, 1:Narr );
            Arr.SrcAngle(  itheta, ir, 1:Narr, ird, isd ) = da( 5, 1:Narr );
            Arr.RcvrAngle( itheta, ir, 1:Narr, ird, isd ) = da( 6, 1:Narr );
            Arr.NumTopBnc( itheta, ir, 1:Narr, ird, isd ) = da( 7, 1:Narr );
            Arr.NumBotBnc( itheta, ir, 1:Narr, ird, isd ) = da( 8, 1:Narr );
         end
      end		% next receiver range
   end		% next receiver depth
   end      % next receiver bearing
end	% next source depth

fclose( fid );
