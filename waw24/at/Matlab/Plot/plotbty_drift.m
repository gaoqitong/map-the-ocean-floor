function varargout = plotbty_drift( btyfil, drift)

% Plot the bathymetry file
% usage: plotbty_drift( btyfil, drift )
% where btyfil is the BaThYmetry file (the extension is optional)
% e.g. plotbty( 'foofoo' )
%
% plots the BaThymetrY file used by Bellhop
% MBP July 1999

global xBot NbtyPts
global units

BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;
readbty_drift( btyfil, BotBTY, depthB, rBox, drift) % read the bathymetry data

% copy, removing +/- inf values
PlotNbtyPts = NbtyPts - 2;
r = xBot( 1, 2 : end - 1 );
z = xBot( 2, 2 : end - 1 );

% set labels in m or km
xlab     = 'Range (m)';
if ( strcmp( units, 'km' ) )
  r      = r / 1000.0;
  xlab   = 'Range (km)';
end
%%

hold on
xlabel( xlab )
ylabel( 'Depth (m)' )

% close the polygon
zmax      = max( z );
thickness = ( max( z ) - min( z ) );
thickness = max( thickness, 1 );   % make sure there's some thickness

r( PlotNbtyPts + 1 ) = r( PlotNbtyPts );
z( PlotNbtyPts + 1 ) = zmax + thickness;
r( PlotNbtyPts + 2 ) = r( 1 );
z( PlotNbtyPts + 2 ) = zmax + thickness;

earthbrown = [ 0.5 0.3 0.1 ];
h          = fill( r, z, earthbrown );

set( gca, 'YDir', 'Reverse' )   % plot with depth-axis positive down

if ( nargout == 1 )
   varargout( 1 ) = { h };   % return a handle to the figure
end

