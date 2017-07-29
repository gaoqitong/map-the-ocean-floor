function [goodDelay, goodA]=plotarr3d( ARRFIL, itheta, irr, ird, isd )

% plot the arrivals calculated by BELLHOP
%
% usage:
% plotarr( filename, irr, ird, isd )
% where:
% irr = index of receiver range
% ird = index of receiver depth
% itheta = index of receiver bearing
% isd = index of source   depth
%
% mbp, April 2009
% William Willis, July 2017

% read

Narrmx = 1000;
 [ Arr, Pos ] = read_arrivals_asc3d( ARRFIL, Narrmx );
% [ Arr, Pos ] = read_arrivals_bin3d( ARRFIL, Narrmx ); In Development
size(Arr.Narr);
disp(['Number of arrivals is ',num2str(Arr.Narr(itheta,irr,ird,isd)),' for receiver ',...
    num2str(itheta),' ',num2str(irr),' ',...
    num2str(ird),' and source ',num2str(isd)])
% stem plot for a single receiver
figure
Narr = Arr.Narr( itheta, irr, ird, isd );
size(Arr.delay);
tempdelay = Arr.delay( itheta, irr, 1:Narr, ird, isd );
goodDelay = transpose(squeeze(tempdelay(1,:,:)));
tempa = Arr.A( itheta, irr, 1:Narr, ird, isd );
goodA = abs(transpose(squeeze(tempa(1,:,:))));

stem( goodDelay, goodA )
xlabel( 'Time (s)' )
ylabel( 'Amplitude' )
title( [ 'Sd = ', num2str( Pos.s.depth( isd ) ), ...
   ' m    Rd = ', num2str( Pos.r.depth( ird ) ), ...
   ' m    Rr = ', num2str( Pos.r.range( irr ) ), ' m    Rtheta = ',...
   num2str( Pos.r.theta( itheta ))] )

% % depth-time stem plot
% figure
% for ird1 = 1 : size( Arr.A, 1 )
%     ird1;
%    Narr = Arr.Narr( ird1, irr, isd );
%    stem3( Arr.delay( irr, 1:Narr, ird1, isd ), Pos.r.depth( ird1 ) * ones( length( Arr.delay( irr, 1:Narr, ird1, isd ) ), 1 ), ...
%        abs( Arr.A( irr, 1:Narr, ird1, isd ) ) )
% hold on
% end
% 
% xlabel( 'Time (s)' )
% ylabel( 'Depth (m)' )
% title( [ 'Sd = ', num2str( Pos.s.depth( isd ) ), ' m    Rr = ', num2str( Pos.r.range( irr ) ), ' m' ] )
% 
% % range-time stem plot
% figure
% for irr = 1 : size( Arr.A, 1 )
%    Narr = Arr.Narr( ird, irr, isd );
%    stem3( Arr.delay( ird, irr,1:Narr ), Pos.r.range( irr ) * ones( length( Arr.delay( ird, irr,1:Narr ) ), 1 ), ...
%        abs( Arr.A(ird, irr,1:Narr) ) )
% hold on
% end
% 
% xlabel( 'Time (s)' )
% ylabel( 'Range (m)' )
% title( [ 'Sd = ', num2str( Pos.s.depth( isd ) ), ' m    Rd = ', num2str( Pos.r.depth( ird ) ), ' m' ]