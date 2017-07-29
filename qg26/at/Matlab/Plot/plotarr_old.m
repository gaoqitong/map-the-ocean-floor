function plotarr( ARRFIL, irr, ird, isd )

Narrmx = 100;
[ Arr, Pos ] = read_arrivals_bin( ARRFIL, Narrmx );
figure
Narr = Arr.Narr( ird, irr, isd );
tempdelay = Arr.delay( ird, irr, 1:Narr, isd ); 
goodDelay = transpose(squeeze(tempdelay(1,:,:))); 
tempa = Arr.A( ird, irr, 1:Narr, isd );
goodA = abs(transpose(squeeze(tempa(1,:,:))));
ird;
irr;
isd;
stem( goodDelay, goodA )
xlabel( 'Time (s)' )
ylabel( 'Amplitude' )
title( [ 'Sd = ', num2str( Pos.s.depth( isd ) ), ...
' m Rd = ', num2str( Pos.r.depth( ird ) ), ...
'm Rr=',num2str(Pos.r.range(irr)),'m'])
end