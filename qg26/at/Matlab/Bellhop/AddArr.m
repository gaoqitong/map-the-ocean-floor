function AddArr( omega, id, ir, amp, delay, angle, MxNarr )
global Arr
PhaseTol = 0.5;
Nt = Arr.Narr( id, ir );
NewRay = 1;
if ( NewRay )
    if ( Nt >= MxNarr )
        [ ~, iArr ] = min( Arr.A( id, ir, : ) );
        iArr_rand = random('unid',length(iArr),1);
        if ( amp > Arr.A( id, ir, iArr_rand ) )
            Arr.A( id, ir, iArr_rand ) = amp; 
            Arr.delay(id,ir,iArr_rand)=delay; 
            Arr.angle(id,ir,iArr_rand)=angle; 
        end
    else
        Arr.Narr( id, ir ) = Nt + 1;
        for abc = 1:length(Nt)
            Arr.A(id, ir, Nt(abc) + 1 ) = amp;
            Arr.delay(id, ir, Nt(abc) + 1 ) = delay;
            Arr.angle(id, ir, Nt(abc) + 1 ) = angle;
        end
    end
else
    ampTot = Arr.A( id, ir, Nt ) + amp;
    Arr.A( id, ir, Nt ) = Arr.A( id, ir, Nt ) + amp;
    Arr.delay( id, ir, Nt ) = ( Arr.A( id, ir, Nt ) * Arr.delay( id, ir, Nt ) ...
        + amp * delay ) / ampTot;
    Arr.ang( id, ir, Nt ) = angle;
end
end
