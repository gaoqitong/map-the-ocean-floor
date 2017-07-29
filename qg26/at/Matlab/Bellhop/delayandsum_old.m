clear all
v =[2,0]; %Receivermotionvector(vr,vz)inm/s
v =[0,0]; %Receivermotionvector(vr,vz)inm/s
c0 = 1500; % reference speed to convert v to proportional Doppler
name = 'bump_1';
ARRFIL = name;
fileroot = name
ARRFIL = [fileroot];
pcmfile = [fileroot '.pcm'];

STSFIL = 'source.wav'
[ stsTemp, sample_rate ] = audioread( STSFIL );
sts = stsTemp; %( 1 : 2 : 5*96000 ); % sub-sample down to 48000/s 
sample_rate = 48000;
sts = sts / max( abs( sts ) );
nts = length( sts );
if ( norm( v ) > 0 )
    disp( 'Setting up Dopplerized waveforms' )
    v_vec = linspace( min( v ), max( v ), 10 );
    v_vec = linspace( 1.9, 2, 51 );
    alpha_vec = 1 - v_vec / c0;
    nalpha = length(alpha_vec);
    sts_hilbert = zeros(nts, nalpha);
    for ialpha = 1 : length( alpha_vec )
        disp( ialpha )
        sts_hilbert( :, ialpha ) = hilbert( arbitrary_alpha( sts', alpha_vec( ialpha ) )' );
    end
else
    sts_hilbert = hilbert( sts );
end
c = 1537.0; % reduction velocity (should exceed fastest possible arrival) 
T = 5.0; % time window to capture
Narrmx = 100;
[ Arr, Pos ] = read_arrivals_bin( ARRFIL, Narrmx );
disp( 'Done reading arrivals' )
ir = 1;
isd = 1;
deltat = 1 / sample_rate;
nt = round(T / deltat);
rtsmat = zeros(nt, length(Pos.r.range));
for ird = 500:500
    for ir = 50:50
        disp( [ ird, ir ] )
        tstart = Pos.r.range( ir ) / c - 0.1; % min( delay( ir, :, ird ) ) 
        tend = tstart + T - deltat;
        time = tstart : deltat : tend;
        rts = zeros(nt, 1);
        for iarr = 1 : Arr.Narr( ir, ird, isd )
            Tarr = Arr.delay( ir, ird, iarr, isd ) - tstart;
            it1 = round(Tarr / deltat + 1);
            it2 = it1+nts-1;
            its1 = 1;
            its2 = nts;
            if (it1 < 1)
                its1=its1+(1-it1); 
                it1 =1;
            end
            if (it2 > nt)
                its2=its2-(it2 - nt); 
                it2 = nt;
            end
            if ( norm( v ) > 0 )
                theta_ray = Arr.RcvrAngle( ir, iarr, ird ) * pi / 180;
                tan_ray = [ cos( theta_ray ) sin( theta_ray ) ];
                alpha =1-dot(v/c0,tan_ray);
                ialpha =1+round((alpha-alpha_vec(1))/(alpha_vec( end ) - alpha_vec( 1 ) ) * ( length( alpha_vec ) - 1 ) );
                if ( ialpha < 1 || ialpha > nalpha )
                    disp( 'Doppler exceeds pre-tabulated values' ) 
                    ialpha = max( ialpha, 1 );
                    ialpha = min( ialpha, nalpha );
                end
                rts( it1 : it2 ) = rts( it1 : it2 ) + real( Arr.A( ir, ird,iarr ) * sts_hilbert( its1 : its2, ialpha ) ); 
            else
                rts( it1 : it2 ) = rts( it1 : it2 ) + real( Arr.A( ir, ird,iarr ) * sts_hilbert( its1 : its2, 1 ) ); 
            end
        end
    audiowrite( [ 'rts_Rd_' num2str( ird ) '_Rr_' num2str( ir ) '.wav' ], 0.95 * rts / max( abs( rts ) ) , sample_rate);
    rtsmat( :, ir ) = rts; 
    end
    eval( [ ' save ' fileroot '_Rd_' num2str( ird ) ' rtsmat Pos sample_rate' ] );
end