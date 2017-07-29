function drawonevalley( filename, center_1, width_1 , ifdrawblur , ifplot )
%% Setting Default Value

if nargin < 4
     ifdrawblur = 0;
end

if nargin < 5
     ifplot = 0;
end

%% Start

f1 = @( x ) ( x - center_1 ).^2;
x1 = ( center_1 - width_1 / 2 ) : 0.1 : ( center_1 + width_1 / 2 );
y1 = - f1( x1 ) * 1000;
height_1 = 500 - min( y1 );
y1 = height_1 + y1;
x1y1 = [ x1; y1 ];

x01 = -3 : 0.1 : x1( 1 );
y01 = zeros( 1, size(x01, 2) );
y01( : ) = 500;
x01y01 = [x01; y01];

x12 = x1( end ) : 0.1 : 3;
y12 = zeros( 1, size(x12, 2) );
y12( : ) = 500;
x12y12 = [ x12 ; y12 ];

xy = [ x01 x1 x12 ; y01 y1 y12 ];

xy( 1, : ) = xy( 1, : ) + rand( 1, size( xy, 2 ) ) ./ 5;
xy( 2, : ) = xy( 2, : ) + rand( 1, size( xy, 2 ) ) .* 20;


%% Draw the Blurred Bathymetry
if ifdrawblur == 1

    dlmwrite([ filename '_blurred.bty' ], '''L''', '');
    fid = fopen([ filename '_blurred.bty' ], 'a');
    fprintf(fid, '%g\n', size(x1, 2) + size(x01, 2) + size(x12, 2) );
    fprintf(fid, '%5.2f %5.2f\n', xy);
    
end

%% Draw the Real Bathyemtry

xy( 1, : ) = xy( 1, : ) + rand( 1, size( xy, 2 ) ) ./ 3.7;
xy( 2, : ) = xy( 2, : ) + rand( 1, size( xy, 2 ) ) .* 28;

if ifdrawblur == 1

dlmwrite([ filename '_real.bty' ], '''L''', '');
fid = fopen([ filename '_real.bty' ], 'a');
fprintf(fid, '%g\n', size(x1, 2) + size(x01, 2) + size(x12, 2) );
fprintf(fid, '%5.2f %5.2f\n', xy);

end

if ifdrawblur == 0
    
    dlmwrite([ filename '.bty' ], '''L''', '');
    fid = fopen([ filename '.bty' ], 'a');
    fprintf(fid, '%g\n', size(x1, 2) + size(x01, 2) + size(x12, 2) );
    fprintf(fid, '%5.2f %5.2f\n', xy);
    
end

%% Plot

if ifplot == 1
    
    if ifdrawblur == 1

        plotbty([ filename '_blurred.bty' ])
        title( 'Blurred' )
        
        plotbty([ filename '_real.bty' ])
        title( 'Real' )
        
    end
    
    plotbty([ filename '.bty' ])
    title( 'Real' )
    

end

end

