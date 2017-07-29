clear all

global xBot;

BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;
%% Real

inputreal = zeros( 240000 , 100 );

k = 1;

for i =  1 : 100
    
        inputreal ( : , i ) = audioread(['bump_' , num2str( k ) , '_real_Pos', ...
            num2str( 1 ) , '_rts_Rd_1_Rr_1.wav']);
        
        readbty(['bump_' , num2str( k ), '_real'] , BotBTY ,depthB , rBox );
        
        inputrealbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
        
        k = k + 1;
        
end


%% Blurred

inputblurred = zeros( 240000 , 100 );

k = 1;

for i =  1 : 100
    
    inputblurred ( : , i ) = audioread(['bump_' , num2str( i ) , ...
    '_blurred_Pos' , num2str(1), '_rts_Rd_1_Rr_1.wav']);

    readbty(['bump_' , num2str( k ), '_blurred'] , BotBTY ,depthB , rBox );
    
    inputblurredbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
    
    k = k + 1;
    
end


%% Combine

audioinput = zeros ( 240000, 80 );
audioinput( : , : ) = inputreal( : , 1:80 ) - inputblurred( : , 1:80 );

% fftinput = abs( fft(audioinput) );

btyinput = zeros ( 64 , 80 );
btyinput( : , : ) = inputrealbty( : , 1:80 ) - inputblurredbty( : , 1:80 );

target = zeros( 64 , 80 );
target ( : , : ) = btyinput;

audiotest = inputreal( : , 81:100 ) - inputblurred( : , 81:100 );
btytest = inputrealbty( : , 81:100 ) - inputblurredbty( : , 81:100 );


