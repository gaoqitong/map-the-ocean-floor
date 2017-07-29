clear all

global xBot;

BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;
%% Real

inputreal = zeros( 240000 , 100 );

for i =  1 : 100
    
        inputreal ( : , i ) = audioread(['bump_' , num2str( i ) , '_real_Pos', ...
            num2str( 1 ) , '_rts_Rd_1_Rr_1.wav']);
        
        readbty(['bump_' , num2str( i ), '_real'] , BotBTY ,depthB , rBox );
        
        inputrealbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
        
end


%% Blurred

inputblurred = zeros( 240000 , 100 );

for i =  1 : 100
    
%     inputblurred ( : , i ) = audioread(['bump_' , num2str( i ) , ...
%     '_blurred_Pos' , num2str(1), '_rts_Rd_1_Rr_1.wav']);

    readbty(['bump_' , num2str( i ), '_blurred'] , BotBTY ,depthB , rBox );
    
    inputblurredbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
    
end


%% Combine

audioinput = zeros ( 240000, 100 );
audioinput( : , : ) = inputreal;

fftinput = abs(fft(audioinput));

btyinput = zeros ( 64 , 100 );
btyinput( : , : ) = inputblurredbty;

target = zeros( 64 , 100 );
target ( : , : ) = inputrealbty;



