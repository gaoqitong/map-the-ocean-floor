clear all

global xBot;

BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;
%% Real

inputaudio = zeros( 240000 , 200 );
inputbty = zeros( 64 , 200 );

for i =  1 : 100
    
        inputaudio ( : , i ) = audioread(['bump_' , num2str( i ) , '_Pos', ...
            num2str( 1 ) , '_rts_Rd_1_Rr_1.wav']);
        
        readbty(['bump_' , num2str( i )] , BotBTY ,depthB , rBox );
        
        inputbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
        
end

for i = 101 : 200
   
        inputaudio ( : , i ) = audioread(['valley_' , num2str( i - 100 ) , '_Pos', ...
            num2str( 1 ) , '_rts_Rd_1_Rr_1.wav']);
        
        readbty(['valley_' , num2str( i - 100 )] , BotBTY ,depthB , rBox );
        
        inputbty( 1:64 , i ) = xBot ( 2 , 1:64 )' ;
        
end



%% Combine


fftinput = abs(fft(inputaudio));






