function [ bty_matrix ] = extractbty( filepath )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

global xBot;
BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;

readbty(filepath , BotBTY ,depthB , rBox );
bty_matrix = xBot( 2 , : )';

end

