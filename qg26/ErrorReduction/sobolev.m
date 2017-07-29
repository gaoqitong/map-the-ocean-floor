function [ soblev_norm ] = sobolev( real, test )
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

soblev_norm = norm(gradient(real-test))/(norm(gradient(real))+norm(gradient(test)));

end

