clear all
close all
clc
%% Add Path
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/ReadWrite/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/tests/block/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/Plot/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/triangle/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/valley/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/quadratic_valley/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/mtriangles/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/Simple_Structure/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/Arr/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/Bellhop/')
addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/NN/')

addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/at/Matlab/btyenv/bumps/No Blurred Version/')
% addpath('~/Library/Mobile Documents/com~apple~CloudDocs/Data+/No Blurred Version/')

%% Begin Tests
% close all
% clc
% bellhopM test
% plotarr('test', 250, 25, 1)

% plotbty 'blockB_ray'   % superimpose a bathymetry plot
% figure
% plotray test


%% 
% 
% % TL: Geometric ray theory
% bellhopM blockB_geo
% plotshd( 'blockB_geo.shd.mat', 2, 1, 1 )
% plotbty 'blockB_geo'   % superimpose a bathymetry plot
% 
% % TL: Gaussian beams
% bellhopM 'blockB_gb'
% 
% plotshd( 'blockB_gb.shd.mat', 2, 1, 2 )
% plotbty 'blockB_gb'  % superimpose a bathymetry plot
