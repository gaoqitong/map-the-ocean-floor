
%% 
% Initializing;
filename = 'flat';
load index_July21st_20ErrFcs.mat
% for i = 1 : 1000
%     
%     addpath(num2str(i)); 
%     
% end

global xBot;
BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;

max_comparision = 900;

%% Read Top 10
[~, err_amplitude_mse_index] = sort( err_amplitude_mse );
[~, err_angle_mse_index] = sort( err_angle_mse );
[~, err_amplitude_xcorr_index] = sort( err_amplitude_xcorr, 'descend' );
[~, err_angle_xcorr_index] = sort( err_angle_xcorr, 'descend' );
[~, err_amplitude_cov_index] = sort( err_amplitude_cov, 'descend' );
[~, err_angle_cov_index] = sort( err_angle_cov, 'descend' );
[~, err_amplitude_sob_index] = sort( err_amplitude_sob );
[~, err_angle_sob_index] = sort( err_angle_sob );
[~, err_amplitude_corrcoeff_index] = sort( err_amplitude_corrcoeff, 'descend' );
[~, err_angle_corrcoeff_index] = sort( err_angle_corrcoeff, 'descend' );

amplitude_mse = zeros(44, max_comparision);
angle_mse = zeros(44, max_comparision);
amplitude_xcorr = zeros(44, max_comparision);
angle_xcorr = zeros(44, max_comparision);
amplitude_cov = zeros(44, max_comparision);
angle_cov = zeros(44, max_comparision);
amplitude_sob = zeros(44, max_comparision);
angle_sob = zeros(44, max_comparision);
amplitude_corrcoeff = zeros(44, max_comparision);
angle_corrcoeff = zeros(44, max_comparision);

for i = 1 : max_comparision
%     
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_mse_index(i) ), '/' , filename, '_m_', ...
        num2str( ( err_amplitude_mse_index(i) - 1 )*100 ...
        + index_amplitude_mse( err_amplitude_mse_index(i), 1) ), '_', ...
        num2str( index_amplitude_mse(err_amplitude_mse_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    amplitude_mse(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_mse_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_mse_index(i) - 1 )*100 ...
        + index_angle_mse( err_angle_mse_index(i), 1) ), '_',...
        num2str( index_angle_mse(err_angle_mse_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    angle_mse(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_xcorr_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_xcorr_index(i) - 1 )*100 + ...
        index_amplitude_xcorr( err_amplitude_xcorr_index(i), 1) ),...
        '_', num2str( index_amplitude_xcorr(err_amplitude_xcorr_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    amplitude_xcorr(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_xcorr_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_xcorr_index(i) - 1 )*100 + ...
        index_angle_xcorr( err_angle_xcorr_index(i), 1) ),...
        '_', num2str( index_angle_xcorr(err_angle_xcorr_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    angle_xcorr(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_cov_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_cov_index(i) - 1 )*100 + ...
        index_amplitude_cov( err_amplitude_cov_index(i), 1) ),...
        '_', num2str( index_amplitude_cov(err_amplitude_cov_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    amplitude_cov(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_cov_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_cov_index(i) - 1 )*100 + ...
        index_angle_cov( err_angle_cov_index(i), 1) ),...
        '_', num2str( index_angle_cov(err_angle_cov_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    angle_cov(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_sob_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_sob_index(i) - 1 )*100 + ...
        index_amplitude_sob( err_amplitude_sob_index(i), 1) ),...
        '_', num2str( index_amplitude_sob(err_amplitude_sob_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    amplitude_sob(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_sob_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_sob_index(i) - 1 )*100 + ...
        index_angle_sob( err_angle_sob_index(i), 1) ),...
        '_', num2str( index_angle_sob(err_angle_sob_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    angle_sob(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_corrcoeff_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_corrcoeff_index(i) - 1 )*100 + ...
        index_amplitude_corrcoeff( err_amplitude_corrcoeff_index(i), 1) ),...
        '_', num2str( index_amplitude_corrcoeff(err_amplitude_corrcoeff_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    amplitude_corrcoeff(:, i) = xBot( 2 , : )';
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_corrcoeff_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_corrcoeff_index(i) - 1 )*100 + ...
        index_angle_corrcoeff( err_angle_corrcoeff_index(i), 1) ),...
        '_', num2str( index_angle_corrcoeff(err_angle_corrcoeff_index(i), 2) )] ;
    readbty(filepath , BotBTY ,depthB , rBox );
    angle_corrcoeff(:, i) = xBot( 2 , : )';
    
    
end

close all

readbty('flat_r', BotBTY ,depthB , rBox);
real = xBot(2, :)';

%% Analysis

corr_amplitude_mse = zeros(1, max_comparision);
corr_angle_mse = zeros(1, max_comparision);
corr_amplitude_xcorr = zeros(1, max_comparision);
corr_angle_xcorr = zeros(1, max_comparision);
corr_amplitude_cov = zeros(1, max_comparision);
corr_angle_cov = zeros(1, max_comparision);
corr_amplitude_sob = zeros(1, max_comparision);
corr_angle_sob = zeros(1, max_comparision);
corr_amplitude_corrcoeff = zeros(1, max_comparision);
corr_angle_corrcoeff = zeros(1, max_comparision);

parfor i = 1 : max_comparision
    temp = corrcoef(amplitude_mse(:, i), real);
    corr_amplitude_mse(i) = temp(1, 2);
    
    temp = corrcoef(angle_mse(:, i), real);
    corr_angle_mse(i) = temp(1, 2);
    
    temp = corrcoef(amplitude_xcorr(:, i), real);
    corr_amplitude_xcorr(i) = temp(1, 2);
    
    temp = corrcoef(angle_xcorr(:, i), real);
    corr_angle_xcorr(i) = temp(1, 2);
    
    temp = corrcoef(amplitude_cov(:, i), real);
    corr_amplitude_cov(i) = temp(1, 2);
    
    temp = corrcoef(angle_cov(:, i), real);
    corr_angle_cov(i) = temp(1, 2);
    
    temp = corrcoef(amplitude_sob(:, i), real);
    corr_amplitude_sob(i) = temp(1, 2);
    
    temp = corrcoef(angle_sob(:, i), real);
    corr_angle_sob(i) = temp(1, 2);
    
    temp = corrcoef(amplitude_corrcoeff(:, i), real);
    corr_amplitude_corrcoeff(i) = temp(1, 2);
    
    temp = corrcoef(angle_corrcoeff(:, i), real);
    corr_angle_corrcoeff(i) = temp(1, 2);
       
end

