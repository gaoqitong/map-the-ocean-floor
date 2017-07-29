Initializing;
sizeofreal = 42;
real = load('real.mat');
filename = 'flat';

%% Create Folders

% for i = 1 : 1000
%     
%    mkdir([num2str(i)]); 
%     
% end


%% Generate btys&envs

% merged = real.real;
% merged = merged';
% parfor i = 1 : 100000
% 
%     noise = perlin2d( sizeofreal ); 
%     gen_files( filename, merged, sizeofreal, noise, i );
% 
% end

%% Run Bellhop

% parfor i = 1 : 100000
%     
%     run_bellhop(filename, i);
%     
% end

%% Generate Audio Files

% parfor i = 1 : 100000
%     
%     run_delayandsum(filename, i)
%     
% end

%% Analysis

target = audioread( 'flat_r_Pos1_rts_Rd_1_Rr_1.wav' );
index_amplitude_mse = zeros(1000, 2);
index_angle_mse = zeros(1000, 2);
index_amplitude_xcorr = zeros(1000, 2);
index_angle_xcorr = zeros(1000, 2);
index_amplitude_cov = zeros(1000, 2);
index_angle_cov = zeros(1000, 2);
index_amplitude_sob = zeros(1000, 2);
index_angle_sob = zeros(1000, 2);
index_amplitude_corrcoeff = zeros(1000, 2);
index_angle_corrcoeff = zeros(1000, 2);

err_amplitude_mse = zeros(1000, 1);
err_angle_mse = zeros(1000, 1);
err_amplitude_xcorr = zeros(1000, 1);
err_angle_xcorr = zeros(1000, 1);
err_amplitude_cov = zeros(1000, 1);
err_angle_cov = zeros(1000, 1);
err_amplitude_sob = zeros(1000, 1);
err_angle_sob = zeros(1000, 1);
err_amplitude_corrcoeff = zeros(1000, 1);
err_angle_corrcoeff = zeros(1000, 1);

parfor i = 1 : 1000
    
    [ row_amplitude_mse, col_amplitude_mse, row_angle_mse, col_angle_mse,...
    row_amplitude_xcorr, col_amplitude_xcorr, row_angle_xcorr, col_angle_xcorr, ...
    row_amplitude_cov, col_amplitude_cov, row_angle_cov, col_angle_cov, ...
    row_amplitude_sob, col_amplitude_sob, row_angle_sob, col_angle_sob, ...
    row_amplitude_corrcoeff, col_amplitude_corrcoeff, row_angle_corrcoeff, col_angle_corrcoeff,...
    error_amplitude_mse, error_angle_mse, error_amplitude_xcorr, error_angle_xcorr, ...
    error_amplitude_cov, error_angle_cov, error_amplitude_sob, error_angle_sob, ...
    error_amplitude_corrcoeff, error_angle_corrcoeff ] = ...
    analyze_each_folder( filename , i, target )

    index_amplitude_mse(i, :) = [row_amplitude_mse col_amplitude_mse];
    err_amplitude_mse(i) = error_amplitude_mse;
    index_angle_mse(i, :) = [row_angle_mse col_angle_mse];
    err_angle_mse(i) = error_angle_mse;
    index_amplitude_xcorr(i, :) = [row_amplitude_xcorr col_amplitude_xcorr];
    err_amplitude_xcorr(i) = error_amplitude_xcorr;
    index_angle_xcorr(i, :) = [row_angle_xcorr col_angle_xcorr];
    err_angle_xcorr(i) = error_angle_xcorr;
    index_amplitude_cov(i, :) = [row_amplitude_cov col_amplitude_cov];
    err_amplitude_cov(i) = error_amplitude_cov;
    index_angle_cov(i, :) = [row_angle_cov col_angle_cov];
    err_angle_cov(i) = error_angle_cov;
    index_amplitude_sob(i, :) = [row_amplitude_sob col_amplitude_sob];
    err_amplitude_sob(i) = error_amplitude_sob;
    index_angle_sob(i, :) = [row_angle_sob col_angle_sob];
    err_angle_sob(i) = error_angle_sob;
    index_amplitude_corrcoeff(i, :) = [row_amplitude_corrcoeff col_amplitude_corrcoeff];
    err_amplitude_corrcoeff(i) = error_amplitude_corrcoeff;
    index_angle_corrcoeff(i, :) = [row_angle_corrcoeff col_angle_corrcoeff];
    err_angle_corrcoeff(i) = error_angle_corrcoeff;
    
end
save('index_July21st_20ErrFcs.mat','index_amplitude_mse','err_amplitude_mse',...
    'index_angle_mse','err_angle_mse', 'index_amplitude_xcorr', 'err_amplitude_xcorr',...
    'index_angle_xcorr', 'err_angle_xcorr', 'index_amplitude_cov', 'err_amplitude_cov', ...
    'index_angle_cov', 'err_angle_cov', 'index_amplitude_sob', 'err_amplitude_sob', ...
    'index_angle_sob', 'err_angle_sob', 'index_amplitude_corrcoeff', 'err_amplitude_corrcoeff', ...
    'index_angle_corrcoeff', 'err_angle_corrcoeff');

 
