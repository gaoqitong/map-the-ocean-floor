function [ row_amplitude_mse, col_amplitude_mse, row_angle_mse, col_angle_mse,...
    row_amplitude_xcorr, col_amplitude_xcorr, row_angle_xcorr, col_angle_xcorr, ...
    row_amplitude_cov, col_amplitude_cov, row_angle_cov, col_angle_cov, ...
    row_amplitude_sob, col_amplitude_sob, row_angle_sob, col_angle_sob, ...
    row_amplitude_corrcoeff, col_amplitude_corrcoeff, row_angle_corrcoeff, col_angle_corrcoeff,...
    error_amplitude_mse, error_angle_mse, error_amplitude_xcorr, error_angle_xcorr, ...
    error_amplitude_cov, error_angle_cov, error_amplitude_sob, error_angle_sob, ...
    error_amplitude_corrcoeff, error_angle_corrcoeff ] = ...
    analyze_each_folder( filename , i, target )

folder_index = i;

err_amplitude_mse = inf( 100, 42 );
err_angle_mse = inf( 100, 42 );
err_amplitude_xcorr = zeros( 100, 42 );
err_angle_xcorr = zeros( 100 , 42 );
err_amplitude_cov = zeros( 100, 42 );
err_angle_cov = zeros( 100, 42 );
err_amplitude_sob = inf( 100, 42 );
err_angle_sob = inf( 100, 42 );
err_amplitude_corrcoeff = zeros( 100, 42 );
err_angle_corrcoeff = zeros( 100, 42 );

parfor i = 1 : 100
    
    for j = 1 : 42
        
        filepath = [ '/s/qitong/btyenv7/', num2str(folder_index),...
             '/', filename, '_m_', num2str( (folder_index-1)*100 + i ), '_', num2str(j), '_Pos1'] ;
        
        if fopen([filepath, '_rts_Rd_1_Rr_1.wav']) ~= -1
            input = audioread([filepath, '_rts_Rd_1_Rr_1.wav']);
            if size(input) == size(target)
                err_amplitude_mse(i, j) = immse(input, target);
                err_angle_mse(i, j) = immse( angle(fft(input)), angle(fft(target)) );
                
                err_amplitude_xcorr(i, j) = xcorr(input, target, 0, 'coeff');
                err_angle_xcorr(i, j) = xcorr(angle(fft(input)), angle(fft(target)), 0, 'coeff');
                
                cov_amplitude = cov( input, target );
                cov_angle = cov( angle(fft(input)), angle(fft(target)) );
                err_amplitude_cov(i, j) = cov_amplitude(1, 2);
                err_angle_cov(i, j) = cov_angle(1, 2);
                
                err_amplitude_sob(i, j) = sobolev( input, target );
                err_angle_sob(i, j) = sobolev( angle(fft(input)), angle(fft(target)) ); 
                
                corrcoeff_amplitude = corrcoef( input, target );
                corrcoeff_angle = corrcoef(angle(fft(input)), angle(fft(target)) );
                err_amplitude_corrcoeff(i, j) = corrcoeff_amplitude(1, 2);
                err_angle_corrcoeff(i, j) = corrcoeff_angle(1, 2);

            end
        end
    
%         fclose('all'); 
    end
    
end

if err_amplitude_mse == inf
    row_amplitude_mse = 0;
    col_amplitude_mse = 0;
    row_angle_mse = 0;
    col_angle_mse = 0;
    row_amplitude_xcorr = 0;
    col_amplitude_xcorr = 0;
    row_angle_xcorr = 0;
    col_angle_xcorr = 0;
    row_amplitude_cov = 0;
    col_amplitude_cov = 0;
    row_angle_cov = 0;
    col_angle_cov = 0;
    row_amplitude_sob = 0;
    col_amplitude_sob = 0;
    row_angle_sob = 0;
    col_angle_sob = 0;
    row_amplitude_corrcoeff = 0;
    col_amplitude_corrcoeff = 0;
    row_angle_corrcoeff = 0;
    col_angle_corrcoeff = 0;
    
    error_amplitude_mse = inf;
    error_angle_mse = inf;
    error_amplitude_xcorr = 0;
    error_angle_xcorr = 0; 
    error_amplitude_cov = 0;
    error_angle_cov = 0;
    error_amplitude_sob = inf;
    error_angle_sob = inf;
    error_amplitude_corrcoeff = 0;
    error_angle_corrcoeff = 0;
    
else
    [row_amplitude_mse, col_amplitude_mse] = find( err_amplitude_mse == min( min(err_amplitude_mse) ) );
    error_amplitude_mse = err_amplitude_mse(row_amplitude_mse, col_amplitude_mse);
    [row_angle_mse, col_angle_mse] = find( err_angle_mse == min( min(err_angle_mse) ) );
    error_angle_mse = err_angle_mse(row_angle_mse, col_angle_mse);
    [row_amplitude_xcorr, col_amplitude_xcorr] = find( err_amplitude_xcorr == max( max(err_amplitude_xcorr) ) );
    error_amplitude_xcorr = err_amplitude_xcorr(row_amplitude_xcorr, col_amplitude_xcorr);
    [row_angle_xcorr, col_angle_xcorr] = find( err_angle_xcorr == max( max(err_angle_xcorr) ) );
    error_angle_xcorr = err_angle_xcorr(row_angle_xcorr, col_angle_xcorr);
    [row_amplitude_cov, col_amplitude_cov] = find( err_amplitude_cov == max( max(err_amplitude_cov) ) );
    error_amplitude_cov = err_amplitude_cov(row_amplitude_cov, col_amplitude_cov);
    [row_angle_cov, col_angle_cov] = find( err_angle_cov == max( max(err_angle_cov) ) );
    error_angle_cov = err_angle_cov(row_angle_cov, col_angle_cov);
    [row_amplitude_sob, col_amplitude_sob] = find( err_amplitude_sob == min( min(err_amplitude_sob) ) );
    error_amplitude_sob = err_amplitude_sob(row_amplitude_sob, col_amplitude_sob);
    [row_angle_sob, col_angle_sob] = find( err_angle_sob == min( min(err_angle_sob) ) );
    error_angle_sob = err_angle_sob(row_angle_sob, col_angle_sob);
    [row_amplitude_corrcoeff, col_amplitude_corrcoeff] = find( err_amplitude_corrcoeff == max( max(err_amplitude_corrcoeff) ) );
    error_amplitude_corrcoeff = err_amplitude_corrcoeff(row_amplitude_corrcoeff, col_amplitude_corrcoeff);
    [row_angle_corrcoeff, col_angle_corrcoeff] = find( err_angle_corrcoeff == max( max(err_angle_corrcoeff) ) );
    error_angle_corrcoeff = err_angle_corrcoeff(row_angle_corrcoeff, col_angle_corrcoeff);
    
end


end