
%% 
% Initializing;
filename = 'flat';
load index_July21st_20ErrFcs.mat
% for i = 1 : 100
%     
%     addpath(num2str(i)); 
%     
% end
%% Plot All
% for i = 1 : 100
%     if index(i, :)~=0
%         filepath = [ filename, '_m_', num2str( (i-1)*100 + index(i, 1) ), '_', num2str( index(i, 2) )] ; 
%         plotbty(filepath)  
%     end       
% end
%% Plot Top 10
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


for i = 1 : 10
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_mse_index(i) ), '/' , filename, '_m_', ...
%         num2str( ( err_amplitude_mse_index(i) - 1 )*100 ...
%         + index_amplitude_mse( err_amplitude_mse_index(i), 1) ), '_', ...
%         num2str( index_amplitude_mse(err_amplitude_mse_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_mse_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_mse_index(i) - 1 )*100 ...
%         + index_angle_mse( err_angle_mse_index(i), 1) ), '_',...
%         num2str( index_angle_mse(err_angle_mse_index(i), 2) )] ;
%     plotbty(filepath);
    
    
    filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_xcorr_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_xcorr_index(i) - 1 )*100 + ...
        index_amplitude_xcorr( err_amplitude_xcorr_index(i), 1) ),...
        '_', num2str( index_amplitude_xcorr(err_amplitude_xcorr_index(i), 2) )] ;
    plotbty(filepath);
    
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_xcorr_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_xcorr_index(i) - 1 )*100 + ...
%         index_angle_xcorr( err_angle_xcorr_index(i), 1) ),...
%         '_', num2str( index_angle_xcorr(err_angle_xcorr_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_cov_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_cov_index(i) - 1 )*100 + ...
%         index_amplitude_cov( err_amplitude_cov_index(i), 1) ),...
%         '_', num2str( index_amplitude_cov(err_amplitude_cov_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_cov_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_cov_index(i) - 1 )*100 + ...
%         index_angle_cov( err_angle_cov_index(i), 1) ),...
%         '_', num2str( index_angle_cov(err_angle_cov_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_sob_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_sob_index(i) - 1 )*100 + ...
%         index_amplitude_sob( err_amplitude_sob_index(i), 1) ),...
%         '_', num2str( index_amplitude_sob(err_amplitude_sob_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_sob_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_sob_index(i) - 1 )*100 + ...
%         index_angle_sob( err_angle_sob_index(i), 1) ),...
%         '_', num2str( index_angle_sob(err_angle_sob_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_amplitude_corrcoeff_index(i) ), '/' ,filename, '_m_', num2str( ( err_amplitude_corrcoeff_index(i) - 1 )*100 + ...
%         index_amplitude_corrcoeff( err_amplitude_corrcoeff_index(i), 1) ),...
%         '_', num2str( index_amplitude_corrcoeff(err_amplitude_corrcoeff_index(i), 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ '/s/qitong/btyenv7/', num2str( err_angle_corrcoeff_index(i) ), '/' ,filename, '_m_', num2str( ( err_angle_corrcoeff_index(i) - 1 )*100 + ...
%         index_angle_corrcoeff( err_angle_corrcoeff_index(i), 1) ),...
%         '_', num2str( index_angle_corrcoeff(err_angle_corrcoeff_index(i), 2) )] ;
%     plotbty(filepath);
    
    
end

%% Plot Randomly


% for i = 1 : 5
% %     
%     a = ceil(rand * 100);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 ...
%         + index_amplitude_mse( a, 1) ), '_', ...
%         num2str( index_amplitude_mse(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 ...
%         + index_angle_mse( a, 1) ), '_',...
%         num2str( index_angle_mse(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_amplitude_xcorr( a, 1) ),...
%         '_', num2str( index_amplitude_xcorr(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_angle_xcorr( a, 1) ),...
%         '_', num2str( index_angle_xcorr(a, 2) )] ;
%     plotbty(filepath);
% 
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_amplitude_cov( a, 1) ),...
%         '_', num2str( index_amplitude_cov(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_angle_cov( a, 1) ),...
%         '_', num2str( index_angle_cov(a, 2) )] ;
%     plotbty(filepath);
% 
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_amplitude_sob( a, 1) ),...
%         '_', num2str( index_amplitude_sob(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_angle_sob( a, 1) ),...
%         '_', num2str( index_angle_sob(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_amplitude_corrcoeff( a, 1) ),...
%         '_', num2str( index_amplitude_corrcoeff(a, 2) )] ;
%     plotbty(filepath);
%     
%     filepath = [ filename, '_m_', num2str( ( a - 1 )*100 + ...
%         index_angle_corrcoeff( a, 1) ),...
%         '_', num2str( index_angle_corrcoeff(a, 2) )] ;
%     plotbty(filepath);
%     
% end


