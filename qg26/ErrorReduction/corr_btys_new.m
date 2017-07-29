%% 

% Initializing;
filename = 'flat';
load index_July21st_20ErrFcs.mat

global xBot;
BotBTY = '*';   % flag to read from bty file
depthB = inf;
rBox   = inf;
readbty('flat_r', BotBTY ,depthB , rBox);
real = xBot(2, :)';

%% Initialize two matrices

best = zeros(44, 1000);
correlation = zeros(1, 1000);
k = 1;
for i = 1 : 100    
    for j = 1 : 10
        filepath = [ '/s/qitong/btyenv7/', num2str( 1 ), '/' ,filename, '_m_', num2str(i),...
                   '_', num2str(j)];
        best(:, k) = extractbty(filepath);
        corrcoeff = corrcoef(best(:, i), real);
        correlation(k) = corrcoeff(1, 2);      
        k = k + 1;
    end    
end

clear k

%% Find top 1000 btys
for i = 1 : 100000

    folder_index = ceil( i / 100 );
    
        for j = 1 : 42
            
            filepath = [ '/s/qitong/btyenv7/', num2str( folder_index ), '/' ,filename, '_m_', num2str(i),...
                   '_', num2str(j)] ;
            temp_bty = extractbty(filepath);
            temp_xcorr = xcorr(temp_bty, real, 0, 'coeff');
            
            if temp_xcorr > min(min(correlation))
                
                min_index = find( correlation == min(correlation) );
                if size(min_index, 2) > 1
                    min_index = min_index(1);
                end
                
                best(:, min_index) = temp_bty;
                correlation(min_index) = temp_xcorr;
                
            end 
        end
      
end

save('top1000btys_July25th.mat','best');


