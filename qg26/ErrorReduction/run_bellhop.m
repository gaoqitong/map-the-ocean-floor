function run_bellhop ( filename, i )    

    folder_index = ceil( i / 100 );

    parfor j = 1 : 42
        
        filepath = [ '/s/qitong/btyenv7/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j)] ;
        
        if fopen([filepath, '.arr.mat'])~=-1 && fopen([filepath,'_rts_Rd_1_Rr_1.wav']) == -1
        bellhopMDrift( filepath , 100 , 2 );  
        end
   
    end
    
    
end