function count_wav( filename , i )

global k;

folder_index = ceil( i / 100 );

for j = 1 : 42
    
    filepath = [ '/s/qitong/btyenv7/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j) ] ;
        
    if fopen([filepath, '_Pos1_rts_Rd_1_Rr_1.wav']) ~= -1
        k = k + 1;
    end
    
    fclose('all');
    
    
end

end