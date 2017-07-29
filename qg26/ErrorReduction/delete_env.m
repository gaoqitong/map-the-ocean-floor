function delete_env( filename , i )

folder_index = ceil( i / 100 );

parfor j = 1 : 42
    
    filepath = [ '/home/qg26/Noise/flat/btyenv5/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j) ] ;
        
    if fopen([filepath, '_Pos1_rts_Rd_1_Rr_1.wav']) ~= -1
        delete([filepath, '.env']);
    end
    
    
end

end