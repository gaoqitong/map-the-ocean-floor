function delete_arr( filename , i )

folder_index = ceil( i / 100 );

parfor j = 1 : 42
    
%     filepath = [ '/home/qg26/Noise/flat/btyenv5/', num2str(folder_index),...
%             '/', filename, '_m_', num2str(i), '_', num2str(j), '_Pos1'] ;
%         
%     if fopen([filepath, '_rts_Rd_1_Rr_1.wav']) ~= -1
%         delete([filepath, '.arr.mat']);
%         delete([filepath, '_Rd_1.mat']);
%     end
    
    delete(['/s/qitong/btyenv7/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j), '_Pos2.arr.mat']);
    
    
end

end