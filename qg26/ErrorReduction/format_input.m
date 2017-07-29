function [input_j] = format_input( filename , i )

folder_index = ceil( i / 100 );
input_j = cell(1, 42);

parfor j = 1 : 42
    
    filepath = [ '/home/qg26/Noise/flat/btyenv5/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j), '_Pos1'] ;
        
    if fopen([filepath, '_rts_Rd_1_Rr_1.wav']) ~= -1
       input_j{j} = audioread([filepath, '_rts_Rd_1_Rr_1.wav']);
    else
       input_j{j} = nan;
    end
        
    
end

fclose('all');

end