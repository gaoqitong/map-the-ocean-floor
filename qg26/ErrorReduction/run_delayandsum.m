function run_delayandsum( filename, i )

folder_index = ceil( i / 100 );

parfor j = 1 : 42
    
        filepath = [ '/s/qitong/btyenv7/', num2str(folder_index),...
                    '/', filename, '_m_', num2str(i), '_', num2str(j), '_Pos1'] ;
        
        if fopen([filepath,'_rts_Rd_1_Rr_1.wav']) == -1
             delayandsum( filepath , 'chirp_0.1s' );
             delete([ filepath, '.arr.mat']);
             delete([ filepath, '_Rd_1.mat']);
             delete(['/s/qitong/btyenv7/', num2str(folder_index),...
            '/', filename, '_m_', num2str(i), '_', num2str(j), '_Pos2.arr.mat']);
        end
        fclose('all');
end


end