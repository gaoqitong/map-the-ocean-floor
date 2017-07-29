function gen_files( filename, merged, sizeofreal, noise, i )

    folder_index = ceil( i / 100 );
    

    parfor j = 1 : sizeofreal
        filepath = [ '/s/qitong/btyenv7/', num2str(folder_index), '/', filename, '_m_', num2str(i), '_', num2str(j)] ;
        out = merged;
        out(2, :) = noise(j, :) * 800;
        dlmwrite([ filepath, '.bty' ], '''L''', '');
        fid = fopen([ filepath, '.bty' ], 'a');
        fprintf(fid, '%g\n', sizeofreal );
        fprintf(fid, '%5.2f %5.2f\n', out);
        copyfile('flat.env', [ filepath , '.env' ]);
        fclose('all');
        
    end


end