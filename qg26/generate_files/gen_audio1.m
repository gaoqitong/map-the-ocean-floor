parfor i = 1 : 100
    
    filename = ['flat_f_', num2str(i)];
    bellhopMDrift( filename , 100 , 2 );  
    
end

parfor i = 1 : 100
    for j = 1 : 1
        filename = ['flat_f_', num2str(i) , '_Pos', num2str(j)];
        delayandsum( filename , 'sin_0.1s' );
    end
end