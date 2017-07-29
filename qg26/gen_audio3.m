for i = 51 : 75
    
    filename = ['bump_', num2str(i), '_blurred'];
    bellhopMDrift( filename , 100 , 2 );  
    
    filename = ['bump_', num2str(i), '_real'];
    bellhopMDrift( filename , 100 , 2 );
    
end

for i = 51 : 75
    
    for j = 1 : 1
        filename = ['bump_', num2str(i), '_blurred_Pos', num2str(j)];
        delayandsum( filename , 'sin_0.1s' );
    end
    
    for j = 1 : 1
        filename = ['bump_', num2str(i), '_real_Pos', num2str(j)];
        delayandsum( filename , 'sin_0.1s' );
    end
end