k = 1;

for i = linspace( -2 , 2 , 10 )
    for j = linspace( 0.7 , 1.8 , 10)
        filename = [ 'valley_' , num2str(k) ];
        drawonevalley( filename, i , j , 0 , 0 );
        k = k + 1;
    end
end

for i = 1 : k - 1
   
    copyfile( 'valley.env' , [ 'valley_' , num2str(i) , '.env' ] );
    
end