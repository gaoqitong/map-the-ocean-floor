% Initializing;
filename = 'flat';
parfor i = 1 : 10000
    
   delete_env( filename, i ); 
    
end